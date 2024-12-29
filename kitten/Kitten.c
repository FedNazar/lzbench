/*
	Kitten Compression Library
	Copyright (C) 2024 Nazar Fedorenko

	Licensed under the BSD 2-Clause License.

	Redistribution and use in source and binary forms, with or without modification, are permitted 
	provided that the following conditions are met:

	1. Redistributions of source code must retain the above copyright notice, this list of conditions 
	and the following disclaimer.

	2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions 
	and the following disclaimer in the documentation and/or other materials provided with the distribution.

	THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY EXPRESS OR IMPLIED 
	WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A 
	PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
	ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
	LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
	INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
	TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF 
	ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "Kitten.h"

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

// Large file (over 4 GB) support
#define __USE_LARGEFILE64
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE

#include <sys/stat.h>

#ifndef _WIN32
	#define _stat64 stat64
#endif

#define KITTEN_FILE_IO_CHUNK_SIZE 2000000000

// Branch prediction optimization
#define LIKELY(x) __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

#define KITTEN_TINY_OFFSET_TYPE 64
#define KITTEN_SMALL_OFFSET_TYPE 128
#define KITTEN_LARGE_OFFSET_TYPE 192

#define KITTEN_TOKEN_TYPES 4

const uint32_t KITTEN_OFFSET_MASKS[KITTEN_TOKEN_TYPES] = 
    { 0, 255, 65535, 16777215 };
const uint32_t KITTEN_MIN_DATA_SLICE_LENGTH[KITTEN_TOKEN_TYPES] = 
    { 1, 4, 4, 4 };

#define KITTEN_MIN_UNCOMPRESSED_SIZE 1024

#define KITTEN_SIGNATURE_AND_VERSION_SIZE KITTEN_SIGNATURE_SIZE + sizeof(uint16_t)
#define KITTEN_FILE_HEADER_SIZE 16

const char* KITTEN_SIGNATURE = "kitten";
const uint16_t KITTEN_BYTESTREAM_VERSION = 0;

#define KITTEN_SIGNATURE_SIZE 6

const uint32_t KITTEN_MAX_OFFSETS[KITTEN_COMPRESSION_LEVELS] =
{
    65536 * 2,
    65536 * 4,
    65536 * 8,
    65536 * 16,
    65536 * 24,
    65536 * 32,
    65536 * 40,
    65536 * 48,
    65536 * 56,
    65536 * 64,
    65536 * 80,
    65536 * 96,
    65536 * 112,
    65536 * 128,
	65536 * 144,
    65536 * 160,
	65536 * 176,
    65536 * 192,
	65536 * 208,
    65536 * 224,
	65536 * 240,
    65536 * 256 - 1
};

typedef struct HashTableChainNode
{
    unsigned char* patternPtr;
    struct HashTableChainNode* next;
    struct HashTableChainNode* prev;
} HashTableChainNode;

typedef struct HashTable
{
    HashTableChainNode** chains;
    HashTableChainNode* nodes;

    uint32_t capacity;
    uint32_t numOfNodes;
    uint32_t chainLengthLimit;
    uint8_t dataSliceSize;

    unsigned long* lastHashes;
    int curNode;

    bool allNodesAreOccupied;
} HashTable;

// Custom vectorized memory copy function capable of copying up to 64 bytes
inline void MemCopy(unsigned char* dst, unsigned char* src, unsigned short size)
{
    __builtin_prefetch((const void*)(src), 0);

    #if defined(__AVX__)
        memcpy(dst, src, 32);
        if (size >= 32) memcpy(dst + 32, src + 32, 32);
    #elif defined(__SSE__)
        if (size <= 16)
            memcpy(dst, src, 16);
        else if (size <= 32)
            memcpy(dst, src, 32);
        else if (size <= 48)
            memcpy(dst, src, 48);
        else if (size <= 64)
            memcpy(dst, src, 64);
    #else
        memcpy(dst, src, size);
    #endif
}

// Determines the number of chains in a hash table
inline uint32_t HashTableCapacity(uint32_t maxOffset, uint32_t multiplier)
{
    return (1 << (uint32_t)(ceil(log2(maxOffset * multiplier))));
}

// FNV-1a hash function
inline uint32_t HashTableIndex(const unsigned char* patternStartPos,
    const HashTable* hashTable)
{
    uint32_t hash = 2166136261;

    for (uint8_t i = 0; i < hashTable->dataSliceSize; i++)
    {
        hash ^= patternStartPos[i];
		hash *= 16777619;
    }

    return hash & (hashTable->capacity - 1);
}

inline void AddEntryToHashTable(HashTable* hashTable, unsigned char* patternStartPos)
{
    unsigned long hashIndex = HashTableIndex(patternStartPos, hashTable);
    HashTableChainNode* nextNode;

    if (UNLIKELY(!(hashTable->allNodesAreOccupied)))
    {
        nextNode = hashTable->chains[hashIndex];
        hashTable->chains[hashIndex] = &hashTable->nodes[hashTable->curNode];

        hashTable->chains[hashIndex]->next = nextNode;
        hashTable->chains[hashIndex]->patternPtr = patternStartPos;
        hashTable->chains[hashIndex]->prev = NULL;
    }
    else
    {
        HashTableChainNode* node = &hashTable->nodes[hashTable->curNode];

        if (node->prev == NULL)
        {
            unsigned long oldHashIndex = hashTable->lastHashes[hashTable->curNode];
            hashTable->chains[oldHashIndex] = NULL;
        }
        else
        {
            node->prev->next = NULL;
        }

        nextNode = hashTable->chains[hashIndex];
        hashTable->chains[hashIndex] = node;

        node->next = nextNode;
        node->patternPtr = patternStartPos;
        node->prev = NULL;
    }

    if (nextNode != NULL) nextNode->prev = hashTable->chains[hashIndex];

    hashTable->lastHashes[hashTable->curNode] = hashIndex;
    hashTable->curNode++;

    if (hashTable->curNode == hashTable->numOfNodes)
    {
        hashTable->curNode = 0;
        hashTable->allNodesAreOccupied = true;
    }
}

inline uint64_t WriteToFile(unsigned char* buffer, uint64_t bufferSize, FILE* file)
{
	uint64_t wholeChunks = bufferSize / KITTEN_FILE_IO_CHUNK_SIZE;
	
	// Write whole chunks
	for (uint64_t curChunk = 0; curChunk < wholeChunks; curChunk++)
	{
		if (fwrite(buffer, 1, KITTEN_FILE_IO_CHUNK_SIZE, file) < KITTEN_FILE_IO_CHUNK_SIZE)
			return 0;
		
		buffer += KITTEN_FILE_IO_CHUNK_SIZE;
	}
	
	// Write remainder
	uint64_t remainder = bufferSize % KITTEN_FILE_IO_CHUNK_SIZE;
	if (remainder != 0)
	{
		if (fwrite(buffer, 1, remainder, file) < remainder)
			return 0;
	}
	
	return bufferSize;
}

/*
	Compresses data from an input buffer to an output buffer.
	
	Min. input data size (KITTEN_MIN_UNCOMPRESSED_SIZE constant) is
	8192 bytes.
	
	Compression levels: 1-22. You can also use compLevel argument
	to specify custom max. offset value (8192-16777215).
	
	Memory usage levels: 0-4. Higher value decreases compression
	times but increases RAM consumption. Default (KITTEN_DEFAULT_MEMORY_USAGE_LEVEL
	constant) is 3.
	
	Min. match length range: 4-62. Default (KITTEN_DEFAULT_MIN_MATCH_LENGTH
	constant) is 5. Higher value decreases compression times but 
	degrades compression ratio.
	
	Hash table chain length limit range: 1-16777215. Zero
	(KITTEN_NO_HASH_TABLE_CHAIN_LIMIT constant) disables the limit.
	Lower value decreases compression times but degrades compression ratio.
	
	Returns the size of the decompressed size. Possible errors: 
	KITTEN_INCOMPRESSIBLE_DATA_ERROR, KITTEN_TOO_SMALL_DATA_ERROR,
	KITTEN_PARAMETER_ERROR, KITTEN_MEMORY_ERROR.
*/
uint64_t KittenCompress(unsigned char* inputData, unsigned char* outputData,
    uint64_t inputDataSize, uint32_t comprLevel, uint8_t memoryUsageLevel,
    uint8_t minMatchLength, uint32_t hashTableChainLengthLimit)
{
    if (inputDataSize <= KITTEN_MIN_UNCOMPRESSED_SIZE)
        return KITTEN_TOO_SMALL_DATA_ERROR;

    if (memoryUsageLevel >= KITTEN_MEMORY_USAGE_LEVELS)
        return KITTEN_PARAMETER_ERROR;

    if (minMatchLength < KITTEN_MIN_CUSTOM_MATCH_LENGTH ||
        minMatchLength > KITTEN_MAX_MATCH_LENGTH - 1)
        return KITTEN_PARAMETER_ERROR;

    uint32_t maxOffset;
    if (comprLevel > 0 && comprLevel <= KITTEN_COMPRESSION_LEVELS)
        maxOffset = KITTEN_MAX_OFFSETS[comprLevel - 1];
    // Custom offset
    else if (comprLevel >= KITTEN_MIN_CUSTOM_MAX_OFFSET &&
        comprLevel <= KITTEN_MAX_CUSTOM_MAX_OFFSET)
        maxOffset = comprLevel;
    else return KITTEN_PARAMETER_ERROR;

    // Initialize hash table

    HashTable hashTable;
    hashTable.capacity = HashTableCapacity(maxOffset, 1 << memoryUsageLevel);
    hashTable.numOfNodes = maxOffset;
    hashTable.dataSliceSize = minMatchLength;
    hashTable.chainLengthLimit = (hashTableChainLengthLimit != KITTEN_NO_HASH_TABLE_CHAIN_LIMIT) ?
        hashTableChainLengthLimit : maxOffset;
    hashTable.curNode = 0;
    hashTable.allNodesAreOccupied = false;

    hashTable.chains = (HashTableChainNode**)calloc(
        hashTable.capacity, sizeof(HashTableChainNode*));
    if (hashTable.chains == NULL) return KITTEN_MEMORY_ERROR;

    hashTable.nodes = (HashTableChainNode*)malloc(sizeof(HashTableChainNode) *
        maxOffset);
    if (hashTable.nodes == NULL)
    {
        free(hashTable.chains);
        return KITTEN_MEMORY_ERROR;
    }

    hashTable.lastHashes = (unsigned long*)malloc(sizeof(unsigned long) * maxOffset);
    if (hashTable.lastHashes == NULL)
    {
        free(hashTable.nodes);
        free(hashTable.chains);
        return KITTEN_MEMORY_ERROR;
    }

    hashTable.curNode = 0;

    uint64_t inputDataPos = 0, outputDataPos = 0;

    uint16_t curLiteralLength;
    uint64_t literalPos;

    uint16_t curMatchLength, matchLength = 0;
    uint64_t matchPos;

    unsigned char* longestMatch = NULL;

    // Main loop

    while (inputDataPos < inputDataSize - minMatchLength)
    {
        curLiteralLength = 0;
        literalPos = inputDataPos;
        matchPos = inputDataPos;
        curMatchLength = minMatchLength;

        longestMatch = NULL;

        while (curLiteralLength < KITTEN_MAX_LITERAL_LENGTH &&
               curMatchLength < KITTEN_MAX_MATCH_LENGTH &&
               inputDataPos < inputDataSize - minMatchLength)
        {
            unsigned long hashIndex = HashTableIndex(
                inputData + inputDataPos, &hashTable);
            matchPos = inputDataPos;

			// Scan current data slice for a match
            HashTableChainNode* curHashTableChainNode = hashTable.chains[hashIndex];
            uint32_t chainDepth = 0;
            while (curHashTableChainNode != NULL)
            {
                if (chainDepth >= hashTable.chainLengthLimit)
                    break;

                if (UNLIKELY(curHashTableChainNode->patternPtr <
                    &inputData[matchPos] - maxOffset))
                    break;

                if (UNLIKELY(curHashTableChainNode->patternPtr >
                    &inputData[matchPos] - curMatchLength))
                {
                    curHashTableChainNode = curHashTableChainNode->next;
                    continue;
                }

                if (!memcmp(&inputData[matchPos], curHashTableChainNode->patternPtr,
                    curMatchLength)) // Match found
                {
                    bool breakTheLoop = false;

                    // Search for the longest match
                    do
                    {
                        longestMatch = curHashTableChainNode->patternPtr;
                        matchLength = curMatchLength;

                        if (UNLIKELY(matchPos + curMatchLength >= inputDataSize))
                        {
                            breakTheLoop = true;
                            break;
                        }

                        if (UNLIKELY(curMatchLength >= KITTEN_MAX_MATCH_LENGTH))
                        {
                            breakTheLoop = true;
                            break;
                        }

                        curMatchLength++;

                        if (UNLIKELY(curHashTableChainNode->patternPtr >
                            &inputData[matchPos] - curMatchLength))
                            break;
                    } while (curHashTableChainNode->patternPtr[curMatchLength - 1] ==
                        inputData[matchPos + curMatchLength - 1]);

                    if (breakTheLoop)
                        break;
                }

                curHashTableChainNode = curHashTableChainNode->next;
                chainDepth++;
            }

            if (longestMatch != NULL && matchLength == 4 &&
                &inputData[matchPos] - longestMatch > 65535)
                longestMatch = NULL;

            if (longestMatch != NULL)
            {
                break;
            }
            else
            {
                AddEntryToHashTable(&hashTable, &inputData[inputDataPos]);

                inputDataPos++;
                curLiteralLength++;
            }
        }

        if (curLiteralLength != 0) // Encode literal
        {
            outputData[outputDataPos] = curLiteralLength;
            outputDataPos++;
            MemCopy(&outputData[outputDataPos], &inputData[literalPos], curLiteralLength);
            outputDataPos += curLiteralLength;
        }

        if (longestMatch != NULL) // Encode match
        {
            outputData[outputDataPos] = matchLength;
            outputDataPos++;

            uint32_t offset = &inputData[matchPos] - longestMatch;

            if (offset < 256) // Tiny offset (1 byte)
            {
                outputData[outputDataPos - 1] |= KITTEN_TINY_OFFSET_TYPE;
                memcpy(&outputData[outputDataPos], &offset, 1);
                outputDataPos++;
            }
            else if (offset < 65536) // Small offset (2 bytes)
            {
                outputData[outputDataPos - 1] |= KITTEN_SMALL_OFFSET_TYPE;
                memcpy(&outputData[outputDataPos], &offset, 2);
                outputDataPos += 2;
            }
            else // Large offset (3 bytes)
            {
                outputData[outputDataPos - 1] |= KITTEN_LARGE_OFFSET_TYPE;
                memcpy(&outputData[outputDataPos], &offset, 4);
                outputDataPos += 3;
            }

            for (int i = 0; i < matchLength; i++)
            {
                AddEntryToHashTable(&hashTable, &inputData[inputDataPos]);
                inputDataPos++;
            }
        }

        if (outputDataPos >= inputDataSize - KITTEN_SAFE_DISTANCE)
        {
            // Deinitialize hash table

            free(hashTable.lastHashes);
            free(hashTable.nodes);
            free(hashTable.chains);

            return KITTEN_INCOMPRESSIBLE_DATA_ERROR;
        }
    }

    uint64_t remainder = inputDataSize - inputDataPos;
    if (remainder != 0)
    {
        outputData[outputDataPos] = remainder;
        outputDataPos++;
        MemCopy(&outputData[outputDataPos], &inputData[inputDataPos], remainder);
        outputDataPos += remainder;
    }

    // Deinitialize hash table

    free(hashTable.lastHashes);
    free(hashTable.nodes);
    free(hashTable.chains);

    return outputDataPos; // Size of compressed data
}

/*
	Compresses a file and writes it to a new file.
	
	Min. input data size (KITTEN_MIN_UNCOMPRESSED_SIZE constant) is
	8192 bytes.
	
	Compression levels: 1-22. You can also use compLevel argument
	to specify custom max. offset value (8192-16777215).
	
	Memory usage levels: 0-4. Higher value decreases compression
	times but increases RAM consumption. Default (KITTEN_DEFAULT_MEMORY_USAGE_LEVEL
	constant) is 3.
	
	Min. match length range: 4-62. Default (KITTEN_DEFAULT_MIN_MATCH_LENGTH
	constant) is 5. Higher value decreases compression times but 
	degrades compression ratio.
	
	Hash table chain length limit range: 1-16777215. Zero
	(KITTEN_NO_HASH_TABLE_CHAIN_LIMIT constant) disables the limit.
	Lower value decreases compression times but degrades compression ratio.
	
	Returns the size of the compressed size. Possible errors: 
	KITTEN_INCOMPRESSIBLE_DATA_ERROR, KITTEN_TOO_SMALL_DATA_ERROR,
	KITTEN_PARAMETER_ERROR, KITTEN_MEMORY_ERROR,
	KITTEN_FILE_READ_ERROR, KITTEN_FILE_WRITE_ERROR.
*/
uint64_t KittenCompressFile(const char* inFile, const char* outFile,
    uint32_t comprLevel, uint8_t memoryUsageLevel,
    uint8_t minMatchLength, uint32_t hashTableChainLengthLimit)
{
    FILE* uncompressedFile = fopen(inFile, "rb");
    if (uncompressedFile == NULL) return KITTEN_FILE_READ_ERROR;

    // Get input file size
	
	struct _stat64 fileStat;
	int statResult = _stat64(inFile, &fileStat);
	
	if (statResult < 0)
	{
		fclose(uncompressedFile);
        return KITTEN_FILE_READ_ERROR;
	}
	
	uint64_t inputDataSize = fileStat.st_size;

    unsigned char* inputData = (unsigned char*)malloc(KittenBufferSize(inputDataSize));
    if (inputData == NULL)
    {
        fclose(uncompressedFile);
        return KITTEN_MEMORY_ERROR;
    }

	// Read the file into memory

    if (fread(inputData, 1, inputDataSize, uncompressedFile) < inputDataSize)
    {
        fclose(uncompressedFile);
        free(inputData);
        return KITTEN_FILE_READ_ERROR;
    }

    fclose(uncompressedFile);

    unsigned char* outputData = (unsigned char*)malloc(KITTEN_FILE_HEADER_SIZE +
        KittenBufferSize(inputDataSize));
    if (outputData == NULL)
    {
        free(inputData);
        return KITTEN_MEMORY_ERROR;
    }

	// Check file signature
    memcpy(outputData, KITTEN_SIGNATURE, KITTEN_SIGNATURE_SIZE);
    memcpy(outputData + KITTEN_SIGNATURE_SIZE, &KITTEN_BYTESTREAM_VERSION,
        sizeof(KITTEN_BYTESTREAM_VERSION));
		
	// Get output data size
    memcpy(outputData + KITTEN_SIGNATURE_AND_VERSION_SIZE, &inputDataSize,
        sizeof(inputDataSize));

    uint64_t result = KittenCompress(inputData, outputData + KITTEN_FILE_HEADER_SIZE,
        inputDataSize, comprLevel, memoryUsageLevel, minMatchLength, hashTableChainLengthLimit);

    free(inputData);

    if (result < KITTEN_ERRORS)
    {
        free(outputData);
        return result;
    }

	// Write to a new file

    FILE* compressedFile = fopen(outFile, "wb");
    if (compressedFile == NULL)
    {
        free(outputData);
        return KITTEN_FILE_WRITE_ERROR;
    }

    if (WriteToFile(outputData, KITTEN_FILE_HEADER_SIZE + result, compressedFile) == 0)
    {
        fclose(compressedFile);
        free(outputData);
        return KITTEN_FILE_WRITE_ERROR;
    }

    fclose(compressedFile);

    free(outputData);

    return result;
}

/*
	Decompresses data from an input buffer and writes it to an output buffer.
	
	Returns the size of the decompressed size. Possible error - 
	KITTEN_DECOMPRESSION_ERROR.
*/
uint64_t KittenDecompress(unsigned char* inputData, unsigned char* outputData,
    uint64_t inputDataSize, uint64_t outputDataSize)
{
    uint64_t inputDataPos = 0, outputDataPos = 0;

    while (LIKELY(outputDataPos < outputDataSize && inputDataPos < inputDataSize))
    {
		// Get token metadata
		
        uint8_t dataSliceLength = inputData[inputDataPos];
        inputDataPos++;

        uint32_t tokenType = dataSliceLength >> 6;
        uint32_t matchOffset = *(const uint32_t*)(&inputData[inputDataPos]) & KITTEN_OFFSET_MASKS[tokenType];
        dataSliceLength &= 63;

        bool isMatch = tokenType >= 1;

        // Integrity checks
        if (UNLIKELY(matchOffset > outputDataPos)) return KITTEN_DECOMPRESSION_ERROR;
        if (UNLIKELY(dataSliceLength < KITTEN_MIN_DATA_SLICE_LENGTH[tokenType])) return KITTEN_DECOMPRESSION_ERROR;
        if (UNLIKELY(isMatch && matchOffset < dataSliceLength)) return KITTEN_DECOMPRESSION_ERROR;

		// Write decompressed data
        MemCopy(&outputData[outputDataPos], (isMatch) ? &outputData[outputDataPos] -
               matchOffset : &inputData[inputDataPos], dataSliceLength);
			   
        inputDataPos += (isMatch) ? tokenType : dataSliceLength;
        outputDataPos += dataSliceLength;
    }

    if (inputDataPos != inputDataSize || outputDataPos != outputDataSize)
        return KITTEN_DECOMPRESSION_ERROR;

    return outputDataPos; // Size of decompressed data
}

/*
	Decompresses data from a file and writes it to an output buffer.
	
	This function allocates an output buffer internally and returns a
	pointer to it. Use KittenDeallocate() or free() to deallocate
	that memory.
	
	decompBytesOrErrorCode argument takes a pointer to a variable where
	the size of the decompressed data or an error code will get stored.
	
	Possible errors: KITTEN_DECOMPRESSION_ERROR, KITTEN_FILE_READ_ERROR,
	KITTEN_FILE_WRITE_ERROR, KITTEN_MEMORY_ERROR, KITTEN_SIGNATURE_ERROR.
*/
unsigned char* KittenDecompressFileToMemory(const char* inFile,
    uint64_t* decompBytesOrErrorCode)
{
    if (decompBytesOrErrorCode == NULL) return NULL;

    FILE* compressedFile = fopen(inFile, "rb");
    if (compressedFile == NULL)
    {
        *decompBytesOrErrorCode = KITTEN_FILE_READ_ERROR;
        return NULL;
    }

    // Get input file size
	
	struct _stat64 fileStat;
	int statResult = _stat64(inFile, &fileStat);
	
	if (statResult < 0)
	{
		fclose(compressedFile);
        *decompBytesOrErrorCode = KITTEN_FILE_READ_ERROR;
		return NULL;
	}
	
	uint64_t inputDataSize = fileStat.st_size;

    if (inputDataSize < KITTEN_FILE_HEADER_SIZE)
    {
        fclose(compressedFile);
        *decompBytesOrErrorCode = KITTEN_DECOMPRESSION_ERROR;
        return NULL;
    }

    unsigned char* inputData = (unsigned char*)malloc(sizeof(unsigned char) *
        (KittenBufferSize(inputDataSize)));
    if (inputData == NULL)
    {
        fclose(compressedFile);
        *decompBytesOrErrorCode = KITTEN_MEMORY_ERROR;
        return NULL;
    }
	
	// Read the file into memory

    if (fread(inputData, 1, inputDataSize, compressedFile) < inputDataSize)
    {
        fclose(compressedFile);
        free(inputData);
        *decompBytesOrErrorCode = KITTEN_FILE_READ_ERROR;
        return NULL;
    }

    fclose(compressedFile);

    // Check signature
    if (memcmp(inputData, KITTEN_SIGNATURE, KITTEN_SIGNATURE_SIZE) &&
        *(const uint16_t*)(inputData + KITTEN_SIGNATURE_SIZE) != KITTEN_BYTESTREAM_VERSION)
    {
        free(inputData);
        *decompBytesOrErrorCode = KITTEN_SIGNATURE_ERROR;
        return NULL;
    }

    // Get uncompressed data size
    uint64_t outputDataSize = *(const uint64_t*)(inputData + KITTEN_SIGNATURE_AND_VERSION_SIZE);
    if (outputDataSize < KITTEN_MIN_UNCOMPRESSED_SIZE)
    {
        free(inputData);
        *decompBytesOrErrorCode = KITTEN_DECOMPRESSION_ERROR;
        return NULL;
    }

    unsigned char* outputData = (unsigned char*)malloc(sizeof(unsigned char) *
        (KittenBufferSize(outputDataSize)));
    if (outputData == NULL)
    {
        free(inputData);
        *decompBytesOrErrorCode = KITTEN_MEMORY_ERROR;
        return NULL;
    }

    uint64_t result = KittenDecompress(inputData + KITTEN_FILE_HEADER_SIZE,
        outputData, inputDataSize - KITTEN_FILE_HEADER_SIZE, outputDataSize);

    free(inputData);

    if (result == KITTEN_DECOMPRESSION_ERROR)
    {
        free(outputData);
        *decompBytesOrErrorCode = KITTEN_DECOMPRESSION_ERROR;
        return NULL;
    }

    *decompBytesOrErrorCode = result;
    return outputData;
}

/*
	Decompresses data from a file and writes it to a new file.
	
	Returns the size of the decompressed size. Possible errors: 
	KITTEN_DECOMPRESSION_ERROR, KITTEN_FILE_READ_ERROR,
	KITTEN_FILE_WRITE_ERROR, KITTEN_MEMORY_ERROR, KITTEN_SIGNATURE_ERROR.
*/
uint64_t KittenDecompressFile(const char* inFile, const char* outFile)
{
    uint64_t result;
    unsigned char* outputData = KittenDecompressFileToMemory(inFile, &result);

    if (outputData == NULL) return result;

    FILE* decompressedFile = fopen(outFile, "wb");
    if (decompressedFile == NULL)
    {
        KittenDeallocate(outputData);
        return KITTEN_FILE_WRITE_ERROR;
    }
	
    if (WriteToFile(outputData, result, decompressedFile) == 0)
    {
        fclose(decompressedFile);
        KittenDeallocate(outputData);
        return KITTEN_FILE_WRITE_ERROR;
    }

    fclose(decompressedFile);

    KittenDeallocate(outputData);

    return result;
}

/* 
	Adds 128 bytes of additional safe distance memory to provided buffer size.
	Used to get buffer size when allocating compression and decompression buffers. 
*/
uint64_t KittenBufferSize(uint64_t dataSize)
{
    return dataSize + KITTEN_SAFE_DISTANCE;
}

/* 
	Deallocates buffer allocated by KittenDecompressFileToMemory().
	free() can also be used for this. This function serves as a
	syntactic sugar. 
*/
void KittenDeallocate(unsigned char* buffer)
{
    free(buffer);
}
