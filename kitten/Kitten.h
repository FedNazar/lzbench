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

#pragma once

#include <stdint.h>

#define KITTEN_VERSION "1.0"

#define KITTEN_MAX_LITERAL_LENGTH 63
#define KITTEN_MAX_MATCH_LENGTH 63
#define KITTEN_MIN_MATCH_LENGTH 5

#define KITTEN_MIN_CUSTOM_MAX_OFFSET 8192
#define KITTEN_MAX_CUSTOM_MAX_OFFSET 16777215
#define KITTEN_MIN_CUSTOM_MATCH_LENGTH 4
#define KITTEN_MAX_CUSTOM_MATCH_LENGTH 62

#define KITTEN_DEFAULT_MIN_MATCH_LENGTH 5
#define KITTEN_DEFAULT_MEMORY_USAGE_LEVEL 3
#define KITTEN_NO_HASH_TABLE_CHAIN_LIMIT 0

#define KITTEN_COMPRESSION_LEVELS 22

#define KITTEN_ERRORS 8

#define KITTEN_INCOMPRESSIBLE_DATA_ERROR 0
#define KITTEN_DECOMPRESSION_ERROR 1
#define KITTEN_MEMORY_ERROR 2
#define KITTEN_SIGNATURE_ERROR 3
#define KITTEN_PARAMETER_ERROR 4
#define KITTEN_TOO_SMALL_DATA_ERROR 5
#define KITTEN_FILE_READ_ERROR 6
#define KITTEN_FILE_WRITE_ERROR 7

#define KITTEN_MEMORY_USAGE_LEVELS 5

#define KITTEN_SAFE_DISTANCE 128

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
    uint8_t minMatchLength, uint32_t hashTableChainLengthLimit);	
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
    uint8_t minMatchLength, uint32_t hashTableChainLengthLimit);

/*
	Decompresses data from an input buffer and writes it to an output buffer.
	
	Returns the size of the decompressed size. Possible error - 
	KITTEN_DECOMPRESSION_ERROR.
*/
uint64_t KittenDecompress(unsigned char* inputData, unsigned char* outputData,
    uint64_t inputDataSize, uint64_t outputDataSize);
/*
	Decompresses data from a file and writes it to a new file.
	
	Returns the size of the decompressed size. Possible errors: 
	KITTEN_DECOMPRESSION_ERROR, KITTEN_FILE_READ_ERROR,
	KITTEN_FILE_WRITE_ERROR, KITTEN_MEMORY_ERROR, KITTEN_SIGNATURE_ERROR.
*/
uint64_t KittenDecompressFile(const char* inFile, const char* outFile);
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
    uint64_t* decompBytesOrErrorCode);

/*
	Adds 128 bytes of additional safe distance memory to provided buffer size.
	Used to get buffer size when allocating compression and decompression buffers. 
*/
uint64_t KittenBufferSize(uint64_t dataSize);

/* 
	Deallocates buffer allocated by KittenDecompressFileToMemory().
	free() can also be used for this. 
*/
void KittenDeallocate(unsigned char* buffer);