
//#include "audio.h"
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void *roc_alloc(size_t size)
{
    return malloc(size);
}

void *roc_realloc(void *ptr, size_t new_size, size_t old_size, unsigned int alignment)
{
    return realloc(ptr, new_size);
}

void roc_dealloc(void *ptr, unsigned int alignment)
{
    free(ptr);
}

void *roc_memset(void *str, int c, size_t n)
{
    return memset(str, c, n);
}





// struct RocStr { char *bytes; size_t len; size_t capacity; };
// const size_t SLICE_BIT = ((size_t)1) << (8 * sizeof(size_t) - 1);
// const size_t REFCOUNT_MAX = 0;
// int main()
// {
//     struct RocStr str1;

//     // int  res =  runAudio();
//     // if (res)
//     // {
//     //     printf("audio  problem  %d\n", res);

//     // }
//     char *zink  = (char*)malloc(10);
//     size_t rc = REFCOUNT_MAX;
//     size_t slice_bits = (((size_t)&rc) >> 1) | SLICE_BIT;


//     struct RocStr  str2 = { zink, 10, slice_bits };
//     float input[300];
//     roc__mainForHost_1_exposed_generic(&str1, &str2);

//     for (int i=0; i<str1.len && i<3000; ++i)
//        putchar(str1.bytes[i]);


//     printf("sfsdfsfds\n");
// }



struct RocStr { char *bytes; size_t len; size_t capacity; };
struct RocList { float *data; size_t len; size_t capacity; };
const size_t SLICE_BIT = ((size_t)1) << (8 * sizeof(size_t) - 1);
const size_t REFCOUNT_MAX = 0;
int main()
{
float inputs[100];

    struct RocStr out;
    struct RocList in;

    in.data = inputs;
    in.len = 100;

    // This is not actually the capacity but something special for seamless slices.
    size_t rc = REFCOUNT_MAX;
    size_t slice_bits = (((size_t)&rc) >> 1) | SLICE_BIT;
    in.capacity = slice_bits;

    roc__mainForHost_1_exposed_generic(&out, &in);
    const char *bytes;
    size_t len;
	if (((ptrdiff_t)out.capacity) < 0) {
		bytes = (const char*)&out;
		len = (size_t)(bytes[sizeof(size_t) * 3 - 1] & 0x7F);
	} else {
		bytes = out.bytes;
		len = (out.len << 1) >> 1;
	}

    for (int i=0; i<len ; ++i)
       putchar(bytes[i]);


    printf("sfsdfsfds\n");
}


