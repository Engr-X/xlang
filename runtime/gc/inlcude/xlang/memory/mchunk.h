#ifndef _XLANG_MCHUNK_H_
#define _XLANG_MCHUNK_H_

#define CHUNK_INFO


struct chunk_info
{
    short class_size;
    short slot;
};

extern struct chunk_info ksize_classes[24] = {
    {8,    8192},
    {16,   4096},
    {24,   2688},
    {32,   2048},
    {40,   1600},
    {48,   1344},
    {56,   1152},
    {64,   1024},

    {80,    768},
    {96,    640},
    {112,   576},
    {128,   512},

    {160,   768},
    {192,   640},
    {224,   576},
    {256,   512},

    {320,   384},
    {384,   320},
    {448,   256},
    {512,   256},

    {768,   320},
    {1024,  256},
    {1536,  128},
    {2048,  128},
};


struct chunk
{
    struct chunk_info info;
    
    void* memory;
};



#endif