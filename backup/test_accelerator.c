#include "rocc.h"
#include <stdio.h>

// static inline void accum_write(int idx, unsigned long data)
// {
// 	ROCC_INSTRUCTION_SS(0, data, idx, 0);
// }

// static inline int accum_read()
// {
// 	int value;
// 	ROCC_INSTRUCTION_D(0, value, 2);
// 	return value;
// }

static inline void accum_write( void *ptrLoad, void *ptrStore, int idx)
{
    asm volatile("fence");
    // ROCC_INSTRUCTION_SS(0, (uintptr_t)ptrLoad, (uintptr_t)ptrStore, 1);
    ROCC_INSTRUCTION_SS(0, (uintptr_t)ptrLoad, idx, 2);
    asm volatile("fence");
}

// static inline void accum_store(void *ptr)
// {
//     asm volatile ("fence");
//     int DNC; // Dummy No-Compute
//     ROCC_INSTRUCTION_DS(0, DNC , (uintptr_t) ptr, 3);
// }

// static inline void accum_add(int idx, unsigned long addend)
// {
// 	ROCC_INSTRUCTION_SS(0, addend, idx, 3);
// }

// unsigned long data = 0x3421L;
// int data1 = 12;
volatile long data = 15;
volatile long result = 0;

int main(void)
{

    printf("Testing write operation. R1\n");

    printf("DEBUG: data[0] at %ld\n", (long)&data);
    printf("DEBUG: result at %ld\n", (long)&result);

    accum_write((void *)&data, (void *)&result, 2);

    printf("Got result: %ld at %ld\n", result, (long)&result);

    // accum_write((void *)&data, (void *)&result, 2);

    printf("done\n");

    // printf("Read from result: %ld address at %ld\n", result, (long)&result);


    // // int result;

    // // accum_load(0, &data1);
    // // accum_load(1, &data2);
    // // accum_load(2, &data3);
    // // accum_load(3, &data4);
    // // result = accum_read(0);

    // // int sum = data1 + data2 + data3 + data4;

    // // printf("Expected: %d, got: %d\n", sum, result);

    // printf("Testing store operation\n");

    // accum_load(1, &data2);
    // printf("Wrote data2: %d address at %ld\n", data2, (long)&data2);

    // accum_load(2, &data1);
    // printf("Wrote data1: %d address at %ld\n", data1, (long)&data1);

    // accum_load(3, &data3);
    // printf("Wrote data3: %d address at %ld\n", data3, (long)&data3);
    // // accum_store(&memResult);
    // // accum_store(&memResult);
    // // printf("Waiting for memory result...\n");
    // long sleep_count = 0;
    // while (sleep_count < 1000000)
    // {
    //     sleep_count++;
    // }

    // printf("Done waiting for memory result.\n");

    // // while(memResult == 0) {
    // //     printf(".");
    // // }
    // printf("Memory result: %d from var data1 at %p\n", data1, (void *)&data1);
    // printf("Memory result: %d from var data2 at %p\n", data2, (void *)&data2);
    // printf("Memory result: %d from var data3 at %p\n", data3, (void *)&data3);

    // // if (result != data + 2)
    // // 	return 1;

    // // accum_write(0, 3);
    // // accum_add(0, 1);
    // // result = accum_read(0);

    // // if (result != 4)
    // // 	return 2;

    return 0;
}
