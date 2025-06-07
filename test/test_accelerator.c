#include "rocc.h"
#include <stdio.h>
#include <stdlib.h>

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

static inline void accum_do(void *source, void *dest)
{
    asm volatile("fence");
    ROCC_INSTRUCTION_SS(0, (uintptr_t)source, (uintptr_t)dest, 2);
    // dereference the destination to flush the cache
    // otherwise we get stale value
    volatile int dummy = *(volatile int *)dest;
    (void)dummy; // Prevent unused variable warning
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

volatile int data[10];
volatile int result;

int main(void)
{

    printf("Testing accumulator...\n");

    for (int iter = 0; iter < 2; iter++)
    {
        // Initialize the data array with some values
        printf("\nPopulating data array with random values:\n[ ");
        for (int i = 0; i < 10; i++)
        {
            data[i] = rand() % 1000; // Random values between 0 and 999
            printf("%d ", data[i]);
        }
        printf("]\n\n");

        // Expected sum
        int expected_sum = 0;
        for (int i = 0; i < 10; i++)
        {
            expected_sum += data[i];
        }
        printf("Expected sum: %d\n", expected_sum);
        accum_do((void *)&data[0], (void *)&result);
        asm volatile("fence rw, rw"); // Ensure memory operations complete
        printf("Sum from accelerator: %d\n", result);
        
    }

    return 0;
}
