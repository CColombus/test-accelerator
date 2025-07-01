#ifndef ACC_UTILS_H
#define ACC_UTILS_H

#include "rocc.h"
#include "mmpriv.h"

#define Q32_32_SCALE ((int64_t)1 << 32)

static inline uint64_t ROCC_SUM_QSPAN(mm128_t *source, uint32_t n)
{
    uint64_t sum_qspan = 0;
    ROCC_INSTRUCTION_DSS(0, sum_qspan, (uintptr_t)source, n, 0);
    return sum_qspan;
}

static inline void ROCC_LOAD_PARAMS(void *source)
{
    asm volatile("fence");
    ROCC_INSTRUCTION_SS(0, (uintptr_t)source, 0, 1);
}

static inline uint64_t ROCC_COJ(int64_t j)
{
    uint64_t sc = 0;
    asm volatile("fence");
    ROCC_INSTRUCTION_DSS(0, sc, j, 0, 2);
    return sc;
}

// Convert float/double to Q32.32 fixed-point (int64_t)
int64_t to_q32_32(double val) {
    return (int64_t)(val * (double)Q32_32_SCALE);
}

// Convert back from Q32.32 to double
double from_q32_32(int64_t val) {
    return (double)val / (double)Q32_32_SCALE;
}

#endif