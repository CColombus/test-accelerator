#include <stdlib.h>
#include "kalloc.h"
#include "mmpriv.h"
#include "rocc.h"
#include "acc_utils.h"

#define RS_MIN_SIZE 64
#define RS_MAX_BITS 8

typedef struct
{
    mm128_t *b, *e;
} rsbucket_128x_t;

typedef struct
{
    uint64_t *b, *e;
} rsbucket_64_t;

static void rs_insertsort_128x(mm128_t *beg, mm128_t *end)
{
    mm128_t *i;
    for (i = beg + 1; i < end; ++i)
    {
        if (i->x < (i - 1)->x)
        {
            mm128_t *j, tmp = *i;
            for (j = i; j > beg && tmp.x < (j - 1)->x; --j)
                *j = *(j - 1);
            *j = tmp;
        }
    }
}

static void rs_sort_128x(mm128_t *beg, mm128_t *end, int n_bits, int s)
{
    mm128_t *i;
    int size = 1 << n_bits, m = size - 1;
    rsbucket_128x_t *k, b[1 << RS_MAX_BITS], *be = b + size;
    assert(n_bits <= RS_MAX_BITS);
    for (k = b; k != be; ++k)
        k->b = k->e = beg;
    for (i = beg; i != end; ++i)
        ++b[i->x >> s & m].e;
    for (k = b + 1; k != be; ++k)
        k->e += (k - 1)->e - beg, k->b = (k - 1)->e;
    for (k = b; k != be;)
    {
        if (k->b != k->e)
        {
            rsbucket_128x_t *l;
            if ((l = b + (k->b->x >> s & m)) != k)
            {
                mm128_t tmp = *k->b, swap;
                do
                {
                    swap = tmp;
                    tmp = *l->b;
                    *l->b++ = swap;
                    l = b + (tmp.x >> s & m);
                } while (l != k);
                *k->b++ = tmp;
            }
            else
                ++k->b;
        }
        else
            ++k;
    }
    for (b->b = beg, k = b + 1; k != be; ++k)
        k->b = (k - 1)->e;
    if (s)
    {
        s = s > n_bits ? s - n_bits : 0;
        for (k = b; k != be; ++k)
            if (k->e - k->b > RS_MIN_SIZE)
                rs_sort_128x(k->b, k->e, n_bits, s);
            else if (k->e - k->b > 1)
                rs_insertsort_128x(k->b, k->e);
    }
}

void radix_sort_128x(mm128_t *beg, mm128_t *end)
{
    if (end - beg <= RS_MIN_SIZE)
        rs_insertsort_128x(beg, end);
    else
        rs_sort_128x(beg, end, RS_MAX_BITS, (sizeof(uint64_t) - 1) * RS_MAX_BITS);
}

static void rs_insertsort_64(uint64_t *beg, uint64_t *end)
{
    uint64_t *i;
    for (i = beg + 1; i < end; ++i)
    {
        if (*i < *(i - 1))
        {
            uint64_t *j, tmp = *i;
            for (j = i; j > beg && tmp < *(j - 1); --j)
                *j = *(j - 1);
            *j = tmp;
        }
    }
}

static void rs_sort_64(uint64_t *beg, uint64_t *end, int n_bits, int s)
{
    uint64_t *i;
    int size = 1 << n_bits, m = size - 1;
    rsbucket_64_t *k, b[1 << RS_MAX_BITS], *be = b + size;
    assert(n_bits <= RS_MAX_BITS);
    for (k = b; k != be; ++k)
        k->b = k->e = beg;
    for (i = beg; i != end; ++i)
        ++b[*i >> s & m].e;
    for (k = b + 1; k != be; ++k)
        k->e += (k - 1)->e - beg, k->b = (k - 1)->e;
    for (k = b; k != be;)
    {
        if (k->b != k->e)
        {
            rsbucket_64_t *l;
            if ((l = b + (*k->b >> s & m)) != k)
            {
                uint64_t tmp = *k->b, swap;
                do
                {
                    swap = tmp;
                    tmp = *l->b;
                    *l->b++ = swap;
                    l = b + (tmp >> s & m);
                } while (l != k);
                *k->b++ = tmp;
            }
            else
                ++k->b;
        }
        else
            ++k;
    }
    for (b->b = beg, k = b + 1; k != be; ++k)
        k->b = (k - 1)->e;
    if (s)
    {
        s = s > n_bits ? s - n_bits : 0;
        for (k = b; k != be; ++k)
            if (k->e - k->b > RS_MIN_SIZE)
                rs_sort_64(k->b, k->e, n_bits, s);
            else if (k->e - k->b > 1)
                rs_insertsort_64(k->b, k->e);
    }
}

void radix_sort_64(uint64_t *beg, uint64_t *end)
{
    if (end - beg <= RS_MIN_SIZE)
        rs_insertsort_64(beg, end);
    else
        rs_sort_64(beg, end, RS_MAX_BITS, (sizeof(uint64_t) - 1) * RS_MAX_BITS);
}

typedef struct header_t
{
    size_t size;
    struct header_t *ptr;
} header_t;

typedef struct
{
    void *par;
    size_t min_core_size;
    header_t base, *loop_head, *core_head; /* base is a zero-sized block always kept in the loop */
} kmem_t;

static void panic(const char *s)
{
    fprintf(stderr, "%s\n", s);
    abort();
}

void *km_init2(void *km_par, size_t min_core_size)
{
    kmem_t *km;
    km = (kmem_t *)kcalloc(km_par, 1, sizeof(kmem_t));
    km->par = km_par;
    km->min_core_size = min_core_size > 0 ? min_core_size : 0x80000;
    return (void *)km;
}

void *km_init(void) { return km_init2(0, 0); }

void km_destroy(void *_km)
{
    kmem_t *km = (kmem_t *)_km;
    void *km_par;
    header_t *p, *q;
    if (km == NULL)
        return;
    km_par = km->par;
    for (p = km->core_head; p != NULL;)
    {
        q = p->ptr;
        kfree(km_par, p);
        p = q;
    }
    kfree(km_par, km);
}

static header_t *morecore(kmem_t *km, size_t nu)
{
    header_t *q;
    size_t bytes, *p;
    nu = (nu + 1 + (km->min_core_size - 1)) / km->min_core_size * km->min_core_size; /* the first +1 for core header */
    bytes = nu * sizeof(header_t);
    q = (header_t *)kmalloc(km->par, bytes);
    if (!q)
        panic("[morecore] insufficient memory");
    q->ptr = km->core_head, q->size = nu, km->core_head = q;
    p = (size_t *)(q + 1);
    *p = nu - 1;      /* the size of the free block; -1 because the first unit is used for the core header */
    kfree(km, p + 1); /* initialize the new "core"; NB: the core header is not looped. */
    return km->loop_head;
}

void kfree(void *_km, void *ap) /* kfree() also adds a new core to the circular list */
{
    header_t *p, *q;
    kmem_t *km = (kmem_t *)_km;

    if (!ap)
        return;
    if (km == NULL)
    {
        free(ap);
        return;
    }
    p = (header_t *)((size_t *)ap - 1);
    p->size = *((size_t *)ap - 1);
    /* Find the pointer that points to the block to be freed. The following loop can stop on two conditions:
     *
     * a) "p>q && p<q->ptr": @------#++++++++#+++++++@-------    @---------------#+++++++@-------
     *    (can also be in    |      |                |        -> |                       |
     *     two cores)        q      p           q->ptr           q                  q->ptr
     *
     *                       @--------    #+++++++++@--------    @--------    @------------------
     *                       |            |         |         -> |            |
     *                       q            p    q->ptr            q       q->ptr
     *
     * b) "q>=q->ptr && (p>q || p<q->ptr)":  @-------#+++++   @--------#+++++++     @-------#+++++   @----------------
     *                                       |                |        |         -> |                |
     *                                  q->ptr                q        p       q->ptr                q
     *
     *                                       #+++++++@-----   #++++++++@-------     @-------------   #++++++++@-------
     *                                       |       |                 |         -> |                         |
     *                                       p  q->ptr                 q       q->ptr                         q
     */
    for (q = km->loop_head; !(p > q && p < q->ptr); q = q->ptr)
        if (q >= q->ptr && (p > q || p < q->ptr))
            break;
    if (p + p->size == q->ptr)
    { /* two adjacent blocks, merge p and q->ptr (the 2nd and 4th cases) */
        p->size += q->ptr->size;
        p->ptr = q->ptr->ptr;
    }
    else if (p + p->size > q->ptr && q->ptr >= p)
    {
        panic("[kfree] The end of the allocated block enters a free block.");
    }
    else
        p->ptr = q->ptr; /* backup q->ptr */

    if (q + q->size == p)
    { /* two adjacent blocks, merge q and p (the other two cases) */
        q->size += p->size;
        q->ptr = p->ptr;
        km->loop_head = q;
    }
    else if (q + q->size > p && p >= q)
    {
        panic("[kfree] The end of a free block enters the allocated block.");
    }
    else
        km->loop_head = p, q->ptr = p; /* in two cores, cannot be merged; create a new block in the list */
}

void *kmalloc(void *_km, size_t n_bytes)
{
    kmem_t *km = (kmem_t *)_km;
    size_t n_units;
    header_t *p, *q;

    if (n_bytes == 0)
        return 0;
    if (km == NULL)
        return malloc(n_bytes);
    n_units = (n_bytes + sizeof(size_t) + sizeof(header_t) - 1) / sizeof(header_t); /* header+n_bytes requires at least this number of units */

    if (!(q = km->loop_head)) /* the first time when kmalloc() is called, intialize it */
        q = km->loop_head = km->base.ptr = &km->base;
    for (p = q->ptr;; q = p, p = p->ptr)
    { /* search for a suitable block */
        if (p->size >= n_units)
        { /* p->size if the size of current block. This line means the current block is large enough. */
            if (p->size == n_units)
                q->ptr = p->ptr; /* no need to split the block */
            else
            {                           /* split the block. NB: memory is allocated at the end of the block! */
                p->size -= n_units;     /* reduce the size of the free block */
                p += p->size;           /* p points to the allocated block */
                *(size_t *)p = n_units; /* set the size */
            }
            km->loop_head = q; /* set the end of chain */
            return (size_t *)p + 1;
        }
        if (p == km->loop_head)
        { /* then ask for more "cores" */
            if ((p = morecore(km, n_units)) == 0)
                return 0;
        }
    }
}

void *kcalloc(void *_km, size_t count, size_t size)
{
    kmem_t *km = (kmem_t *)_km;
    void *p;
    if (size == 0 || count == 0)
        return 0;
    if (km == NULL)
        return calloc(count, size);
    p = kmalloc(km, count * size);
    memset(p, 0, count * size);
    return p;
}

void *krealloc(void *_km, void *ap, size_t n_bytes) // TODO: this can be made more efficient in principle
{
    kmem_t *km = (kmem_t *)_km;
    size_t cap, *p, *q;

    if (n_bytes == 0)
    {
        kfree(km, ap);
        return 0;
    }
    if (km == NULL)
        return realloc(ap, n_bytes);
    if (ap == NULL)
        return kmalloc(km, n_bytes);
    p = (size_t *)ap - 1;
    cap = (*p) * sizeof(header_t) - sizeof(size_t);
    if (cap >= n_bytes)
        return ap; /* TODO: this prevents shrinking */
    q = (size_t *)kmalloc(km, n_bytes);
    memcpy(q, ap, cap);
    kfree(km, ap);
    return q;
}

void km_stat(const void *_km, km_stat_t *s)
{
    kmem_t *km = (kmem_t *)_km;
    header_t *p;
    memset(s, 0, sizeof(km_stat_t));
    if (km == NULL || km->loop_head == NULL)
        return;
    for (p = km->loop_head;; p = p->ptr)
    {
        s->available += p->size * sizeof(header_t);
        if (p->size != 0)
            ++s->n_blocks; /* &kmem_t::base is always one of the cores. It is zero-sized. */
        if (p->ptr > p && p + p->size > p->ptr)
            panic("[km_stat] The end of a free block enters another free block.");
        if (p->ptr == km->loop_head)
            break;
    }
    for (p = km->core_head; p != NULL; p = p->ptr)
    {
        size_t size = p->size * sizeof(header_t);
        ++s->n_cores;
        s->capacity += size;
        s->largest = s->largest > size ? s->largest : size;
    }
}

static const char LogTable256[256] = {
#define LT(n) n, n, n, n, n, n, n, n, n, n, n, n, n, n, n, n
    -1, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
    LT(4), LT(5), LT(5), LT(6), LT(6), LT(6), LT(6),
    LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7), LT(7)};

static inline int ilog2_32(uint32_t v)
{
    uint32_t t, tt;
    if ((tt = v >> 16))
        return (t = tt >> 8) ? 24 + LogTable256[t] : 16 + LogTable256[tt];
    return (t = v >> 8) ? 8 + LogTable256[t] : LogTable256[v];
}

mm128_t *mm_chain_dp(int max_dist_x, int max_dist_y, int bw, int max_skip, int max_iter, int min_cnt, int min_sc, float gap_scale, int is_cdna, int n_segs, int64_t n, mm128_t *a, int *n_u_, uint64_t **_u, void *km)
{ // TODO: make sure this works when n has more than 32 bits

    // printf("n = %ld\n", n);
    // // print the anchors
    // for (int i = 0; i < n; i++) {
    // 	printf("a[%d].x = %ld, a[%d].y = %ld\n", i, a[i].x, i, a[i].y);
    // }
    int32_t k, *f, *p, *t, *v, n_u, n_v;
    int64_t i, j, st = 0;
    uint64_t *u, *u2, sum_qspan = 0;
    float avg_qspan;
    mm128_t *b, *w;

    if (_u)
        *_u = 0, *n_u_ = 0;
    if (n == 0 || a == 0)
    {
        kfree(km, a);
        return 0;
    }
    f = (int32_t *)kmalloc(km, n * 4);
    p = (int32_t *)kmalloc(km, n * 4);
    t = (int32_t *)kmalloc(km, n * 4);
    v = (int32_t *)kmalloc(km, n * 4);
    memset(t, 0, n * 4);

    sum_qspan = ROCC_SUM_QSPAN(a, n);
    avg_qspan = (float)sum_qspan / n;

    // fill the score and backtrack arrays
    for (i = 0; i < n; ++i)
    {
        uint64_t ri = a[i].x;
        int64_t max_j = -1;
        int32_t qi = (int32_t)a[i].y, q_span = a[i].y >> 32 & 0xff; // NB: only 8 bits of span is used!!!
        int32_t max_f = q_span, n_skip = 0, min_d;
        int32_t sidi = (a[i].y & MM_SEED_SEG_MASK) >> MM_SEED_SEG_SHIFT;

        // Temporary array of setup values to pass onto accelerator
        int64_t setup_array[7];
        setup_array[0] = (int64_t)is_cdna;
        setup_array[1] = (int64_t)ri;
        setup_array[2] = (int64_t)qi;
        setup_array[3] = (int64_t)q_span;
        setup_array[4] = (int64_t)sidi;
        setup_array[5] = to_q32_32(avg_qspan);
        setup_array[6] = to_q32_32(gap_scale);

        // Load parameters into the accelerator
        ROCC_LOAD_PARAMS((void *)setup_array);

        while (st < i && ri > a[st].x + max_dist_x)
            ++st;
        if (i - st > max_iter)
            st = i - max_iter;

        for (j = i - 1; j >= st; --j)
        {
            printf("a[%ld].x = %ld, a[%ld].y = %ld\n", j, a[j].x, j, a[j].y);
            // print ri,qi
            printf("ri = %ld, qi = %d\n", ri, qi);

            uint64_t result;
            result = ROCC_COJ(j);
            // print output as from_q32_32(result)
            printf("from_q32_32(result) = %f\n", from_q32_32(result));


            int64_t dr = ri - a[j].x;
            int32_t dq = qi - (int32_t)a[j].y, dd, sc, log_dd, gap_cost;
            int32_t sidj = (a[j].y & MM_SEED_SEG_MASK) >> MM_SEED_SEG_SHIFT;

            // if ((sidi == sidj && dr == 0) || dq <= 0)
            //     continue; // don't skip if an anchor is used by multiple segments; see below
            // if ((sidi == sidj && dq > max_dist_y) || dq > max_dist_x)
            //     continue;
            dd = dr > dq ? dr - dq : dq - dr;
            // if (sidi == sidj && dd > bw)
            //     continue;
            // if (n_segs > 1 && !is_cdna && sidi == sidj && dr > max_dist_y)
            //     continue;
            min_d = dq < dr ? dq : dr;
            sc = min_d > q_span ? q_span : dq < dr ? dq
                                                   : dr;
            log_dd = dd ? ilog2_32(dd) : 0;
            gap_cost = 0;

            // print dr,dq,sidj,dd,min_d,sc,log_dd

            if (is_cdna || sidi != sidj)
            {
                int c_log, c_lin;
                c_lin = (int)(dd * .01 * avg_qspan);
                c_log = log_dd;
                if (sidi != sidj && dr == 0)
                    ++sc; // possibly due to overlapping paired ends; give a minor bonus
                else if (dr > dq || sidi != sidj)
                    gap_cost = c_lin < c_log ? c_lin : c_log;
                else
                    gap_cost = c_lin + (c_log >> 1);
            }
            else
                gap_cost = (int)(dd * .01 * avg_qspan) + (log_dd >> 1);

            printf("i=%ld, j=%ld, dr=%ld, dq=%d, sidj=%d, dd=%d, min_d=%d, sc=%d, log_dd=%d, gap_cost=%d\n\n", i, j, dr, dq, sidj, dd, min_d, sc, log_dd, gap_cost);

            sc -= (int)((double)gap_cost * gap_scale + .499);
            sc += f[j];
            if (sc > max_f)
            {
                max_f = sc, max_j = j;
                if (n_skip > 0)
                    --n_skip;
            }
            else if (t[j] == i)
            {
                if (++n_skip > max_skip)
                    break;
            }
            if (p[j] >= 0)
                t[p[j]] = i;
        }
        f[i] = max_f, p[i] = max_j;
        v[i] = max_j >= 0 && v[max_j] > max_f ? v[max_j] : max_f; // v[] keeps the peak score up to i; f[] is the score ending at i, not always the peak
    }

    // find the ending positions of chains
    memset(t, 0, n * 4);
    for (i = 0; i < n; ++i)
        if (p[i] >= 0)
            t[p[i]] = 1;
    for (i = n_u = 0; i < n; ++i)
        if (t[i] == 0 && v[i] >= min_sc)
            ++n_u;
    if (n_u == 0)
    {
        kfree(km, a);
        kfree(km, f);
        kfree(km, p);
        kfree(km, t);
        kfree(km, v);
        return 0;
    }
    u = (uint64_t *)kmalloc(km, n_u * 8);
    for (i = n_u = 0; i < n; ++i)
    {
        if (t[i] == 0 && v[i] >= min_sc)
        {
            j = i;
            while (j >= 0 && f[j] < v[j])
                j = p[j]; // find the peak that maximizes f[]
            if (j < 0)
                j = i; // TODO: this should really be assert(j>=0)
            u[n_u++] = (uint64_t)f[j] << 32 | j;
        }
    }
    radix_sort_64(u, u + n_u);
    for (i = 0; i < n_u >> 1; ++i)
    { // reverse, s.t. the highest scoring chain is the first
        uint64_t t = u[i];
        u[i] = u[n_u - i - 1], u[n_u - i - 1] = t;
    }

    // backtrack
    memset(t, 0, n * 4);
    for (i = n_v = k = 0; i < n_u; ++i)
    { // starting from the highest score
        int32_t n_v0 = n_v, k0 = k;
        j = (int32_t)u[i];
        do
        {
            v[n_v++] = j;
            t[j] = 1;
            j = p[j];
        } while (j >= 0 && t[j] == 0);
        if (j < 0)
        {
            if (n_v - n_v0 >= min_cnt)
                u[k++] = u[i] >> 32 << 32 | (n_v - n_v0);
        }
        else if ((int32_t)(u[i] >> 32) - f[j] >= min_sc)
        {
            if (n_v - n_v0 >= min_cnt)
                u[k++] = ((u[i] >> 32) - f[j]) << 32 | (n_v - n_v0);
        }
        if (k0 == k)
            n_v = n_v0; // no new chain added, reset
    }
    *n_u_ = n_u = k, *_u = u; // NB: note that u[] may not be sorted by score here

    // free temporary arrays
    kfree(km, f);
    kfree(km, p);
    kfree(km, t);

    // write the result to b[]
    b = (mm128_t *)kmalloc(km, n_v * sizeof(mm128_t));
    for (i = 0, k = 0; i < n_u; ++i)
    {
        int32_t k0 = k, ni = (int32_t)u[i];
        for (j = 0; j < ni; ++j)
            b[k] = a[v[k0 + (ni - j - 1)]], ++k;
    }
    kfree(km, v);

    // sort u[] and a[] by a[].x, such that adjacent chains may be joined (required by mm_join_long)
    w = (mm128_t *)kmalloc(km, n_u * sizeof(mm128_t));
    for (i = k = 0; i < n_u; ++i)
    {
        w[i].x = b[k].x, w[i].y = (uint64_t)k << 32 | i;
        k += (int32_t)u[i];
    }
    radix_sort_128x(w, w + n_u);
    u2 = (uint64_t *)kmalloc(km, n_u * 8);
    for (i = k = 0; i < n_u; ++i)
    {
        int32_t j = (int32_t)w[i].y, n = (int32_t)u[j];
        u2[i] = u[j];
        memcpy(&a[k], &b[w[i].y >> 32], n * sizeof(mm128_t));
        k += n;
    }
    if (n_u)
        memcpy(u, u2, n_u * 8);
    if (k)
        memcpy(b, a, k * sizeof(mm128_t)); // write _a_ to _b_ and deallocate _a_ because _a_ is oversized, sometimes a lot
    kfree(km, a);
    kfree(km, w);
    kfree(km, u2);
    return b;
}

int main()
{
    // Define the parameters for the mm_chain_dp function
    int max_chain_gap_ref = 5000;
    int max_chain_gap_qry = 5000;
    int bw = 500;
    int max_chain_skip = 25;
    int max_chain_iter = 5000;
    int min_cnt = 3;
    int min_chain_score = 40;
    float chain_gap_scale = 1.0;
    int is_splice = 0;
    int n_segs = 1;
    int64_t n_a = 8;

    // Allocate memory for the input arrays
    mm128_t *a = (mm128_t *)malloc(n_a * sizeof(mm128_t));
    if (a == NULL)
    {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    // Initialize the input array with provided data
    a[0].x = -9223372036854763668, a[0].y = 64424509459;
    a[1].x = -9223372036854763661, a[1].y = 64424509466;
    a[2].x = -9223372036854763651, a[2].y = 64424509476;
    a[3].x = -9223372036854763648, a[3].y = 64424509479;
    a[4].x = -9223372036854763643, a[4].y = 64424509484;
    a[5].x = -9223372036854763633, a[5].y = 64424509494;
    a[6].x = -9223372036854763623, a[6].y = 64424509504;
    a[7].x = -9223372036854763622, a[7].y = 64424509505;

    // output of mm_chain_dp
    int n_regs0;
    uint64_t *u;
    void *km = NULL; // NULL memory pool

    mm128_t *result = mm_chain_dp(max_chain_gap_ref, max_chain_gap_qry, bw, max_chain_skip, max_chain_iter, min_cnt, min_chain_score, chain_gap_scale, is_splice, n_segs, n_a, a, &n_regs0, &u, km);

    // Print the output
    printf("Number of regions: %d\n", n_regs0);
    for (int i = 0; i < n_regs0; i++)
    {
        printf("u[%d] = %ld\n", i, (long)u[i]);
    }

    // Free allocated memory
    // free(a);
    // free(u);

    return 0;
}