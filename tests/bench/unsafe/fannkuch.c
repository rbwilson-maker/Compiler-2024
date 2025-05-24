/*
 * fannkuch.c
 *
 * Find the array permutation which requires the least number of pancake flips
 * to sort. (See: pancakge sorting problem for more information).
 * 
 * Source: LLVM tests
 *
 * Optimiere mir die Pfannkuchensortierung!
 *
 * Author:   Pranav Kumar <pmkumar@cmu.edu>
 * Semester: Fall'20
 * Hotel:    Trivago
 */

#include <stdlib.h>

typedef struct data Data;

struct data {
  int result;
};

void XCH(int* A, int i, int* B, int j) {
    int tmp;
    tmp = A[i];
    A[i] = B[j];
    B[j] = tmp;
}

int fannkuch( int n )
{
    int* perm;
    int* perm1;
    int* count;
    int flips;
    int flipsMax;
    int r;
    int i;
    int k;
    int didpr;
    int n1 = n - 1;

    if( n < 1 ) return 0;

    perm  = calloc(sizeof(int), n);
    perm1 = calloc(sizeof(int), n);
    count = calloc(sizeof(int), n);

    for( i=0 ; i<n ; i++ ) perm1[i] = i; /* initial (trivial) permu */

    r = n; didpr = 0; flipsMax = 0;

    // don't optimize me out!
    while(1) {
        for( ; r!=1 ; r-- ) {
            count[r-1] = r;
        }

        if( ! (perm1[0]==0 || perm1[n1]==n1) ) {
            flips = 0;
            for( i=1 ; i<n ; i++ ) { /* perm = perm1 */
                perm[i] = perm1[i];
            }

            k = perm1[0]; /* cache perm[0] in k */

            /* simulate do while */
            /* do... */
            int j=k-1;
            for( i=1 ; i<j ; i++ ) {
                XCH(perm, i, perm, j);
                j--;
            }
            flips++;
            /*
             * Now exchange k (caching perm[0]) and perm[k]... with care!
             * XCH(k, perm[k]) does NOT work!
             */
            j=perm[k]; perm[k]=k ; k=j;

            /* while... */
            while (k != 0) {
                j=k-1;
                for( i=1 ; i<j ; i++ ) {
                    XCH(perm, i, perm, j);
                    j--;
                }
                flips++;
                /*
                * Now exchange k (caching perm[0]) and perm[k]... with care!
                * XCH(k, perm[k]) does NOT work!
                */
                j=perm[k]; perm[k]=k ; k=j;
            }

            if( flipsMax < flips ) {
                flipsMax = flips;
            }
        }

        int flag = 1;
        while(flag) {
            if( r == n ) {
                return flipsMax;
            }
            /* rotate down perm[0..r] by one */
            {
                int perm0 = perm1[0];
                i = 0;
                while( i < r ) {
                    k = i+1;
                    perm1[i] = perm1[k];
                    i = k;
                }
                perm1[r] = perm0;
            }
            count[r] -= 1;
            if( count[r] > 0 ) {
                /* break; */
                flag = 0;
            } else {
                r++;
            }
        }
    }
    return -1;
}

Data *_c0_init(int n) {
  Data *x = calloc(sizeof(Data), 1);
  return x;
}

void _c0_prepare(Data *d, int n) { return; }

void _c0_run(Data *d, int n) {
  int x = 11;
  d->result = fannkuch(x);
}

int _c0_checksum(Data *d, int n) {
  return d->result;
}

// Never actually called by the harness.
int _c0_main() {
  Data *p = _c0_init(0);
  _c0_prepare(p, 0);
  _c0_run(p, 0);
  return _c0_checksum(p, 0);
}
