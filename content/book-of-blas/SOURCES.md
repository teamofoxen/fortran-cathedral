# Sources — The Book of BLAS

Research record for every claim and every external link on the Book of
BLAS page. Verified 2026-07-18. The validator refuses any external link
on the page that does not appear here.

| # | Source | URL | Supports |
|---:|---|---|---|
| 1 | Netlib BLAS | https://www.netlib.org/blas/ | The BLAS as published specifications with a Fortran reference implementation at Netlib |
| 2 | Netlib BLAS FAQ | https://www.netlib.org/blas/faq.html | The three defining papers and their dates (Level 1: 1979; Level 2: 1988; Level 3: 1990); reference implementation vs machine-optimized implementations |
| 3 | LAPACK Working Note 81: Level 1, 2, and 3 BLAS | https://www.netlib.org/lapack/lawn81-3.0/node7.html | Level definitions: vector, matrix-vector, and matrix-matrix operation classes |
| 4 | Netlib LAPACK | https://www.netlib.org/lapack/ | LAPACK's scope; written in Fortran 90 |
| 5 | LAPACK Users' Guide: LAPACK and the BLAS | https://www.netlib.org/lapack/lug/node11.html | LAPACK routines are written so that as much computation as possible is performed by calls to the BLAS; designed at the outset to exploit Level 3 |
| 6 | LAPACK Users' Guide: The BLAS as the Key to Portability | https://www.netlib.org/lapack/lug/node65.html | Blocked algorithms; performance portability via optimized BLAS; matrix-matrix operations in inner loops |
| 7 | NumPy manual: Linear algebra | https://numpy.org/doc/stable/reference/routines.linalg.html | NumPy's linear-algebra functions rely on BLAS and LAPACK |
| 8 | SciPy manual: low-level LAPACK functions | https://docs.scipy.org/doc/scipy/reference/linalg.lapack.html | SciPy exposes low-level LAPACK interfaces |
| 9 | OpenBLAS documentation | https://www.openmathlib.org/OpenBLAS/docs/faq/ | OpenBLAS as an optimized implementation of the BLAS interface |
| 10 | Intel oneMKL Developer Reference (Fortran) | https://www.intel.com/content/www/us/en/docs/onemkl/developer-reference-fortran/2024-0/overview.html | oneMKL as a vendor library providing optimized BLAS and LAPACK |

Also relied upon for arithmetic-intensity phrasing: sources 5 and 6
(Level 3 performs matrix-matrix work that blocked algorithms place in
inner loops precisely because it admits architecture-specific
optimization). No benchmark numbers, market-share figures, or
universal-performance claims are made on the page.

The numerical exhibits on the page are computed at generation time by
this repository's own Fortran (transparent reference arithmetic in
`src/cathedral_blas.f90`), not by a linked optimized BLAS library, and
the page says so explicitly.
