# Sources — The Testaments

Research record for every dated or standards claim made on the
Testaments page. Verified 2026-07-18. Claims not verifiable were
omitted from the page.

| Claim | Source |
|---|---|
| Fixed-form layout: comments marked in column 1, statement labels in columns 1–5, continuation in column 6, statements in columns 7–72 | FORTRAN 77 (ANSI X3.9-1978) source form, as summarized in the NASA "Fortran 90: A Conversion Course for Fortran 77 Programmers" notes — https://www.nccs.nasa.gov/sites/default/docs/tutorials/f90studentnotes.pdf |
| Free source form, modules, allocatable arrays, whole-array operations, `EXIT`/`CYCLE`, and `DO … END DO` introduced in Fortran 90 | NASA F90 conversion course (above); Fortran Wiki standards page — https://fortranwiki.org/fortran/show/Standards |
| `ISO_FORTRAN_ENV` intrinsic module origin Fortran 2003; `REAL32`/`REAL64`/`REAL128` kind constants added in Fortran 2008 | Fortran Wiki — https://fortranwiki.org/fortran/show/iso_fortran_env ; GNU Fortran manual — https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html |
| `SELECTED_REAL_KIND` introduced with Fortran 90; `DOUBLE PRECISION` standard since early standards; `REAL*8` never part of any standard (vendor extension) | NASA F90 conversion course (above) |
| Arithmetic IF **deleted** in Fortran 2018; non-block DO (shared termination label, or termination other than `END DO`/`CONTINUE`) **deleted** in Fortran 2018 | NAG Fortran 2018 overview — https://support.nag.com/nagware/np/r72_doc/nag_f2018.html |
| Labeled DO construct, `COMMON`, `EQUIVALENCE`, and `BLOCK DATA` **obsolescent** in Fortran 2018 | NAG Fortran 2018 overview (above) |
| Standards timeline: first Fortran delivered by IBM in 1957; FORTRAN 66 (ANSI, 1966); FORTRAN 77 (published 1978); Fortran 90 (ISO, 1991); Fortran 95 (1997); Fortran 2003 (2004); Fortran 2008 (2010); Fortran 2018 (2018); Fortran 2023 (ISO/IEC 1539-1:2023, November 2023) | WG5 — https://wg5-fortran.org/fearlier.html ; Fortran Wiki standards page (above); Wikipedia overview — https://en.wikipedia.org/wiki/Fortran |

Compilability of every exhibit is not a claim taken from a source; it is
enforced mechanically by `forty validate`, which syntax-checks each
verse with GFortran at survey time.
