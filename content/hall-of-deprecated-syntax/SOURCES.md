# Sources — The Hall of Deprecated Syntax

Research record for every classification and external link on the Hall
page. Verified 2026-07-18. The validator refuses any external link on
the page that does not appear here, and separately re-runs every
compiler probe to confirm the displayed testimony.

| # | Source | URL | Supports |
|---:|---|---|---|
| 1 | NAG Fortran 2018 overview | https://support.nag.com/nagware/np/r72_doc/nag_f2018.html | Arithmetic IF and non-block DO deleted in Fortran 2018; labeled DO, COMMON, EQUIVALENCE, BLOCK DATA obsolescent |
| 2 | Appendix 4: Backward and Forward compatibility (NSC, Fortran 77 to 90 course) | https://www.nsc.liu.se/~boein/f77to90/a4.html | Fortran 95 deletions (ASSIGN/assigned GO TO, PAUSE, real DO variables, H edit descriptor); computed GO TO and statement functions obsolescent with preferred replacements |
| 3 | Fortran Wiki: Modernizing Old Fortran | https://fortranwiki.org/fortran/show/Modernizing+Old+Fortran | Modern replacements for legacy constructs |
| 4 | GNU Fortran manual: Hollerith constants support | https://gcc.gnu.org/onlinedocs/gfortran/Hollerith-constants-support.html | Hollerith constants live on as a documented compiler extension |
| 5 | GNU Fortran manual: Extensions | https://gcc.gnu.org/onlinedocs/gfortran/Extensions.html | Old-style kind specifications (REAL*8) and other vendor customs as documented extensions, never standard |
| 6 | WG5: earlier Fortran standards | https://wg5-fortran.org/fearlier.html | The standards line referenced by the classifications |

Hollerith history: Hollerith constants were Fortran 66 practice removed
from the FORTRAN 77 standard (retained only in an appendix), while the
H edit descriptor survived until its deletion in Fortran 95 (sources 2,
4). Plain unconditional GO TO remains fully standard; no source lists it
as obsolescent or deleted, and the page's claim is additionally proven
mechanically by the `-std=f2018` compiler probe accepting it cleanly.

Compiler testimony on the page is not taken from any source: it is
measured at generation time by Forty's own probes against GFortran, and
re-measured by `forty validate`.
