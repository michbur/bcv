## R CMD check results

### Not misspelled words

> Possibly misspelled words in DESCRIPTION:
>   AoAS (19:50)
>   SVD (4:33, 15:41)
>   Perry's (19:37, 20:32)
>   SVDImpute (18:27)

These words are not misspelled.

### CRAN repository db overrides

> CRAN repository db overrides:
>  X-CRAN-Comment: Archived on 2022-05-21 as check issues were not corrected in time.

This is a package re-submission with corrected check issues.

### lastMiKTeXException

> * checking for non-standard things in the check directory ... NOTE
> Found the following files/directories:
>  ''NULL''
> * checking for detritus in the temp directory ... NOTE
> Found the following files/directories:
>  'lastMiKTeXException'

RHub return two notes which seem to be false positives (as per https://github.com/r-hub/rhub/issues/503):

### Apparent methods

>  Mismatches for apparent methods not registered:
>  round:
>    function(x, digits)
>  round.fold:
>    function(n, k)

round.fold was not a method and its name was changed to round_fold.
