# About bcv

Methods for choosing the rank of an SVD approximation via cross validation.  The package provides both Gabriel-style "block" holdouts and Wold-style "speckled" holdouts.  It also includes an implementation of the SVDImpute algorithm.  For more information about Bi-cross-validation, see [Owen & Perry's 2009 AoAS article](https://arxiv.org/abs/0908.2062) and [Perry's 2009 PhD thesis](https://arxiv.org/abs/0909.3052).

Installation
------------

**bcv** is available [on CRAN](https://cran.r-project.org/package=bcv), so installation is as simple as:

```
install.packages("bcv")
```

You can install the latest development version of the code using the `devtools` R package.

```
# Install devtools, if you haven't already.
install.packages("devtools")

library(devtools)
install_github("michbur/bcv")
```