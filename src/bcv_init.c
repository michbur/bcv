
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP R_cv_svd_gabriel(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_cv_svd_wold(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_impute_svd(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_cv_svd_gabriel", (DL_FUNC) &R_cv_svd_gabriel, 6},
    {"R_cv_svd_wold",    (DL_FUNC) &R_cv_svd_wold,    6},
    {"R_impute_svd",     (DL_FUNC) &R_impute_svd,     4},
    {NULL, NULL, 0}
};

void R_init_bcv(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
