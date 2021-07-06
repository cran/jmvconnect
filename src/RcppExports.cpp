// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// readDF
DataFrame readDF(String path, SEXP columnsReq, bool headerOnly, bool requiresMissings);
RcppExport SEXP _jmvconnect_readDF(SEXP pathSEXP, SEXP columnsReqSEXP, SEXP headerOnlySEXP, SEXP requiresMissingsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< String >::type path(pathSEXP);
    Rcpp::traits::input_parameter< SEXP >::type columnsReq(columnsReqSEXP);
    Rcpp::traits::input_parameter< bool >::type headerOnly(headerOnlySEXP);
    Rcpp::traits::input_parameter< bool >::type requiresMissings(requiresMissingsSEXP);
    rcpp_result_gen = Rcpp::wrap(readDF(path, columnsReq, headerOnly, requiresMissings));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_jmvconnect_readDF", (DL_FUNC) &_jmvconnect_readDF, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_jmvconnect(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
