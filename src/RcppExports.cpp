// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// grow
NumericMatrix grow(int n, int f, double d, NumericMatrix graph, NumericVector scores, NumericMatrix feat_mat, NumericMatrix score_mat, double tol, NumericMatrix weight_mat, NumericVector selected);
RcppExport SEXP _FamilyRank_grow(SEXP nSEXP, SEXP fSEXP, SEXP dSEXP, SEXP graphSEXP, SEXP scoresSEXP, SEXP feat_matSEXP, SEXP score_matSEXP, SEXP tolSEXP, SEXP weight_matSEXP, SEXP selectedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type f(fSEXP);
    Rcpp::traits::input_parameter< double >::type d(dSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type graph(graphSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type scores(scoresSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type feat_mat(feat_matSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type score_mat(score_matSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type weight_mat(weight_matSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type selected(selectedSEXP);
    rcpp_result_gen = Rcpp::wrap(grow(n, f, d, graph, scores, feat_mat, score_mat, tol, weight_mat, selected));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FamilyRank_grow", (DL_FUNC) &_FamilyRank_grow, 10},
    {NULL, NULL, 0}
};

RcppExport void R_init_FamilyRank(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
