#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void _tm_copyCorpus(SEXP x, SEXP y);
SEXP _tm_tdm(SEXP stringsSEXP, SEXP remove_digitsSEXP, SEXP stopwordsSEXP, SEXP dictionarySEXP, SEXP min_term_freqSEXP, SEXP max_term_freqSEXP, SEXP min_word_lengthSEXP, SEXP max_word_lengthSEXP);
SEXP _tm_Boost_Tokenizer(SEXP stringsSEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_tm_copyCorpus", (DL_FUNC) &_tm_copyCorpus, 2},
    {"_tm_tdm", (DL_FUNC) &_tm_tdm, 8},
    {"_tm_Boost_Tokenizer", (DL_FUNC) &_tm_Boost_Tokenizer, 1},
    {NULL, NULL, 0}
};

void R_init_tm(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
