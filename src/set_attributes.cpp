#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
DataFrame set_ipums_var_attributes_(DataFrame x, DataFrame var_info) {
  int num_vars = var_info.nrows();
  CharacterVector var_names = var_info["var_name"];
  CharacterVector var_labels = var_info["var_label"];
  CharacterVector var_descs = var_info["var_desc"];
  List val_labels = var_info["val_labels"];
  Function labelled_f = Environment::namespace_env("haven")["labelled"];

  for (int i = 0; i < num_vars; i++) {
    const char* vn = var_names[i];
    if (!Rf_isNull(val_labels[i])) {
      x[vn] = labelled_f(x[vn], val_labels[i]);
    }

    SEXP col = x[vn];

    SEXP vl = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(vl, 0, var_labels[i]);
    Rf_setAttrib(col, Rf_install("label"), vl);
    UNPROTECT(1);

    SEXP vd = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(vd, 0, var_descs[i]);
    Rf_setAttrib(col, Rf_install("var_desc"), vd);
    UNPROTECT(1);
  }
  return x;
}


// [[Rcpp::export]]
DataFrame set_imp_decim_(DataFrame x, DataFrame var_info) {
  int num_vars = var_info.nrows();
  CharacterVector var_names = var_info["var_name"];
  NumericVector imp_decim = var_info["imp_decim"];

  for (int i = 0; i < num_vars; i++) {
    if (imp_decim[i] != 0) {
      const char* vn = var_names[i];
      NumericVector col = x[vn];
      col = col / pow(10, imp_decim[i]);
    }
  }
  return x;
}
