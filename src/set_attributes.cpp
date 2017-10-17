#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
DataFrame set_ipums_var_attributes_(DataFrame x, DataFrame var_info) {
  int num_vars = var_info.nrows();
  CharacterVector var_names = var_info["var_name"];
  CharacterVector var_labels = var_info["var_label"];
  CharacterVector var_descs = var_info["var_desc"];
  List val_labels = var_info["val_labels"];

  for (int i = 0; i < num_vars; i++) {
    const char* vn = var_names[i];
    SEXP col = x[vn];

    SEXP vl = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(vl, 0, var_labels[i]);
    Rf_setAttrib(col, Rf_install("label"), vl);
    UNPROTECT(1);

    SEXP vd = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(vd, 0, var_descs[i]);
    Rf_setAttrib(col, Rf_install("var_desc"), vd);
    UNPROTECT(1);

    if (!Rf_isNull(val_labels[i])) {
      SEXP new_class = PROTECT(Rf_allocVector(STRSXP, 1));
      SET_STRING_ELT(new_class, 0, Rf_mkChar("labelled"));
      Rf_setAttrib(col, Rf_install("class"), new_class);
      UNPROTECT(1);

      Rf_setAttrib(col, Rf_install("labels"), val_labels[i]);
    }
  }
  return x;
}
