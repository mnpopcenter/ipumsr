#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject raw_to_df_hier_long(
  List raw_,
  int num_vars,
  List rt_info_,
  List var_info_,
  CharacterVector encoding_
) {
  // C++ conversion
  List raw = as<List>(raw_);
  List rt_info = as<List>(rt_info_);
  List var_info = as<List>(var_info_);
  std::string encoding = as<std::string>(encoding_);

  // Convert to C encoding enum
  cetype_t encoding_ce;
  if (encoding == "UTF-8") {
    encoding_ce = CE_UTF8;
  } else if (encoding == "latin1") {
    encoding_ce = CE_LATIN1;
  } else {
    Rcpp::stop("Unexpected encoding");
  }

  // Make empty list with rows and cols
  int num_rows = raw.length();
  List out = List(num_vars);

  for (int i = 0; i < num_vars; i++) {
    out[i] = CharacterVector(num_rows);
  }

  // Get c++ objects out of lists
  int rt_start = as<int>(rt_info["start"]);
  int rt_width = as<int>(rt_info["width"]);

  std::vector<std::string> rectypes = var_info.names();
  int num_rt = rectypes.size();

  std::vector<std::vector<int> > starts;
  std::vector<std::vector<int> > widths;
  std::vector<std::vector<int> > var_pos;
  std::vector<int> num_vars_rectype;
  std::vector<int> max_ends;
  for (int i = 0; i < num_rt; i++) {
    starts.push_back(as<List>(var_info[i])["start"]);
    widths.push_back(as<List>(var_info[i])["width"]);
    var_pos.push_back(as<List>(var_info[i])["var_pos"]);
    num_vars_rectype.push_back(starts[i].size());
    max_ends.push_back(as<IntegerVector>(as<List>(var_info[i])["max_end"])[0]);
  }

  // Parse raw into list
  for (int i = 0; i < num_rows; i++) {
    if (i % 1000000 == 0) {
      Rcpp::checkUserInterrupt();
    }
    Rbyte* row_raw = RAW(raw[i]);

    // Get rectype
    char *rt = new char[rt_width + 1];
    std::strncpy(rt, (const char *) row_raw + rt_start, rt_width);
    rt[rt_width] = 0;

    int rt_index = -1;
    for (int j = 0; j < num_rt; j++) {
      if (rt == rectypes[j]) {
        rt_index = j;
        break;
      }
    }
    delete [] rt;
    if (rt_index == -1) {
      break;
    }

    // Check if raw line is long enough
    if (Rf_length(raw[i]) < max_ends[rt_index]) {
      Rcpp::stop("Line is too short for rectype.");
    }

    // Loop through vars in rectype and add to out
    for (int j = 0; j < num_vars_rectype[rt_index]; j++) {
      Rbyte* cur_text_pos = row_raw + starts[rt_index][j];
      int cur_var_width = widths[rt_index][j];
      int cur_var_pos = var_pos[rt_index][j];

      SET_STRING_ELT(
        out[cur_var_pos], i,
        Rf_mkCharLenCE((const char*)cur_text_pos, cur_var_width, encoding_ce)
      );
    }
  }

  return out;
}

