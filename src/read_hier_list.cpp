#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
RObject raw_to_df_hier_list(
    List raw_,
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

  // Get c++ objects out of lists
  int num_rows = raw.length();

  int rt_start = as<int>(rt_info["start"]);
  int rt_width = as<int>(rt_info["width"]);

  std::vector<std::string> rectypes = var_info.names();
  int num_rt = rectypes.size();

  std::vector<std::vector<int> > starts;
  std::vector<std::vector<int> > widths;
  std::vector<int> num_vars_rectype;
  std::vector<int> max_ends;
  for (int i = 0; i < num_rt; i++) {
    starts.push_back(as<List>(var_info[i])["start"]);
    widths.push_back(as<List>(var_info[i])["width"]);
    num_vars_rectype.push_back(starts[i].size());
    max_ends.push_back(as<IntegerVector>(as<List>(var_info[i])["max_end"])[0]);
  }

  // Create placeholder object (not sure if this is a performant way to do this)
  std::vector<std::vector <std::vector <std::string> > > out;
  out.resize(num_rt);
  for (int i = 0; i < num_rt; i++) {
    out[i].resize(num_vars_rectype[i] + 1); // Add cross-record ID
  }

  // Parse raw into list
  size_t id = 1; // 1-based for R
  int rt_index_first = -1;
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

    if (i == 0) {
        rt_index_first = rt_index;
    } else if (rt_index == rt_index_first) {
      id++;
    }

    // Check if raw line is long enough
    if (Rf_length(raw[i]) < max_ends[rt_index]) {
      Rcpp::stop("Line is too short for rectype.");
    }

    // Add ID to out
    out[rt_index][0].push_back (std::to_string (id));
    // Loop through vars in rectype and add to out
    for (int j = 0; j < num_vars_rectype[rt_index]; j++) {
      Rbyte* cur_text_pos = row_raw + starts[rt_index][j];
      int cur_var_width = widths[rt_index][j];
      std::string cur_text = as<std::string>(Rf_mkCharLenCE((const char*)cur_text_pos, cur_var_width, encoding_ce));

      out[rt_index][j + 1].push_back(cur_text);
    }
  }

  return wrap(out);
}

