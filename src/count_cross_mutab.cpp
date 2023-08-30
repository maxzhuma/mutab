#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export]]
DataFrame create_mr_count_crosstable(DataFrame df, int count_value, IntegerVector by) {
  
  int nRows = df.nrow();
  int nCols = df.length();
  
  IntegerVector vals = na_omit(sort_unique(by));
  int nVals = vals.length();
  
  CharacterVector vars = df.names();
  DataFrame table = DataFrame::create(
    Named("vars") = vars
  );
  
  CharacterVector names = {"vars"};
  
  for (int v = 0; v < nVals; ++v) {

    String name = "by_" +  std::to_string(vals[v]);
    names.push_back(name);
    
    NumericVector counts;
    for (int c = 0; c < nCols; ++c) {
      NumericVector col = df[c];
      int count = 0;
      for (int i = 0; i < nRows; ++i) {
        if (by[i] == vals[v] && col[i] == count_value) {
          count++;
        }
      }
      counts.push_back(count);
    }
    table.push_back(counts);
  }
  
  names.push_back("by_NA");
  
  NumericVector na_counts;
  for (int c = 0; c < nCols; ++c) {
    NumericVector col = df[c];
    int count = 0;
    for (int i = 0; i < nRows; ++i) {
      if (by[i] == NA_INTEGER && col[i] == count_value) {
        count++;
      }
    }
    na_counts.push_back(count);
  }
  table.push_back(na_counts);
  
  table.names() = names;
  return table;
}
    
