#include <Rcpp.h>
using namespace Rcpp;

// Function to create a summary table for a data frame
// [[Rcpp::export]]
DataFrame create_mr_table(DataFrame df, int countValue) {
  // Get the number of rows and columns in the data frame
  int nRows = df.nrow();
  int nCols = df.length();
  
  // Create a vector to store column indices
  std::vector<int> colindices;
  
  CharacterVector colnames;
  CharacterVector df_names = df.names();
  
  // Create a vector to store counts of 1's
  std::vector<int> counts;
  
  // Iterate over the columns of the data frame
  for (int i = 0; i < nCols; ++i) {
    
    // Extract the column as a logical vector
    LogicalVector column = df[i];
    
    // Count the number of countValues in the column
    int count = 0;
    for (int j = 0; j < nRows; ++j) {
      if (column[j] == countValue) {
        count++;
      }
    }
    
    // Add the column name and count to the respective vectors
    colindices.push_back(i+1);
    counts.push_back(count);
    colnames.push_back(df_names[i]);
    
  }
  
  // Create a new data frame with the column names and counts
  DataFrame summaryTable = DataFrame::create(
    Named("colidx") = colindices,
    Named("colname") = colnames,
    Named("counts") = counts
  );
  
  return summaryTable;

}


