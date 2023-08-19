#' @rawNamespace useDynLib(mutab, .registration=TRUE)
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @import Rcpp
#' @import haven
#' @import labelled
#' @import dplyr
#' @import purrr


#' @name mutab_labelled
#' @title Frequency table for multi-response set from labelled df
#'
#' @description
#' Creates a basic frequency table for multi-nominal variable (multiple response
#' set) from labelled data frame.
#'
#' @param dfLabelled Labelled data frame
#' @param countValue Numeric. Value to count as valid response.
#' @param extractLabelPattern Regex pattern string, optional. A pattern to
#' extract a substring from variable label, the desired pattern has to be a
#' within the first regex group "()".
#' @returns A data frame with frequency table.
#' @examples
#' \dontrun{
#' # Make a frequency table for multinom. Used `dplyr::select()` for subsetting.
#'
#' freq <- mutab_labelled(
#'   select(df, starts_with("V12_")),        # data frame, with subset of columns selected
#'   1,                                      # counted value
#'   extractLabelPattern = "^(.*):.*".       # extract all before first colon as a label
#' )
#'
#' }
#' @export
mutab_labelled <-
  function(dfLabelled,
           countValue,
           extractLabelPattern = NULL) {
    input_labels <- var_label(dfLabelled)
    
    if (is.null(extractLabelPattern) == F) {
      input_labels <- input_labels %>%
        gsub(extractLabelPattern, "\\1", .)
    }
    
    df <- dfLabelled %>%
      map(\(x) {
        numcol <- as.numeric(x)
      }) %>%
      bind_cols()
    
    freq <- create_mr_count_table(df, countValue)
    
    tab_out <- freq %>%
      mutate(labels = input_labels) %>%
      relocate(labels, .after = 2)
    
    return(tab_out)
    
  }
