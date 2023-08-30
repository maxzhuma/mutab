#' @rawNamespace useDynLib(mutab, .registration=TRUE)
#' @rawNamespace exportPattern("^[[:alpha:]]+")
#' @import Rcpp
#' @import haven
#' @import labelled
#' @import dplyr
#' @import purrr


#' @name mutab_labelled
#' @title Table for multi-response set from labelled df
#'
#' @description
#' Creates either a frequency table or a contingency table for 
#' multi-nominal variable (multiple response set) from labelled data frame.
#'
#' @param dfLabelled Labelled data frame
#' @param countValue Numeric. Value to count as valid response.
#' @param extractLabelPattern Regex pattern string, optional. A pattern to
#' extract a substring from variable label, the desired pattern has to be a
#' within the first regex group "()".
#' @param by Vector of class `labelled`, containing split variable.
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
           extractLabelPattern = NULL,
           by = NULL) {
    input_labels <- var_label(dfLabelled)
    
    if (is.null(extractLabelPattern) == F) {
      input_labels <- input_labels %>%
        gsub(extractLabelPattern, "\\1", .)
    }
    
    df <-
      tryCatch({
      dfLabelled %>%
      map(\(x) {
        numcol <- as.numeric(x)
      })},
      error = function(err) {
        message("Can't coerce all columns in df into numeric")
      }) %>%
      bind_cols()
    
    if (is.null(by)) {
      tab_out <- 
        create_mr_count_table(df, countValue) %>%
        mutate(labels = input_labels) %>%
        relocate(labels, .after = 2)
    } else {
      val_labels_by <- names(val_labels(by))
      by <- as.numeric(by)
      tab_out <-
        create_mr_count_crosstable(df, countValue, by) %>%
        mutate(
          labels = input_labels
          ) %>%
        relocate(labels, .after = 1)
      for(i in seq(3,ncol(tab_out)-1)) {
        tab_out[[i]] <- set_label_attribute(tab_out[[i]], val_labels_by[[i-2]])
      }
    }
    
    return(tab_out)
  }
