#' Parse row number to grouping id.
#'
#' This function make index_1.fasta and index_2.fasta from SampleSheet.csv.
#' @param .df tibble, data.frame
#' @param .col_of_row_number name of row number column.
#' @param .group name of group column.
#' @export
#' @examples
#'
#' df <-
#'    data.frame(x = seq(1, 10, 1),
#'               y = c(1, 2, 3, 1, 2, 1, 2, 3, 4, 5))
#'
#' row_number_to_group(df, .col_of_row_number = "y", .group = "group")
#'
row_number_to_group <- function(.df,
                                .col_of_row_number,
                                .group) {
  if(!is.numeric(.df[[.col_of_row_number]])) {
    stop("`.col_of_row_number` must be <int> or <dbl>")
  }
  df <- .df
  df[.group] <- NA_integer_
  PlaceColOfRowNumber <- which(colnames(df) == .col_of_row_number)
  PlaceColGroup <- which(colnames(df) == .group)
  for(i in 1:nrow(.df)) {
    if(i == 1) {
      df[i, PlaceColGroup] <- 1
    } else{
      if(df[i-1, PlaceColOfRowNumber] + 1 == df[i, PlaceColOfRowNumber]) {
        df[i, PlaceColGroup] <- df[i-1, PlaceColGroup]
      } else {
        df[i, PlaceColGroup] <- df[i-1, PlaceColGroup] + 1
      }
    }
  }
  return(df)
}
