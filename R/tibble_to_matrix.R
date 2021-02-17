#' Convert tibble to matrix.
#'
#' Convert tibble to matrix.
#' @param .tbl tibble
#' @param .row logical, Want rownames?
#' @param .rowname character, names of rownames column.
#' @export
#' @examples
#' library(tibble)
#'
#' tbl <-
#'    tibble(com = sprintf("com_%d", 1:10),
#'           V1 = rnorm(10),
#'           V2 = rnorm(10))
#'
#'# convert to matrix.
#'tibble_to_matrix(.tbl = tbl, .row = FALSE)
#'# move rownames to column.
#'tibble_to_matrix(.tbl = tbl, .row = TRUE, .rowname = "com")
#'
tibble_to_matrix <- function(.tbl, .row = FALSE, .rowname = NULL) {
  if(.row == TRUE) {
    if(is.null(.rowname)) {
      stop("You must set `.rowname`.")
    } else {
      rlt <- .tbl %>%
        tibble::column_to_rownames(.rowname)
    }
  } else {
    rlt <- .tbl %>%
      as.data.frame()
  }
  rlt_2 <- rlt %>%
    as.matrix()
  return(rlt_2)
}
