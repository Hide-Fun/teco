#' Convert matrix to tibble.
#'
#' @param .mat matrix
#' @param .row logical, convert rowname to column?
#' @param .rowname character, rowname.
#' @export
#' @examples
#' mat <-
#'    matrix(data = rnorm(100),
#'           nrow = 10,
#'           ncol = 10)
#'
#'rownames(mat) <- sprintf("com_%d", 1:10)
#'
#'# convert to tibble.
#'matrix_to_tibble(.mat = mat, .row = FALSE)
#'# move rownames to column.
#'matrix_to_tibble(.mat = mat, .row = TRUE, .rowname = "com")
#'
matrix_to_tibble <- function(.mat, .row = FALSE, .rowname = NULL) {
  rlt <- .mat %>%
    as.data.frame()
  if(.row == TRUE) {
    if(is.null(.rowname)) {
      stop("You must set .rownames")
    } else{
      rlt <- rlt %>%
        tibble::rownames_to_column(var = .rowname)
    }
  } else {
    rlt
  }
  rlt %>% tibble::as_tibble()
}
