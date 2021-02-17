#' Convert tibble to dist.
#'
#' This function is inverse operation of broom::tidy() against dist object.
#' @param .dist_tbl tibble, data.frame
#' @param .matrix output
#' @export
tibble_to_dist <- function(.dist_tbl, .matrix = F) {
  # get column.
  col <- levels(.dist_tbl$item1)

  same_pair <- tibble::tibble(
    item1 = col,
    item2 = col,
    distance = 0
  )

  mat <- dist_tbl %>%
    dplyr::bind_rows(same_pair) %>%
    tidyr::pivot_wider(names_from = "item2",
                       values_from = "distance",
                       values_fill = 0) %>%
    dplyr::arrange(item1) %>%
    dplyr::relocate(item1, col) %>%
    tibble::column_to_rownames("item1") %>%
    as.matrix()

  dist_mat <- mat + t(mat)
  dist <- as.dist(dist_mat)
  if(.matrix == T) {
    dist <- dist_mat
  }
  return(dist)
}
