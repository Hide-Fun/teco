#' Convert tibble to dist.
#'
#' This function is inverse operation of broom::tidy() against dist object.
#' @param .dist_tbl tibble, data.frame
#' @param .col_name column name.
#' @param .matrix output
#' @export
tibble_to_dist <- function (.dist_tbl, .col_name = "ses", .matrix = F)
{
  # select column.
  dist_tbl <- .dist_tbl %>%
    select(item1, item2, .col_name)
  # select unique column.
  col <- unique(c(dist_tbl$item1, dist_tbl$item2))
  # add same pair.
  same_pair <- tibble::tibble(item1 = col,
                              item2 = col,
                              .col_name = 0)
  # convert to matrix.
  mat <- dist_tbl %>%
    dplyr::bind_rows(same_pair) %>%
    tidyr::pivot_wider(names_from = "item2",
                       values_from = .col_name,
                       values_fill = 0) %>%
    dplyr::relocate(item1, all_of(col)) %>%
    dplyr::arrange(factor(item1, col)) %>%
    tibble::column_to_rownames("item1") %>%
    as.matrix()
  dist_mat <- mat + t(mat)
  dist <- as.dist(dist_mat)
  if (.matrix == T) {
    dist <- dist_mat
  }
  return(dist)
}
