#' Extract point from nMDS.
#'
#' @param .mds mds
#' @export
mds_point <- function(.mds) {
  .mds$mds[[1]]$points %>%
    as.data.frame() %>%
    tibble::rownames_to_column("individual") %>%
    tibble::as_tibble()
}
