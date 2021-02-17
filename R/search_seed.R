#' search best seed in nMDS
#'
#' @param n number of seed
#' @param .data data
#' @param k dimention
#' @param trymax trymax
#' @param maxit maxit
#' @param ... see metaMDS
#' @export
search_seed <- function(n = 100, .data, k = 2, trymax = 1000, maxit = 1000, ...){
  tbl <- tibble::tibble(mds = vector("list", length = n),
                        stress = vector("double", length = n))
  for (i in 1:n) {
    set.seed(i)
    tbl$mds[[i]] <- vegan::metaMDS(.data, k = k, trymax = trymax, maxit = maxit, ...)
    tbl$stress[[i]] <- tbl$mds[[i]]$stress
    set.seed(NULL)
  }
  out <- tbl %>%
    tibble::rownames_to_column("seed") %>%
    dplyr::filter(stress == min(stress))
  return(out)
}
