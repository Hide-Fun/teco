#' Calculate beta diversity.
#'
#' @param .method dissimilarity index.
#' @param .comm community data matix.
#' @param .tree an object of class "phylo".
#' @param .alpha passed through Gunifrac
#' @param .weighted logical, presence/absence or abundance.
#' @export
beta_dist <- function(.method, .comm, .tree, .alpha = 1, .weighted) {
  if(!.method %in% c("unifrac", "beta.mpd", "beta.mntd")) {
    dist <- vegan::vegdist(x = .comm, method = .method, binary = .weighted)
  } else if(.method == "unifrac") {
    if(.weighted == T) {
      .weighted <- "W"
    } else {
      .weighted <- "UW"
    }
    dist <- as.dist(get_unifrac(.com = .comm, .tree = .tree, .alpha = .alpha, .data_type = .weighted))
  } else if(.method == "beta.mpd") {
    dist <- picante::comdist(comm = .comm, dis = cophenetic(.tree), abundance.weighted = .weighted)
  } else if(.method == "beta.mntd") {
    dist <- picante::comdistnt(comm = .comm, dis = cophenetic(.tree), abundance.weighted = .weighted)
  }
  return(dist)
}
