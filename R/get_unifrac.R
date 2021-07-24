#' Get UniFrac distance.
#'
#' @param .com community data matrix.
#' @param .tree phylogenetic tree (phylo object).
#' @param .alpha controlling the weight on abundant.
#' @param .data_type "W", "UW", "VAW". see GUniFrac::GUniFrac help.
#' @export
get_unifrac <- function(.com, .tree, .alpha = 0.5, .data_type) {
  gunifrac <- GUniFrac::GUniFrac(otu.tab = .com,
                                 tree = .tree,
                                 alpha = .alpha)
  unifracs <- gunifrac$unifracs
  if(.data_type == "W") {
    .alpha <- 1
    W <- paste0("d_", as.character(.alpha))
    dist_mat <- unifracs[, , W]
  } else if(.data_type == "UW") {
    dist_mat <- unifracs[, , "d_UW"]
  } else if(.data_type == "VAW") {
    dist_mat <- unifracs[, , "d_VAW"]
  } else if(.data_type == "ALL"){
    dist_mat <- unifracs
  } else {
    W <- paste0("d_", as.character(.alpha))
    dist_mat <- unifracs[, , W]
  }
  return(dist_mat)
}
