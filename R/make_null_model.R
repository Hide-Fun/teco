#' Make null model matrix.
#'
#' @param .n number of iteration.
#' @param .comm community data matrix.
#' @param .model_type randomization method, see vegan::randomizeMatrix
#' @param .method dissimilarity index. see vegan::vegdist.
#' @param .weighted convert `.comm` to presence/absence data.
#' @param ... augument passed through `.f`.
#' @export
make_null_model <- function(.n,
                            .comm,
                            .tree,
                            .model_type,
                            .method,
                            .weighted,
                            #.f = c("permatfull", "permatswap", "randomizeMatrix"),
                            ...){
  message("This function use furrr package internally.\nIf you run this function in parallel, set a `plan` for how the code should run.")
  # make empty matrix.
  null_model <- rep(list(.comm), .n)
  # insert value.
  if(.method %in% c("beta.mpd", "beta.mntd", "unifrac")) {
    print("use picante::tipShuffle.")
    null_models <- furrr::future_map_dfr(.x = null_model,
                                         .f = ~CalcNullModelTree(.comm = .x,
                                                                 .weighted = .weighted,
                                                                 .method = .method,
                                                                 .tree = .tree),
                                         .id = "iteration",
                                         .options = furrr::furrr_options(seed = 1L),
                                         .progress = TRUE)
  } else {
    null_models <- furrr::future_map_dfr(.x = null_model,
                                         .f = ~CalcNullModel(.comm = .x,
                                                             .model_type = .model_type,
                                                             .method = .method,
                                                             .weighted = .weighted),
                                         .id = "iteration",
                                         .options = furrr::furrr_options(seed = 1L),
                                         .progress = TRUE)
  }
  # calculate SES.
  null_model <- null_models %>%
    tidyr::pivot_wider(names_from = "iteration", values_from = "distance") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mean = mean(dplyr::c_across(where(is.numeric))),
                  sd = sd(dplyr::c_across(where(is.numeric)))) %>%
    dplyr::ungroup()
  return(null_model)
}

CalcNullModel <- function(.comm,
                          .model_type,
                          .weighted,
                          .method,
                          #.f = c("permatfull", "permatswap", "randomizeMatrix"),
                          ...) {
  rmat <- picante::randomizeMatrix(samp = .comm, null.model = .model_type, ...)
  dist <- try(vegan::vegdist(x = rmat, method = .method, binary = .weighted))
  null_model <- tidy_dist(dist) %>%
    dplyr::mutate(pair = stringr::str_c(item1, item2, sep = "-")) %>%
    dplyr::select(-item1, -item2)
  return(null_model)
}

CalcNullModelTree <- function(.comm,
                              .weighted,
                              .method,
                              .tree) {
  shuffule_tree <- picante::tipShuffle(phy = .tree)
  if(.method == "unifrac") {
    if(.weighted == T) {
      .weighted <- "W"
    } else {
      .weighted <- "UW"
    }
    dist <- as.dist(get_unifrac(.com = .comm, .tree = shuffule_tree, .alpha = 0.5, .data_type = .weighted))
  } else if(.method == "beta.mpd") {
    dist <- comdist(comm = .comm, dis = cophenetic(shuffule_tree), abundance.weighted = .weighted)
  } else if(.method == "beta.mntd") {
    dist <- comdistnt(comm = .comm, dis = cophenetic(shuffule_tree), abundance.weighted = .weighted)
  }
  null_model <- tidy_dist(dist) %>%
    dplyr::mutate(pair = stringr::str_c(item1, item2, sep = "-")) %>%
    dplyr::select(-item1, -item2)
  return(null_model)
}

# code from tidy.dist in broom package (https://github.com/tidymodels/broom).
# This function will be deprecated in next release of broom package.
# so, write below.
tidy_dist <- function(x, diagonal = attr(x, "Diag"),
                      upper = attr(x, "Upper"), ...) {

  ret <- as.matrix(x) %>%
    tibble::as_tibble(rownames = "item1") %>%
    tidyr::pivot_longer(cols = c(dplyr::everything(), -1)) %>%
    dplyr::rename(item2 = 2, distance = 3) %>%
    dplyr::mutate(item1 = as.factor(item1), item2 = as.factor(item2))

  if (!upper) {
    ret <- as.data.frame(ret)[!upper.tri(as.matrix(x)), ]
  }

  if (!diagonal) {
    ret <- dplyr::filter(ret, item1 != item2)
  }
  tibble::as_tibble(ret)
}
