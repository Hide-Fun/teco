#' Make null model matrix.
#'
#' @param .n number of iteration.
#' @param .mat community data matrix.
#' @param .model_type randomization method, see vegan::randomizeMatrix
#' @param .method dissimilarity index. see vegan::vegdist.
#' @param .binary convert `.mat` to presence/absence data.
#' @param ... augument passed through `.f`.
#' @export
make_null_model <- function(.n,
                            .mat,
                            .model_type,
                            .method,
                            .binary,
                            #.f = c("permatfull", "permatswap", "randomizeMatrix"),
                            ...){
  message("This function use furrr package internally.\nIf you run this function in parallel, set a `plan` for how the code should run.")
  # make empty matrix.
  null_model <- matrix(nrow = nrow(.mat) * (nrow(.mat) - 1) / 2, ncol = .n)
  # insert value.
  null_model <- furrr::future_map_dfr(.x = .mat,
                               .f = CalcNullModel,
                               .mat = .mat,
                               .model_type = .model_type,
                               .binary = .binary,
                               .id = "iteration",
                               .options = furrr_options(seed = 1L),
                               .progress = TRUE)
  # calculate SES.
  null_model <- null_model %>%
    tidyr::pivot_wider(names_from = "iteration", values_from = "distance") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(mean = mean(dplyr::c_across(where(is.numeric))),
                  sd = sd(dplyr::c_across(where(is.numeric)))) %>%
    dplyr::ungroup()
  return(null_model)
}

CalcNullModel <- function(.mat,
                          .model_type,
                          .binary,
                          #.f = c("permatfull", "permatswap", "randomizeMatrix"),
                          ...) {
  rmat <- picante::randomizeMatrix(samp = .mat, null.model = .model_type, ...)
  dist <- try(vegan::vegdist(x = rmat, method = .method, binary = .binary))
  null_model <- broom::tidy(dist)
  return(null_model)
}
