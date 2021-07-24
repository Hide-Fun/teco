#' Assign VTX
#'
#' @param .blast_result data.frame
#' @param .param_strict
#' @param .param_relax
#' @param .headers header names.
#' @export
assign_vtx <- function(.blast_result,
                       .labels = c(strict = "no", relax = "closest_to"),
                       .param_strict = c(evalue = 1e-50, pident = 97, qcov = 80),
                       .param_relax = c(evalue = 1e-50, pident = 90, qcov = 80),
                       .headers = NULL,
                       .print_data = FALSE) {
  if(is.null(.headers)) {
    blast_result <- .blast_result
  } else {
    pat_blast <- set_names(
      nm = sprintf("X%d", 1:length(.headers)),
      x = rev(.headers)
      )
    blast_result <- .blast_result %>%
      rename_with(str_replace_all, everything(), pat_blast)
  }
  # strict.
  strict <- blast_result %>%
    filter(evalue <= .param_strict[[1]],
           pident >= .param_strict[[2]],
           qcov >= .param_strict[[3]])
  # assign VTX.
  assign_strict <- strict %>%
    group_by(qacc) %>%
    slice_max(pident, n = 1, with_ties = TRUE) %>%
    nest() %>%
    mutate(nrow = map_int(data, nrow))

  assign_strict_pidentmax <- assign_strict %>%
    filter(nrow == 1) %>%
    mutate(assigned = map_chr(data, pull, vtx))

  assign_strict <- strict %>%
    mutate(percent = map(data, CalcPercent),
           nrow = map_int(percent, nrow),
           vtx = map(data, pull, vtx))
  VTX <- assign_strict %>%
    filter(nrow == 1) %>%
    select(qacc, data) %>%
    unnest(data) %>%
    group_by(qacc) %>%
    slice_max(pident, n = 1, with_ties = F) %>%
    mutate(label = str_c(qacc, "putative", vtx, sep = "-"))

  # select putative OTUs.
  putatives <- maarjam_assign_vtx97 %>%
    filter(nrow == 1) %>%
    select(qacc, data) %>%
    unnest(data) %>%
    group_by(qacc) %>%
    slice_max(pident, n = 1, with_ties = F) %>%
    mutate(label = str_c(qacc, "putative", vtx, sep = "-"))
  relax <- blast_result %>%
    filter(evalue <= .param_relax[[1]],
           pident >= .param_relax[[2]],
           qcov >= .param_relax[[3]])
}
# "qseqid", "qacc", "qlen", "sseqid", "sacc", "slen", "evalue", "score", "pident", "qcovs"

CalcPercent <- function(.df) {
  df <- .df %>%
    group_by(vtx) %>%
    summarise(n = n())
  all <- sum(df$n)
  rlt <- df %>%
    mutate(per = 100*n/all)
  return(rlt)
}

AssignTaxon <- function(.df, .priority_param = "pident") {
  top_hit <- .df %>%
    dplyr::group_by(qacc) %>%
    dplyr::slice_max(pident, n = 1, with_ties = TRUE) %>%
    dplyr::filter(n() == 1)
  majority <- .df %>%
    dplyr::group_by(qacc) %>%
    dplyr::slice_max(pident, n = 1, with_ties = TRUE) %>%
    dplyr::filter(n() != 1) %>%
    dplyr::group_by(vtx, .add = TRUE) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::slice_max(n, n = 1, with_ties = TRUE)
  majority_hit <- majority %>%
    dplyr::filter(n() == 1)
  residuals <- majority %>%
    dplyr::filter(n() != 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(qacc, vtx) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE)
}


