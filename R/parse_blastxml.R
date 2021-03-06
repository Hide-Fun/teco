#' Parse blast output xml
#'
#' @param .xml blast output xml.
#' @param .col colnames. (not inplemented)
#' @param .multi logical, (not inplemented)
#' @export
parse_blastxml <- function(.xml, .col = NULL, .multi = FALSE) {
  message("This function use furrr package internally.\nIf you run this function in parallel, set a `plan` for how the code should run.")
  if(is.null(.col)) {
    # get iteration id.
    iteration_col <- list(
      "Iteration_query-ID",
      "Iteration_query-def",
      "Iteration_query-len"
    )
    iteration <- furrr::future_map_dfc(iteration_col, GetXmlText, .xml = .xml) %>%
      dplyr::rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all, pattern = PatColnames(.x = iteration_col))

    # get hit table.
    hit_col <- list(
      "Hit_num",
      "Hit_id",
      "Hit_def",
      "Hit_accession",
      "Hit_len"
    )
    hit <- furrr::future_map_dfc(hit_col, GetXmlText, .xml = .xml) %>%
      dplyr::rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all, pattern = PatColnames(.x = hit_col))

    # get hsp table.
    hsp_col <- list(
      "Hsp_num",
      "Hsp_bit-score",
      "Hsp_score",
      "Hsp_evalue",
      "Hsp_query-from",
      "Hsp_query-to",
      "Hsp_hit-from",
      "Hsp_hit-to",
      "Hsp_query-frame",
      "Hsp_hit-frame",
      "Hsp_identity",
      "Hsp_positive",
      "Hsp_gaps",
      "Hsp_align-len",
      "Hsp_hseq"
    )

    hsp <- furrr::future_map_dfc(hsp_col, GetXmlText, .xml = .xml) %>%
      dplyr::rename_with(.cols = dplyr::everything(), .fn = stringr::str_replace_all, pattern = PatColnames(.x = hsp_col))

    # bind hit and hsp.
    hit_hsp <- dplyr::bind_cols(hit, hsp) %>%
      dplyr::mutate(Hit_num = readr::parse_number(Hit_num),
                    Hsp_evalue = readr::parse_number(Hsp_evalue),
                    Hsp_identity = readr::parse_number(Hsp_identity),
                    `Hsp_align-len` = readr::parse_number(`Hsp_align-len`))
    # add id.
    hit_hsp_id <- row_number_to_group(hit_hsp,
                                      .col_of_row_number = "Hit_num",
                                      .group = "itr") %>%
      dplyr::group_nest(itr)
    # bind.
    rlt <- iteration %>%
      dplyr::bind_cols(data = hit_hsp_id) %>%
      tidyr::unnest(data) %>%
      dplyr::mutate(accession_id = stringr::str_extract(Hit_def, "[A-Z]+\\d+"),
                    vtx = stringr::str_extract(Hit_def, "VTX\\d+")) %>%
      dplyr::rename(otu = `Iteration_query-def`)
    return(rlt)
  }
}


GetXmlText <- function(.xml, .xpath) {
  xml2::xml_find_all(.xml, paste0("//", .xpath)) %>%
    xml2::xml_text()
}

# make colname pattern.
PatColnames <- function(.x) {
  len <- length(.x)
  colname <- paste0("...", rev(1:len))
  rlang::set_names(nm = colname,
                   x = rev(unlist(.x)))
}
