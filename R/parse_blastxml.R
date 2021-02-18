#' Parse blast output xml
#'
#' @param .xml blast output xml.
#' @param .col colnames. (not inplemented)
#' @param .multi logical, (not inplemented)
#' @export
parse_blastxml <- function(.xml, .col = NULL, .multi = FALSE) {
  if(is.null(.col)) {
    # get iteration id.
    iteration_col <- list(
      "Iteration_query-ID",
      "Iteration_query-def",
      "Iteration_query-len"
    )
    iteration <- purrr::map_dfc(iteration_col, GetXmlText, .xml = .xml)

    # get hit table.
    hit_col <- list(
      "Hit_num",
      "Hit_id",
      "Hit_def",
      "Hit_accession",
      "Hit_len"
    )
    hit <- purrr::map_dfc(hit_col, GetXmlText, .xml = .xml)
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
    hsp <- purrr::map_dfc(hsp_col, GetXmlText, .xml = .xml)

    # bind hit and hsp.
    hit_hsp <- bind_cols(hit, hsp) %>%
      dplyr::mutate(Hit_num = parse_number(Hit_num),
                    Hsp_evalue = parse_number(Hsp_evalue),
                    Hsp_identity = parse_number(Hsp_identity),
                    `Hsp_align-len` = parse_number(`Hsp_align-len`))
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
