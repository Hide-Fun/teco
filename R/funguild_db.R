#' \code{funguild_db}: Creating table for FUNGuild.
#'
#' Creating table for analysis of FUNGuild.
#' @param .otu_table table (see example).
#' @param .identify_list identifying result (see example).
#' @param .tax_pat character, specify taxonomic levels containing .identify_list in same order.
#' @export
#' @examples
#'
#' # make otu table.
#' otu_table <- data.frame(
#'    samplename = c("sample1", "sample2"),
#'    otu1 = c(1, 0),
#'    otu2 = c(0, 39)
#' )
#'
#' # make identify list.
#' identify_list <- data.frame(
#'    otu = c("otu1", "otu2"),
#'    phylum = c("p1", "p2"),
#'    order = c("o1", "o2"),
#'    family = c("f1", "f2"),
#'    genus = c("g1", "g2")
#' )
#'
#' funguild_db(.otu_table = otu_table,
#'             .identify_list = identify_list,
#'             .tax_pat = c("phylum", "order", "family", "genus"))

funguild_db <- function(.otu_table, .identify_list, .tax_pat) {
  table <- .otu_table
  group <-  .identify_list

  if("OTU" %in% colnames(group)) {
    group <- group %>%
      dplyr::rename(otu = OTU)
  }

  print(paste0("Row number of otu_table is", " ", nrow(table), "."))
  print(paste0("Row number of group is", " ", nrow(group), "."))

  if(any(colnames(group) %in% "query" == T)) {
    print("Error: identify_list has query column, you should rename query to OTU !!")
  } else{
    taxonomy <- group %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~stringr::str_replace_na(., "NA"))) %>%
      tidyr::unite(col = "taxonomy", .tax_pat, sep = ";", remove = T) %>%
      dplyr::mutate(taxonomy = stringr::str_replace_all(taxonomy, "NA", "unidentified"))
    Funguild_DB <- table %>%
      tidyr::pivot_longer(-samplename, names_to = "otu", values_to = "read_num") %>%
      tidyr::pivot_wider(names_from = "samplename", values_from = "read_num") %>%
      dplyr::right_join(taxonomy, by = "otu") %>%
      dplyr::select(-taxonomy, dplyr::everything())
    return(Funguild_DB)
  }
}
