#' Get ID attributes from the `Biomart` database.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to.Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @importFrom dplyr filter
#' @importFrom stringr str_length
#' @importFrom biomaRt useEnsembl listAttributes
#' @importFrom magrittr %>%
#' @return A dataframe.
#' @export

#Select the Ensemble dataset of eligible human genes in the BioMart database.
mi_get_ID_attr <- function(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  #Connect to selected BioMart databases and datasets hosted by Ensemble.
  ensemb_hm_dset <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  #Filter attributes from the description column that contain ID or name but not end, start, description, probe, version, content, Aberrant,Source,Strain ,Chromosome,BioGrid,evidence and are less than 18 characters long
  attributes <- listAttributes(ensemb_hm_dset) %>%
    filter(grepl(.[["description"]], pattern = "([iI][dD])|(name)", ignore.case = TRUE)) %>%
    filter(!grepl(.[["description"]], pattern = "(end)|(start)|(description)|(probe)|(version)|(content)|(Aberrant)|(Source)|(Strain)|(Chromosome)|(BioGRID)|(evidence)", ignore.case = TRUE)) %>%
    filter(!grepl(.[["name"]], pattern = "(homolog)|(paralog)", ignore.case = TRUE))
}
