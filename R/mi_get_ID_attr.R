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

#在BioMart数据库中选择符合条件的人类基因的Ensemble数据集
mi_get_ID_attr <- function(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  #连接到选定的BioMart数据库和由Ensemble托管的数据集
  ensemb_hm_dset <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  #从description列中筛选包含ID或name但不包含end，start，description，probe，version，content，Aberrant,Source,Strain,Chromosome,BioGrid,evidence且字符长度小于18的attributes
  attributes <- listAttributes(ensemb_hm_dset) %>%
    filter(grepl(.[["description"]], pattern = "([iI][dD])|(name)", ignore.case = TRUE)) %>%
    filter(!grepl(.[["description"]], pattern = "(end)|(start)|(description)|(probe)|(version)|(content)|(Aberrant)|(Source)|(Strain)|(Chromosome)|(BioGRID)|(evidence)", ignore.case = TRUE)) %>%
    filter(!grepl(.[["name"]], pattern = "(homolog)|(paralog)", ignore.case = TRUE))
}
