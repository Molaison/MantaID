library(biomartr)
library(biomaRt)
library(tidyverse)
library(magrittr)


#' Get ID attributes from `Biomart` database.
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
GET_ID_attributes <- function(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  # 使用asia mirror获取hsapiens_gene_ensembl数据库
  ensemb_hm_dset <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  attributes <- listAttributes(ensemb_hm_dset) %>%
    # 筛选可能包含ID的属性
    filter(grepl(.[["description"]], pattern = "(id)|(name)", ignore.case = TRUE)) %>%
    # 去除一些不需要的属性
    filter(!grepl(.[["description"]], pattern = "(end)|(start)|(description)|(probe)|(version)|(content)|(Aberrant)|(Source)|(Strain)|(Chromosome)", ignore.case = TRUE)) %>%
    # 名字过长的属性往往不是id,而是基因的位置等信息对应的索引等
    filter(str_length(.[["name"]]) < 18)
}

#' Get ID data from `Biomart` database use `attributes`.
#'
#' @param Ensembl `Mart` object from `biomaRt`.
#' @param attributes A dataframe.The information we want to retrieve.Use `GET_ID_attributes` to hava try.
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across rename select
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @return A `tibble` dataframe.
#' @export
#'
#' @examples
#' data(attributes)
#' GETID(attributes[1:3, ])
GETID <- function(attributes, Ensembl = NULL) {
  if (is.null(Ensembl)) {
    Ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia", verbose = TRUE)
  }
  out <- vector("list", length = nrow(attributes))
  for (i in 1:nrow(attributes)) {
    try_result <- try({
      out[[i]] <- getBM(attributes = unique(attributes[["name"]])[i], mart = Ensembl)
      sprintf("The %dth getFunc successed!", i)
    })
    if ("try-error" %in% class(try_result)) {
      next
    }
    Sys.sleep(0.5)
  }
  to_2col <- function(df) {
    df %>%
      as_tibble() %>%
      mutate(class = colnames(.)[1]) %>%
      rename(ID = 1) %>%
      mutate(across(.cols = 1, .fns = as.character))
  }
  map_dfr(out, .f = to_2col)
}
