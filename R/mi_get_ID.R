#' Get ID data from the `Biomart` database using `attributes`.
#'
#' @param dataset Datasets of the selected BioMart database.
#' @param mirror Specify an Ensembl mirror to connect to.
#' @param biomart BioMart database name you want to connect to. Use `biomaRt::listEnsembl` to retrieve the possible database names.
#' @param attributes A dataframe.The information we want to retrieve.Use `mi_get_ID_attr` to hava try.
#'
#' @importFrom biomaRt useEnsembl getBM
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across rename select
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
#' @return A `tibble` dataframe.
#' @export
mi_get_ID <- function(attributes, biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") {
  #Connect to selected BioMart databases and datasets hosted by Ensemble.
  Ensembl <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  #Convert attributes to a list.
  out <- vector("list", length = nrow(attributes))
  #Retrieve ID according to the name of attributes and sleep for 0.5 seconds if an error occurs during the retrieval process.
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
  #The first column of tibble is named ID and the elements inside are converted to characters, the second column is named class.
  to_2col <- function(df) {
    df <- df %>%
      as_tibble() %>%
      mutate(class = colnames(.)[1]) %>%
      rename(ID = 1) %>%
      mutate(across(.cols = 1, .fns = as.character))
    #If there is no match for the ID character, the ID of the corresponding position of tibble is converted to NA and class is converted to NA.
    if(any(str_detect(pull(df,ID)," "))){
        return(tibble(ID=NA,class=NA))
    }else{
      return(df)
    }
  }
  #Batch read data file and merge (same column name), remove rows with missing ID and class.
  map_dfr(out, .f = to_2col) %>% drop_na()
}
