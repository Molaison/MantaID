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
  #连接到选定的BioMart数据库和由Ensemble托管的数据集
  Ensembl <- useEnsembl(biomart = biomart, dataset = dataset, mirror = mirror, verbose = TRUE)
  #将attributes转换成列表
  out <- vector("list", length = nrow(attributes))
  #根据attributes的name检索ID，如果在检索过程中出现错误，则睡眠0.5秒
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
  #将tibble的第一列命名为ID，并将里面的元素转换成字符,第二列命名为class
  to_2col <- function(df) {
    df <- df %>%
      as_tibble() %>%
      mutate(class = colnames(.)[1]) %>%
      rename(ID = 1) %>%
      mutate(across(.cols = 1, .fns = as.character))
    #如果存在匹配不到ID字符的情况，则tibble对应位置的ID为NA，class为NA
    if(any(str_detect(pull(df,ID)," "))){
        return(tibble(ID=NA,class=NA))
    }else{
      return(df)
    }
  }
  #批量读取数据文件并合并（列名相同），去除ID和class都缺失的行
  map_dfr(out, .f = to_2col) %>% drop_na()
}
