# MantaID

A machine-learning-based tool that automatically recognizes biological database IDs using machine learning. Additionally, a shiny application is offered, accessible via [MantaID (shinyapps.io)](https://molaison.shinyapps.io/MantaID/) and API via [MantaIDapi](http://164.92.98.237/MantaIDapi/__docs__/).

-------------

## R Version 

R (>= 4.2.0)

## Import

biomaRt, ggplot2, caret, data.table, dplyr, keras, magrittr, mlr3, purrr, reshape2, scutr, stringr, tibble, tidyr, tidyselect, paradox.

```R
#Note: Run the following code for installing biomaRt, mlr3, mlr3tuning packages of specific version.
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager",repos = "http://cran.us.r-project.org")
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes",repos = "http://cran.us.r-project.org")
library(remotes)
install_version("mlr3learners","0.5.1",force = T,upgrade ="never")
install_version("mlr3tuning","0.13.1",force = T,upgrade ="never")
install_version("mlr3","0.13.4",force = T,upgrade ="never")
library(BiocManager)
BiocManager::install("biomaRt", version = "3.16")
```

## Installation

```R
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
install_bitbucket("Molaison/MantaID")
```

If installation fails, please run the following command before re-installing:

```r
options(download.file.method = "wininet")
```

## Description

The MantaID package provides a pipeline for gene ID identification based on R. Via MantaID, users can identify IDs quickly based on integrating a machine-learning-based model on a large scale. The general workflow includes data retrieving, processing and balancing, model tuning, training, and explaining. Each procedure is implemented with the functions in the R sub-fold.


## MantaID Framework

![Framework](Graph/FIG1.png)

## How To Use An API

First, remove any mismatched databases using regular expressions. Then visit the corresponding resource page using the URL, filter the results based on the access status code. Finally, retrieve the text of the web page and determine whether it contains information about nonexistent resources.

In addition, users can choose the search mode according to their personal needs, which can be divided into "quick" and "general".

```bash
curl -X GET "http://164.92.98.237/MantaIDapi/ID_Search?ID={ID of interest}&SearchMode={mode}" -H "accept: */*"
```

You can also perform it in R with the `GET` method. The result can be retrieved as a dataframe, containing matched databases, by running the script below in R.

```R
install.packages("httr","jsonlite")
library(httr)
library(jsonlite)
res = GET("http://164.92.98.237/MantaIDapi/ID_Search?ID={ID of interest}&SearchMode={mode}")
resultDF <- fromJSON(as.data.frame(fromJSON(rawToChar(res$content)))[1,1])
```

## MantaID Package User Instructions

```R
library(MantaID) 
```

### Data Retrieving

Search public databases for and download ID datasets. Here we choose to use the human genome dataset, Mirror choose asia mirror (depending on the region).     
`mi_get_ID_attr`: Get the attributes of the dataset associated with the ID.       
`flt_attri`: Select the dataset of interest by looking at the dataset.        
`mi get ID`: Compile the findings into a large table.

```R
attributes = mi_get_ID_attr(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
flt_attri = attributes %>% dplyr::slice(1,2,6,7)
data_ID = mi_get_ID(flt_attri,biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
data_ID
```

### Data Processing

Convert ID data into the format required for training.        
`mi_clean_data`: Do the training you need to reorganize the table and remove invalid values.        
`mi_get_padlen`: Get max length of ID data.        
`mi_split_col`: Split the string of the ID column word by word into multiple columns.          
`mi_to_numer`: Convert the splitted ID to a factor in a fixed mapping and then to a numeric type.

```R
data <- tibble::tibble(
	"ensembl_gene_id" = c("ENSG00000001626","ENSG00000002549","ENSG00000002586","ENSG00000002745"),
	'ensembl_exon_id' = c("ENSE00002398851","ENSE00002398851","ENSE00002398851","ENSE00002398851"),
	'refseq_peptide' = c("NP_001303256","-","NP_001382772","NP_001340728")
)
data_ID_clean = mi_clean_data(data,placeholder="-")
pad_len = mi_get_padlen(data_ID)
data_splt = mi_split_col(data_ID,cores = NULL,pad_len = pad_len)
str(data_splt)
data_fct = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
```

### Data Balancing

Balance the training datasets' minority and majority classes.          
`mi_balance_data`: Prevent the trained model from losing its ability to distinguish between databases with small numbers of IDs.

```R
data_blcd = mi_balance_data(data_fct,ratio = 0.3,parallel = F)
```

### Models Training

Train the models using the divided training set.     
`mi_run_bmr`: Compare classification models with small samples. 

```R
result <- mi_run_bmr(data_fct, row_num = 4000)
benchmark <- result[1]
score <- result[2] %>% as.data.table() %T>% print()
```

The results were used to determine the choice of models for Decision Tree (DT), Random Forest (RF), and eXtreme Gradient Boosting (XGBoost).    
`mi_tune_rp`: Tune the DT model by hyperband.     
`mi_train_rp`: Train the DT model.     
`mi_tune_rg`: Tune the RF model by hyperband.        
`mi_train_rg`: Train the RF model.        
`mi_tune_xgb`: Tune the XGBoost model by hyperband.      
`mi_train_xgb`: Train the XGBoost model.

```R
train = data_blcd[[1]] %>% mutate(across(-class,.fns = ~tidyr::replace_na(.x,0))) %>% dplyr::slice(sample(nrow(data_blcd[[1]]), 2000), preserve = TRUE) 
test = data_blcd[[2]]
#Decision Tree
inst_rp <- mi_tune_rp(train, test)
result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"), instance = inst_rp[[1]])
#Random Forest
inst_rg <- mi_tune_rg(train, test)
result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"), instance = inst_rg[[1]])
#Xgboost
inst_xgb <- mi_tune_xgb(train, test)
result_xgb <- mi_train_xgb(train, test, measure = msr("classif.acc"), instance = inst_xgb[[1]])
```

In addition to several classical machine learning algorithms, a Back Propagation Neural Network (BPNN) is used for classification.

This is achieved by calling `tensorflow` via the `keras` package, so `tensorflow` needs to be installed first. 

1. To install python and tensorflow dependencies, please follow the instructions provided by Tensorflow via [TensorFlow installation](https://www.tensorflow.org/install/pip?hl=zh-cn#system-install)

2. Use tensorflow in R. Please replace "/path/to/python.exe" with your python path.

```R
install.packages("reticulate")
library(reticulate)
path_to_python <- use_python(python = "/path/to/python.exe")
```

`mi_train_BP`: Train the BPNN model with a three-layers neural network.

```R
result_BP <- mi_train_BP(train, test, path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3)
```

### Models Explaining

The heatmap of the model and confusion matrix is returned after training.        
`mi_get_confusion`: Plot confusion matrix with the results obtained from trained model.          
`mi_plot_heatmap`: Plot the heatmap for the confusion matrix.        
`mi_unify_mod`: Predict with four models and unify results by the submodels' specificity score.

```R
matri_rp <- mi_get_confusion(result_rp)
matri_rg <- mi_get_confusion(result_rg)
matri_xgb <- mi_get_confusion(result_xgb)
matri_BP <- mi_get_confusion(result_BP,ifnet = T)

mi_plot_heatmap(matri_rp, name="rp",filepath = "Graph/")
mi_plot_heatmap(matri_rg, name="rg",filepath = "Graph/")
mi_plot_heatmap(matri_xgb, name="xgb",filepath = "Graph/")
mi_plot_heatmap(matri_BP, name="BP",filepath = "Graph/")
data("mi_data_rawID")
mi_unify_mod(mi_data_rawID, "ID",result_rg,result_rp,result_xgb,result_BP,c_value = 0.75, pad_len = pad_len)
```
