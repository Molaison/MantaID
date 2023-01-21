# MantaID <a href="https://molaison.github.io/MantaID/"><img src="man/figures/logo.svg" align="right" height="139" /></a>

## Introduction

This tutorial introduces MantaID, a machine-learning-based tool to automate the identification of biological database IDs. IDs are required for simple access to biological data and for facilitating cross-referencing between databases. However, the lack of universal agreement on the composition of a database ID, results in the situation that the same biological entity may have various IDs. Current ID conversion tools all require previous knowledge of the database to which they belong and are incapable of identifying the IDs in the absence of database names. MantaID bridges this gap enabling the identification of IDs, facilitating the text-mining in scientific documents and ID conversion. Additionally, a shiny application is offered, accessible via [MantaID (shinyapps.io)](https://molaison.shinyapps.io/MantaID/) and API via [MantaIDapi](http://164.92.98.237/MantaIDapi/__docs__/).

-------------

## How to use MantaID?

The computational framework and all the approaches of MantaID are implemented as a software package that handles all the different steps of the model development process and makes it easy to create user-defined ID recognition models by adjusting a few parameters.

### Preparation and session set up

This tutorial is based on R. If you have not installed R, you will find an introduction to run it in your computer [here](https://rstudio-education.github.io/hopr/starting.html). Before turning to the workflow, please install all the packages by running the code below.
	
```R
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager",repos = "http://cran.us.r-project.org")
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes",repos = "http://cran.us.r-project.org")
library(remotes)
library(BiocManager)
if (!requireNamespace("biomaRt", quietly = TRUE))
	BiocManager::install("biomaRt", version = "3.16")
install_bitbucket("Molaison/MantaID")
```

Next, we load the `MantaID` package.

```R
library(MantaID)
```

Because deep-learning is achieved in MantaID by calling `tensorflow` via the `keras` package, so `python` and `tensorflow` need to be installed first. 

1. To install python and tensorflow dependencies, please follow the instructions provided by Tensorflow via [TensorFlow installation](https://www.tensorflow.org/install/pip?hl=zh-cn#system-install).

2. Use tensorflow in R. Please replace "/path/to/python.exe" with your python path.

```R
install.packages("reticulate")
library(reticulate)
# path_to_python <- use_python(python = "/path/to/python.exe")
```

### Data preparation

This section illustrates how to procure data for training with `biomaRt` package. To give `MantaID` a test drive, we use human genome-related datasets to train a `MantaID` model. Users can specify another dataset or use customized ID data.
First, use `mi_get_ID_attr` function to get the attributes of the dataset associated with the ID. Then, with the attributes screened out, retrieve the corresponding data as a data frame with two columns, `ID` and `class`.
```R
attributes = mi_get_ID_attr(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia") %>% dplyr::slice(1:10)
data_ID = mi_get_ID(attributes,biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
data_ID
```
Sometimes the data obtained from other sources is an ID mapping table, with each row corresponding to an ID entity, and each column corresponds to a different database. So, do the training you need to reorganize the table and remove invalid values using the `mi_clean_data` function. For example:
	
```R
data <- tibble::tibble(
	"ensembl_gene_id" = c("ENSG00000001626","ENSG00000002549","ENSG00000002586","ENSG00000002745"),
	'ensembl_exon_id' = c("ENSE00002398851","ENSE00002398851","ENSE00002398851","ENSE00002398851"),
	'refseq_peptide' = c("NP_001303256","-","NP_001382772","NP_001340728")
)
data_ID_clean = mi_clean_data(data,placeholder="-")
data_ID_clean
```
Further processing of IDs is needed for training. The first step is to split the IDs into multiple columns character by character. The IDs is filled with "*" up to the maximum length of IDs to guarantee consistent data dimension. Users can also specify the `pad_len` parameter to control the number of features used in the following training.
	
```R
pad_len = mi_get_padlen(data_ID)
data_splt = mi_split_col(data_ID,cores = NULL,pad_len = pad_len)
str(data_splt)
```
The IDs has been split into several columns with single characters, which is not available for training. The `mi_to_numer` function specifies the levels of each feature column prefixed with "pos" and converts them into `factor` type. To learn more about R `factor`, please refer to [here](https://r4ds.had.co.nz/factors.html). For the concern of compatibility, the feature columns are then converted into `integer` type which can directly be used for training.

```R
data_fct = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
```

Features are then all converted to numeric type while the target column `class` remains `factor` type.

### Data Balancing

To prevent the trained model from losing its ability to distinguish between databases with small numbers of IDs, it is necessary to balance the data. On the one hand, the smote method is used to oversample databases with small numbers of IDs to increase the data density, and on the other hand, databases with large numbers of IDs are undersampled through random sampling to reduce the number of IDs.

The performing of data balancing in the function `mi_balance_data` is based on `scutr` package, allowing parallel only for Mac computers. The data set produced by balancing cannot be used as the test set, so the `ratio` proportion of the original data set is divided as the test set, and this portion of the data is removed from the balanced data set, while the remaining portion is used as the training set. 

```R
data_blcd = mi_balance_data(data_fct,ratio = 0.3,parallel = F)
train = data_blcd[[1]] %>% mutate(across(-class,.fns = ~tidyr::replace_na(.x,0)))%>% dplyr::slice(sample(nrow(data_blcd[[1]]), 10000), preserve = TRUE) 
test = data_blcd[[2]]
print(train %>% group_by(class) %>% summarize(n = n()))
print(data_fct %>% group_by(class) %>% summarize(n = n()))
```

After data balancing, the number of majority and minority classes are generally balanced.

### Models Tuning

MantaID contains four machine-learning submodels: Classification and Regression Tree (CART), Random Forest (RF), eXtreme Gradient Boosting (XGBoost), and Back Propagation Neural Network (BPNN). 
For the first three models, we achieve `hyperband` tuning to find the best parameter configurations based on `mlr3hyperband` package. For further understanding of `hyperband` algorithm, please visit [here](https://arxiv.org/abs/1603.06560). In each iteration, a complete run of sequential halving is executed. In it, after evaluating each configuration on the same subset size, only a fraction of `1/eta` of them ‘advances’ to the next round. The paramter `eta` is default set to 3.
	In the `mi_tune_*` function, except for the performing of `hyperband` tuning, the stage plots are also drawn to manifest the "competition" between different parameter configurations.
	
```R
#Decision Tree
inst_rp <- mi_tune_rp(train, eta = 3, resampling = rsmp("bootstrap", ratio = 0.8, repeats = 3))
#Random Forest
inst_rg <- mi_tune_rg(train, eta = 3, resampling = rsmp("bootstrap", ratio = 0.8, repeats = 3))
#Xgboost
inst_xgb <- mi_tune_xgb(train, eta = 3, resampling = rsmp("bootstrap", ratio = 0.8, repeats = 3))

inst_rp[[2]]
inst_rg[[2]]
inst_xgb[[2]]
```

### Models Training

After tuning, the optimal parameter configuration calculated can be used for the following training by passing the tuner instances to the `mi_train_*` function.
	
```R
#Decision Tree
result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"), instance = inst_rp[[1]])
#Random Forest
result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"), instance = inst_rg[[1]])
#Xgboost
result_xgb <- mi_train_xgb(train, test, measure = msr("classif.acc"), instance = inst_xgb[[1]])

#BPNN
result_BP <- mi_train_BP(train, test, path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3)
```

### Models Explaining
To evaluate the performance of models, the confusion matrices of test set are calculated and visualized with heatmap. Each row of the matrix represents the instances in an actual class while each column represents the instances in a predicted class, or vice versa. 
	
```R
matri_rp <- mi_get_confusion(result_rp)
matri_rg <- mi_get_confusion(result_rg)
matri_xgb <- mi_get_confusion(result_xgb)
matri_BP <- mi_get_confusion(result_BP,ifnet = T)

mi_plot_heatmap(matri_rp, name="rp",filepath = "Graph/")
mi_plot_heatmap(matri_rg, name="rg",filepath = "Graph/")
mi_plot_heatmap(matri_xgb, name="xgb",filepath = "Graph/")
mi_plot_heatmap(matri_BP, name="BP",filepath = "Graph/")
```

In the heatmaps, the value in the squares depicts the number of samples whose actual classes are `Reference`, while predicted as `Prediction`. For clarity, zero values are omitted. 
MantaID implements submodel unification based on a new method. For cases with a majority class in prediction results, MantaID adopts the voting method directly, while for cases with scattered opinions, MantaID calculates the score of results of each model to find the optimal result. 

```R
data("mi_data_rawID")
data = mi_data_rawID
final_result = mi_unify_mod(data, "ID",result_rg,result_rp,result_xgb,result_BP,c_value = 0.75, pad_len = pad_len)
final_result
```
The `integrated` column is the final result MantaID determined. 
To prove the effectiveness of unification method, we can compare the balance accuracy by classes of test set. The integrated model inherits the individual models' strengths well, scoring high in almost all classes when compared to the sub-models. 
```R
balance_accuracy = map_dfc(as.list(final_result),function(x){
	confusion = confusionMatrix(data = x,reference = data$class)$byClass %>% as.data.frame()
	return(confusion%>% select(`Balanced Accuracy`))
}) %>% set_names(names(final_result))
balance_accuracy
```	
### Session information

```R
sessionInfo()
# R version 4.2.0 (2022-04-22 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19044)
# 
# Matrix products: default
# 
# locale:
# 	[1] LC_COLLATE=Chinese (Simplified)_China.utf8  LC_CTYPE=Chinese (Simplified)_China.utf8   
# [3] LC_MONETARY=Chinese (Simplified)_China.utf8 LC_NUMERIC=C                               
# [5] LC_TIME=Chinese (Simplified)_China.utf8    
# 
# attached base packages:
# 	[1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# 	[1] devtools_2.4.4 usethis_2.1.6 
# 
# loaded via a namespace (and not attached):
# 	[1] tidyselect_1.2.0    xfun_0.30           remotes_2.4.2       purrr_1.0.0        
# [5] reshape2_1.4.4      vctrs_0.5.1         generics_0.1.3      miniUI_0.1.1.1     
# [9] htmltools_0.5.4     yaml_2.3.5          utf8_1.2.2          rlang_1.0.6        
# [13] pkgbuild_1.3.1      pkgdown_2.0.7       urlchecker_1.0.1    pillar_1.8.1       
# [17] later_1.3.0         withr_2.5.0         glue_1.6.2          DBI_1.1.3          
# [21] sessioninfo_1.2.2   lifecycle_1.0.3     plyr_1.8.7          stringr_1.5.0      
# [25] htmlwidgets_1.5.4   memoise_2.0.1       evaluate_0.19       knitr_1.40         
# [29] callr_3.7.3         fastmap_1.1.0       httpuv_1.6.7        ps_1.7.2           
# [33] fansi_1.0.3         Rcpp_1.0.9          xtable_1.8-4        promises_1.2.0.1   
# [37] BiocManager_1.30.19 cachem_1.0.6        desc_1.4.1          pkgload_1.3.0      
# [41] mime_0.12           fs_1.5.2            digest_0.6.31       stringi_1.7.8      
# [45] processx_3.8.0      dplyr_1.0.9         shiny_1.7.4         rprojroot_2.0.3    
# [49] cli_3.5.0           tools_4.2.0         magrittr_2.0.3      tibble_3.1.8       
# [53] profvis_0.3.7       crayon_1.5.2        pkgconfig_2.0.3     ellipsis_0.3.2     
# [57] prettyunits_1.1.1   assertthat_0.2.1    rmarkdown_2.16      rstudioapi_0.14    
# [61] R6_2.5.1            compiler_4.2.0     
```
