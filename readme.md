MantaID: a machine-learning based tool to automate the identification of biological database IDs. A shiny application is also provided and can be accessed through [MantaID (shinyapps.io)](https://molaison.shinyapps.io/MantaID/) and API through [MantaIDapi](http://164.92.98.237/MantaIDapi/__docs__/).

====================

### R version Depends: 

​    R (>= 4.2.0)

### Imports:

​	biomaRt, caret, data.table, dplyr, ggplot2, keras,magrittr, mlr3, purrr, reshape2, scutr, stringr,tibble, tidyr, tidyselect,mlr3tuning,paradox.

### Installation:

```R
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
install_bitbucket("Molaison/MantaID")
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("biomaRt", version = "3.8")
```

### Descriptions:

​	The MantaID package provides a pipeline for gene IDs identification based on R. Via MantaID, users can identify IDs quickly based on integrated machine-learning-based model  on a large scale. The general workflow includes data retrieving, processing and balancing, model tuning, training, and explaining. Each of procedure is implemented with the functions in R sub-fold. 


### How to use API:

MantaID achieves a more comprehensive approach for a small number of ID identification and can be used with MantaIDAPI. First, we remove the mismatched databases by regular expressions, then visit the corresponding resource page by URL and filter them according to the access status code, and finally retrieve the text of the web page and judge whether it contains the information of non-existent resources, and thus remaining ones are the possible databases. 

To retrieve the results, you can use the `curl` in the terminal. Please replace the brackets with you own choices. The mode must be either `quick` or `general`.

```bash
curl -X GET "http://164.92.98.237/MantaIDapi/ID_search?ID={ID of interest}&quick={mode}" -H "accept: */*"
```

You can also perform it in R with `GET` method. The result can retrieved as a dataframe by running the script below in R.

```R
install.packages("httr","jsonlite")
library(httr)
library(jsonlite)
res = GET("http://164.92.98.237/MantaIDapi/ID_search?ID={ID of interest}&quick={mode}")
resultDF <- fromJSON(as.data.frame(fromJSON(rawToChar(res$content)))[1,1])
```

### MantaID pacakage User instructions:

```R
library(MantaID) 
```

#### Data Acquisition:

biomaRt provides an interface to R and the BioMart software suite databases (e.g. `Ensembl`, `Uniprot`, `HapMap`), allowing direct access to information in the databases via R.

Set the `BioMart` dataset to be connected to via `biomart`, `dataset`, here choose to use the human genome dataset, Mirror choose: `asia mirror` (depending on the region); use `mi_get_ID_attr` to get the attributes of the dataset associated with the ID;

Then filter the attributes further by looking at the dataset to select the dataset of interest;
The 'mi_get_ID' function automatically retrieves the incoming attributes and processes the results into a long table.

```r
attributes = mi_get_ID_attr(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
flt_attri = attributes %>% slice(1,3,5,7)
data_ID = mi_get_ID(flt_attri,biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
data_ID
```

#### Data Processing:

Sometimes the data obtained is an ID mapping table, with each row corresponding to an ID
entity, and each column corresponds to a different database, so to do the training you need to reorganise the table and remove invalid values; use the `mi_clean_data` function to
to do this. For example:

```r
data <- tibble::tibble(
	"ensembl_gene_id" = c("ENSG00000001626","ENSG00000002549","ENSG00000002586","ENSG00000002745"),
	'ensembl_exon_id' = c("ENSE00002398851","ENSE00002398851","ENSE00002398851","ENSE00002398851"),
	'refseq_peptide' = c("NP_001303256","-","NP_001382772","NP_001340728")
)
data_ID = mi_clean_data(data,placeholder="-")
```

Unlike other data, the features of ID are the positions of the constituent characters, and it is not possible to train only one column of "ID", so it is split into a single character vector to obtain the features, and the vector is filled with "\*" to the length of the maximum ID to ensure consistent data dimension.

```r
pad_len = mi_get_padlen(data_ID)
data_splt = mi_split_col(data_ID,cores = NULL,pad_len = pad_len)
str(data_splt)
```

The current feature columns are strings, which cannot be used for training yet, so they need to be converted to factor types. The levels parameter needs to be set as the order of appearance of the characters in the different feature columns is different and the factor levels need to be standardised. This can then be used directly as a numeric type, but for compatibility reasons, it will also be converted to a numeric type.

```r
data_fct = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
```

### Data Balancing:

Depending on the size and importance of the database, the difference in the number of IDs retrieved can be surprisingly large, so it is necessary to balance the data, otherwise the trained model loses its ability to discriminate between databases with small numbers of IDs; on the one hand, the smote method is used to oversample databases with small numbers of IDs to increase the data density, and on the other hand, databases with large numbers of IDs are undersampled through random sampling to reduce the number. The data set obtained by balancing can no longer be used as the test set, so the `ratio` proportion of data from the original data set is divided as the test set, and this part of the data is removed from the balanced data set, and the remaining part is used as the training set; however, it should be noted that the `parallel` parameter is only available for Mac.

```r
data_blcd = mi_balance_data(data_fct,ratio = 0.3,parallel = F)
```

#### Model Training:

Due to the large size of the dataset, the model training time is too long, so only a certain number of samples are taken for training; where the training set and the dataset are divided by calling the `partition` function, which exists as an index of the original data; three models are used for benchmark training, namely decision tree, random forest and plain Bayes, and resampling is performed using the five-fold crossover method; `benchmark()` The training was performed, and the training and test sets were evaluated separately after training (costs&ce);
accepts four parameters, all of which have default parameters except data; `data` is the incoming data, where the target column (i.e. the column where the ID database name is located) must have the column name `"class"`, and all columns are of type factor;

The first thing that needs to be done is the initial selection of a machine learning classification model, where multiple models are trained and the most suitable ones are selected;
The `row_num` parameter determines the number of data items to be used, if the data is large then a portion of the data will be extracted for testing, otherwise all of the data will be used for testing. This step requires the use of the original dataset rather than the balanced dataset;

```{r echo=FALSE, message=FALSE, hide=TRUE}
result <- mi_run_bmr(data_fct, row_num = 4000)
benchmark <- result[1]
score <- result[2] %>% as.data.table() %T>% print()
```

The results were used to determine the choice of models for decision trees, random forests, and XGBoost.

```r
train = data_blcd[[1]]
test = data_blcd[[2]]
#Decision Tree
result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"))
#Random Forest
result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"))
#Xboost
result_xgboost <- mi_train_xgb(train, test, measure = msr("classif.acc"))
```

In addition to several classical machine learning algorithms, a BP neural network is used for classification.

This is achieved by calling `tensorflow` via the `keras` package, so `tensorflow` needs to be installed first.

```r
tensorflow::install_tensorflow()
```

The meaning of the parameters are (1) train, the training set; (2) test, the test set; (3) path2save, the path of the trained model, the default is NULL, not saved; (4) batch_size, the size of the training batch, the larger the training period, the shorter the training period, but the number of periods to achieve the same accuracy increases; (5) epochs, the number of training (5) epochs, the number of training periods, all samples are trained once; (6) validation_split, the proportion of data sets in the training set that are used to divide into validation sets;

```r
result_net <- mi_train_BP(train, test, path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3)
```

### Confusion matrix :

`cnfs_matri` function converts the results of model training into an obfuscation matrix; the results of the model training function are used directly as input; the `ifnet` argument is a logical value, TRUE for a neural network model;
`mi_plot_heatmap` plots the heatmap for the confusion matrix; name, the model name, the suffix when the file is stored; filepath, the path when the model is stored.

```r
matri_rg <- mi_get_confusion(result_rg)
matri_rp <- mi_get_confusion(result_rp)
matri_xgb <- mi_get_confusion(result_xgb)
matri_net <- mi_get_confusion(result_net,ifnet = T)

mi_plot_heatmap(matri_rg, name="rg",filepath = "Graph/")
mi_plot_heatmap(matri_rp, name="rp",filepath = "Graph/")
mi_plot_heatmap(matri_xgb, name="xgb",filepath = "Graph/")
mi_plot_heatmap(matri_net, name="xgb",filepath = "Graph/")
```