MantaID: a machine-learning-based tool that automatically recognizes biological database IDs using machine learning. Additionally, a shiny application is offered, accessible via [MantaID (shinyapps.io)](https://molaison.shinyapps.io/MantaID/) and API via [MantaIDapi](http://164.92.98.237/MantaIDapi/__docs__/).

====================

### R Version Depends: 

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
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
library(remotes)
install_version("mlr3learners","0.5.1",force = T,upgrade ="never")
install_version("mlr3tuning","0.13.1",force = T,upgrade ="never")
install_version("mlr3","0.13.4",force = T,upgrade ="never")
BiocManager::install("biomaRt", version = "3.16")
```

### Descriptions:

​	The MantaID package provides a pipeline for gene ID identification based on R. Via MantaID, users can identify IDs quickly based on integrating a machine-learning-based model on a large scale. The general workflow includes data retrieving, processing and balancing, model tuning, training, and explaining. Each procedure is implemented with the functions in the R sub-fold.


### How To Use An API:

For a smaller number of ID identifications, MantaID accomplishes a more thorough approach, and it can also be utilized with MantaID API. To identify potential databases, we first remove any mismatched databases using regular expressions, visit the corresponding resource page using the URL, filter the results based on the access status code, then retrieve the text of the web page and determine whether it contains information about nonexistent resources.

In addition, users can choose the search mode according to their personal needs, which can be divided into "quick" and "general".


```bash
curl -X GET "http://164.92.98.237/MantaIDapi/ID_search?ID={ID of interest}&quick={mode}" -H "accept: */*"
```

You can also perform it in R with the `GET` method. The result can be retrieved as a data frame, containing matched databases, by running the script below in R.

```R
install.packages("httr","jsonlite")
library(httr)
library(jsonlite)
res = GET("http://164.92.98.237/MantaIDapi/ID_search?ID={ID of interest}&quick={mode}")
resultDF <- fromJSON(as.data.frame(fromJSON(rawToChar(res$content)))[1,1])
```

### MantaID Package User Instructions:

```R
library(MantaID) 
```

#### Data Acquisition:

biomaRt provides an interface to R and the BioMart software suite databases (e.g.  Ensembl,  Uniprot,  HapMap), allowing direct access to information in the databases via R.

Set the BioMart dataset to be connected to via biomaRt. Here we choose to use the human genome dataset, Mirror choose asia mirror (depending on the region).

First, use 'mi_get_ID_attr' function to get the attributes of the dataset associated with the ID. Then utilize 'flt_attri' function further by looking at the dataset to select the dataset of interest. Finally, The incoming attributes are automatically retrieved by the 'mi get ID' function, which then compiles the findings into a large table.

```R
attributes = mi_get_ID_attr(biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
flt_attri = attributes %>% slice(1,2,6,7)
data_ID = mi_get_ID(flt_attri,biomart = "genes", dataset = "hsapiens_gene_ensembl", mirror = "asia")
data_ID
```

#### Data Processing:

Sometimes the data obtained is an ID mapping table, with each row corresponding to an ID entity, and each column corresponds to a different database. So, do the training you need to reorganize the table and remove invalid values. Use the 'mi_clean_data' function to do this. For example:

```R
data <- tibble::tibble(
	"ensembl_gene_id" = c("ENSG00000001626","ENSG00000002549","ENSG00000002586","ENSG00000002745"),
	'ensembl_exon_id' = c("ENSE00002398851","ENSE00002398851","ENSE00002398851","ENSE00002398851"),
	'refseq_peptide' = c("NP_001303256","-","NP_001382772","NP_001340728")
)
data_ID_clean = mi_clean_data(data,placeholder="-")
```

The features of ID, in contrast to other data, are the placements of the constituent characters. Since it is impossible to train on only one column of "ID," it is split into a single character vector to extract the features, and the vector is filled with "*" up to the maximum ID to guarantee consistent data dimension. 

```r
pad_len = mi_get_padlen(data_ID)
data_splt = mi_split_col(data_ID,cores = NULL,pad_len = pad_len)
str(data_splt)
```

It is necessary to convert the existing feature columns to factor types because they are currently strings and cannot be used for training. Because of characters appear in different feature columns in a different order, the levels parameter must be set in order to standardize the factor levels. This can then be used directly as a numeric type, but for compatibility reasons it will also be converted into a numeric type.

```r
data_fct = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
```

#### Data Balancing:

To prevent the trained model from losing its ability to distinguish between databases with small numbers of IDs, it is necessary to balance the data. On the one hand, the smote method is used to oversample databases with small numbers of IDs to increase the data density, and on the other hand, databases with large numbers of IDs are undersampled through random sampling to reduce the number. 

The data set produced by balancing cannot be used as the test set any longer, so the ratio proportion of the original data set is divided as the test set, and this portion of the data is removed from the balanced data set, while the remaining portion is used as the training set. It should be noted, however, that the parallel parameter is only supported by Mac.

```r
data_blcd = mi_balance_data(data_fct,ratio = 0.3,parallel = F)
```

#### Model Training:

Due to the large size of the dataset, the model training time is too long, so only a certain number of samples are taken for training; where the training set and the dataset are divided by calling the partition function, which exists as an index of the original data; three models are used for benchmark training, namely decision tree, random forest and plain Bayes, and resampling is performed using the five-fold crossover method; benchmark() The training was performed, and the training and test sets were evaluated separately after training (costs&ce); accepts four parameters, all of which have default parameters except data; data is the incoming data, where the target column (i.e. the column where the ID database name is located) must have the column name "class", and all columns are of type factor;

The first thing that needs to be done is the initial selection of a machine learning classification model, where multiple models are trained and the most suitable ones are selected; The `row_num` parameter determines the number of data items to be used, if the data is large then a portion of the data will be extracted for testing, otherwise, all of the data will be used for testing. This step requires the use of the original dataset rather than the balanced dataset;

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
#Xgboost
result_xgboost <- mi_train_xgb(train, test, measure = msr("classif.acc"))
```

In addition to several classical machine learning algorithms, a BP neural network is used for classification.

This is achieved by calling `tensorflow` via the `keras` package, so `tensorflow` needs to be installed first. 

1. If you already have python installed, please skip to step 2; First, download python interpreter via https://www.python.org/downloads/release/. Please check the "add python.exe to PATH" box when installing;

2. Download tensorflow packages in python.
```bash
pip install tensorflow
pip install tensorflow-gpu
```
> If you get an error, please upgrade `pip` using the commands "python -m pip install --upgrade pip".

3. Use tensorflow in R. Please replace "/path/to/python.exe" with your python path.
```R
install.packages("reticulate")
library(reticulate)
path_to_python <- use_python(python = "/path/to/python.exe")
```


The meaning of parameters are (1) train, the training set; (2) test, the test set; (3) path2save, the path of the trained model, the default is NULL, not saved; (4) batch_size, the size of the training batch, the larger the training period, the shorter the training period, but the number of periods to achieve the same accuracy increases (5) epochs, the number of training periods, all samples are trained once; (6) validation_split, the proportion of data sets in the training set that are used to divide into validation sets;

```r
result_net <- mi_train_BP(train, test, path2save = NULL, batch_size = 128, epochs = 64, validation_split = 0.3)
```

#### Confusion Matrix :

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
