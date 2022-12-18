MantaID: a machine-learning based tool to automate the identification of biological database IDs. A shiny application is also provided and can be accessed through [MantaID (shinyapps.io)](https://molaison.shinyapps.io/MantaID/).

====================

##### R version Depends: 

​    R (>= 4.2.0)

##### Imports:

​	biomaRt, caret, data.table, dplyr, ggplot2, keras,magrittr, mlr3, purrr, reshape2, scutr, stringr,tibble, tidyr, tidyselect,mlr3tuning,paradox.

##### Installation:

```R
require("devtools")
install_bitbucket("Molaison/MantaID")
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("biomaRt", version = "3.8")
```

##### Descriptions:

​	The MantaID package provides a pipeline for gene IDs identification based on R. Via MantaID, users can identify IDs quickly based on integrated machine-learning-based model  on a large scale. The general workflow includes data retrieving, processing and balancing, model tuning, training, and explaining. Each of procedure is implemented with the functions in R sub-fold. 