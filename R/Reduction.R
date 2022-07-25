# data_usmld = read_rds("Rdata/vec_matrix.rds")
# data_smld = read_rds("Rdata/data_smtd.rds")
# library(Rtsne)
#
# data_us = unique(data_usmld) # 去除重复数据
# class_us = data_usmld %>% select(class)%>% sample(nrow(.),5000)
# data_us = data_usmld %>% select(-class)%>% sample(nrow(.),5000)
#
# data_s = unique(data_smld) # 去除重复数据
# class_s = data_smld %>% select(class) %>% sample(nrow(.),5000)
# data_s = data_smld %>% select(-class) %>% sample(nrow(.),5000)
#
# set.seed(321)
# tsne_out = Rtsne(
#   data_s ,
#   dims = 2,
#   pca = T,
#   max_iter = 1000,
#   theta = 0.4,
#   perplexity = 20,
#   verbose = T
# )
# tsne_out = Rtsne(
#   data_us ,
#   dims = 2,
#   pca = T,
#   max_iter = 1000,
#   theta = 0.4,
#   perplexity = 20,
#   verbose = T
# )
# library(magrittr)
# library(ggplot2)
# pca_us <- FactoMineR::PCA(data_us, graph = F)
# pca_s <- FactoMineR::PCA(data_s, graph = F)
# factoextra::fviz_pca_ind(
#   pca_us,
#   habillage = class_us,
#   label = "none",
#   mean.point = F
# )
# factoextra::fviz_pca_ind(
#   pca_s,
#   habillage = factor(class_s),
#   label = "none",
#   mean.point = F
# )
# pca_prcp_us <- data_us %>%
#   scale %>%
#   prcomp()
# pca_prcp$x %>% .[,1:2] %>% as.data.frame() %>%
#   cbind(class_us) %>%
#   ggplot(aes(x = PC1, y = PC2)) +
#   geom_point(aes(color = class, shape = class))
# data <- reshape2::melt(Titanic)
# data <- gather_set_data(data, 1:4)
#
# ggplot(data, aes(x, id = id, split = y, value = value)) +
#   geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
#   geom_parallel_sets_axes(axis.width = 0.1) +
#   geom_parallel_sets_labels(colour = 'white')
# # }
