mi <- function(cores = NULL,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"),ratio = 0.3,para_blc = F, model_path = NULL, batch_size = 128, epochs = 64, validation_split = 0.3,graph_path=NULL){
    pad_len = mi_get_padlen(mi_data_allID)
    data_split = mi_split_col(mi_data_allID,cores = cores,pad_len = pad_len)
    data_num = mi_to_numer(data_splt,levels = c("*", 0:9, letters, LETTERS, "_", ".", "-", " ", "/", "\\", ":"))
    cor_plot = mi_plot_cor(data_num,cls)
    data_blcd = mi_balance_data(data_num,ratio = 0.3,parallel = F)
    train = data_blcd[[1]]
    test = data_blcd[[2]]
    inst_rg = mi_tune_rg(train,test)
    inst_rp = mi_tune_rp(train,test)
    inst_xgb = mi_tune_xgb(train,test)
    result_rg <- mi_train_rg(train, test, measure = msr("classif.acc"),instance = inst_rg[[1]])
    result_rp <- mi_train_rp(train, test, measure = msr("classif.acc"),instance = inst_rp[[1]])
    result_xgboost <- mi_train_xgb(train, test, measure = msr("classif.acc"),instance = inst_xgb[[1]])
    stageplot_rg <- inst_rg[[1]]
    stageplot_rp <- inst_rp[[1]]
    stageplot_xgb <- inst_xgb[[1]]
    result_net <- mi_train_BP(train, test, path2save = path2save, batch_size = batch_size, epochs = epochs, validation_split = validation_split)
    matri_rg <- mi_get_confusion(result_rg)
    matri_rp <- mi_get_confusion(result_rp)
    matri_xgb <- mi_get_confusion(result_xgb)
    matri_net <- mi_get_confusion(result_net,ifnet = T)
    heatmap_rg = mi_plot_heatmap(matri_rg, name="rg",filepath = graph_path)
    heatmap_rp = mi_plot_heatmap(matri_rp, name="rp",filepath = graph_path)
    heatmap_xgb = mi_plot_heatmap(matri_xgb, name="xgb",filepath = graph_path)
    heatmap_net = mi_plot_heatmap(matri_net, name="net",filepath = graph_path)
    learners = list(rp = result_rp[[1]],rg = result_rg[[1]],xgb = result_xgb[[1]],BP = result_net[[1]])
    heatmaps <- list(rp = heatmap_rp,rg = heatmap_rg,xgb = heatmap_xgb,BP = heatmap_net)
    stageplots <- list(rp = stageplot_rp,rg = stageplot_rg,xgb=stageplot_xgb)
    return(list(learners,heatmaps,stageplots))
}