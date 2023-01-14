#' Title
#'
#' @param data
#' @param cor_thresh
#' @param imp_thresh
#' @param union
#'
#' @return
#' @export
#'
#' @examples
mi_filter_feat <- function(data,cor_thresh = 0.7,imp_thresh = 0.99,union = FALSE){
	cor_mt <- data %>%
		select(-class) %>%
		mutate(across(.fns = as.numeric)) %>%
		as.matrix() %>%
		# 计算correlation
		cor() %>%
		as.data.frame() %>%
		mutate(across(.cols = everything(), .fns = replace_na, replace = 0)) %>%
		as.matrix() %>%
		# 将table变换成dataframe
		reshape2::melt() %>%
		# 删除对角线元素
		filter(Var1!=Var2) %>%
		# 筛出大于阈值的特征对
		filter(abs(value)>=cor_thresh)
	# 映射去重(var1-var2和var2-var1是同一个)
	key = cor_mt %>%
		pmap(function(Var1,Var2,value)paste0(collapse = "",sort(c(Var1,Var2)))) %>% unlist()
	cor_mt = cor_mt %>% add_column(Key = key) %>% distinct(Key,.keep_all = TRUE) %>% select(-Key)
	# 计数, 每个数值代表与该特征很相关的特征个数
	counts = cor_mt %>% pivot_longer(starts_with("Var"),values_to = "var") %>% group_by(var) %>% count()%>% ungroup()
	to_flt_ls = c()
	# 每次都挑选出有最多相关特征的丢掉(本质上是一堆相关的特征只留一个,即去除冗余)
	while(max(counts$n)>0){
		to_flt = counts$var[order(counts$n,decreasing = TRUE)][1]
		to_flt_ls = c(to_flt_ls,as.character(to_flt))
		cor_mt = cor_mt %>% filter(Var1!=to_flt&Var2!=to_flt)
		counts = cor_mt %>% pivot_longer(starts_with("Var"),values_to = "var") %>% group_by(var) %>% count() %>% ungroup()
	}
	to_flt_ls = to_flt_ls %>% unique()


	learner <- lrn("classif.ranger",
				   importance = "impurity"
	)
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"), id = "importance")
	train_set <- partition(task, ratio = 1)$train
	set_threads(learner)
	learner$train(task, row_ids = train_set)
	importance = learner$importance() %>% .[order(.)]
	accum = 0
	imp_thresh = 1- imp_thresh
	to_flt_ip = c()
	# 从最不重要的特征加起, 直到其总和超过允许丢失的最大值(如默认阈值为0.99, 则最多丢0.01)
	for(i in 1:length(importance)){
		accum = accum + importance[i]
		if(accum > imp_thresh){
			break
		}
		to_flt_ip = c(to_flt_ip,names(importance[i]))
	}
	if(union){
		return(union(to_flt_ip,to_flt_ls))
	}else{
		return(intersect(to_flt_ip,to_flt_ls))
	}
}
