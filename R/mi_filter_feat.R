#' Performing feature selection in a automatic way based on correlation and feature importance.
#'
#' @param data The data frame returned by `mi_to_numer()`.
#' @param cor_thresh The threshold set for Pearson correlation. If correlation value is over this threshold, the two features will be viewed as redundant and one of them will be removed.
#' @param imp_thresh The threshold set for feature importance. The last several features with the lowest importance will be removed if remained importance lower than `imp_thresh`.
#' @param union The method for combining the decisions of correlation method and importance method. If `TRUE`, any of the features calculated by the two methods will be returned. Otherwise, only features in the results of both methods will be returned.
#'
#' @return The names of the features that should be removed.
#' @export
mi_filter_feat <- function(data,cor_thresh = 0.7,imp_thresh = 0.99,union = FALSE){
	#Select the data in columns other than the class column, convert these data to a numeric type, and then convert them to matrix form.
	cor_mt <- data %>%
		select(-class) %>%
		mutate(across(.fns = as.numeric)) %>%
		as.matrix() %>%
		#To calculate the correlation, convert the "cor" to a data frame, replace the missing values in it with zeros, then convert it to will matrix form, merge the columns, remove the diagonal elements, and filter out the feature pairs that are greater than a threshold.
		cor() %>%
		as.data.frame() %>%
		mutate(across(.cols = everything(), .fns = replace_na, replace = 0)) %>%
		as.matrix() %>%
		reshape2::melt() %>%
		filter(Var1!=Var2) %>%
		filter(abs(value)>=cor_thresh)
	#De-duplication by mapping (var1-var2 and var2-var1 are the same).
	key = cor_mt %>%
		pmap(function(Var1,Var2,value)paste0(collapse = "",sort(c(Var1,Var2)))) %>% unlist()
	cor_mt = cor_mt %>% add_column(Key = key) %>% distinct(Key,.keep_all = TRUE) %>% select(-Key)
	#Count, the value represents the number of features that are very relevant to the feature.
	counts = cor_mt %>% pivot_longer(starts_with("Var"),values_to = "var") %>% group_by(var) %>% count()%>% ungroup()
	to_flt_ls = c()
	#The most relevant features are selected and discarded each time (essentially, a bunch of relevant features are left alone, i.e., redundancy is removed).
	while(max(counts$n)>0){
		to_flt = counts$var[order(counts$n,decreasing = TRUE)][1]
		to_flt_ls = c(to_flt_ls,as.character(to_flt))
		cor_mt = cor_mt %>% filter(Var1!=to_flt&Var2!=to_flt)
		counts = cor_mt %>% pivot_longer(starts_with("Var"),values_to = "var") %>% group_by(var) %>% count() %>% ungroup()
	}
	to_flt_ls = to_flt_ls %>% unique()

	#Initialize the random forest learner and set the value of the hyperparameter.
	learner <- lrn("classif.ranger",
				   importance = "impurity"
	)
	#Convert the results of the repeated sampling into a table, and later convert it into a classification task.
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"), id = "importance")
	#Divide the training set.
	train_set <- partition(task, ratio = 1)$train
	#Set the Number of Threads.
	set_threads(learner)
	#Train with a subset of data.
	learner$train(task, row_ids = train_set)
	#The $importance command is used to observe the importance of the independent variables, sorting the results in order from smallest to largest.
	importance = learner$importance() %>% .[order(.)]
	accum = 0
	imp_thresh = 1- imp_thresh
	to_flt_ip = c()
	#Add the least significant features until the sum exceeds the maximum allowed loss (e.g. default threshold is 0.99, then the maximum loss is 0.01).
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
