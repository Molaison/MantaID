#' Plot correlation heatmap.
#'
#' @param data Data frame that including IDs' position features.
#' @param cls The name of the class column.
#'
#' @importFrom stats cor
#' @importFrom grid unit
#' @importFrom ggplot2 ggtitle unit xlab ylab
#' @importFrom ggcorrplot ggcorrplot
#' @return A heatmap.
#' @export
#'
mi_plot_cor <- function(data, cls = "class") {
    cor_mt <- data %>% select({{cls}})%>% as.matrix()%>%cor()
    heat <- ggcorrplot(cor_mt, show.diag = T) +
        theme_bw() + theme(
            axis.text.x = element_text(
                angle = 45,
                vjust = 1, size = 35, hjust = 1,
                lineheight = 10
            ),
            axis.text.y = element_text(size = 35),
            strip.text.y = element_text(
                angle = 0,
                vjust = 0.5,
                hjust = 0.5,
                size = 10
            ), axis.title = element_text(size = 60),
            plot.title = element_text(size = 50),
            legend.key.size = unit(55, "pt"),
            legend.text = element_text(size = 20),
            legend.title = element_text(size = 30)
        ) + ggtitle("Correlation") +
        xlab("") +
        ylab("")
        return(heat)
}
