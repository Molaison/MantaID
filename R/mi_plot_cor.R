#' Plot correlation heatmap.
#'
#' @param data Data frame including IDs' position features.
#' @param cls The name of the class column.
#'
#' @importFrom stats cor
#' @importFrom grid unit
#' @importFrom ggplot2 ggtitle unit xlab ylab
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate across
#' @importFrom tidyselect everything
#' @return A heatmap.
#' @export
#' @examples
#' data(mi_data_procID)
#' data_num <- mi_to_numer(mi_data_procID)
#' mi_plot_cor(data_num)
mi_plot_cor <- function(data, cls = "class") {
  #Select the data in columns other than the class column, convert these data to a numeric type, and then convert them to matrix form.
  cor_mt <- data %>%
    mutate(across(.cols = everything(), .fns = as.numeric)) %>%
    as.matrix() %>%
    #To calculate the correlation, convert the "cor" to a data frame, replace the missing values in it with zeros, then convert it to will matrix form, merge the columns, remove the diagonal elements, and filter out the feature pairs that are greater than a threshold.
    cor() %>%
    as.data.frame() %>%
    mutate(across(.cols = everything(), .fns = replace_na, replace = 0)) %>%
    as.matrix()
  #Plot correlation heatmap.
  heat <- ggcorrplot(cor_mt, show.diag = T) + theme_bw() + theme(
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
  ) +

    ggtitle("Correlation") +
    xlab("") +
    ylab("")
  return(heat)
}
