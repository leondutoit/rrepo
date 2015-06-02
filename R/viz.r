
#' @import ggplot2

#' @export
project_heatmap <- function(df) {
  colours <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
    "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  labels <- as.character(10 * 1:10)
  ggplot(df, aes(
    date, repo_name, fill = as.factor(ceiling(percentage)))) +
      geom_raster() +
      scale_fill_manual(
        values = colours,
        labels = labels,
        name = "percentage") +
      ggtitle("Per project proportional commit activity per week")
}
