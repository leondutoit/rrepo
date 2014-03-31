
#' @export
project_heatmap <- function(df) {
  # TODO fix colours
  colours <- c( "#a50026", "#d73027", "#f46d43", "#fdae61",
    "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695", "#313695")
  labels <- as.character(10 * 1:10)
  ggplot(df, aes(
    date, name, fill = as.factor(ceiling(percentage)))) +
      geom_raster() +
      scale_fill_manual(
        values = colours,
        labels = labels,
        name = "percentage")
}
