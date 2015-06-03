
#' A heatmap of relative commit activity per repo per week
#'
#' \code{project_heatmap(repos)}
#'
#' This creates a picture of repo activity for the organisation
#'
#' @param commits_per_project, aggregated per project per period - in a data frame
#' @return a ggplot object
#' @examples
#' project_heatmap(projects_over_time(commits))
#'
#' @export
project_heatmap <- function(commits_per_project) {
  colours <- c("#a50026", "#d73027", "#f46d43", "#fdae61",
    "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")
  labels <- as.character(10 * 1:10)
  ggplot(commits_per_project, aes(
    date, repo, fill = as.factor(ceiling(percentage)))) +
      geom_raster() +
      scale_fill_manual(
        values = colours,
        labels = labels,
        name = "percentage") +
      ggtitle("Per project proportional commit activity per week")
}
