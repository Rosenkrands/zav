#' Plot demand points for problem instance
#'
#' @param instance A list returned from generate_2d_instance
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' plot_point(instance = generate_2d_instance())
plot_point <- function(instance) {
  ggplot2::ggplot(instance$data) +
    ggplot2::geom_point(ggplot2::aes(x,y)) +
    ggplot2::theme_void()
}

#' Plot centroid locations with demand points
#'
#' @param instance A list returned from generate_2d_instance
#' @param centroids A list returned from grid_centroids
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' instance <- generate_2d_instance()
#' centroids <- grid_centroids(instance, dimension = 3)
#' plot_centroid(instance = instance, centroids = centroids)
plot_centroid <- function(instance, centroids) {
  if ("Centroid id" %in% colnames(instance$data)) {
    instance$data <- instance$data %>% select(-`Centroid id`)
  }

  ggplot2::ggplot(instance$data) +
    ggplot2::geom_point(ggplot2::aes(x,y)) +
    ggplot2::geom_point(
      data = centroids$locations, ggplot2::aes(x, y), shape = 10, size = 5
    ) +
    ggplot2::theme_void()
}
