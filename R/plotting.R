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
    ggplot2::geom_point(ggplot2::aes(x,y,size=`Arrival rate`),
                        shape = 21, fill = ggplot2::alpha("black", .2)) +
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
    ggplot2::geom_point(ggplot2::aes(x,y,size=`Arrival rate`),
                        shape = 21, fill = ggplot2::alpha("black", .2)) +
    ggplot2::geom_point(
      data = centroids$locations, ggplot2::aes(x, y), shape = 10, size = 5
    ) +
    ggplot2::theme_void()
}

#' Plot base locations for solution
#'
#' @param solution A list returned from a solution function
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' instance <- generate_2d_instance()
#' solution <- solve_kmeans(instance = instance, no_of_centers = 5)
#' plot_bases(solution = solution)
plot_bases <- function(solution) {
  centroids <- solution$instance %>%
    dplyr::select(`Centroid id`, x.centroid, y.centroid) %>%
    dplyr::distinct()

  ggplot2::ggplot(solution$instance) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = x.centroid, yend = y.centroid),
                 color = "gray") +
    ggplot2::geom_point(ggplot2::aes(x,y, fill = `Centroid id`, size = `Arrival rate`, alpha = .2),
                        shape = 21, color = "black") +
    ggplot2::geom_point(
      data = centroids,
      ggplot2::aes(x.centroid, y.centroid), shape = 15, size = 2
    ) +
    ggplot2::theme_void()
}
