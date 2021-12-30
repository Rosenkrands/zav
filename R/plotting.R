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
