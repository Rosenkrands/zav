#' KMeans solution approach
#'
#' @param instance A list returned from generate_2d_instance
#' @param no_of_centers The number of base locations to generate
#'
#' @return A list
#' @export
#'
#' @examples
#' instance <- generate_2d_instance()
#' solution <- solve_kmeans(instance = instance, no_of_centers = 5)
solve_kmeans <- function(instance, no_of_centers = 5) {
  coordinates <- instance$data %>% dplyr::select(x, y)
  centroids <- stats::kmeans(x = coordinates, centers = no_of_centers, nstart = 1000)
  clusters <- as.data.frame(centroids$centers)
  clustering_vector <- centroids$cluster

  instance <- instance$data %>%
    dplyr::mutate("Centroid id" = clustering_vector %>% as.character()) %>%
    dplyr::left_join(
      tibble::tibble(clusters) %>% dplyr::mutate(`Centroid id` = dplyr::row_number() %>% as.character()),
      by =c("Centroid id"), suffix = c("",".centroid")
    )

  return(list("clusters" = clusters,
              "clustering_vector" = clustering_vector,
              "no_of_centers" = no_of_centers,
              "instance" = instance))
}
