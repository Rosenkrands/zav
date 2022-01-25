#' Weighted KMeans solution approach
#'
#' @param instance A list returned from generate_2d_instance
#' @param no_of_centers The number of base locations to generate
#' @param type The type of WKmeans implematation to use, options are "flexclust" or "swkm"
#'
#' @return A list
#' @export
#'
#' @examples
#' instance <- generate_2d_instance()
#' # solution <- solve_wkmeans(instance = instance, no_of_centers = 5)
solve_wkmeans <- function(instance, no_of_centers = 5, type = c("flexclust", "swkm")) {
  if (type == "flexclust") {
    coordinates <- instance$data %>% dplyr::select(.data$x, .data$y)
    centroids <- flexclust::stepFlexclust(x = coordinates, k = no_of_centers, nrep = 10,
                                          weights = instance$data$`Arrival rate`, FUN = "cclust",
                                          dist = "euclidean", verbose = F,
                                          method = "hardcl", control = list(iter.max = 1000))
    #
    clusters <- as.data.frame(centroids@centers)
    clustering_vector <- centroids@cluster
  } else if (type == "swkm") {
    centroids <- SWKM::kmeans.weight(
      x = instance$data %>% dplyr::select(x,y) %>% data.matrix(),
      K = no_of_centers,
      weight = instance$data$`Arrival rate`
    )

    clusters <- as.data.frame(centroids$centers) %>% dplyr::rename(x = V1, y = V2)
    clustering_vector <- centroids$cluster
  }

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
