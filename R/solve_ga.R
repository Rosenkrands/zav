#' Genetic algorithm solution approach
#'
#' @param instance A list returned from generate_2d_instance
#' @param centroids A list returned from grid_centroids
#' @param no_of_centers The number of base locations to generate
#' @param obj Objective function to use (currently "TOT" or "SAFE")
#' @param miter Max number of iterations
#'
#' @return A list
#' @export
#'
#' @examples
#' # WIP
solve_ga <- function(instance,
                     centroids,
                     no_of_centers,
                     obj = c("TOT", "SAFE"),
                     miter = 10) {
  bit_to_cent <- function(bitstring) {
    tibble::tibble(
      `Centroid id` = as.character(1:length(bitstring) * bitstring)
    ) %>% dplyr::filter(`Centroid id` != 0)
  }

  eval_func <- if (obj == "TOT") {
    function(bitstring) {
      # first we ignore cases with less than 2 centroids by returning -Inf
      if (sum(bitstring) < 2) {return(-Inf)}
      centroids_used <- bit_to_cent(bitstring)

      # Assume constant speed so time and distance are interchangeable
      # Fixed service time per delivery
      tau <- 0
      # Punishment for unwanted centroids
      len <- abs(no_of_centers - sum(bitstring))
      # Computation of objective value
      result <- centroids$distances %>%
        # filter the centroids to use the ones included in the solution
        dplyr::filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
        # for each demand point show only the closest centroid
        dplyr::group_by(`Demand point id`) %>%
        dplyr::filter(Distance == min(Distance)) %>%
        dplyr::ungroup() %>%
        # join with instance$data to get the arrival rate for each demand point
        dplyr::inner_join(
          dplyr::select(instance$data, `Demand point id`, `Arrival rate`),
          by = "Demand point id"
        ) %>%
        # calculate operation time for each centroid, as the sum of weigthed distances to demand point in the service area of the centroid
        dplyr::group_by(`Centroid id`) %>%
        dplyr::summarise(`Operation time` = sum(`Arrival rate` * (Distance + tau))) %>%
        # lastly we sum over all centroids, with a penalty term that is zero if we have the specified amount of centroids
        dplyr::summarise(TOT = sum(`Operation time`) + len*1000)
      # we return the negative TOT as we are maximizing but TOT should be minimized
      return(-result$TOT)
    }
  } else if (obj == "SAFE") {
    function(bitstring) {
      # first we ignore cases with less than 2 centroids by returning -Inf
      if (sum(bitstring) < 2) {return(-Inf)}
      len <- abs(no_of_centers - sum(bitstring))

      centroids_used <- bit_to_cent(bitstring)
      points <- instance$data %>%
        dplyr::select(`Demand point id`, x, y)
      # Computation of objective value
      result <- centroids$distances %>%
        # filter the centroids to use the ones included in the solution
        dplyr::filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
        # for each demand point show only the closest centroid
        dplyr::group_by(`Demand point id`) %>%
        dplyr::filter(Distance == min(Distance)) %>%
        dplyr::ungroup() %>%
        # join with instance$data to get coordinates for demand points
        dplyr::inner_join(
          dplyr::select(points, `Demand point id`, x, y),
          by = "Demand point id"
        ) %>%
        dplyr::group_by(`Centroid id`)
      # Calculate safety distance with c++ function to speed up computation
      return(safe_dist(result %>% data.matrix) - len*1000)
    }
  }

  ga_model <- GA::ga(
    type="binary", fitness=eval_func, nBits=centroids$no_of_centroids,
    popSize=100, pmutation=0.1, maxiter=miter, parallel = F, run = 500
  )

  centroids_used <- bit_to_cent(summary(ga_model)$solution[1,])
  assignment <- centroids$distances %>%
    dplyr::filter(`Centroid id` %in% centroids_used$`Centroid id`) %>%
    dplyr::group_by(`Demand point id`) %>%
    dplyr::filter(Distance == min(Distance))

  instance <- instance$data %>%
    dplyr::left_join(assignment %>% dplyr::select(-Distance), by = "Demand point id") %>%
    dplyr::left_join(centroids$locations, by = "Centroid id", suffix = c("",".centroid"))

  # change centroid id to be in 1:no_of_centers, for use in the simulation
  clusters <- centroids_used %>%
    dplyr::left_join(centroids$locations, by = "Centroid id") %>%
    dplyr::mutate(`Cluster id` = dplyr::row_number())

  # Change the original centroid ids to the new ones
  instance <- instance %>%
    dplyr::inner_join(clusters %>% dplyr::select(-x,-y), by = "Centroid id") %>%
    dplyr::select(-`Centroid id`) %>%
    dplyr::mutate(`Centroid id` = as.character(`Cluster id`)) %>%
    dplyr::select(-`Cluster id`)

  return(list(
    "ga" = ga_model,
    "no_of_centers" = no_of_centers,
    "clusters" = clusters,
    "instance" = instance
  ))
}
