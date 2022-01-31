#' Generate a problem instance
#'
#' @param seed Seed to ensure reproducibility
#' @param no_of_points The number of demand points
#' @param interval A named vector of the range for the x and y axis
#' @param arv A named vector of the range in the arrival rate (measured in requests per minute)
#' @param ar_dist_type Whether the distribution of arrival rates follows a "uniform" or "reciprocal" distribution
#'
#' @return A list
#' @export
#'
#' @examples
#' generate_2d_instance()
generate_2d_instance <- function(
  seed = 1,
  no_of_points = 50,
  interval = c("min" = -10, "max" = 10),
  arv = c("min" = 1/120, "max" = 1),
  ar_dist_type = "uniform"
) {
  id <- 1:no_of_points
  set.seed(seed)
  x <- stats::runif(no_of_points, min = interval["min"], max = interval["max"])
  y <- stats::runif(no_of_points, min = interval["min"], max = interval["max"])
  set.seed(NULL)
  if (ar_dist_type == "uniform") {
    arrival_rate <- stats::runif(no_of_points, min = arv["min"]/60, max = arv["max"]/60)
    # hist(stats::runif(no_of_points, min = arv["min"]/60, max = arv["max"]/60))
    # hist(1/round((stats::runif(no_of_points, min = arv["min"]*60, max = arv["max"]*60))*60))
  } else if (ar_dist_type == "reciprocal") {
    arrival_rate <- 1/round((stats::runif(no_of_points, min = arv["min"]*60, max = arv["max"]*60))*60)
  }
  data <- tibble::tibble(
    "Demand point id" = as.character(id),
    "x" = x,
    "y" = y,
    "Arrival rate" = arrival_rate
  ) %>%
    dplyr::mutate(prob = cumsum(`Arrival rate`/sum(`Arrival rate`)))
  results <- list("data" = data, "interval" = interval,
                  "no_of_points" = no_of_points, "arv" = arv,
                  "ar_dist_type" = ar_dist_type)
  return(results)
}
