#' Gather results from the experiment
#'
#' This function will access the specific metadata files generated for the experiment conducted and perform some data cleaning.
#' After the data is cleaned a separate data set is generated for instances, solutions and simulations.
#' These are stored in a list which is returned by the function.
#'
#' The purpose of the function is to show how the raw metadata from the instances, solutions and simulations is transformed into the results data set.
#'
#' The function is tailored to the specific experiment conducted and will need to be revised to handle a new experiment.
#' This is as a results of, but not limited to, the arrival rate variance and number of UAVs levels being hardcoded.
#'
#' @return A list of 3 tibbles.
#' @export
#'
experiment_results <- function() {
  # gather all metadata files
  instance_metadata <- readRDS(system.file("extdata", "instance_metadata.rds", package = "zav"))
  solution_metadata <- readRDS(system.file("extdata", "solution_metadata.rds", package = "zav"))
  simulation_metadata <- readRDS(system.file("extdata", "simulation_metadata.rds", package = "zav"))

  # clean metadata files
  instance_data_clean <- instance_metadata |>
    dplyr::transmute(`Instance id` = instance_id,
                     `Point distribution` = factor(point_location_id),
                     `Arrival rate distribution` = factor(ar_dist_type, levels = c("uniform", "reciprocal"), labels = c("uniform", "unbalanced")),
                     `Arrival rate variance` = factor(arv_max - arv_min, levels = c(0.2, 0.98), labels = c("low", "high")),
                     `Total arrival rate` = total_arrival_rate)
  solution_data_clean <- solution_metadata |>
    dplyr::filter(solution_method != "wkm-flexclust") |>
    dplyr::transmute(`Solution id` = solution_file,
                     `Instance id` = instance_id,
                     `Solution method` = factor(solution_method),
                     `Number of UAVs` = factor(number_of_uavs, levels = c(5, 15), labels = c("low", "high")),
                     TOT,
                     WCSS,
                     SAFE)
  simulation_data_clean <- simulation_metadata |>
    dplyr::transmute(`Solution id` = solution_file,
                     `Flight configuration` = factor(
                       ifelse(flight_id == "zoned", "Zoned", paste0("Free-flight: ", flight_id)),
                       levels = c("Zoned", "Free-flight: 0", "Free-flight: .2", "Free-flight: .5", "Free-flight: no constraint")
                     ),
                     `Queue strategy` = factor(queue, levels = c(T,F), labels = c("FCFS", "No queue")),
                     `Mean response`,
                     `90th percentile response`,
                     Ploss,
                     `Demands in queue` = ifelse(`Queue strategy` == "No queue", 0, `Demands in queue`),
                     `Minimum distance`,
                     `Mean distance`,
                     `5th percentile distance`,
                     utilization,
                     response_times,
                     min_distances)

  # join cleaned files together
  results <- list(
    "instance" = instance_data_clean,
    "solution" = instance_data_clean |>
      dplyr::inner_join(solution_data_clean, by = "Instance id"),
    "simulation" = instance_data_clean |>
      dplyr::inner_join(solution_data_clean, by = "Instance id") |>
      dplyr::inner_join(simulation_data_clean, by = "Solution id")
  )

  return(results)
}

# clean_solution_metadata <- solution_metadata |>
#   dplyr::inner_join(readRDS("instance_metadata.rds"), by = "instance_id") |>
#   dplyr::mutate(number_of_uavs = factor(number_of_uavs, levels = c(5,15), labels = c("low", "high")),
#                 arrival_rate_variance = factor(arv_min, levels = c(.4, .01), labels = c("low", "high"))) |>
#   tidyr::pivot_longer(c(TOT, WCSS), names_to = "objective") |>
#   # dplyr::group_by(solution_method, number_of_uavs, arv_min, objective) %>%
#   # dplyr::summarise(value = mean(value)) %>%
#   dplyr::filter(objective == "TOT", solution_method != "ga-safe")
#
# clean_solution_metadata |>
#   ggplot2::ggplot() +
#   ggplot2::geom_boxplot(ggplot2::aes(x = number_of_uavs, y = value, fill = solution_method)) +
#   ggplot2::facet_wrap(arrival_rate_variance~ar_dist_type, scales = "free", labeller = ggplot2::label_both)
#
# # Significant for the high number of uavs but not for low
# lm_fit <- lm(value ~ solution_method, data = clean_solution_metadata |>  dplyr::filter(arrival_rate_variance == "high", number_of_uavs == "high", ar_dist_type == "uniform"))
#
# summary(lm_fit)
