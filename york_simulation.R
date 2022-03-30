library(zav)
library(ggplot2)
library(dplyr)

ar_adjust = 10

# Read precalculated york solutions
clean_york <- function(york_solution) {
  if (grepl("GA|VOT", york_solution)) {
    solution <- readRDS(york_solution)

    solution$no_of_centers <- nrow(solution$centroids)

    solution$clusters <- solution$centroids %>%
      dplyr::mutate(`Cluster id` = dplyr::row_number())

    solution$instance <- solution$instance %>%
      dplyr::inner_join(solution$clusters %>% dplyr::select(-x,-y), by = "Centroid id") %>%
      dplyr::select(-`Centroid id`) %>%
      dplyr::mutate(`Centroid id` = as.character(`Cluster id`)) %>%
      dplyr::select(-`Cluster id`) %>%
      dplyr::mutate(`Arrival rate` = `Arrival rate`/ar_adjust)

    return(solution)
  } else {
    solution <- readRDS(york_solution)

    solution$instance <- solution$instance |>
      dplyr::mutate(`Arrival rate` = `Arrival rate`/ar_adjust)

    return(solution)
  }
}

york_solutions <- lapply(
  list.files("inst/extdata/york", full.names = T) %>% as.list(),
  clean_york
)

solutions <- tibble::tibble(
  solution = york_solutions
)

# Set parameters for simulation
flight = c("zoned", "free")
# free_max_dist = c("0", ".2", ".5", "no constraint")
free_max_dist = c("0", "no constraint")
queue = c(T, F)
replication = 1:20

# Create directory to hold simulations
if (!dir.exists("inst/extdata/york_simulations")) {
  dir.create("inst/extdata/york_simulations")
}

# Assemble parameters in list format
params <- tibble::as_tibble_col(york_solutions) %>%
  dplyr::rename(solution = value) %>%
  dplyr::mutate(solution_file = list.files("inst/extdata/york")) %>%
  dplyr::full_join(tibble::tibble(max_dist = c("zoned", free_max_dist)), by = character()) |>
  dplyr::full_join(tibble::tibble(queue), by = character()) |>
  dplyr::full_join(tibble::tibble(replication), by = character())

params_list <- split(params, 1:nrow(params))

# Function to run the simulation
run_simulation <- function(param) {
  file <- paste0('inst/extdata/york_simulations/sim', param$replication, '_', param$max_dist, '_', param$queue, '_', param$solution_file)
  if (file.exists(file)) {message("File already exists, continuing..."); return()}

  # Determine flight method
  if (param$max_dist == "zoned") {
    flight = "zoned"
    max_dist = 1000000
  } else {
    flight = "free"
    if (param$max_dist != "no constraint") {
      max_dist <- param$solution[[1]]$instance %>%
        dplyr::mutate(distance = sqrt((x - x.centroid)^2 + (y - y.centroid)^2)) %>%
        dplyr::summarise(distance = max(distance)) %>%
        dplyr::mutate(distance = distance + (as.numeric(param$max_dist) * nrow(param$solution[[1]]$clusters))/110) %>%
        as.numeric()
    } else {
      max_dist = 1000000
    }
  }

  rslt <- simulation(
    param$solution[[1]],
    flight = flight,
    max_dist = max_dist,
    queue = param$queue,
    speed_agent = 0.025/110,
    dist_haversine = F,
    seed = param$replication
  )
  saveRDS(rslt, file = file)
}

# set up of parallel computation
num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('york_solutions'))
invisible(parallel::clusterEvalQ(cl, {library(zav)}))

pbapply::pblapply(
  params_list,
  run_simulation,
  cl = cl
)

# generate simulation metadata
simulation_meta <- function(simulation_file) {
  simulation <- readRDS(paste0("inst/extdata/york_simulations/",simulation_file))
  split_name <- stringr::str_split(string = tools::file_path_sans_ext(simulation_file),
                                   pattern = "_")

  utilization <- simulation$metrics[[1]]$agent_log %>%
    dplyr::select(centroid_id, status, time) %>%
    dplyr::mutate(utilization = ifelse(status != "IDLE", 1, 0)) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(utilization = mean(utilization)) %>%
    dplyr::mutate(utilization = tidyr::replace_na(cumsum(utilization)/time, 0),
                  hours = time/60^2) %>%
    dplyr::filter(time %% 60 == 0) %>%
    dplyr::select(-time)
  # ggplot2::ggplot(ggplot2::aes(x = hours, y = utilization)) +
  # ggplot2::geom_line()

  tibble::tibble(
    solution_file = paste0(
      stringr::str_c(split_name[[1]][4:6], collapse = "_"),
      "_100.rds"
    ),
    `Solution method` = factor(
      ifelse(grepl("GA|VOT", solution_file),
             ifelse(grepl("GA", solution_file),"ga-tot", "ga-vot"),
             "wkm-swkm"),
      levels = c("ga-tot", "ga-vot", "wkm-swkm")
    ),
    `Number of UAVs` = factor(
      ifelse(grepl("highUAV", solution_file), "high", "low"),
      levels = c("low", "high")
    ),
    `Arrival rate variance` = factor(
      ifelse(grepl("highARV", solution_file), "high", "low"),
      levels = c("low", "high")
    ),
    flight_id = split_name[[1]][2],
    `Flight configuration` = factor(
      ifelse(flight_id == "zoned", "Zoned", paste0("Free-flight: ", flight_id)),
      levels = c("Zoned", "Free-flight: 0", "Free-flight: .2", "Free-flight: .5", "Free-flight: no constraint")
    ),
    queue = as.logical(split_name[[1]][3]),
    `Queue strategy` = factor(queue, levels = c(T,F), labels = c("FCFS", "No queue")),
    simulation_file = simulation_file,
    # response time metrics
    `Mean response` = mean(simulation$metrics[[1]]$response_time_performance$response_time),
    `90th percentile response` = stats::quantile(simulation$metrics[[1]]$response_time_performance$response_time, probs = c(.9)),
    # fulfillment metrics
    Ploss = 1 - mean(
      simulation$metrics[[1]]$demand_performance$n_covered/simulation$metrics[[1]]$demand_performance$n_generated, na.rm = T
    ),
    `Demands in queue` = ifelse(
      `Queue strategy` == "No queue",
      0,
      simulation$metrics[[1]]$demand_performance %>%
        dplyr::summarise(demands_in_queue = sum(n_generated) - sum(n_covered)) %>%
        as.numeric()
    ),
    # distance metrics (TODO: should maybe group by time and summarise distance = min(distance))
    `Minimum distance` = min(simulation$metrics[[1]]$distances$distance),
    `Mean distance` = mean(simulation$metrics[[1]]$distances$distance),
    `5th percentile distance` = stats::quantile(simulation$metrics[[1]]$distances$distance, probs = c(.05)),
    utilization = list(utilization),
    response_times = list(simulation$metrics[[1]]$response_time_performance),
    min_distances = list(simulation$metrics[[1]]$distances %>% dplyr::group_by(time) %>% dplyr::summarise(distance = min(distance)))
  )
}

metadata <- do.call(
  dplyr::bind_rows,
  pbapply::pblapply(
    list.files("inst/extdata/york_simulations") %>% as.list(),
    simulation_meta,
    cl = cl
  )
)

# calculating objective for solution and joining with simulation results
solutions_metadata <- tibble::tibble(
  solution_file = list.files("inst/extdata/york"),
  solution = york_solutions
) |>
  rowwise() |>
  dplyr::mutate(
    TOT = TOT(solution),
    WCSS = WCSS(solution),
    SAFE = SAFE(solution)
  )

saveRDS(
  metadata |>
    inner_join(solutions_metadata |> select(-solution), by = c("solution_file")),
  file = "inst/extdata/york_simulation_metadata.rds"
)
