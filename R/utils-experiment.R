#' hexadecimal of given size
#'
#' @param size Length of hexadecimal
#'
#' @return A string
#' @export
#'
#' @examples
#' hexadec()
hexadec <- function(size = 64) {
  paste0(
    paste0(sample(c(0:9, LETTERS), size, T), collapse = '')
  )
}

#' Generate instances for experiment
#'
#' @param n number of instances to generate per class
#' @param no_of_points number of demand points in an instance
#' @param ar_mean the arrival rate mean (arrivals per minute)
#' @param ar_deviation the deviation from arrival rate mean
#' @param ar_dist_type Argument passed to `generate_2d_instance`
#'
#' @return nothing
#' @export
#'
generate_instances <- function(n = 5,
                               no_of_points = 100,
                               ar_mean = .5,
                               ar_deviation = c(.10,.49),
                               ar_dist_type = c("uniform", "reciprocal")) {
  # first we make a instance directory if not already present
  if (dir.exists("instances")) {
    message("A instance directory was found so new instances will be placed in the existing directory")
  } else {
    message("No instance directory was found so a new one will be created")
    dir.create("instances")
  }

  # generate the instances and save them in the directory
  for (i in 1:n) {
    for (j in ar_deviation) {
      instance <- generate_2d_instance(
        seed = i,
        no_of_points = 100,
        arv = c("min" = ar_mean - j,
                "max" = ar_mean + j),
        ar_dist_type = ar_dist_type
      )
      saveRDS(instance, file = paste0('./instances/',
                                      stringr::str_pad(i,2,"left","0"),
                                      hexadec(size = 2),
                                      '.rds'))
    }
  }

  # Write file with metadata for the instances
  instance_meta <- function(instance_file) {
    instance <- readRDS(paste0("./instances/", instance_file))
    instance_id <- tools::file_path_sans_ext(instance_file)

    tibble::tibble(
      instance_id = instance_id,
      point_location_id = substr(instance_id,1,2),
      demand_dist_id = substr(instance_id,3,nchar(instance_id)),
      ar_dist_type = instance$ar_dist_type,
      arv_min = instance$arv["min"],
      arv_max = instance$arv["max"],
      total_arrival_rate = sum(instance$data$`Arrival rate`)
    )
  }

  message("Saving metadata for instances")
  metadata <- do.call(
    dplyr::bind_rows,
    lapply(
      list.files("instances") %>% as.list(),
      instance_meta
    )
  )
  saveRDS(object = metadata, file = "instance_metadata.rds")
}

#' Generate solutions to instances
#'
#' A solution is generated and saved in the solution folder for all combinations of methods and number of centers.
#'
#' @param methods character vector of methods to use for solution (currently: km, wkm)
#' @param no_of_centers integer vector of number of centers, or uavs, to include
#'
#' @return nothing
#' @export
#'
generate_solutions <- function(methods = c("km", "wkm-flexclust", "wkm-swkm", "ga-safe", "ga-tot", "ga-otv"),
                               no_of_centers = c(5, 15)) {
  # first we make a instance directory if not already present
  if (dir.exists("solutions")) {
    message("A solution directory was found so new instances will be placed in the existing directory")
  } else {
    message("No solution directory was found so a new one will be created")
    dir.create("solutions")
  }

  # find instances and load into a list
  message("Reading instances from the instance directory")
  instances <<- lapply(
    list.files("instances") %>% as.list(),
    function(instance_file) readRDS(paste0("./instances/", instance_file))
  )
  names(instances) <- tools::file_path_sans_ext(list.files("instances"))

  # precalculate centroids if we are using ga methods
  if (("ga-safe" %in% methods) | ("ga-tot" %in% methods) | ("ga-otv" %in% methods)) {
    message("Precalculating grid centroids for all instances")

    num_cores <- parallel::detectCores(logical = F)
    cl <- parallel::makeCluster(num_cores)
    invisible(parallel::clusterEvalQ(cl, library(zav)))

    centroids <- pbapply::pblapply(
      instances,
      function(instance) grid_centroids(instance, dimension = 8),
      cl = cl
    )

    parallel::stopCluster(cl)
  }

  # generate solution parameters based on instances and methods
  message("Generating solution parameters")
  params <- expand.grid(names(instances), methods, no_of_centers) %>%
    dplyr::rename(instance = Var1, method = Var2, no_of_centers = Var3)

  params_list <- split(params, 1:nrow(params))

  solve_and_save <- function(param) {
    # filename for the solution
    file = paste0(
      "./solutions/",
      # hexadec(size = 4),'_',
      param$instance,'_',
      param$method,'_',
      param$no_of_centers,'.rds'
    )

    if (file.exists(file)) {message("File already exists, continuing..."); return()}

    # iterations to use in ga
    miter = 100000

    # choose solution method based on param$method
    if (param$method == "km") {
      solution <- solve_kmeans(instance = instances[[param$instance]],
                               no_of_centers = param$no_of_centers)
    } else if (param$method == "wkm-swkm") {
      solution <- solve_wkmeans(instance = instances[[param$instance]],
                                no_of_centers = param$no_of_centers,
                                type = "swkm")
    } else if (param$method == "wkm-flexclust") {
      solution <- solve_wkmeans(instance = instances[[param$instance]],
                                no_of_centers = param$no_of_centers,
                                type = "flexclust")
    } else if (param$method == "ga-tot") {
      solution <- solve_ga(instance = instances[[param$instance]],
                           centroids = centroids[[param$instance]],
                           no_of_centers = param$no_of_centers,
                           obj = "TOT",
                           miter = miter)
    } else if (param$method == "ga-safe") {
      solution <- solve_ga(instance = instances[[param$instance]],
                           centroids = centroids[[param$instance]],
                           no_of_centers = param$no_of_centers,
                           obj = "SAFE",
                           miter = miter)
    } else if (param$method == "ga-otv") {
      solution <- solve_ga(instance = instances[[param$instance]],
                           centroids = centroids[[param$instance]],
                           no_of_centers = param$no_of_centers,
                           obj = "OTV",
                           miter = miter)
    } else {
      stop(paste0("method '", param$method, "' not implemented."))
    }

    # save solution
    saveRDS(solution, file = file)
  }

  # generating solutions in parallel
  message("Generating solutions")
  num_cores <- parallel::detectCores(logical = F)
  cl <- parallel::makeCluster(num_cores)
  message("Exporting instances to cluster")
  invisible(parallel::clusterExport(cl, c('instances')))
  invisible(parallel::clusterEvalQ(cl, library(zav)))
  message("Call to pblapply")
  pbapply::pblapply(params_list, solve_and_save, cl = cl) -> res

  parallel::stopCluster(cl)

  # generating solution metadata
  message("Generating metadata for solutions")
  solution_meta <- function(solution_file) {
    solution <- readRDS(paste0("./solutions/",solution_file))
    split_name <- stringr::str_split(string = tools::file_path_sans_ext(solution_file),
                                     pattern = "_")

    tibble::tibble(
      # solution_id = split_name[[1]][1],
      solution_file = solution_file,
      instance_id = split_name[[1]][1],
      solution_method = split_name[[1]][2],
      number_of_uavs = as.numeric(split_name[[1]][3]),
      TOT = TOT(solution),
      WCSS = WCSS(solution),
      SAFE = SAFE(solution),
      OTV = OTV(solution)
    )
  }

  metadata <- do.call(
    dplyr::bind_rows,
    lapply(
      list.files("solutions") %>% as.list(),
      solution_meta
    )
  )
  saveRDS(metadata, file = "solution_metadata.rds")
}

#' Generate simulations based on solutions
#'
#' A simulation is generated and saved in the solution folder for specified flight method, and max_dist for free flight.
#'
#' @param flight character vector of flight methods, currently `c("zoned","free")`.
#' @param free_max_dist max distances for use with free flight, currently `c("0", ".2", ".5", "no constraint")`.
#' @param queue Logical specifiying whether a queue is desired or not (or both as default).
#'
#' @return nothing
#' @export
#'
generate_simulations <- function(flight = c("zoned", "free"),
                                 free_max_dist = c("0", ".2", ".5", "no constraint"),
                                 queue = c(T, F)) {
  # create simulations directory if not already present
  if (dir.exists("simulations")) {
    message("A simulation directory was found so new simulations will be placed in the existing directory")
  } else {
    message("No simulation directory was found so a new one will be created")
    dir.create("simulations")
  }

  # read metadata files
  metadata <- readRDS("solution_metadata.rds") %>%
    dplyr::inner_join(readRDS("instance_metadata.rds"), by = c("instance_id"))

  # read solutions into a list
  solutions <- pbapply::pblapply(
    metadata$solution_file %>% split(1:nrow(metadata)),
    function(file) readRDS(paste0('solutions/',file))
  )
  names(solutions) <- metadata$solution_file

  params <- tibble::as_tibble_col(solutions) %>%
    dplyr::rename(solution = value) %>%
    dplyr::mutate(solution_file = metadata$solution_file) %>%
    dplyr::full_join(tibble::tibble(max_dist = c("zoned", free_max_dist)), by = character()) |>
    dplyr::full_join(tibble::tibble(queue), by = character())

  params_list <- split(params, 1:nrow(params))

  run_simulation <- function(param) {
    file <- paste0('simulations/sim_', param$max_dist, '_', param$queue, '_', param$solution_file)
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
          dplyr::mutate(distance = distance + as.numeric(param$max_dist) * nrow(param$solution[[1]]$clusters)) %>%
          as.numeric()
      } else {
        max_dist = 1000000
      }
    }

    rslt <- simulation(param$solution[[1]], flight = flight, max_dist = max_dist, queue = param$queue)
    saveRDS(rslt, file = file)
  }

  # set up of parallel computation
  num_cores <- parallel::detectCores(logical = F)
  cl <- parallel::makeCluster(num_cores)

  parallel::clusterExport(cl, c('solutions'))
  invisible(parallel::clusterEvalQ(cl, {library(zav)}))

  pbapply::pblapply(
    params_list,
    run_simulation,
    cl = cl
  )

  # generate simulation metadata
  message("Generating metadata for simulations")
  simulation_meta <- function(simulation_file) {
    simulation <- readRDS(paste0("./simulations/",simulation_file))
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
        ".rds"
      ),
      flight_id = split_name[[1]][2],
      queue = as.logical(split_name[[1]][3]),
      simulation_file = simulation_file,
      # response time metrics
      `Mean response` = mean(simulation$metrics[[1]]$response_time_performance$response_time),
      `90th percentile response` = stats::quantile(simulation$metrics[[1]]$response_time_performance$response_time, probs = c(.9)),
      # fulfillment metrics
      Ploss = 1 - mean(
        simulation$metrics[[1]]$demand_performance$n_covered/simulation$metrics[[1]]$demand_performance$n_generated, na.rm = T
      ),
      `Demands in queue` = simulation$metrics[[1]]$demand_performance %>%
        dplyr::summarise(demands_in_queue = sum(n_generated) - sum(n_covered)) %>%
        as.numeric(),
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
      list.files("simulations") %>% as.list(),
      simulation_meta,
      cl = cl
    )
  )
  saveRDS(metadata, file = "simulation_metadata.rds")
}
