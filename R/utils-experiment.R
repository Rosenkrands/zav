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
#' @param ar_mean the arrival rate mean
#' @param ar_deviation the deviation from arrival rate mean
#'
#' @return nothing
#' @export
#'
generate_instances <- function(n = 5,
                               no_of_points = 100,
                               ar_mean = 40,
                               ar_deviation = c(10,35)) {
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
                "max" = ar_mean + j)
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
      arv_min = instance$arv["min"],
      arv_max = instance$arv["max"]
    )
  }

  metadata <- do.call(
    dplyr::bind_rows,
    lapply(
      list.files("instances") %>% as.list(),
      instance_meta
    )
  )

  saveRDS(object = metadata, file = "instance_metadata.rds")
}

generate_solution <- function(methods = c("km", "wkm"), no_of_centers = c(5, 15)) {
  # first we make a instance directory if not already present
  if (dir.exists("solutions")) {
    message("A solution directory was found so new instances will be placed in the existing directory")
  } else {
    message("No solution directory was found so a new one will be created")
    dir.create("solutions")
  }

  # find instances and load into a list
  instances <- lapply(
    list.files("instances") %>% as.list(),
    function(instance_file) readRDS(paste0("./instances/", instance_file))
  )
  names(instances) <- tools::file_path_sans_ext(list.files("instances"))

  # generate solution parameters based on instances and methods
  params <- expand.grid(names(instances), methods, no_of_centers) %>%
    dplyr::rename(instance = Var1, method = Var2, no_of_centers = Var3)

  params_list <- split(params, 1:nrow(params))

  solve_and_save <- function(param) {
    # filename for the solution
    file = paste0(
      "./solutions/",
      hexadec(size = 4),'_',
      param$instance,'_',
      param$method,'_',
      param$no_of_centers,'.rds'
    )

    # choose solution method based on param$method
    if (param$method == "km") {
      solution <- solve_kmeans(instance = instances[[param$instance]],
                               no_of_centers = param$no_of_centers)
    } else if (param$method == "wkm") {
      solution <- solve_wkmeans(instance = instances[[param$instance]],
                                no_of_centers = param$no_of_centers)
    } else {
      stop(paste0("method '", param$method, "' not implemented."))
    }

    # save solution
    saveRDS(solution, file = file)
  }

  # generating solution in parallel
  num_cores <- parallel::detectCores(logical = F)
  cl <- parallel::makeCluster(num_cores)

  invisible(parallel::clusterExport(cl, c('instances')))
  invisible(parallel::clusterEvalQ(cl, library(zav)))
  pbapply::pblapply(params_list, solve_and_save, cl = cl) -> res

  parallel::stopCluster(cl)

  # generating solution metadata

}
