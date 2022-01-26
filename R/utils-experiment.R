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
