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
      saveRDS(instance, file = paste0('./instances/',i,hexadec(size = 2),'.rds'))
    }
  }
}
