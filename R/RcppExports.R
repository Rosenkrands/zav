# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculate euclidean distance for the safety distances
#'
#' @param A data.matrix produced in the simulation
#' @export
sim_dist <- function(A) {
    .Call('_zav_sim_dist', PACKAGE = 'zav', A)
}

#' Calculate smallest distance between points not in same zone
#'
#' @param A data.matrix produced from solve_ga function with obj == "SAFE"
#' @export
safe_dist <- function(A) {
    .Call('_zav_safe_dist', PACKAGE = 'zav', A)
}

