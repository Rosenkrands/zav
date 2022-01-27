
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Zoning for Autonomous Vehicles

<!-- badges: start -->
<!-- badges: end -->

The goal of zav (Zoning for Autonomous Vehicles) is to show the
implementation of the problem instance generation and simulation
experiment conducted for the project.

## Installation

You can install the development version of zav from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rosenkrands/zav")
```

## Instance generation

To generate a problem instance we can utilize the `generate_2d_instance`
function.

``` r
library(zav)
library(ggplot2)

instance <- generate_2d_instance(no_of_points = 100)
# plot_point(instance = instance)
ggplot2::ggplot(instance$data) +
    ggplot2::geom_point(ggplot2::aes(x,y,size=`Arrival rate`),
                        shape = 21, fill = alpha("black", .2)) +
    ggplot2::theme_void()
```

<img src="man/figures/README-example-1.png" width="75%" />

## Zoning solutions

### KMeans

The below code chunk shows how we can utilize the `solve_kmeans`
function to generate a solution for our problem instance.

``` r
solution_km <- solve_kmeans(instance, no_of_centers = 5)
plot_bases(solution = solution_km)
```

<img src="man/figures/README-solution-1.png" width="75%" />

### Genetic Algorithm

The below chunk shows how we can utilize the `solve_ga` function to
generate a solution for our problem instance.

First we need to precalculate centroids to use as input for the GA.

``` r
centroids <- grid_centroids(instance, dimension = 5)
```

Having now the centroids and distances between demand points and
centroids, we are able to give this as input for the GA. As a default
the GA will have a maximum of 10 iterations for demonstration purposes,
in reality we would have a much higher number of iterations.

``` r
solution <- solve_ga(instance, centroids, no_of_centers = 5, obj = "SAFE")
```

``` r
plot_bases(solution)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="75%" />

## Simulation

Here it is shown how you can make a simulation based on the solution
just found:

``` r
#' The below line is for testing purposes
#' solution = solution; seed = 1;n_replications = 1;flight = "zoned";max_dist = 1000000;LOS = 600;warmup = 0;speed_agent = .25;verbose = F
simulation_result <- simulation(
  solution = solution_km,
  seed = 1,
  n_replications = 1,
  flight = "zoned",
  max_dist = 1000000,
  LOS = 600,
  warmup = 0,
  speed_agent = .25,
  verbose = F
)
```

``` r
# simulation_result$metrics[[1]]$response_time_performance
```

``` r
simulation_result$metrics[[1]]$distances %>%
  ggplot(aes(x = time, 
             y = distance,
             color = paste0(id1,'-',id2))) +
  geom_line() +
  labs(color = "Pair") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-distances-1.png" width="75%" />
