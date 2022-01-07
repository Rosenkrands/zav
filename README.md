
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
plot_point(instance = instance)
```

<img src="man/figures/README-example-1.png" width="75%" />

## Zoning solutions

### KMeans

The below code chunk shows how we can utilize the `solve_kmeans`
function to generate a solution for our problem instance.

``` r
solution <- solve_kmeans(instance, no_of_centers = 5)
plot_bases(instance = instance, solution = solution) +
  theme(legend.position = "none")
```

<img src="man/figures/README-solution-1.png" width="75%" />

## Simulation

Here it is shown how you can make a simulation based on the solution
just found:

``` r
#' The below line is for testing purposes
#' solution = solution; seed = 1;n_replications = 1;flight = "zoned";max_dist = 1000000;LOS = 600;warmup = 0;speed_agent = .25;verbose = F
simulation_result <- simulation(
  solution = solution,
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
