
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
simulation_result$metrics[[1]]$response_time_performance
#>    demand_id_handling response_time
#> 1                  35            12
#> 2                  92            14
#> 3                   7            17
#> 4                  33            12
#> 5                  69            19
#> 6                  85            31
#> 7                  43             5
#> 8                  32            12
#> 9                  63            19
#> 10                  5            18
#> 11                 42            18
#> 12                  8            21
#> 13                 75            20
#> 14                 43             6
#> 15                 11            30
#> 16                 80            28
#> 17                 10            20
#> 18                 18            26
#> 19                 45            36
#> 20                 16            13
#> 21                  4            22
#> 22                 19            15
#> 23                 95             1
#> 24                 13            10
#> 25                  3            15
#> 26                 71             9
#> 27                  9            19
#> 28                 60            11
#> 29                 10            19
#> 30                 28            22
#> 31                 45            17
#> 32                 18            21
#> 33                 52            20
#> 34                 81            17
#> 35                 56            22
#> 36                 88            27
#> 37                 87            18
#> 38                 16            13
#> 39                 59            11
#> 40                 22             8
#> 41                 20            15
#> 42                 98            15
#> 43                 22             6
#> 44                 32            11
#> 45                 76            31
#> 46                 73             9
#> 47                 18            21
#> 48                 40            23
#> 49                 91             7
#> 50                 97             7
#> 51                 96            30
#> 52                 71            10
#> 53                 81            10
#> 54                 18            33
#> 55                 63            19
#> 56                 25             3
#> 57                 42            24
#> 58                 75            23
#> 59                 43             9
#> 60                 48            19
#> 61                 85             9
#> 62                  8            23
#> 63                 24            21
#> 64                 53            13
#> 65                 11            28
#> 66                  4            20
#> 67                 46            35
#> 68                 33            14
#> 69                 22            12
#> 70                 45            20
#> 71                 96            16
#> 72                  2            13
#> 73                 53            21
#> 74                  7            16
#> 75                 84            12
#> 76                 45            39
#> 77                 75            17
#> 78                 82            19
#> 79                 21            21
#> 80                  2            19
#> 81                  1             8
#> 82                 58            11
#> 83                 23            25
#> 84                 60            12
#> 85                 80            25
#> 86                 91             6
#> 87                 63            20
#> 88                 34            24
#> 89                 46            15
#> 90                 22             7
#> 91                 52            23
#> 92                 81            10
#> 93                 51            23
#> 94                 15            10
#> 95                 27            23
#> 96                  3            10
#> 97                 43            10
#> 98                 80            28
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
