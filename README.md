
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

## Example

This is a basic example which shows how to generate a problem instance:

``` r
library(zav)
library(ggplot2)

instance <- generate_2d_instance(no_of_points = 100)
plot_point(instance = instance)
```

<img src="man/figures/README-example-1.png" width="75%" />

Here it is shown how the instance can be solved by the KMeans solution
approach:

``` r
solution <- solve_kmeans(instance, no_of_centers = 5)
plot_bases(instance = instance, solution = solution)
```

<img src="man/figures/README-solution-1.png" width="75%" />

Here it is shown how you can make a simulation based on the solution
just found:

``` r
# solution = solution; seed = 1;n_replications = 1;flight = "zoned";max_dist = 1000000;LOS = 600;warmup = 0;speed_agent = .25;verbose = F
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
simulation_result$metrics[[1]]$response_time_performance
#>    demand_id_handling response_time
#> 1                  33            12
#> 2                  37            17
#> 3                  49            15
#> 4                  88            16
#> 5                  27            18
#> 6                  32            12
#> 7                  49            15
#> 8                  12            11
#> 9                  21            21
#> 10                 19            13
#> 11                 97             7
#> 12                 73             9
#> 13                  8            20
#> 14                 19            12
#> 15                 28            10
#> 16                 37            17
#> 17                 31            20
#> 18                 28             9
#> 19                 19            12
#> 20                  7            16
#> 21                 78             6
#> 22                 16            13
#> 23                 21            20
#> 24                 55            14
#> 25                 73             9
#> 26                 13            10
#> 27                 16            13
#> 28                 65            14
#> 29                 30            12
#> 30                 55            14
#> 31                 70             8
#> 32                 75            16
#> 33                 77            13
#> 34                 48            14
#> 35                 90            12
#> 36                 46            15
#> 37                 64             4
#> 38                100            14
#> 39                 14            13
#> 40                 33            11
#> 41                 89            17
#> 42                 18            21
#> 43                  2            13
#> 44                 94            13
#> 45                 81            11
#> 46                 62            12
#> 47                 87             6
#> 48                 93            14
#> 49                 56            22
#> 50                 13            11
#> 51                 88            15
#> 52                 68             6
#> 53                 35            12
#> 54                 81            11
#> 55                 86            19
#> 56                 73             9
#> 57                 49            14
#> 58                 97             6
#> 59                 21            21
#> 60                 89            16
#> 61                 58             8
#> 62                 61             9
#> 63                 24            21
#> 64                 25             3
#> 65                 39            16
#> 66                 75            17
#> 67                 49            14
#> 68                 45            17
#> 69                 96            15
#> 70                 25             3
#> 71                 47            25
#> 72                 49            14
#> 73                 32            11
#> 74                 11            20
#> 75                 99             4
#> 76                 80            17
#> 77                 27            19
#> 78                 33            11
#> 79                 68             5
#> 80                 95             2
#> 81                 64             4
#> 82                  3             9
#> 83                 94            13
#> 84                 89            16
#> 85                 46            15
#> 86                 22             6
#> 87                 81            10
#> 88                 80            17
#> 89                 68             6
#> 90                 24            22
#> 91                 30            13
#> 92                 43             4
#> 93                 97             7
#> 94                 72             6
#> 95                 49            14
#> 96                 89            17
#> 97                 90            13
```

``` r
simulation_result$metrics[[1]]$distances %>%
  ggplot(aes(x = time, 
             y = distance,
             color = paste0(id1,'-',id2))) +
  geom_line() +
  labs(color = "Pair") +
  theme_bw()
```

<img src="man/figures/README-distances-1.png" width="75%" />
