
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

instance <- generate_2d_instance()
plot_point(instance = instance)
```

<img src="man/figures/README-example-1.png" width="100%" />

Here it is shown how the instance can be solved by the KMeans solution
approach:

``` r
solution <- solve_kmeans(instance, no_of_centers = 5)
plot_bases(instance = instance, solution = solution)
```

<img src="man/figures/README-solution-1.png" width="100%" />
