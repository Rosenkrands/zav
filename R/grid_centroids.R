#' Generate grid centroids for a problem instance
#'
#' @param instance A list returned from generate_2d_instance
#' @param dimension The dimension of the grid
#'
#' @return A list
#' @export
#'
#' @examples
#' # WIP
grid_centroids <- function(
  instance,
  dimension = 3
) {
  # Define the possible x and y possibilities
  x = utils::tail(utils::head(seq(
    instance$interval['min'],instance$interval['max'],
    length.out=dimension+2
  ),-1),-1)
  y = utils::tail(utils::head(seq(
    instance$interval['min'],instance$interval['max'],
    length.out=dimension+2
  ),-1),-1)

  # Find the cartesian product from the possibilities
  locations <- tibble::tibble(expand.grid(x,y)) %>%
    dplyr::rename(x = Var1, y = Var2) %>%
    dplyr::mutate(`Centroid id` = as.character(dplyr::row_number()))

  # Function to calculate distance given a point id and centroid id
  cent_dist <- function(id=c("p_id"=-1,"c_id"-1)){
    distance <- euclid_norm(
      c(
        dplyr::pull(instance$data[id[1,"p_id"], "x"] - locations[id[1,"c_id"], "x"]),
        dplyr::pull(instance$data[id[1,"p_id"], "y"] - locations[id[1,"c_id"], "y"])
      )
    )
    return(
      tibble::tibble(
        `Demand point id` = id[1,"p_id"],
        `Centroid id` = id[1,"c_id"],
        `Distance` = distance
      )
    )
  }

  # Find all possible combinations of demand points and centroids
  arg_df <- expand.grid(
    instance$data$`Demand point id`,
    locations$`Centroid id`
  ) %>%
    dplyr::rename(p_id = Var1, c_id = Var2) %>%
    dplyr::mutate(p_id = as.character(p_id), c_id = as.character(c_id))
  # Convert dataframe to list
  arg_list <- split(arg_df,1:nrow(arg_df))

  # TODO: Parallelize this if it becomes slow
  result_list <- lapply(arg_list,cent_dist)
  distances <- do.call(dplyr::bind_rows,result_list)
  return(list("locations" = locations, "distances" = distances, "no_of_centroids" = dimension^2))
}
