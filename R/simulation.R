#' Simulation for a zoning solution
#'
#' @param solution A list returned from one of the solution approaches
#' @param seed Passed to the set.seed function for reproducibility of results
#' @param n_replications Number of times to repeat the simulation
#' @param flight Whether to use hard zoning or soft zoning (free-flight)
#' @param max_dist The maximum distance a demand point can be of a base to be serviced by that base
#' @param LOS Length of simulation in seconds
#' @param warmup Fraction of LOS to discard as warmup period
#' @param speed_agent Movement per time unit for an agent
#' @param verbose Whether or not to print the information on the simulation
#'
#' @return None at the moment
#' @export
#'
#' @examples
#' # WIP
simulation <- function(
  solution,
  seed = 1,
  n_replications = 1,
  flight = c("zoned", "free"),
  max_dist = 1000000,
  LOS = 600,
  warmup = 0,
  speed_agent = 0.25, # Movement per time unit
  verbose = F
) {
  set.seed(seed)

  # Get number of demand points from the solution instance
  n_demands = nrow(solution$instance)

  # G
  total_demand_rate = sum(solution$instance$`Arrival rate`)

  n_agents = solution$no_of_centers
  agent_base_info = data.frame(
    id = c(1:n_agents),
    X = solution$clusters$x,
    Y = solution$clusters$y
  )

  # Helper functions
  get_demand_point_id <- function(df){
    prob = stats::runif(1)
    for (i in 1:length(df)){
      if(prob < df[i]){
        return(i)
      }
    }
    return(0)
  }

  get_distance <- function (x1, y1, x2, y2){
    return(((x1-x2)^2 +(y1-y2)^2)^0.5)
  }

  if (flight == "zoned") {
    get_nearest_agent <- function(demand_id, agent_list){
      x = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demand_id, "x"
      ])
      y = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demand_id, "y"
      ])
      df_temp <- data.frame(id = agent_list$id, dist = rep(0, nrow(agent_list)))
      demand_id_zone <- solution$instance[
        solution$instance$`Demand point id` == demand_id, 'Centroid id'
      ] %>% as.character()
      for (i in 1:nrow(agent_list)){
        if (demand_id_zone == agent_base_info[agent_base_info$id == agent_list$id[i], "id"]) {
          df_temp$dist[i] = dplyr::if_else(
            agent_list$status[i] == "IDLE",
            get_distance(x,y, agent_list$x_now[i], agent_list$y_now[i]),
            1000000
          )
        }
        else {
          df_temp$dist[i] = 1000000
        }
      }
      df_temp <- df_temp[sample(nrow(df_temp)), ] # to avoid selection with lowest id.
      df_temp <- df_temp[order(df_temp$dist), ]  # sort the list by distance
      if (df_temp[1,2] < 1000000){
        return (df_temp[1,1]) # return agent id
      }
      else {# all agents are busy
        return(0)
      }
    }
  } else if (flight == "free") {
    get_nearest_agent <- function(demand_id, agent_list){
      x = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demand_id, "x"
      ])
      y = as.numeric(solution$instance[
        solution$instance$`Demand point id` == demand_id, "y"
      ])
      df_temp <- data.frame(id = agent_list$id, dist = rep(0, nrow(agent_list)))
      for (i in 1:nrow(agent_list)){
        df_temp$dist[i] = dplyr::if_else(
          agent_list$status[i] == "IDLE",
          get_distance(x,y, agent_list$x_now[i], agent_list$y_now[i]),
          1000000
        )
      }
      df_temp <- df_temp[sample(nrow(df_temp)), ] # to avoid selection with lowest id.
      df_temp <- df_temp[order(df_temp$dist), ]  # sort the list by distance
      if (df_temp[1,2] < max_dist){
        return (df_temp[1,1]) # return agent id
      }
      else {# all agents are busy
        return(0)
      }
    }
  }

  df_demandpoints = solution$instance %>%
    dplyr::rename(X = x, Y = y)

  metric_list <- list()
  # agent_log_list <- list()
  utilization_list <- list()

  for (n in 1:n_replications){
    if (verbose) cat(sprintf("Replication = : %s\n", n))
    # Initialize an agent list (assume they are at 0,0 at the beginning)
    agent_list = data.frame(id = agent_base_info$id,
                           x_now = agent_base_info$X,
                           y_now = agent_base_info$Y,
                           goal_x= agent_base_info$X,
                           goal_y= agent_base_info$Y,
                           demand_id_handling = rep(0, n_agents),
                           t_deployed = rep(0, n_agents),
                           status = rep("IDLE", n_agents), stringsAsFactors=FALSE)

    agent_log = agent_list %>% dplyr::mutate(time = -1)

    # Initialize a simulation result
    demand_performance = data.frame(
      n_generated = rep(0, n_demands),
      n_covered = rep(0, n_demands),
      total_response_time = rep(0, n_demands)
    )
    agent_performance = data.frame(
      n_dispatched= rep(0, n_agents),
      total_usage= rep(0,n_agents)
    )
    response_time_performance = data.frame(
      demand_id_handling = integer(),
      response_time = integer()
    )

    # Initialize the event list
    event_list = data.frame(
      event = character(),
      time = numeric(),
      agentid = numeric(),
      demand_id = numeric()
    )
    t_next = round(stats::rexp(1, total_demand_rate)) # Sample next demand arrival time
    demand_id = get_demand_point_id(df_demandpoints$prob) # assign demand points based on their demand rates
    event_list <- dplyr::bind_rows(
      event_list,
      data.frame(event = c("Call"),
                 time = c(t_next),
                 demand_id = c(demand_id))
    ) # put the first event into the event_list

    event_list <- dplyr::bind_rows(
      event_list,
      data.frame(event = c("Move"),
                 time = c(t_next),
                 demand_id = c(0))
    ) # put the "move" event into the event_list

    t_now = t_next
    reset = F # used in relation to excluding the warm up period
    while(nrow(event_list)>0 && t_now <LOS){
      # Extract the current event from the list and remove the event from the list
      event_now <- event_list[1,]
      event_list <- event_list[-c(1),]
      t_now = event_now$time
      if (verbose) cat(sprintf("EVENT = : %s\t", event_now$event), sprintf("Time = : %s\n", t_now))

      # Exclude the warmup period (i.e. the first hour of the simulation)
      if ((t_now > LOS*(1-warmup)) & (reset == F)) {
        demand_performance = data.frame(
          n_generated = rep(0, n_demands),
          n_covered = rep(0, n_demands),
          total_response_time = rep(0, n_demands)
        )
        agent_performance = data.frame(
          n_dispatched = rep(0, n_agents),
          total_usage = rep(0,n_agents)
        )
        response_time_performance = data.frame(
          demand_id_handling = integer(),
          response_time = integer()
        )
        reset = T
      }

      switch(as.character(event_now$event),
             "Call"={
               # update call performance data
               demand_performance$n_generated[event_now$demand_id] <- demand_performance$n_generated[event_now$demand_id] +1
               # Find the nearest agent
               agent_id = get_nearest_agent(event_now$demand_id, agent_list)
               if (agent_id > 0){
                 # Update status of the agent assigned to the call
                 agent_list$status[agent_id] <- "BUSY"
                 agent_list$goal_x[agent_id] <- df_demandpoints$X[event_now$demand_id]
                 agent_list$goal_y[agent_id] <- df_demandpoints$Y[event_now$demand_id]
                 agent_list$demand_id_handling[agent_id] <- event_now$demand_id
                 agent_list$t_deployed[agent_id] <- t_now
                 agent_performance$n_dispatched[agent_id] <- agent_performance$n_dispatched[agent_id] + 1
                 demand_performance$n_covered[event_now$demand_id] <-  demand_performance$n_covered[event_now$demand_id] + 1
               }
               else{
                 # No agent is available.
                 # Assume that this call is discharged in this example.
               }

               # Generate next call
               t_next = t_now + round(stats::rexp(1, total_demand_rate))
               demand_id = get_demand_point_id(df_demandpoints$prob)
               event_list <- dplyr::bind_rows(
                 event_list, data.frame(event = c("Call"),
                                       time = c(t_next),
                                       demand_id = c(demand_id))
               )
             },

             "Move"={
               movingAgents = agent_list[(agent_list$status == "BUSY" | agent_list$status == "BACK"), ]
               if(nrow(movingAgents) > 0){
                 for(i in 1:nrow(movingAgents)){
                   agent_id = movingAgents$id[i]
                   # position update
                   x_increment = (agent_list$goal_x[agent_id]- agent_list$x_now[agent_id])
                   y_increment = (agent_list$goal_y[agent_id]- agent_list$y_now[agent_id])
                   length_to_move = (x_increment^2 + y_increment^2)^0.5
                   if(length_to_move > speed_agent){
                     angle = atan2(y_increment, x_increment)
                     x_update = speed_agent*cos(angle)
                     y_update = speed_agent*sin(angle)
                     agent_list$x_now[agent_id] = agent_list$x_now[agent_id] + x_update
                     agent_list$y_now[agent_id] = agent_list$y_now[agent_id] + y_update
                   }
                   else{ # Agent arrived at the destination
                     agent_list$x_now[agent_id]  = agent_list$goal_x[agent_id]
                     agent_list$y_now[agent_id]  = agent_list$goal_y[agent_id]
                     if (agent_list$status[agent_id] == "BUSY"){ # arrived at the demand point
                       if (verbose) cat(sprintf("Agent %s\t", agent_id), sprintf(" arrived at demand point %s ", agent_list$demand_id_handling[agent_id]), sprintf("at time %s\n", t_now))
                       # Record demand performance
                       demand_performance$total_response_time[agent_list$demand_id_handling[agent_id]] <-  demand_performance$total_response_time[agent_list$demand_id_handling[agent_id]] + (t_now-agent_list$t_deployed[agent_id])
                       response_time_performance <- dplyr::bind_rows(
                         response_time_performance,
                         data.frame(demand_id_handling = agent_list$demand_id_handling[agent_id],
                                    response_time = t_now-agent_list$t_deployed[agent_id])
                       )

                       # Assign the return trip
                       agent_list$status[agent_id] = "BACK"
                       agent_list$goal_x[agent_id] = agent_base_info$X[agent_id]
                       agent_list$goal_y[agent_id] = agent_base_info$Y[agent_id]
                     }
                     else { # Agent returned to its base
                       if (verbose) cat(sprintf("Agent %s\t", agent_id), sprintf(" returns its home base at time %s\n", t_now))
                       # update agent usage data
                       agent_performance$total_usage[agent_id] <- agent_performance$total_usage[agent_id] + (t_now - agent_list$t_deployed[agent_id])
                       # update agent status
                       agent_list$status[agent_id] = "IDLE"
                       # We may add set-up time for a next deployment
                     }
                   }
                 }
               }
               # Generate next "Move" event
               event_list <- dplyr::bind_rows(
                 event_list,
                 data.frame(event = c("Move"),
                            time = c(t_now + 1),
                            demand_id = c(0))
               )
               agent_log <- dplyr::bind_rows(
                 agent_log,
                 agent_list %>% dplyr::mutate(time = t_now)
               )
             },
             {
               print('A wrong event generated')
             }
      )
      # SORT the events in the list by their time
      event_list <- event_list[order(event_list$time),]
    }

    ### Post processing of the agent log ###

    # Find the time for first demand arrival
    t_first_demand <- agent_log$time[agent_log$time != -1][1]

    # Generate missing rows in the agent log until first demand arrival

    missing <- agent_log[agent_log$time == -1,]
    for (i in 1:t_first_demand) {
      missing <- dplyr::bind_rows(
        missing,
        agent_log[agent_log$time == -1, ] %>%
          dplyr::mutate(time = i - 1)
      )
    }
    missing <- missing[missing$time != -1, ]

    # Add missing rows to the agent log
    agent_log <- dplyr::bind_rows(
      missing,
      agent_log[agent_log$time >= t_first_demand, ]
    )

    ### Calculate safety distances ###

    # Calculate distance between agents at any given time
    locations <- agent_log %>%
      dplyr::select(id, x = x_now, y = y_now, time) %>%
      dplyr::mutate(idt = paste0(time,'_',id))

    # Get all combinations of agents
    combinations <- utils::combn(unique(locations$id), 2) %>% t()


    distances <- tibble::tibble( # Repeating possible combinations for each unit of time
      id1 = rep(combinations[,1],length(unique(locations$time))),
      id2 = rep(combinations[,2],length(unique(locations$time))),
      time = sort(rep(seq(0, length(unique(locations$time)) - 1), length(combinations[,1])))
    ) %>%
      # Generating composite key on agent id and time to join position from locations
      dplyr::mutate(idt1 = paste0(time, '_',id1), idt2 = paste0(time, '_',id2)) %>%
      dplyr::inner_join(locations %>% dplyr::select(idt,x,y), by=c("idt1" = "idt")) %>%
      dplyr::inner_join(locations %>% dplyr::select(idt,x,y), by=c("idt2" = "idt"), suffix = c(".1",".2")) %>%
      dplyr::select(-c(idt1, idt2))

    # distance between all agent pairs at all times
    distances <- distances %>% dplyr::mutate(
      distance = sim_dist(
        distances %>% dplyr::select(x.1, y.1, x.2, y.2) %>% data.matrix()
      )
    ) # %>% dplyr::group_by(time) %>% dplyr::summarise(distance = min(distance))

    # distanceSummary <- distances2 %>%
    #   summarise(mean = mean(distance),
    #             median = median(distance),
    #             min = min(distance),
    #             max = max(distance),
    #             `1th percentile` = quantile(distance, probs = c(.01)),
    #             `5th percentile` = quantile(distance, probs = c(.05)),
    #             `10th percentile` = quantile(distance, probs = c(.1))) %>%
    #   pivot_longer(cols = everything())
    #

    # Prepare a list to export metrics for each replication
    metric_list[[n]] <- list("demand_performance" = demand_performance,
                             "agent_performance" = agent_performance,
                             "response_time_performance" = response_time_performance,
                             "distances" = distances,
                             "agent_log" = agent_log
    )

    # The agent log is stored seperately
    # agent_log_list[[n]] <- agent_log

    # utilization_list[[n]] <- agent_log %>%
    #   select(id, status, time) %>%
    #   mutate(inUse = ifelse(status != "IDLE", 1, 0)) %>%
    #   group_by(time) %>%
    #   summarise(inUse = mean(inUse)) %>%
    #   mutate(inUse = cumsum(inUse))
  }
  return(list("metrics" = metric_list))
}
