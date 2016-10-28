#' Plot area plot of time use.
#'
#' Assumes \code{start} and \code{end} in \code{tbl} and the \code{x_max} value
#' are in simulation time (minutes from 4:00am) with \code{end} missing for the
#' last activity of every person.
#'
#' @param tbl tbl of data with at least the following columns: \code{per_id},
#' \code{event_type}, \code{start}, \code{end}.
#' @param x_max Maximum time on x-axis.
#' @param interval Number of minutes between sampling of activity proportions.
#' @param weight Boolean for whether or not weights should be used. By default,
#'   true. If true, a 'weight' column is required. If one is not present,
#'   weight = 1 is assigned to all persons.
#'
#' @return A plot.
#'
#' @export
plot_time_use <- function(tbl, x_max = 1440, interval = 15, weight = T) {

  times <- seq(0, x_max, by = interval)
  times <- times[times < x_max]

  activities_at_time <- list()
  for (i in times) {
    activities_at_time[[i+1]] <- tbl %>%
      filter((start <= i & i < end & start != end) |
               (start <= i & is.na(end))) %>%
      mutate(time = i) %>%
      select(time, per_id, event_type)
  }

  x <- bind_rows(activities_at_time) %>%
    group_by(time, event_type)

  # If weights do not exist in the data or if plotting without weights
  if (!("weight" %in% names(tbl)) || weight == F) {
    x <- mutate(x, weight = 1)
    tbl <- mutate(tbl, weight = 1)
  } else {
    x <- left_join(x, tbl %>% group_by(per_id, weight) %>% summarise(),
                   by = "per_id")
  }

  x <- x %>%
    summarise(count = sum(weight)) %>%
    tidyr::spread(event_type, count, fill = 0) %>%
    tidyr::gather(event_type, count, -time) %>%
    group_by(time) %>%
    mutate(pct = count / sum(count) * 100)

  # Find percent who make no trips
  num_home <- tbl %>%
    filter(start == 0, (is.na(end) || end == x_max)) %>%
    ungroup() %>%
    summarise(count = sum(weight)) %>%
    .$count %>%
    as.numeric()
  tot <- tbl %>%
    group_by(per_id) %>% filter(row_number() == 1) %>%  # get 1 row per person
    ungroup() %>%
    summarise(count = sum(weight)) %>%
    .$count %>%
    as.numeric()
  pct_home <- num_home / tot * 100

  # Plot
  ggplot(x, aes(x = as.numeric(time), y = pct, fill = event_type)) +
    geom_area() +
    geom_hline(aes(yintercept = pct_home), color = "white") +

    theme_tf() +
    theme(legend.title = element_blank()) +
    xlab("Military Time") + ylab("Percent of People") +
    scale_fill_manual(
      values = as.vector(c(tfplotr::solarized["base03"],
                           tfplotr::solarized["violet"],
                           tfplotr::solarized["cyan"],
                           tfplotr::solarized["base3"]))) +
    scale_x_continuous(
      limits = c(0, x_max),
      breaks = seq(0, x_max, by = 60*4),  # label every four hours
      labels = tfr::get_military_time)
}
