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
#' @param baseline The time to start the plot; defaults to 4 AM.
#'
#' @return A plot.
#'
#' @export
plot_time_use <- function(tbl, x_max = 1440, interval = 15, weight = T,
                          baseline = 4) {

  times <- seq(0, x_max, by = interval)
  times <- times[times < x_max]

  activities_at_time <- list()
  for (i in times) {
    activities_at_time[[i+1]] <- tbl %>%
      dplyr::filter((start <= i & i < end & start != end) |
               (start <= i & is.na(end))) %>%
      dplyr::mutate(time = i) %>%
      dplyr::select(time, per_id, event_type)
  }

  x <- dplyr::bind_rows(activities_at_time) %>%
    dplyr::group_by(time, event_type)

  # If weights do not exist in the data or if plotting without weights
  if (!("weight" %in% names(tbl)) || weight == F) {
    x <- dplyr::mutate(x, weight = 1)
    tbl <- dplyr::mutate(tbl, weight = 1)
  } else {
    x <- dplyr::left_join(x, tbl %>% dplyr::group_by(per_id, weight) %>% summarise(),
                   by = "per_id")
  }

  x <- x %>%
    dplyr::summarise(count = sum(weight)) %>%
    tidyr::spread(event_type, count, fill = 0) %>%
    tidyr::gather(event_type, count, -time) %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(pct = count / sum(count) * 100)

  # Find percent who make no trips
  num_home <- tbl %>%
    dplyr::filter(start == 0, (is.na(end) || end == x_max)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(count = sum(weight)) %>%
    .$count %>%
    as.numeric()
  tot <- tbl %>%
    dplyr::group_by(per_id) %>% filter(row_number() == 1) %>%  # get 1 row per person
    dplyr::ungroup() %>%
    dplyr::summarise(count = sum(weight)) %>%
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
      labels = tfr::get_military_time(seq(0, x_max, by = 60*4),
                                      baseline = baseline))
}
