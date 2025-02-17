
# what are the sources of variation?
# When the event (ping) is received
# the time between pings
# when they enter the boundary
# when they leave the boundary (subsequently the duration of the stay)

# functions ---------------------------------------------------------------

summarise_sim <- function(df) {

  df |>
    summarise(
      actual = sum(actual),
      count_2 = sum(count_2),
      count_3 = sum(count_3)
    ) |>
    mutate(
      p_2 = count_2/actual,
      p_3 = count_3/actual
    )

}

summarise_to_matrix <- function(df) {

  mat <- matrix(NA, nrow = 2, ncol = 2, dimnames = list("Actual" = c("No", "Yes"), "Estimated" = c("No", "Yes")))

  df_grid <- expand_grid(
    actual = c(TRUE, FALSE),
    count_2 = c(TRUE, FALSE)
  )

  mat_2 <- df_grid |>
    left_join(
      df |>
        count(actual, count_2),
      join_by(actual, count_2)
    ) |>
    mutate(n = replace_na(n, 0)) |>
    pull(n) |>
    matrix(nrow = 2, ncol = 2, dimnames = list("Actual" = c("Yes", "No"), "Estimated" = c("Yes", "No")), byrow = TRUE)

  mat_3 <- df_grid |>
    left_join(
      df |>
        count(actual, count_3),
      join_by(actual, count_2 == count_3)
    ) |>
    mutate(n = replace_na(n, 0)) |>
    pull(n) |>
    matrix(nrow = 2, ncol = 2, dimnames = list("Actual" = c("Yes", "No"), "Estimated" = c("Yes", "No")), byrow = TRUE)

  list(
    `Two consecutive intervals` = mat_2,
    `Three consecutive intervals` = mat_3
  )

}

visit_sim <- function(N, lambda, trip_duration, duration_mu, entered_boundary_at_mu, fixed = FALSE) {


  if(!fixed) {
    .duration <- rpois(N, duration_mu)
    .entered_boundary_at <- rpois(N, entered_boundary_at_mu)
  } else {
    .duration <- rep(duration_mu, N)
    .entered_boundary_at <- rep(entered_boundary_at_mu, N)
  }

  # calculate the time they left the boundary
  .left_boundary_at <- .entered_boundary_at + .duration

  map_dfr(1:N, ~{

    cat(.x, "\r")

    # draw pings
    # drawing 20 just to make sure there is enough
    pings <- rpois(20, lambda)

    # take the cumulative sum to find the timestamps
    time_stamp <- cumsum(pings)
    time_stamp <- time_stamp[time_stamp < 45]

    # take the time they entered and left the boundary and the duration from the vector
    entered_boundary_at <- .entered_boundary_at[.x]
    left_boundary_at <- .left_boundary_at[.x]
    duration <- .duration[.x]

    # calculate the interval either 0, 1, 2
    interval <- floor(time_stamp/15)

    # set the logical if they are in the boundary or not
    in_boundary <- time_stamp >= entered_boundary_at & time_stamp <= left_boundary_at

    # record if we think the device was in the boundary by taking the first ping
    recorded_in_boundary <- rep(FALSE, 3)
    for(k in 1:3) {
      j <- which(interval == k-1)
      if(length(j) > 0) {
        recorded_in_boundary[k] <- in_boundary[which(interval == k-1)[1]]
      } else {
        if(k > 1) {
          recorded_in_boundary[k] <- recorded_in_boundary[k-1]
        }
      }
    }

    # did they stay longer than 15 mins?
    visit_actual <- duration >= trip_duration

    # would we count as a visit after 2?
    visit_count_2 <- all(recorded_in_boundary[1:2]) | all(recorded_in_boundary[2:3])

    # would we count as a visit after 3?
    visit_count_3 <- all(recorded_in_boundary)

    # if(!visit_count_2 & visit_actual) browser()

    # data frame
    tibble(
      id = .x,
      entered_boundary_at = entered_boundary_at,
      left_boundary_at = left_boundary_at,
      duration = duration,
      actual = visit_actual,
      count_2 = visit_count_2,
      count_3 = visit_count_3
    )

  })

}

# run simulation ----------------------------------------------------------


visit_sim(1000, 9, 15, 14, 18)

x <- visit_sim(4000, 6, 15, 15, 10, fixed = FALSE)

x |>
  summarise_sim()

x |>
  summarise_to_matrix()

# build grid --------------------------------------------------------------

df_grid <- expand_grid(
  lambda = c(3, 6, 9),
  duration_mu = seq(15, 30, 5),
  entered_boundary_at_mu = seq(0, 30, 5)
)

df_sim_results <- map_dfr(1:nrow(df_grid), \(k) {

  cat(glue("k: {k}\n\n"))

  out <- visit_sim(4000, df_grid$lambda[k], 15, df_grid$duration_mu[k], df_grid$entered_boundary_at_mu[k]) |>
    summarise_sim()

  bind_cols(
    df_grid[k,],
    out
  )

})

df_sim_results |>
  select(-actual, -count_2, -count_3) |>
  pivot_longer(c(p_2, p_3), names_to = "method", values_to = "p") |>
  ggplot() +
  geom_tile(aes(entered_boundary_at_mu, duration_mu, fill = p), width = 4, height = 4) +
  geom_text(aes(entered_boundary_at_mu, duration_mu, label = paste0(round(p*100), "%")), colour = "white", size = 12) +
  facet_grid(method ~ lambda) +
  scale_fill_gradientn(colours = sunset) +
  coord_equal()
