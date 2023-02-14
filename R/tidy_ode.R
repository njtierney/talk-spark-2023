# we are going to tidy up ODE output a few times, so wrap it into a function:
tidy_ode <- function(ode_soln) {
  ode_soln %>%
    pivot_longer(cols = -time) %>%
    mutate(parent_state = substr(name, 1, 1)) %>%
    # group_by(time, parent_state) %>%
    summarise(value = sum(value),
              .by = c(time, parent_state)) %>%
    ungroup() %>%
    rename(name = parent_state)
}
