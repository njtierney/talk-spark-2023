
age_structured_sir <- function(time, state, parameters) {
  # Calculate the force of infection for each setting:
  # unstructured SIR beta is age_group_n / pop_n
  
  N_by_age <- map_dbl(
    .x = parameters$s_indexes,
    .f = function(i) {
      current_indexes_to_sum <- c(
        parameters$s_indexes[i],
        parameters$i_indexes[i],
        parameters$r_indexes[i]
      )
      sum(state[current_indexes_to_sum])
    }
  )
  
  # normalise by the age population
  N_infected_by_age <- state[parameters$i_indexes] / N_by_age
  
  # functional method for takign the product of two matrices
  product <- function(transmission, contact) {
    map2(transmission, contact, `*`)
  }
  
  age_normalise <- function(beta) {
    # matrix multiply by infected and normalise by age population
    map(beta, function(beta) {
      beta %*% N_infected_by_age
    })
  }
  
  lambdas <- tibble(
    setting = names(parameters$transmission_matrix),
    transmission_matrix = parameters$transmission_matrix,
    homogeneous_contact = parameters$homogeneous_contact[1:4]
  ) %>%
    mutate(
      beta = product(transmission_matrix, homogeneous_contact),
      lambda = age_normalise(beta)
    )
  
  # Combine them all into one term for ease of computation
  lambda_total <- Reduce("+", lambdas$lambda)
  
  # Don't forget to normalise your infection rate by the population!
  dSdt <- -lambda_total * state[parameters$s_indexes]
  
  dIdt <- lambda_total * state[parameters$s_indexes] -
    parameters$gamma * state[parameters$i_indexes]
  
  dRdt <- parameters$gamma * state[parameters$i_indexes]
  
  return(
    list(
      c(
        dSdt,
        dIdt,
        dRdt
      )
    )
  )
}
