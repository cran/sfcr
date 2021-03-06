eqs <- sfcr_set(
  TX_s ~ TX_d,
  YD ~ W * N_s - TX_s,
  C_d ~ alpha1 * YD + alpha2 * H_h[-1],
  H_h ~ YD - C_d + H_h[-1],
  N_s ~ N_d,
  N_d ~ Y / W,
  C_s ~ C_d,
  G_s ~ G_d,
  Y ~ C_s + G_s,
  TX_d ~ theta * W * N_s,
  H_s ~ G_d - TX_d + H_s[-1]
)

ext <- sfcr_set(
  G_d ~ 20,
  W ~ 1,
  alpha1 ~ 0.6,
  alpha2 ~ 0.4,
  theta ~ 0.2
)

hidden <- c("H_s" = "H_h")

sim_model <- sfcr_baseline(
  equations = eqs,
  external = ext,
  periods = 30,
  initial = NULL,
  hidden = hidden,
  .hidden_tol = 1
)

## Tests
test_that("Mutating a `sfcr_tbl` object does not remove its attributes", {
  sim_model <- sim_model %>%
    dplyr::mutate(D = H_h/Y)
  expect_false(rlang::is_empty(attr(sim_model, "matrix")))
})


test_that("Error if shocks are not created with `sfcr_shock()`", {
  shock1 <- list(
    variables = sfcr_set(G_d ~ 30),
    start = 3,
    end = 4
  )

  expect_error(sfcr_scenario(
    baseline = sim_model,
    scenario = shock1,
    periods = 4
  ), "Please use `sfcr_shock\\(\\)` to create shocks.")
})

test_that("Shock not present in the external set of the model throws an error", {
  shock1 <- sfcr_shock(
    variables = sfcr_set(Gd ~ 30),
    start = 3,
    end = 4
  )

  expect_error(sfcr_scenario(
    baseline = sim_model,
    scenario = shock1,
    periods = 4
  ), "Shocked variable `Gd` is not included in the external variables of the model. Please check your shocks and try again.")
})


test_that("Shock not present in the external set of the model throws an error with two vars in the same shock", {
  shock1 <- sfcr_shock(
    variables = sfcr_set(Gd ~ 30, C_d ~ 25),
    start = 3,
    end = 4
  )

  expect_error(sfcr_scenario(
    baseline = sim_model,
    scenario = shock1,
    periods = 4
  ), "Shocked variables `Gd, C_d` are not present in the external variables of the model. Please check your shocks and try again.")
})

test_that("Shock not present in the external set of the model throws an error with two shocks", {
  shock1 <- sfcr_shock(
    variables = sfcr_set(Gd ~ 30),
    start = 3,
    end = 4
  )

  shock2 <- sfcr_shock(
    variables = sfcr_set(C_d ~ 25),
    start = 3,
    end = 4
  )

  expect_error(sfcr_scenario(
    baseline = sim_model,
    scenario = list(shock1, shock2),
    periods = 4
  ), "Shocked variables `Gd, C_d` are not present in the external variables of the model. Please check your shocks and try again.")
})

test_that("Test that mix of good and bad shock vars throw the correct error", {
  shock1 <- sfcr_shock(
    variables = sfcr_set(Gd ~ 30),
    start = 3,
    end = 4
  )

  shock2 <- sfcr_shock(
    variables = sfcr_set(G_d ~ 25),
    start = 3,
    end = 4
  )

  expect_error(sfcr_scenario(
    baseline = sim_model,
    scenario = list(shock1, shock2),
    periods = 4
  ), "Shocked variable `Gd` is not included in the external variables of the model. Please check your shocks and try again.")
})
