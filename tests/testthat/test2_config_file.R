library(palmsplusr)

test_that("Testing config file workflow", {

  palms_remove_tables()

  palms <- system.file("extdata", "one_participant.csv", package = "palmsplusr")
  config <- system.file("extdata", "config.csv", package = "palmsplusr")


  context("Palmsplus_shell (missing config file)")

  expect_error(palmsplus_shell(palms_data = palms, verbose = FALSE), "config_file")


  context("Palmsplus_shell (complete)")

  result <- palmsplus_shell(palms_data = palms,
                            config_file = config,
                            return_list = TRUE,
                            verbose = FALSE,
                            save_output = FALSE)

  expect_type(result, "list")
  expect_length(result, n = 4)


  context("Palmsplus_shell (no multimodal)")

  result <- palmsplus_shell(palms_data = palms,
                            config_file = config,
                            multimodal = FALSE,
                            return_list = TRUE,
                            verbose = FALSE,
                            save_output = FALSE)

  expect_length(result, n = 3)


  context("Palmsplus_shell (no trajectories)")

  result <- palmsplus_shell(palms_data = palms,
                            config_file = config,
                            trajectories = FALSE,
                            return_list = TRUE,
                            verbose = FALSE,
                            save_output = FALSE)

  expect_length(result, n = 2)


  context("Palmsplus_shell (no days)")

  result <- palmsplus_shell(palms_data = palms,
                            config_file = config,
                            days = FALSE,
                            return_list = TRUE,
                            verbose = FALSE,
                            save_output = FALSE)

  expect_length(result, n = 3)


  context("Palmsplus_shell (no days or trajectories)")

  result <- palmsplus_shell(palms_data = palms,
                            config_file = config,
                            trajectories = FALSE,
                            days = FALSE,
                            return_list = TRUE,
                            verbose = FALSE,
                            save_output = FALSE)

  expect_length(result, n = 1)


  context("Palmsplus_shell (wrong data input format)")

  expect_error(palmsplus_shell(palms_data = palms,
                               habitus = TRUE,
                               config_file = config,
                               verbose = FALSE,
                               save_output = FALSE), "missing columns")

})

