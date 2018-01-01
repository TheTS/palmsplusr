library(palmsplusr)
library(readr)

context("Testing all functions")

test_that("Testing normal workflow", {
  palms <- read_palms(system.file("extdata", "one_participant.csv", package = "palmsplusr"))
  participant_basis <- read_csv(system.file("extdata", "participant_basis.csv", package = "palmsplusr"))
  class_timetable <- read_csv(system.file("extdata", "class_timetable.csv", package = "palmsplusr"))

  home <- read_sf(system.file("extdata/shapefiles/", "home.shp", package = "palmsplusr"))
  home.buffer <- palms_buffer(point = home, distance = 100, crs = 2193)
  school <- read_sf(system.file("extdata/shapefiles/", "school.shp", package = "palmsplusr"))

  # Check files imported correctly
  expect_is(palms, class = c("sf", "tbl_df", "tbl", "data.frame"))
  expect_is(school, class = c("sf", "tbl_df", "tbl", "data.frame"))
  expect_is(home.buffer, class = c("sf", "tbl_df", "tbl", "data.frame"))

  palms_add_field("duration",   "1", TRUE)

  palms_remove_tables()

  palms_add_field("duration",   "1", TRUE)
  palms_add_field("weekday",    "dow < 6")
  palms_add_field("weekend",    "dow > 5")
  palms_add_field("indoors",    "iov == 3")
  palms_add_field("outdoors",   "iov == 1")
  palms_add_field("in_vehicle", "iov == 2")
  palms_add_field("inserted",   "fixtypecode == 6")
  palms_add_field("pedestrian", "tripmot == 1")
  palms_add_field("bicycle",    "tripmot == 2")
  palms_add_field("vehicle",    "tripmot == 3")
  palms_add_field("nonwear",    "activityintensity < 0",  TRUE)
  palms_add_field("wear",       "activityintensity >= 0", TRUE)
  palms_add_field("sedentary",  "activityintensity == 0", TRUE)
  palms_add_field("light",      "activityintensity == 1", TRUE)
  palms_add_field("moderate",   "activityintensity == 2", TRUE)
  palms_add_field("vigorous",   "activityintensity == 3", TRUE)
  palms_add_field("mvpa",       "activityintensity > 1",  TRUE)

  palms_add_domain("d_all",       "1")

  palms_add_trajectory_field("mot",       "first(tripmot)")
  palms_add_trajectory_field("date",      "first(as.Date(datetime))")
  palms_add_trajectory_field("start",     "datetime[triptype==1]")
  palms_add_trajectory_field("end",       "datetime[triptype==4]")
  palms_add_trajectory_field("duration",  "as.numeric(difftime(end, start, units = \"secs\") + epoch)")
  palms_add_trajectory_field("nonwear",   "sum(activityintensity < 0) * epoch")
  palms_add_trajectory_field("wear",      "sum(activityintensity >= 0) * epoch")
  palms_add_trajectory_field("sedentary", "sum(activityintensity == 0) * epoch")
  palms_add_trajectory_field("light",     "sum(activityintensity == 1) * epoch")
  palms_add_trajectory_field("moderate",  "sum(activityintensity == 2) * epoch")
  palms_add_trajectory_field("vigorous",  "sum(activityintensity == 3) * epoch")
  palms_add_trajectory_field("mvpa",      "moderate + vigorous")
  palms_add_trajectory_field("length",    "as.numeric(st_length(.))",  TRUE)
  palms_add_trajectory_field("speed",     "(length / duration) * 3.6", TRUE)

  expect_error(palms_add_trajectory_field("speed", "."))

  # Check field/domain tables built correctly
  expect_length(unlist(palmsplus_domains), 2)
  expect_length(unlist(palmsplus_fields), 51)
  expect_length(unlist(trajectory_fields), 42)

  # Check palmsplus
  palmsplus <- palms_calc_palmsplus(palms)
  expect_equal(sum(palmsplus$mvpa), 791)

  # Check days
  days <- palms_calc_days(palmsplus)
  expect_equal(round(mean(days$d_all_mvpa, na.rm = TRUE), 2), 21.97)

  # Check trajectories
  trajectories <- palms_calc_trajectories(palmsplus)
  expect_equal(nrow(trajectories), 38)
})

