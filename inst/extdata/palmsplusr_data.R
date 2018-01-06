
library("palmsplusr")
library("readr")

palms <- read_palms(system.file("extdata", "one_participant.csv", package = "palmsplusr"))
class_timetable <- read_csv(system.file("extdata", "class_timetable.csv", package = "palmsplusr"))
participant_basis <- read_csv(system.file("extdata", "participant_basis.csv", package = "palmsplusr"))
home <- read_sf(system.file("extdata/shapefiles", "home.shp", package = "palmsplusr"))
school <- read_sf(system.file("extdata/shapefiles", "school.shp", package = "palmsplusr"))

devtools::use_data(palms, class_timetable, participant_basis, home, school,
  overwrite = TRUE)
