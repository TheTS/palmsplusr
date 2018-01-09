library(palmsplusr)
library(readr)

setwd("F:/data")

# Load PALMS dataset
palms <- read_palms("F:/data/csv/palms_output.csv")
#palms <- read_palms("F:/data/csv/one_participant.csv")

#palms2 = palms
palms <- palms[palms$identifier %in% c("BC0627", "BC0629", "BC0670"),]

# Load other csvs
participant_basis <- read_csv("F:/data/csv/participant_basis.csv")
class_timetable <- read_csv("F:/data/csv/class_timetable.csv")

# Load shapefiles
home <- read_sf("F:/data/shapefiles/home.shp")
home <- home %>% rename(identifier = Identifier)
home.buffer <- palms_buffer(point = home, distance = 100, crs = 2193)

school <- read_sf("F:/data/shapefiles/school.shp")
school <- school %>% rename(school_id = schoolID)

epoch <- palms_epoch(palms)

palms_remove_tables()

# palmsplus_fields
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

palms_add_field("in_school_time", "palms_in_time(., i, class_timetable, participant_basis, school_start, school_end)")
palms_add_field("in_recess1",     "palms_in_time(., i, class_timetable, participant_basis, recess1_start, recess1_end)")
palms_add_field("in_recess2",     "palms_in_time(., i, class_timetable, participant_basis, recess2_start, recess2_end)")
palms_add_field("at_home",        "palms_in_polygon(., filter(home.buffer, identifier == i), identifier)")
palms_add_field("at_school",      "palms_in_polygon(., filter(school, school_id == participant_basis %>% filter(identifier == i) %>% pull(school_id)))")

# palmsplus_domains
palms_add_domain("home",      "at_home")
palms_add_domain("school",    "(!at_home & at_school & in_school_time)")
palms_add_domain("transport", "!at_home & !(at_school & in_school_time) & (pedestrian | bicycle | vehicle)")
palms_add_domain("leisure",   "!at_home & !(at_school & in_school_time) & !pedestrian & !bicycle & !vehicle")

# trajectory_fields
palms_add_trajectory_field("mot",       "first(tripmot)",           FALSE, FALSE)
palms_add_trajectory_field("date",      "first(as.Date(datetime))", FALSE, FALSE)
palms_add_trajectory_field("start",     "datetime[triptype==1]",    FALSE, FALSE)
palms_add_trajectory_field("end",       "datetime[triptype==4]",    FALSE, FALSE)
palms_add_trajectory_field("duration",  "as.numeric(difftime(end, start, units = \"secs\") + epoch)")
palms_add_trajectory_field("nonwear",   "sum(activityintensity < 0) * epoch")
palms_add_trajectory_field("wear",      "sum(activityintensity >= 0) * epoch")
palms_add_trajectory_field("sedentary", "sum(activityintensity == 0) * epoch")
palms_add_trajectory_field("light",     "sum(activityintensity == 1) * epoch")
palms_add_trajectory_field("moderate",  "sum(activityintensity == 2) * epoch")
palms_add_trajectory_field("vigorous",  "sum(activityintensity == 3) * epoch")
palms_add_trajectory_field("mvpa",      "moderate + vigorous")
palms_add_trajectory_field("length",    "as.numeric(st_length(.))",  TRUE)
palms_add_trajectory_field("speed",     "(length / duration) * 3.6", TRUE, TRUE, "mean")

# trajectory_locations
palms_add_trajectory_location("home_school",   "at_home",   "at_school")
palms_add_trajectory_location("school_home",   "at_school", "at_home")
palms_add_trajectory_location("home_home",     "at_home",   "at_home")
palms_add_trajectory_location("school_school", "at_school", "at_school")

# Building
t <- proc.time()
palmsplus <- palms_build_palmsplus(palms)
cat("Palmsplus:", (proc.time() - t)[[1]]/60, "minutes")

t <- proc.time()
days <- palms_build_days(palmsplus)
cat("Days:", (proc.time() - t)[[1]]/60, "minutes")

t <- proc.time()
trajectories <- palms_build_trajectories(palmsplus)
cat("Trajectories:", (proc.time() - t)[[1]]/60, "minutes")

t <- proc.time()
multimodal <- palms_build_multimodal(trajectories, 200, 10)
cat("Multimodal:", (proc.time() - t)[[1]]/60, "minutes")

#Saving
write_csv(palmsplus, "palmsplus.csv")
write_csv(days, "days.csv")
write_csv(trajectories, "trajectories.csv")
write_csv(multimodal, "multimodal.csv")

st_write(palmsplus, "palmsplus.shp")
st_write(trajectories, "trajecories.shp")
st_write(multimodal, "multimodal.shp")

# Core i7 3770k 8GB RAM
# 191 participants:
# - Palmsplus: 5.879667 minutes>
# - Days: 0.09216667 minutes>
# - Trajectories: 2.163833 minutes>
# - Multimodal: 12.71067 minutes>



