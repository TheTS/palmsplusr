library(palmsplusr)

palms <- read_palms("D:/data/csv/one_participant.csv")

palms_load_defaults(palms_epoch(palms))

# Building palmsplus
palmsplus <- palms_build_palmsplus(palms)

# Building days
days <- palms_build_days(palmsplus)

# Building trajectories
trajectories <- palms_build_trajectories(palmsplus)

# Building multimodal trajectories
multimodal <- palms_build_multimodal(trajectories, 200, 10)
