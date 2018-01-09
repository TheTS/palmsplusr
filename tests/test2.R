library(palmsplusr)

palms <- read_palms("F:/data/csv/one_participant.csv")

palms_load_defaults()

# Building palmsplus
palmsplus <- palms_build_palmsplus(palms)

# Building days
days <- palms_build_days(palmsplus)

# Building trajectories
trajectories <- palms_build_trajectories(palmsplus)

# Building multimodal trajectories
multimodal <- palms_build_multimodal(trajectories, 200, 10)
