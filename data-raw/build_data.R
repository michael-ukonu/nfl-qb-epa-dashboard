# ============================================================
# Rebuilds pbp_data.rds from nflverse play-by-play data.
# Run from the project root:  source("data-raw/build_data.R")
# Only needed if you want to refresh or change the seasons;
# the app itself just reads pbp_data.rds.
# ============================================================
library(nflreadr)
library(dplyr)

seasons <- 2022:2023

pbp <- load_pbp(seasons) %>%
  filter(pass == 1) %>%
  select(
    season, pass, epa, passer_player_name, posteam,
    down, game_half, cpoe, complete_pass, air_yards,
    yardline_100, ydstogo
  )

saveRDS(pbp, "pbp_data.rds", compress = "xz")
message("Saved ", nrow(pbp), " pass plays to pbp_data.rds")
