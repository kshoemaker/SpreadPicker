devtools::install_github("adror1/nwslR")

library(nwslR)
library(tidyverse)
library(magrittr)
library(dplyr)

head(nwslR::fieldplayer_overall_season_stats)
overall <- nwslR::fieldplayer_overall_season_stats
unique(overall$season)

get_adv_team_stats("utah-royals-vs-washington-spirit-2019-04-20")
get_adv_player_stats("utah-royals-vs-washington-spirit-2019-04-20")
get_events("utah-royals-vs-washington-spirit-2019-04-20")

game_id <- "utah-royals-vs-washington-spirit-2019-04-20"
paste0("http://api.nwslsoccer.com/v2/games/", game_id, "/stats")



library(rvest)
test <- read_html("https://www.nwslsoccer.com/stats/players")
test %>% html_nodes(".view-stats-players") %>% html_text()
