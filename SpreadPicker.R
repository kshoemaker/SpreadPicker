
# spread_data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads.csv", row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)
# data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads2.csv",  row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)
library(tidyverse)

# code from CLD to get the data setup right. 
latest_season <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv")
latest_season <- latest_season %>% filter(is.na(playoff))
latest_season$week <- c(rep(1,16),rep(2,16),rep(3,16),rep(4,16),rep(5,16),rep(6,14),
                        rep(7,14),rep(8,15),rep(9,13),rep(10,14),rep(11,14),rep(12,16),
                        rep(13,15),rep(14,13),rep(15,16),rep(16,16),rep(17,16),rep(18,16))

current_week <- 2
current_teams <- c("WSH")
current_picks <- c(9)
available <- c(1:8, 10:32)

# Combines Team1 and Team2 data into one data frame
TEAM1 <- latest_season %>% select(week,team1,qbelo_prob1) %>% 
  rename(team = team1) %>% rename(weekly_prob =  qbelo_prob1)
TEAM2 <- latest_season %>% select(week,team2,qbelo_prob2) %>% 
  rename(team = team2) %>% rename(weekly_prob =  qbelo_prob2)
ALL_WEEKS <- rbind(TEAM1,TEAM2) %>% arrange(week)
data <- pivot_wider(ALL_WEEKS, names_from = week, values_from = weekly_prob)
team <- data$team
data <- data[,-1]
#write.csv(data, paste0("week",current_week,"data.csv"))

### Function Section 
get_spread <- function(picks){
  spread <- 0
  for (i in 1:18){
    if (is.na(data[picks[i],i])) return(-1000)
    else spread <- spread + data[picks[i],i]
  }
  return(spread)
}

# print_actual_spread <- function(picks){
{#   spread <- vector(length = 18)
#   for (i in 1:18){
#     spread[i] <-  spread_data[picks[i],i]
#   }
#   return(spread)
# }
  }

print_spread <- function(picks){
  spread <- vector(length = 18)
  for (i in 1:18){
    spread[i] <-  data[picks[i],i]
  }
  return(spread)
}

get_each_spread <- function(picks){
  spread <- vector(length = length(picks))
  for (i in 1:18){
    if (is.na(data[picks[i],i])) return(-1000)
    else spread[i] <- data[picks[i],i]
  }
  return(spread)
}




### Random Walk Section

reps = 50
team_matrix <- team_nums <- vector(length = 18)
seed_trace <- vector(length = reps)
seeds <- sample(1:100000, size = reps)

for (s in 1:reps){
  set.seed(seeds[s])
  
  # Start with a viable pick
  ## Change line 59, 74, 80
  
  check = -1000
  while(check == -1000){
    random_start <- c(current_picks, sample(available, 18 - (current_week-1)))
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_spread(random_start)
  iters <- 4000
  trace <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    
    swap_replace <- sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- sample(current_week:18,1)
      new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(current_week:18,2, replace = F)
      new_pick[swap_weeks[1]] <- current_pick[swap_weeks[2]]
      new_pick[swap_weeks[2]] <- current_pick[swap_weeks[1]]
    }
    # compare 
    new_spread <- get_spread(new_pick)
    
    if (new_spread > current_spread){
      # print(paste0("Change! current pick: ", cat(current_pick), ", new pick: ", cat(new_pick)))
      current_pick <- new_pick
      current_spread <- new_spread
    } 
    
    trace[i] <- current_spread[1,1]
  }
  
  tail(trace, 20)
  plot(trace, type = "l")
  
  team_matrix <- rbind(team_matrix, team[current_pick])
  team_nums <- rbind(team_nums, current_pick)
  seed_trace[s] <- trace[iters]
  print(paste0("Prob: ", round(seed_trace[s],3), "; Seed ", s, ": ", paste(team[current_pick], collapse = ", ") ))
}

 team_matrix <- team_matrix[-1, ]
  

table(seed_trace)
team_matrix_tbl <- as_tibble(team_matrix)
team_matrix_tbl$score <- seed_trace
#team_matrix <- cbind()
team_matrix_tbl <- distinct(team_matrix_tbl)
table(team_matrix_tbl$score)
ranked <- team_matrix_tbl %>% select(score, everything()) %>%  arrange(desc(score))
ranked
# top_picks <- team_matrix[order(seed_trace, decreasing = T)[1:50], ]
apply(team_matrix_tbl, 2, table)
write_csv(ranked, paste0("resultsWeek",current_week, "_WSH_w1.csv"), append = T)
