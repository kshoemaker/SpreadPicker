
# spread_data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads.csv", row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)
# data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads2.csv",  row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)

data <- read.csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv")


team <- rownames(data)

### Function Section 
get_spread <- function(picks){
  spread <- 0
  for (i in 1:18){
    if (is.na(data[picks[i],i])) return(-1000)
    else spread <- spread + data[picks[i],i]
  }
  return(spread)
}

print_actual_spread <- function(picks){
  spread <- vector(length = 18)
  for (i in 1:18){
    spread[i] <-  spread_data[picks[i],i]
  }
  return(spread)
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

team_matrix <-team_nums <- vector(length = 18)
seed_trace <- vector(length = 400)

for (s in 1:400){
  set.seed(s*17)
  
  # Start with a viable pick
  ## Change line 59, 74, 80
  
  check = -1000
  while(check == -1000){
    random_start <- c(sample(c(1:32),18))
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_spread(random_start)
  iters <- 2500
  trace <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    
    swap_replace <- sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- sample(13:18,1)
      new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(13:18,2, replace = F)
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
    
    trace[i] <- current_spread
  }
  
  tail(trace, 20)
  plot(trace, type = "l")
  
  team_matrix <- rbind(team_matrix, team[current_pick])
  team_nums <- rbind(team_nums, current_pick)
  seed_trace[s] <- trace[iters]
}

 team_matrix <- team_matrix[-1, ]
  

table(seed_trace)
library(tidyverse)
team_matrix_tbl <- as_tibble(team_matrix)
team_matrix_tbl$score <- seed_trace
#team_matrix <- cbind()
team_matrix_tbl <- distinct(team_matrix_tbl)
table(team_matrix_tbl$score)
team_matrix_tbl %>% select(score, V13, V14, V15, V16, V17, V18) %>%  arrange(desc(score))
# top_picks <- team_matrix[order(seed_trace, decreasing = T)[1:50], ]
apply(team_matrix_tbl, 2, table)
