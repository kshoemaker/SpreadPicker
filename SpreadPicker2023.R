
library(tidyverse)

data <- read.csv("Data2023/Week1.csv", row.names = 1)
teams <- row.names(data)

current_week <- 1
current_teams <- c()
current_picks <- c()
available <- c(1:32)
#available <- c(1:32)[-current_picks]

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

reps = 100
team_matrix <- team_nums <- vector(length = 18)
seed_trace <- vector(length = reps)
seeds <- sample(1:100000, size = reps)

for (s in 1:reps){
  # set the seed
  set.seed(seeds[s])
  

  check = -1000
  while(check == -1000){
    random_start <- c(current_picks, sample(available, 18 - (current_week-1)))
    check <- get_spread(random_start)
    }
  
  
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
      swap_weeks <- sample(current_week:18, 2, replace = F)
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
  
  plot(trace, type = "l")
  
  team_matrix <- rbind(team_matrix, teams[current_pick])
  team_nums <- rbind(team_nums, current_pick)
  seed_trace[s] <- trace[iters]
  print(paste0("Prob: ", round(seed_trace[s],3), "; Seed ", s, ": ", paste(teams[current_pick], collapse = ", ") ))
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
write_csv(ranked, paste0("Data2023/resultsWeek",current_week, ".csv"), append = T)
