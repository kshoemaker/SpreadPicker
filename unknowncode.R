
spread_data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads.csv", row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)

team <- data[,1]
data[data > 0] <- NA
data <- data[,2:19]


### Function Section 
get_spread <- function(picks){
  spread <- 0
  for (i in 1:18){
    if (is.na(data[picks[i],i])) return(1000)
    else spread <- spread + data[picks[i],i]
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
    if (is.na(data[picks[i],i])) return(1000)
    else spread[i] <- data[picks[i],i]
  }
  return(spread)
}




### Random Walk Section

set.seed(1023)

# Start with a viable pick

check = 1000
while(check == 1000){
  random_start <- sample(32,18)
  check <- get_spread(random_start)}

current_pick <- random_start
current_spread <- get_spread(random_start)
iters <- 10000
trace <- vector(length = iters)

for (i in 1:iters){
  new_pick <- current_pick
  
  swap_replace <- sample(2,1)
  
  if (swap_replace == 1){
    # replace  1 
    new_week <- sample(18,1)
    new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)

    new_pick[new_week] <- new_team
  } else {
    # swap
    swap_weeks <- sample(18,2, replace = F)
    new_pick[swap_weeks[1]] <- current_pick[swap_weeks[2]]
    new_pick[swap_weeks[2]] <- current_pick[swap_weeks[1]]
  }
  # compare 
  new_spread <- get_spread(new_pick)
  
  if (new_spread < current_spread){
    print(paste0("Change! current pick: ", cat(current_pick), ", new pick: ", cat(new_pick)))
    current_pick <- new_pick
    current_spread <- new_spread
  } 
  
  trace[i] <- current_spread
}

tail(trace, 20)
plot(trace, type = "l")

team[current_pick]



cbind( team[current_pick], print_spread(current_pick))

###############
### minimize all values 

set.seed(21208)

# uses function from above to start 
check = 1000
while(check == 1000){
  random_start <- sample(32,18)
  check <- get_spread(random_start)}

current_pick <- random_start
current_spread <- get_each_spread(current_pick)

iters <- 1000
trace1 <- trace2 <- vector(length = iters)

for (i in 1:iters){
  new_pick <- current_pick
  to_change <- which.max(current_spread)
  
  swap_replace <- 1 # sample(2,1)
  
  if (swap_replace == 1){
    # replace  1 
    new_week <- to_change
    new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)
    
    new_pick[new_week] <- new_team
  } else {
    # swap
    swap_weeks <- sample(18,2, replace = F)
    new_pick[swap_weeks[1]] <- current_pick[swap_weeks[2]]
    new_pick[swap_weeks[2]] <- current_pick[swap_weeks[1]]
  }
  # compare 
  new_spread <- get_each_spread(new_pick)
  
  if (max(new_spread) < max(current_spread)){
    current_pick <- new_pick
    current_spread <- new_spread
  } 
  print(cbind(current_spread, new_spread))
  trace1[i] <- sum(current_spread)
  trace2[i] <- max(current_spread)
}

#tail(trace, 20)
#plot(trace2, type = "l")

# team[current_pick]



# cbind( team[current_pick], print_spread(current_pick))



######
  
  # uses function from above!! 
  check = 1000
  while(check == 1000){
    random_start <- sample(32,18)
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_each_spread(current_pick)
  
  iters <- 10000
  trace1 <- trace2 <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    to_change <- which.max(current_spread)
    
    swap_replace <- sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- to_change
      new_team <- sample(teams[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(c(1:18)[-to_change],1, replace = F)
      new_pick[to_change] <- current_pick[swap_weeks]
      new_pick[swap_weeks] <- current_pick[to_change]
    }
    # compare 
    new_spread <- get_each_spread(new_pick)
    
    if (sum(new_spread) < sum(current_spread)){
      current_pick <- new_pick
      current_spread <- new_spread
    } 
    print(cbind(current_spread, new_spread))
    trace1[i] <- sum(current_spread)
    trace2[i] <- max(current_spread)
  }
  
##### 
data <- read.csv("/Users/Kate/Documents/FootballThings/Spreads2.csv",  row.names = 1, na.strings = "-",header = F, stringsAsFactors = F)
  
team <- rownames(data)

### Function Section 
  get_spread <- function(picks){
    spread <- 0
    for (i in 1:18){
      if (is.na(data[picks[i],i])) return(1000)
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
      if (is.na(data[picks[i],i])) return(1000)
      else spread[i] <- data[picks[i],i]
    }
    return(spread)
  }
  
  
  
  
  ### Random Walk Section
  
  set.seed(10589)
  
  # Start with a viable pick
  
  check = 1000
  while(check == 1000){
    random_start <- sample(32,18)
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_spread(random_start)
  iters <- 10000
  trace <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    
    swap_replace <- sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- sample(18,1)
      new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(18,2, replace = F)
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
  
  team[current_pick]
  
  
  
seed_10589 <- data.frame(Team =  team[current_pick], Percent = print_spread(current_pick), Spread =  print_actual_spread(current_pick))
  
  ###############
  ### minimize all values 
  
  set.seed(21208)
  
  # uses function from above to start 
  check = 1000
  while(check == 1000){
    random_start <- sample(32,18)
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_each_spread(current_pick)
  
  iters <- 1000
  trace1 <- trace2 <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    to_change <- which.max(current_spread)
    
    swap_replace <- 1 # sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- to_change
      new_team <- sample(c(1:32)[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(18,2, replace = F)
      new_pick[swap_weeks[1]] <- current_pick[swap_weeks[2]]
      new_pick[swap_weeks[2]] <- current_pick[swap_weeks[1]]
    }
    # compare 
    new_spread <- get_each_spread(new_pick)
    
    if (max(new_spread) < max(current_spread)){
      current_pick <- new_pick
      current_spread <- new_spread
    } 
    print(cbind(current_spread, new_spread))
    trace1[i] <- sum(current_spread)
    trace2[i] <- max(current_spread)
  }
  
  #tail(trace, 20)
  #plot(trace2, type = "l")
  
  # team[current_pick]
  
  
  
  # cbind( team[current_pick], print_spread(current_pick))
  
  
  
  ######
  
  # uses function from above!! 
  check = 1000
  while(check == 1000){
    random_start <- sample(32,18)
    check <- get_spread(random_start)}
  
  current_pick <- random_start
  current_spread <- get_each_spread(current_pick)
  
  iters <- 10000
  trace1 <- trace2 <- vector(length = iters)
  
  for (i in 1:iters){
    new_pick <- current_pick
    to_change <- which.max(current_spread)
    
    swap_replace <- sample(2,1)
    
    if (swap_replace == 1){
      # replace  1 
      new_week <- to_change
      new_team <- sample(teams[!(1:32 %in% current_pick)], 1)
      
      new_pick[new_week] <- new_team
    } else {
      # swap
      swap_weeks <- sample(c(1:18)[-to_change],1, replace = F)
      new_pick[to_change] <- current_pick[swap_weeks]
      new_pick[swap_weeks] <- current_pick[to_change]
    }
    # compare 
    new_spread <- get_each_spread(new_pick)
    
    if (sum(new_spread) < sum(current_spread)){
      current_pick <- new_pick
      current_spread <- new_spread
    } 
    print(cbind(current_spread, new_spread))
    trace1[i] <- sum(current_spread)
    trace2[i] <- max(current_spread)
  }
