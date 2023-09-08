### Function Section 
get_spread <- function(picks){
  spread <- 0
  for (i in 1:12){
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
  spread <- vector(length = 12)
  for (i in 1:12){
    spread[i] <-  data[picks[i],i]
  }
  return(spread)
}

get_each_spread <- function(picks){
  spread <- vector(length = length(picks))
  for (i in 1:12){
    if (is.na(data[picks[i],i])) return(-1000)
    else spread[i] <- data[picks[i],i]
  }
  return(spread)
}


