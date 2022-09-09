library(tidyverse)

#loads 2021 data, eliminates playoff games, and assigns week numbers
latest_season <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo_latest.csv")
latest_season <- latest_season %>% filter(is.na(playoff))
latest_season$week <- c(rep(1,16),rep(2,16),rep(3,16),rep(4,16),rep(5,16),rep(6,14),
                        rep(7,13),rep(8,15),rep(9,14),rep(10,14),rep(11,15),rep(12,15),
                        rep(13,14),rep(14,14),rep(15,16),rep(16,16),rep(17,16),rep(18,16))

#Combines Team1 and Team2 data into one data frame
TEAM1 <- latest_season %>% select(week,team1,qbelo_prob1) %>% 
  rename(team = team1) %>% rename(weekly_prob =  qbelo_prob1)
TEAM2 <- latest_season %>% select(week,team2,qbelo_prob2) %>% 
  rename(team = team2) %>% rename(weekly_prob =  qbelo_prob2)
ALL_WEEKS <- rbind(TEAM1,TEAM2) %>% arrange(week)

#Fills in bye weeks
for (g in 1:18){
  for (h in 1:32){
    if(!any(ALL_WEEKS$week == g & ALL_WEEKS$team == ALL_WEEKS$team[h])){
      BYE_ADD <- data.frame(week = g, team = ALL_WEEKS$team[h], weekly_prob = 0)
      ALL_WEEKS <- rbind(ALL_WEEKS,BYE_ADD) %>%  arrange(week)}}}

#Creates a data frame with all 32 team names
team_list <- ALL_WEEKS[1:32,'team']

#Creates a blank data frame that will contain each calculated combination
OUTCOMESA <- data.frame(matrix(ncol = 19,nrow = 200))

#Creates a data frame for initial team selection, populates it, and calculates the season-long probability
for(j in 1:200){
  COMBO_START <- data.frame(team = sample(team_list$team,18,F), weekly_prob = 0)
  for (a in 1:18) {COMBO_START$weekly_prob[a] = ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == a & ALL_WEEKS$team == COMBO_START$team[a])]}
  
  season_prob_start <- 1
  for (b in 1:18) {season_prob_start <- season_prob_start * COMBO_START$weekly_prob[b]}
  
  #Creates a data frame from for team iteration
  COMBO_END <- COMBO_START
  
  swapcount <- 0; tradecount <- 0
  
  PROBABILITY_TRACK <- vector(length = 1)
  PROBABILITY_TRACK[1] <- season_prob_start; season_prob_end <- season_prob_start
    for (d in 1:6000) {
    #Picks Strategy
    strategy <- sample(1:2,1)
    #SWAPPING (Flipping two teams within the set)
    if(strategy == 1){
      swap1 <- sample(1:18,1)
      swap2 <- sample(1:18,1)
      
      #Checks if swapped teams improves probability
      if(ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap1 & ALL_WEEKS$team == COMBO_END$team[swap1])] *
          ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap2 & ALL_WEEKS$team == COMBO_END$team[swap2])] <
         ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap1 & ALL_WEEKS$team == COMBO_END$team[swap2])] *
          ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap2 & ALL_WEEKS$team == COMBO_END$team[swap1])]){
        #Makes swap if an improvement    
        holder_team <- COMBO_END$team[swap1]
        COMBO_END$team[swap1] <- COMBO_END$team[swap2]
        COMBO_END$team[swap2] <- holder_team
        COMBO_END$weekly_prob[swap1] <- ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap1 & ALL_WEEKS$team == COMBO_END$team[swap1])]
        COMBO_END$weekly_prob[swap2] <- ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == swap2 & ALL_WEEKS$team == COMBO_END$team[swap2])]
        swapcount <- swapcount + 1
        #calculates new probability
        season_prob_end <- 1
        for (e in 1:18) {season_prob_end <- season_prob_end * COMBO_END$weekly_prob[e]}}}
    
    #TRADING (flipping a team in the set with a team outside the set)
    if(strategy == 2){
      #Picks a random number inside the data set and a team that could be in or out. Checks to see if unique or not.
      trade1 <- sample(1:18,1)
      trade2 <- sample(1:32,1)
      UNIQUECHECK <- data.frame(team = ALL_WEEKS$team[trade2],weekly_prob = 0)
      COMBO_END <- rbind(COMBO_END,UNIQUECHECK)
      if(length(unique(COMBO_END$team)) == 19){COMBO_END <- COMBO_END[-19, ]}else{COMBO_END <- COMBO_END[-19, ];next}
      
      if(ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == trade1 & ALL_WEEKS$team == COMBO_END$team[trade1])] <
          ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == trade1 & ALL_WEEKS$team == ALL_WEEKS$team[trade2])]){
            COMBO_END$team[trade1] <- ALL_WEEKS$team[trade2]
            COMBO_END$weekly_prob[trade1] <- ALL_WEEKS$weekly_prob[which(ALL_WEEKS$week == trade1 & ALL_WEEKS$team == ALL_WEEKS$team[trade2])]
            tradecount <- tradecount + 1
            #calculates new probability
            season_prob_end <- 1
            for (f in 1:18) {season_prob_end <- season_prob_end * COMBO_END$weekly_prob[f]}}}
    
    PROBABILITY_TRACK[d+1] <- season_prob_end}
    
    OUTCOMESA[j,1] <- max(PROBABILITY_TRACK,na.rm=TRUE)
    for (i in 1:18){OUTCOMESA[j,i+1] <- COMBO_END$team[i]}
}
OUTCOMESA <- OUTCOMESA %>% arrange(-X1)
View(OUTCOMESA)
#plot(PROBABILITY_TRACK,xlab="Iteration",ylab="Season Probability",pch=16,col="blue")
