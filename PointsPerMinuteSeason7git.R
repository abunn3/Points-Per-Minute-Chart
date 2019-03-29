#Create a vector that gives the score at the end of each minute
#---------NOTE: To skip the data collection part, just read in this .csv file in line 100 (3/28/2019 data)
library(nbastatR)
library(zoo)
library(ggplot2)
library(dplyr)
library(reshape2)
library(svMisc)
library(RColorBrewer)

#Data collection ----
delay <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 
}

allTeams <- as.data.frame(rep(0,49))

#list of all games
gl<-game_logs(seasons = 2019, result_types = "Team")
teams <- unique(gl$slugTeam)
games <- data.frame(rep(0,49))

for (j in c(1:30)) {
  games2 <- data.frame(rep(0,49))
  
  print(teams[j]) #for loop progress
  
  #Selected team
  gl2 <- gl[c(gl$slugTeam == teams[j]),]
  location <- as.matrix(gl2[13])

  gameIDs <- gl2$idGame
  k <- 1 #counter for H/A check
  for (i in c(1:length(gameIDs))){
    p<-play_by_play(game_ids = gameIDs[i], nest_data = F, return_message = F)
    delay(.5)
    
    #grab the game time and score columns of a game play by play (4 columns total)
    TScols <- p[c(12,16:18)]
    TScols$marginScore <- TScols$scoreAway - TScols$scoreHome
    
    #Pre-allocate data frame to populate
    minuteGame <- c(0:49)
    TS_red <- as.data.frame(minuteGame)
    
    col_headings <- c("minuteGame", "scoreHome", "scoreAway", "marginScore")
    
    time_round <- ceiling(TScols$minuteGame) 
    TScols$minuteGame <- time_round
    TScols$scoreHome[1] <- 0
    TScols$scoreAway[1] <- 0
    
    dr1 <- TScols[complete.cases(TScols$marginScore),] #removes rows where there is no score change
    dr2 <- dr1[order(dr1$minuteGame, dr1$scoreAway, dr1$scoreHome, decreasing=TRUE),] #sort from end of game, then home score, then away score
    dr3 <- dr2[!duplicated(dr2$minuteGame),] #remove duplicates based on minuteGame
    dr4 <- dr3[order(dr3$minuteGame, decreasing = FALSE),] #reorder from 0-48 for game time
    dr5 <- merge(TS_red,dr4, all.x = TRUE) #merge with list of 0-48 to account for minutes where no points are scored
    dr5[1,2:4] <- 0
    dr6 <-na.locf(dr5, fromLast = TRUE) #duplicate from last for na items

    scoreHomeDelta <- c(rep(0,49))
    scoreAwayDelta <- scoreHomeDelta
    for (i in c(49:1)){
      if (i == 1) {
        scoreHomeDelta[i] <- dr6$scoreHome[i]-0
        scoreAwayDelta[i] <- dr6$scoreAway[i]-0
      } else {
        scoreHomeDelta[i] <- dr6$scoreHome[i] - dr6$scoreHome[i-1]
        scoreAwayDelta[i] <- dr6$scoreAway[i] - dr6$scoreAway[i-1]
      }
    }
    dr7 <- cbind(dr6, scoreHomeDelta, scoreAwayDelta)
    dr7$minuteMargin <- dr7$scoreAwayDelta - dr7$scoreHomeDelta
    
    # Home/Away check. required due to Weird parsing by stats.nba.com
    if (location[k] == "A") {
      dr7[7] <- dr7[7] *-1
      # print("score flipped")
    }
    
    games2 <- cbind(games2, dr7[7])
    k<-k+1

  }
  seasonMargin <- rowSums(games2[2:ncol(games2)])
  allTeams[j] <- seasonMargin
}

#Data processing ----
allTeams <- cbind(dr7[1],allTeams)
names(allTeams) <- c("minuteGame", teams)
allTeams <- allTeams[2:49,] #remove 0 row
allTeamsLong <- melt(allTeams, id.vars = c("minuteGame"))
names(allTeamsLong) <- c("Minute", "Team", "Margin")

#---------NOTE: To skip the data collection part, just read in this .csv file
# allTeamsLong<-read.csv("AllTeamsAllminutes.csv")
# allTeamsLong <- allTeamsLong[,2:4]

#Group by 3 minute chunks
sumT <- allTeamsLong
sumT$Team <- as.character(sumT$Team)
a <- seq(1,nrow(allTeamsLong)-2, 3)
j=1
wide3 <- data.frame(cbind(rep(0,480), rep(0,480), rep(0,480)))
for (i in a){
  wide3[j,1] <- sumT[i,1]
  wide3[j,2] <- sumT[i,2]
  wide3[j,3] <- sum(sumT[i,3], sumT[c(i+1), 3], sumT[c(i+2),3])
  j = j+1
}
names(wide3) <- c("Minute", "Team", "Margin")
allTeamsLong3 <- wide3

allTeamsWide<- dcast(allTeamsLong3, Minute ~Team, value.var ="Margin")
allTeamsWideT <- data.frame(t(allTeamsWide[,2:31]))

#Clustering -----
#kmeans calculation 
cl <- kmeans(allTeamsWideT, 6)

########NOTE: I cheated here and took the kgroups list to Excel so that I could order the groups and teams within the groups how I wanted. I then bring the list back into R for plotting. CSV provided in the folder
kgroups <- data.frame(cl$cluster)
setDT(kgroups, keep.rownames = TRUE)[]
write.csv(kgroups, "kmeansOrder_cluster6.csv")
#Cheating with Excel
korder<- read.csv("kmeansOrder_cluster6.csv")
korder$Team <- as.character(korder$Team)
facet_order <- korder$Team
allTeamsLong3$Team <- factor(allTeamsLong3$Team, levels = facet_order)

#Plotting----
# geom_tile version
coul = colorRampPalette(brewer.pal(8, "RdYlBu"))(299)


q <- ggplot(allTeamsLong3, aes(Minute, Team)) +
  geom_tile(aes(fill = Margin, colour = NULL)) +
  scale_fill_distiller(palette = "RdYlBu", guide = "colorbar") +
  theme_minimal()
q <-
  q + theme(panel.background = element_rect(fill = "white", colour = "white")) +
  theme(
    axis.title.x = element_text(
      color = "#04295e",
      vjust = -0.35,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "#04295e" ,
      vjust = 0.35,
      face = "bold"
    ),
    axis.text.y = element_text(colour = "black")
  )

q <- q + labs(title = "Season total score margin, minute by minute",
              subtitle = "An attempt to see where games are being won and lost during the 2018-19 NBA Season") +
  theme(plot.title = element_text(
    size = 20,
    face = "bold",
    margin = margin(10, 0, 10, 0),
    color = "#04295e"
  )) +
  theme(plot.subtitle = element_text(
    size = 12,
    margin = margin(0, 0, 10, 0),
    color = "#04295e"
  ))
q

