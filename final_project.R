library(rvest)
library(plyr)

nbaHTML <- read_html("https://jeffbeauplan.neocities.org/")
nbaTableNodes <- html_nodes(nbaHTML, ".home__leaders-body table")
categoryNodes <- html_nodes(nbaHTML, ".home__leaders-body .category-header__name a")

categories <- html_text(categoryNodes, trim=TRUE)
statTables <- html_table(nbaTableNodes)

# These list contain the categories
nightlyPlayerCategories <- categories[1:9]
seasonPlayerCategories <- categories[19:27]

nightlyTeamCategories <- categories[10:18]
seasonTeamCategories <- categories[28:36]

# These are the table numbers that correspond to player stats & team stats
playerRange <- c(seq(1, 9, 1), seq(19,27,1))
teamRange <- c(seq(10,18,1), seq(28,36,1))

# Useful for analyzing data for every category for the nightly stats
nightlyPlayerTables <- statTables[1:9]
nightlyTeamTables <- statTables[10:18]

# Useful for analyzing data for every category for the season stats
seasonPlayerTables <- statTables[19:27]
seasonTeamTables <- statTables[28:36]


# Create dictionaries so we can access tables using their categories
nightlyTeamData <- new.env()
nightlyPlayerData <- new.env()

seasonTeamData <- new.env()
seasonPlayerData <- new.env()


for (i in 1:18) {
  if(i %in% playerRange) {
    nightlyPlayerData[[categories[i]]] = statTables[[i]]
  }
  else {
    nightlyTeamData[[categories[i]]] = statTables[[i]]
  }
}

for (i in 19:length(categories)) {
  if(i %in% playerRange) {
    seasonPlayerData[[categories[i]]] = statTables[[i]]
  }
  else {
    seasonTeamData[[categories[i]]] = statTables[[i]]
  }
}

##################### Ideas ###############
# player averages for each category for night - DONE -> Anita
# player averages for each category for season - DONE -> Anita

# team averages for each category for night - DONE -> Anita
# team averages for each category for season - DONE -> Anita

# Spread for each category for player/team for night stats
# Spread for each category for player/team for season stats

# Top player for each category for the night - DONE -> Jeff
# Top player for each category for the season - DONE -> Jeff

# Top team for each category for the night - DONE -> Jeff
# Top team for each category for the season - DONE -> Jeff

# Player in most categories - DONE - > SKY
# Team in the most categories - DONE - > SKY

# players/team that are both in nightly leaders and season leaders

###########################################



################# Anita's Section ###############
# player averages for each category for night
categoryAvg <- c()
for (i in 1:9) {
  nightlyPlayerAvg <- mean(nightlyPlayerTables[[i]][, 'X3'])
  # column for categories, average
  categoryAvg[i] <- nightlyPlayerAvg
}
categoryAvgData <- matrix(categoryAvg, byrow = FALSE, nrow=9, dimnames = list(c(nightlyPlayerCategories),c('Average')))
print('##### Player averages for each category for the night #######################')
print(categoryAvgData)
cat("\n\n")

# player averages for each category for season
categorySeasonAvg <- c()

for (i in 1:9) {
  seasonPlayerAvg <- mean(seasonPlayerTables[[i]][, 'X3'])
  categorySeasonAvg[i] <- seasonPlayerAvg
}
categorySeasonAvgData <- matrix(categorySeasonAvg, byrow = FALSE, nrow = 9, dimnames = list(c(seasonPlayerCategories), c('Average')))
print('##### Player averages for each category for season #######################')
print(categorySeasonAvgData)
cat("\n\n")
# team averages for each category for night

nightlyTeamAvg <- c()
for (i in 1:9) {
  nightTeamAvg <- mean(nightlyTeamTables[[i]][, 'X3'])
  nightlyTeamAvg[i] <- nightTeamAvg
}
nightlyTeamAvgData <- matrix(nightlyTeamAvg, byrow = FALSE, nrow = 9, dimnames = list(c(nightlyPlayerCategories), c('Average')))
print('##### Team averages for each category for the night #######################')
print(nightlyTeamAvgData)
cat("\n\n")

# team averages for each category for season
seasonTeamAvg <- c()
for (i in 1:9) {
  TeamAvg <- mean(seasonTeamTables[[i]][, 'X3'])
  seasonTeamAvg[i] <- TeamAvg
}
seasonTeamAvgData <- matrix(seasonTeamAvg, byrow = FALSE, nrow = 9, dimnames = list(c(seasonPlayerCategories), c('Average')))
print('##### Team averages for each category for season #######################')
print(seasonTeamAvgData)
cat("\n\n")
nightlyTeamSpread <- c()


##################################################





################# Jeff's Section #################
# Top nighlty player for each category
topPlayers <- c()
topCount <- c()
for(i in 1:9) {
  player <- nightlyPlayerTables[[i]][1,'X2']
  pos <- regexpr('\n', player)
  player <- substr(player, 0 ,pos-2)
  topPlayers[i] <- player
  topCount[i] <- nightlyPlayerTables[[i]][1,'X3']
}
topPlayersforNightByCategory <- matrix(c(topPlayers, topCount) , nrow=9, byrow=FALSE, dimnames = list(c(nightlyPlayerCategories), c('Player', 'Amount')))
print('##### Top players for the night by category #######################')
print(topPlayersforNightByCategory)
cat("\n\n")

# Top season players for each category
topPlayers <- c()
topCount <- c()
for(i in 1:9) {
  player <- seasonPlayerTables[[i]][1,'X2']
  pos <- regexpr('\n', player)
  player <- substr(player, 0 ,pos-2)
  topPlayers[i] <- player
  topCount[i] <- seasonPlayerTables[[i]][1,'X3']
}
topPlayersforSeasonByCategory <- matrix(c(topPlayers, topCount) , nrow=9, byrow=FALSE, dimnames = list(c(seasonPlayerCategories), c('Player', 'Amount')))
print('##### Top players for the Season by category #######################')
print(topPlayersforSeasonByCategory)
cat("\n\n")

# Top nighlty team for each category
topTeams <- c()
topCount <- c()
for(i in 1:9) {
  team <- nightlyTeamTables[[i]][1,'X2']
  topTeams[i] <- team
  topCount[i] <- nightlyTeamTables[[i]][1,'X3']
}
topTeamsforNightByCategory <- matrix(c(topTeams, topCount) , nrow=9, byrow=FALSE, dimnames = list(c(nightlyTeamCategories), c('Team', 'Amount')))
print('##### Top Teams for the Night by category #######################')
print(topTeamsforNightByCategory)
cat("\n\n")

# Top season teams for each category
topTeams <- c()
topCount <- c()
for(i in 1:9) {
  team <- seasonTeamTables[[i]][1,'X2']
  topTeams[i] <- team
  topCount[i] <- seasonTeamTables[[i]][1,'X3']
}
topTeamsforSeasonByCategory <- matrix(c(topTeams, topCount) , nrow=9, byrow=FALSE, dimnames = list(c(seasonTeamCategories), c('Team', 'Amount')))
print('##### Top Teams for the Season by category #######################')
print(topTeamsforSeasonByCategory)
cat("\n\n")

##################################################

################# Sky's Section #######################
# player in most categories
# team in most categories
nightlyPlayerCategoryCount = new.env()
nightlyTeamCategoryCount = new.env()
seasonPlayerCategoryCount = new.env()
seasonTeamCategoryCount = new.env()

nightlyPlayerCount <- c()
for(i in 1:9) {
  for(j in 1:5) {
    entry <- nightlyPlayerTables[[i]][j,'X2']
    pos <- regexpr('\n', entry)
    entry <- substr(entry, 0 ,pos-1)
    nightlyPlayerCount <- c(nightlyPlayerCount, entry)
  }
}
nightlyPlayerCountDF <- as.data.frame(nightlyPlayerCount)
cDF <- as.data.frame(count(nightlyPlayerCountDF[[1]]))
cDF <- cDF %>% arrange(desc(freq))
print('##### Number of Categories a player appeared in for Nightly Leaders #######################')
print(cDF) # <-- nightly player leaders in category count
cat('\n\n')

nightlyTeamCount <- c()
for(i in 1:9) {
  for(j in 1:5) {
    entry <- nightlyTeamTables[[i]][j,'X2']
    nightlyTeamCount <- c(nightlyTeamCount, entry)
  }
}
nightlyTeamCountDF <- data.frame(nightlyTeamCount)
cDF <- as.data.frame(count(nightlyTeamCountDF[[1]]))
cDF <- cDF %>% arrange(desc(freq))
print('##### Number of Categories a Team appeared in for Nightly Leaders #######################')
print(cDF) # <-- nightly team leaders in category count
cat('\n\n')

seasonPlayerCount <- c()
for(i in 1:9) {
  for(j in 1:5) {
    entry <- seasonPlayerTables[[i]][j,'X2']
    pos <- regexpr('\n', entry)
    entry <- substr(entry, 0 ,pos-1)
    seasonPlayerCount <- c(seasonPlayerCount, entry)
  }
}
seasonPlayerCountDF <- as.data.frame(seasonPlayerCount)
cDF <- as.data.frame(count(seasonPlayerCountDF[[1]]))
cDF <- cDF %>% arrange(desc(freq))
colnames(cDF) <- c('Player', 'freq')
print('##### Number of Categories a Player appeared in for Season Leaders #######################')
print(cDF) # <-- season player leaders in category count
cat('\n\n')

seasonTeamCount <- c()
for(i in 1:9) {
  for(j in 1:5) {
    entry <- seasonTeamTables[[i]][j,'X2']
    seasonTeamCount <- c(seasonTeamCount, entry)
  }
}

seasonTeamCountDF <- as.data.frame(seasonTeamCount)
cDF <- as.data.frame(count(seasonTeamCountDF[[1]]))
cDF <- cDF %>% arrange(desc(freq))
print('##### Number of Categories a Team appeared in for Season Leaders #######################')
print(cDF) # <-- season team leaders in category count
cat("\n\n")
#####################################################

