#SplitPlaysByWeek
setwd('F:/BigDataBowl2021')

games = read.csv('games.csv')
load('FinalPlays.Rdata')
# load('Combined2018PassingData.RData')

# w1Plays = plays[plays$gameId %in%  games[games$week == 1,'gameId'],]
# w2Plays = plays[plays$gameId %in%  games[games$week == 2,'gameId'],]
# w3Plays = plays[plays$gameId %in%  games[games$week == 3,'gameId'],]
# w4Plays = plays[plays$gameId %in%  games[games$week == 4,'gameId'],]
# w5Plays = plays[plays$gameId %in%  games[games$week == 5,'gameId'],]
# w6Plays = plays[plays$gameId %in%  games[games$week == 6,'gameId'],]
# w7Plays = plays[plays$gameId %in%  games[games$week == 7,'gameId'],]
# w8Plays = plays[plays$gameId %in%  games[games$week == 8,'gameId'],]
# w9Plays = plays[plays$gameId %in%  games[games$week == 9,'gameId'],]
# w10Plays = plays[plays$gameId %in%  games[games$week == 10,'gameId'],]
# w11Plays = plays[plays$gameId %in%  games[games$week == 11,'gameId'],]
# w12Plays = plays[plays$gameId %in%  games[games$week == 12,'gameId'],]
# w13Plays = plays[plays$gameId %in%  games[games$week == 13,'gameId'],]
# w14Plays = plays[plays$gameId %in%  games[games$week == 14,'gameId'],]
# w15Plays = plays[plays$gameId %in%  games[games$week == 15,'gameId'],]
# w16Plays = plays[plays$gameId %in%  games[games$week == 16,'gameId'],]
# w17Plays = plays[plays$gameId %in%  games[games$week == 17,'gameId'],]

findWeekNum <- function(gameNum){
  return(games[games$gameId == gameNum,'week'])
}

splitPlaysByWeek <- function(weekNum){
  return(playsToUse[playsToUse$gameId %in%  games[games$week == weekNum,'gameId'],])
}

splitTrackingByWeek <- function(weekNum){
  return(read.csv(paste('week',weekNum,'.csv',sep='')))
}
