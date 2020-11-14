setwd('F:/BigDataBowl2021')

targetReceivers = read.csv('targetedReceiver.csv')
playsOrig = read.csv('plays.csv')
games = read.csv('games.csv')
players = read.csv('players.csv')
players$targetNflId = players$nflId
load('Plays_Week_Targets_YAC_Route.Rdata')

targetsWithName = merge(targetReceivers,players[,c('displayName','targetNflId')],by = 'targetNflId')

playsOrig








