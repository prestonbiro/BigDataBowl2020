#Find targets 

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
load('Combined2018PassingData.RData')

plays$targetReceiver = NA
for(i in 1:nrow(plays)){
  plays[i,'targetReceiver'] = strsplit(strsplit(strsplit(toString(plays$playDescription[i]),' to ')[[1]][2],' ')[[1]][1],'[.]')[[1]][2]
}

#May also need to split by "intended for", for interceptions
#Seems to have failures also when there are penalties

head(plays)

findOffPlayers <- function(playNum,gameNum,data = allWeeksData){
  firstFrameInfo = isolateFrame(1,playNum,gameNum)
  skillOffPlayers = firstFrameInfo[firstFrameInfo$position %in% skillOffPos,'displayName']
  
  return(as.character(skillOffPlayers))
}


plays$MatchedTargetReceiver = NA
for(i in 1:nrow(plays)){
# for(i in 1:10){
  play = plays[i,]
  thisGameId = play$gameId
  ixGame = which(games$gameId == thisGameId)
  weekNum = games$week[ixGame]
  thisPlayId = play$playId
  tarRec = play$targetReceiver
  posTeam = play$possessionTeam
  thisOffSet = findOffPlayers(thisPlayId,thisGameId)
  ixMatch = which(grepl(tarRec,thisOffSet))
  if(length(ixMatch) == 1) plays[i,'MatchedTargetReceiver'] = thisOffSet[ixMatch]
}

save(plays,file = 'UpdatedPlays.Rdata')

head(plays[is.na(plays$MatchedTargetReceiver),],50)
tail(plays[is.na(plays$MatchedTargetReceiver),],50)
