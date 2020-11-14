#Find YAC

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('UpdatedPlays.Rdata')
w1Data = read.csv('week1.csv')
w1Plays = splitPlaysByWeek(1)

play = 75
game = 2018090600

findYAC <- function(playNum,gameNum,trackData = w1Data,playData = w1Plays){
  thisPlay = findPlay(playNum,gameNum,playData)
  if(thisPlay$passResult != 'C') {
    print('incomplete or something')
    return(NA)
  }
  else{
    fa = findFrameAtArrival(playNum,gameNum,trackData)
    if(any(is.na(fa$x))) return(NA)
    los = thisPlay$absoluteYardlineNumber
    totYards = thisPlay$playResult
    ballCatcher = thisPlay$MatchedTargetReceiver
    xCatch = fa[fa$displayName == ballCatcher,'x']
    if(length(xCatch) == 0) return(NA)
    if(fa$playDirection[1] == 'left'){
      JasonWittenYards = los - xCatch
    }
    else{
      JasonWittenYards = xCatch - los
    }
    return(totYards - JasonWittenYards)
  }
}

findYAC(play,game,w1Data)

playsWithYAC = plays
playsWithYAC$YAC = NA
oldWeek = 0
for(i in 1:nrow(playsWithYAC)){
  play = playsWithYAC[i,'playId']; game = playsWithYAC[i,'gameId']
  newWeek = findWeekNum(game)
  if(newWeek != oldWeek) {
    trackData = splitTrackingByWeek(newWeek)
    playData = splitPlaysByWeek(newWeek)
  }
  if(i %% 50 == 1) print(paste(round(100*i/nrow(playsWithYAC),2),'% Complete',sep=''))
  if(!is.na(playsWithYAC[i,'MatchedTargetReceiver']) & (playsWithYAC[i,'passResult'] == 'C')) 
    playsWithYAC[i,'YAC'] = findYAC(play,game,trackData,playData)
  oldWeek = newWeek
}

sum(!is.na(playsWithYAC$YAC) | (playsWithYAC$passResult != 'C'))
# sum(is.na(playsWithYAC[playsWithYAC$,'YAC']))



plays = playsWithYAC
# save(plays,file = 'Plays_Targets_YAC.Rdata')

plays$week = NA
for(i in 1:nrow(plays)){
  plays[i,'week'] = findWeekNum(plays[i,'gameId'])
}


# save(plays,file = 'Plays_Week_Targets_YAC.Rdata')

#Weeks 4 and 14 seriously missing, but everything else seems fine, almost all missing are no play
for(i in 1:17) print(sum(is.na(plays[(plays$week == i) & (plays$passResult == 'C'),'YAC'])))
# print(plays[is.na(plays[(plays$week == 2) & (plays$passResult == 'C'),'YAC']),'playDescription'])

play4 = splitPlaysByWeek(4)
track4 = splitTrackingByWeek(4)

head(track4)


for(i in 1:nrow(play4)){
  play = play4[i,'playId']; game = play4[i,'gameId']
  if(i %% 50 == 1) print(paste(round(100*i/nrow(play4),2),'% Complete',sep=''))
  if(!is.na(play4[i,'MatchedTargetReceiver']) & (play4[i,'passResult'] == 'C')) 
    play4[i,'YAC'] = findYAC(play,game,trackData,playData)
}

