#Attach Route

setwd('F:/BigDataBowl2021')

source('InstantExam.R')
load('Plays_Week_Targets_YAC.Rdata')
source('SplitPlaysByWeek.R')

plays$route = NA

#Standard Loop through plays
oldWeek = 0
for(i in 1:nrow(plays)){
  play = plays[i,'playId']; game = plays[i,'gameId']
  newWeek = findWeekNum(game)
  if(newWeek != oldWeek) {
    trackData = splitTrackingByWeek(newWeek)
    routeOpts = levels(trackData$route)
    playData = splitPlaysByWeek(newWeek)
  }
  if(i %% 50 == 1) print(paste(round(100*i/nrow(plays),2),'% Complete',sep=''))
  
  #Action
  frame = isolateFrame(1,play,game,trackData)
  targRec = plays[i,'MatchedTargetReceiver']
  if(!is.na(targRec)) plays[i,'route'] = routeOpts[frame[frame$displayName == targRec,'route']]
  
  oldWeek = newWeek
}


# save(plays,file = 'Plays_Week_Targets_YAC_Route.Rdata')

