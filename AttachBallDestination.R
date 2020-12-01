#Attach ball destination

setwd('F:/BigDataBowl2021')

source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_All_Offense_Defense_Ball.Rdata')

trimmedPlays$Dest_X = NA
trimmedPlays$Dest_Y = NA

oldWeek = 0
for(i in 1:nrow(trimmedPlays)){
  play = trimmedPlays[i,'playId']; game = trimmedPlays[i,'gameId']
  newWeek = findWeekNum(game)
  if(newWeek != oldWeek) {
    trackData = splitTrackingByWeek(newWeek)
    routeOpts = levels(trackData$route)
    playData = splitPlaysByWeek(newWeek)
  }
  if(i %% 200 == 1) print(paste(round(100*i/nrow(trimmedPlays),2),'% Complete',sep=''))
  
  #Action
  frame = findFrameAtArrival(play,game,trackData)
  targRec = trimmedPlays[i,'MatchedTargetReceiver']
  
  if(!is.na(targRec) & (any(frame$displayName == targRec))){
    trimmedPlays[i,'Dest_X'] = frame[frame$displayName == targRec,'x']
    trimmedPlays[i,'Dest_Y'] = frame[frame$displayName == targRec,'y']
  } 
  
  oldWeek = newWeek
}

# save(trimmedPlays,file = 'Plays_Off_Def_Ball_Dist.Rdata')
