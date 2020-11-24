#Ball Air Time

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_All_Offense_Defense.Rdata')

trimmedPlays$BallAirDuration = NA

#Standard Loop through trimmedPlays
oldWeek = 0
# for(i in 1:nrow(trimmedPlays)){
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
  frameArrive = findFrameAtArrival(play,game,trackData)
  if(!any(is.na(frameArrive$x))){
    framePass = findFrameAtPass(play,game,trackData)
    timePass = framePass[1,'time']
    timeArrive = frameArrive[1,'time']
    
    trimmedPlays[i,'BallAirDuration'] = timeArrive - timePass
  }
  oldWeek = newWeek
}


# save(trimmedPlays,file = 'Plays_All_Offense_Defense_Ball.Rdata')






















