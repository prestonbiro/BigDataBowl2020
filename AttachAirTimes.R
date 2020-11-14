#Ball Air Time
options(digits.secs = 4)

# setwd('F:/BigDataBowl2021')
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
    timePass = as.POSIXct(framePass[1,'time'],format = '%Y-%m-%dT%H:%M:%OS')
    timeArrive = as.POSIXct(frameArrive[1,'time'],format = '%Y-%m-%dT%H:%M:%OS')
    
    trimmedPlays[i,'BallAirDuration'] = timeArrive - timePass
  }
  oldWeek = newWeek
}


# save(trimmedPlays,file = 'Plays_All_Offense_Defense_Ball.Rdata')
# tail(trimmedPlays)

#Worth noting that there's a lot of games? where they seem to not to track 
#the times (or maybe events) as precisely, resulting in a lot of the ball
#durations to come out to be exactly whole numbers
#We're going to ignore this for now, but maybe come back to it later





















