#Attach play direction

setwd('F:/BigDataBowl2021')

source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_Off_Def_Ball_Dist.Rdata')

trimmedPlays$PlayDirection = NA

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
  frame = isolateFrame(1,play,game,trackData)
  trimmedPlays[i,'PlayDirection'] = levels(frame$playDirection)[frame$playDirection[1]]
  
  oldWeek = newWeek
}

# save(trimmedPlays,file = 'Plays_Off_Def_Ball_Dist_Dir.Rdata')
