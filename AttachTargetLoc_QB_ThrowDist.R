#Attach Target Loc, QB Name, and Throw Distance

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_Week_Targets_YAC_Route.Rdata')



plays$throwDistance = NA
plays$QB = NA
plays$Target_X = NA
plays$Target_Y = NA
plays$QB_X = NA
plays$QB_Y = NA

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
  if(i %% 200 == 1) print(paste(round(100*i/nrow(plays),2),'% Complete',sep=''))
  
  #Action
  frame = findFrameAtPass(play,game,trackData)
  qbName = frame[frame$position == 'QB','displayName']
  targRec = plays[i,'MatchedTargetReceiver']

  if(!is.na(targRec) & (any(frame$displayName == targRec))){
    plays[i,'Target_X'] = frame[frame$displayName == targRec,'x']
    plays[i,'Target_Y'] = frame[frame$displayName == targRec,'y']
  } 

  if(length(qbName) == 1){
    plays[i,'QB'] = as.character(qbName)
    qb_x = frame[frame$position == 'QB','x']
    qb_y = frame[frame$position == 'QB','y']
    plays[i,'QB_X'] = qb_x
    plays[i,'QB_Y'] = qb_y
    if(is.na(plays[i,'QB'])) {
      q = qbName
      p = plays[i,]
      f = frame
    }
  } 
  else{
    qb_x = NA
    qb_y = NA
  }
  

  dist = sqrt((plays[i,'Target_X'] - qb_x)^2 + (plays[i,'Target_Y'] - qb_y)^2)
  if(!is.na(dist)) plays[i,'throwDistance'] = dist
  
  
  oldWeek = newWeek
}


# save(plays,file = 'Plays_All_Offense.Rdata')












