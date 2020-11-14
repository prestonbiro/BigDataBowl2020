#Attach Defenders and Distances

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_All_Offense.Rdata')

colsToUse = c('gameId','playId','playDescription','down',
              'yardsToGo','possessionTeam','playType',
              'absoluteYardlineNumber','passResult','offensePlayResult','isDefensivePI',
              'MatchedTargetReceiver','YAC','week','route','throwDistance','Target_X','Target_Y')
trimmedPlays = plays[!is.na(plays$MatchedTargetReceiver) & !is.na(plays$throwDistance),colsToUse]

trimmedPlays$defender1Name = NA
trimmedPlays$defender1X = NA
trimmedPlays$defender1Y = NA
trimmedPlays$defender1Dist = NA
trimmedPlays$defender1Pos = NA
trimmedPlays$defender1S = NA
trimmedPlays$defender1A = NA
trimmedPlays$defender1dir = NA
trimmedPlays$defender1O = NA

trimmedPlays$defender2Name = NA
trimmedPlays$defender2X = NA
trimmedPlays$defender2Y = NA
trimmedPlays$defender2Dist = NA
trimmedPlays$defender2Pos = NA
trimmedPlays$defender2S = NA
trimmedPlays$defender2A = NA
trimmedPlays$defender2dir = NA
trimmedPlays$defender2O = NA

trimmedPlays$defender3Name = NA
trimmedPlays$defender3X = NA
trimmedPlays$defender3Y = NA
trimmedPlays$defender3Dist = NA
trimmedPlays$defender3Pos = NA
trimmedPlays$defender3S = NA
trimmedPlays$defender3A = NA
trimmedPlays$defender3dir = NA
trimmedPlays$defender3O = NA

load('Plays_All_Offense_Defense.Rdata')



#Standard Loop through trimmedPlays
oldWeek = 0
# for(i in 1:nrow(trimmedPlays)){
for(i in which(is.na(trimmedPlays$defender1X))[which(is.na(trimmedPlays$defender1X)) > 7500]){
  play = trimmedPlays[i,'playId']; game = trimmedPlays[i,'gameId']
  newWeek = findWeekNum(game)
  if(newWeek != oldWeek) {
    trackData = splitTrackingByWeek(newWeek)
    routeOpts = levels(trackData$route)
    playData = splitPlaysByWeek(newWeek)
  }
  if(i %% 200 == 1) print(paste(round(100*i/nrow(trimmedPlays),2),'% Complete',sep=''))
  
  #Action
  targRec = trimmedPlays[i,'MatchedTargetReceiver']
  if(!is.na(targRec)){
    frame = findFrameAtPass(play,game,trackData,assignDef = T)
    defPlays = frame[frame$PlayerDefending == targRec,]
    if(nrow(defPlays) == 1){
      trimmedPlays[i,c('defender1Name','defender1X','defender1Y','defender1A','defender1S','defender1O','defender1Dir','defender1Dist','defender1Pos')] =
        c(levels(defPlays$displayName)[defPlays$displayName],defPlays$x,defPlays$y,defPlays$a,defPlays$s,defPlays$o,defPlays$dir,
          sqrt((defPlays$x - trimmedPlays[i,'Target_X'])^2 + (defPlays$y - trimmedPlays[i,'Target_Y'])^2),levels(defPlays$position)[defPlays$position])
      
      # trimmedPlays[i,'defender1Name'] = levels(defPlays$displayName)[defPlays$displayName]
      # trimmedPlays[i,'defender1X'] = defPlays$x
      # trimmedPlays[i,'defender1Y'] = defPlays$y
      # trimmedPlays[i,'defender1Dist'] = sqrt((trimmedPlays[i,'defender1X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender1Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender1Pos'] = levels(defPlays$position)[defPlays[1,'position']]
    }
    else if(nrow(defPlays) == 2){
      trimmedPlays[i,c('defender1Name','defender1X','defender1Y','defender1A','defender1S','defender1O','defender1Dir','defender1Dist','defender1Pos')] =
        c(levels(defPlays$displayName)[defPlays[1,'displayName']],defPlays[1,'x'],defPlays[1,'y'],defPlays[1,'a'],defPlays[1,'s'],defPlays[1,'o'],defPlays[1,'dir'],
          sqrt((defPlays[1,'x'] - trimmedPlays[i,'Target_X'])^2 + (defPlays[1,'y'] - trimmedPlays[i,'Target_Y'])^2),
          levels(defPlays$position)[defPlays[1,'position']])
      trimmedPlays[i,c('defender2Name','defender2X','defender2Y','defender2A','defender2S','defender2O','defender2Dir','defender2Dist','defender2Pos')] =
        c(levels(defPlays$displayName)[defPlays[2,'displayName']],defPlays[2,'x'],defPlays[2,'y'],defPlays[2,'a'],defPlays[2,'s'],defPlays[2,'o'],defPlays[2,'dir'],
          sqrt((defPlays[2,'x'] - trimmedPlays[i,'Target_X'])^2 + (defPlays[2,'y'] - trimmedPlays[i,'Target_Y'])^2),
          levels(defPlays$position)[defPlays[2,'position']])
      
      
      # trimmedPlays[i,'defender1Name'] = levels(defPlays$displayName)[defPlays[1,'displayName']]
      # trimmedPlays[i,'defender1X'] = defPlays[1,'x']
      # trimmedPlays[i,'defender1Y'] = defPlays[1,'y']
      # trimmedPlays[i,'defender2Name'] = levels(defPlays$displayName)[defPlays[2,'displayName']]
      # trimmedPlays[i,'defender2X'] = defPlays[2,'x']
      # trimmedPlays[i,'defender2Y'] = defPlays[2,'y']
      # trimmedPlays[i,'defender1Dist'] = sqrt((trimmedPlays[i,'defender1X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender1Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender2Dist'] = sqrt((trimmedPlays[i,'defender2X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender2Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender1Pos'] = levels(defPlays$position)[defPlays[1,'position']]
      # trimmedPlays[i,'defender2Pos'] = levels(defPlays$position)[defPlays[2,'position']]
    }
    else if(nrow(defPlays) >= 3){
      trimmedPlays[i,c('defender1Name','defender1X','defender1Y','defender1A','defender1S','defender1O','defender1Dir','defender1Dist','defender1Pos')] =
        c(levels(defPlays$displayName)[defPlays[1,'displayName']],defPlays[1,'x'],defPlays[1,'y'],defPlays[1,'a'],defPlays[1,'s'],defPlays[1,'o'],defPlays[1,'dir'],
          sqrt((defPlays[1,'x'] - trimmedPlays[i,'Target_X'])^2 + (defPlays[1,'y'] - trimmedPlays[i,'Target_Y'])^2),
          levels(defPlays$position)[defPlays[1,'position']])
      trimmedPlays[i,c('defender2Name','defender2X','defender2Y','defender2A','defender2S','defender2O','defender2Dir','defender2Dist','defender2Pos')] =
        c(levels(defPlays$displayName)[defPlays[2,'displayName']],defPlays[2,'x'],defPlays[2,'y'],defPlays[2,'a'],defPlays[2,'s'],defPlays[2,'o'],defPlays[2,'dir'],
          sqrt((defPlays[2,'x'] - trimmedPlays[i,'Target_X'])^2 + (defPlays[2,'y'] - trimmedPlays[i,'Target_Y'])^2),
          levels(defPlays$position)[defPlays[2,'position']])
      trimmedPlays[i,c('defender3Name','defender3X','defender3Y','defender3A','defender3S','defender3O','defender3Dir','defender3Dist','defender3Pos')] =
        c(levels(defPlays$displayName)[defPlays[3,'displayName']],defPlays[3,'x'],defPlays[3,'y'],defPlays[3,'a'],defPlays[3,'s'],defPlays[3,'o'],defPlays[3,'dir'],
          sqrt((defPlays[3,'x'] - trimmedPlays[i,'Target_X'])^2 + (defPlays[3,'y'] - trimmedPlays[i,'Target_Y'])^2),
          levels(defPlays$position)[defPlays[3,'position']])
      # trimmedPlays[i,'defender1Name'] = levels(defPlays$displayName)[defPlays[1,'displayName']]
      # trimmedPlays[i,'defender1X'] = defPlays[1,'x']
      # trimmedPlays[i,'defender1Y'] = defPlays[1,'y']
      # trimmedPlays[i,'defender2Name'] = levels(defPlays$displayName)[defPlays[2,'displayName']]
      # trimmedPlays[i,'defender2X'] = defPlays[2,'x']
      # trimmedPlays[i,'defender2Y'] = defPlays[2,'y']
      # trimmedPlays[i,'defender3Name'] = levels(defPlays$displayName)[defPlays[3,'displayName']]
      # trimmedPlays[i,'defender3X'] = defPlays[3,'x']
      # trimmedPlays[i,'defender3Y'] = defPlays[3,'y']
      # trimmedPlays[i,'defender1Dist'] = sqrt((trimmedPlays[i,'defender1X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender1Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender2Dist'] = sqrt((trimmedPlays[i,'defender2X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender2Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender3Dist'] = sqrt((trimmedPlays[i,'defender3X'] - trimmedPlays[i,'Target_X'])^2 
      #                                        + (trimmedPlays[i,'defender3Y'] - trimmedPlays[i,'Target_Y'])^2)
      # trimmedPlays[i,'defender1Pos'] = levels(defPlays$position)[defPlays[1,'position']]
      # trimmedPlays[i,'defender2Pos'] = levels(defPlays$position)[defPlays[2,'position']]
      # trimmedPlays[i,'defender3Pos'] = levels(defPlays$position)[defPlays[3,'position']]
    }
  }
  oldWeek = newWeek
}

# save(trimmedPlays,file = 'Plays_All_Offense_Defense.Rdata')



