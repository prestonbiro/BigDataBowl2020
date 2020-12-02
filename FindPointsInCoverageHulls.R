#Find stuff
library(sp)
options(stringsAsFactors = F)

setwd('F:/BigDataBowl2021')

source('GridField.R')

rm(plays,trimmedPlays,w1Data)


coverageDf = data.frame(Week = numeric(),Game = numeric(),Play = numeric(),
                        Down = numeric(),Dist = numeric(),LOS = numeric(),PlayResult = numeric(),PassResult = character(),DPI = character(),
                        TargetReceiver = character(),YAC = numeric(),Route = character(),ThrowDistance = numeric(),
                        Def_Name = character(),Def_X = numeric(),Def_Y = numeric(),Def_Dist = numeric(),Def_Pos = character(),
                        Def_S = numeric(),Def_A = numeric(),Def_O = numeric(),Def_Dir = numeric(),AirDuration = numeric(),
                        Dest_X = numeric(),Dest_Y = numeric(),PlayDirection = character(),QB_X = numeric(),QB_Y = numeric())

# load('CoverageData.Rdata')
for(i in 1:nrow(playsToUse)){
  print(i); print(nrow(coverageDf))
  play = playsToUse[i,]
  dest_x = play$Dest_X;dest_y = play$Dest_Y
  if(!is.na(play$defender1Name)){
    p1Hull = findMedianCoverageHull(play$defender1X,play$defender1Y,play$defender1S*sin(play$defender1Dir * pi / 180),
                                    play$defender1S*cos(play$defender1Dir * pi / 180),play$QB_X,play$QB_Y,play$PlayDirection)
    if(nrow(p1Hull) > 0){
      if(point.in.polygon(dest_x,dest_y,p1Hull$xPts,p1Hull$yPts)){
        coverageDf <- rbind(coverageDf,setNames(play[,c('week','gameId','playId','down','yardsToGo','absoluteYardlineNumber',
                                                        'offensePlayResult','passResult','isDefensivePI',
                                                        'MatchedTargetReceiver','YAC','route','throwDistance','defender1Name','defender1X',
                                                        'defender1Y','defender1Dist','defender1Pos','defender1S','defender1A','defender1O',
                                                        'defender1Dir','BallAirDuration','Dest_X','Dest_Y','PlayDirection','QB_X','QB_Y')],names(coverageDf)))
      }
    }
    

    if(!is.na(play$defender2Name)){
      p2Hull = findMedianCoverageHull(play$defender2X,play$defender2Y,play$defender2S*sin(play$defender2Dir * pi / 180),
                                      play$defender2S*cos(play$defender2Dir * pi / 180),play$QB_X,play$QB_Y,play$PlayDirection)
      if(nrow(p2Hull) > 0){
        if(point.in.polygon(dest_x,dest_y,p2Hull$xPts,p2Hull$yPts)){
          coverageDf <- rbind(coverageDf,setNames(play[,c('week','gameId','playId','down','yardsToGo','absoluteYardlineNumber',
                                                          'offensePlayResult','passResult','isDefensivePI',
                                                          'MatchedTargetReceiver','YAC','route','throwDistance','defender2Name','defender2X',
                                                          'defender2Y','defender2Dist','defender2Pos','defender2S','defender2A','defender2O',
                                                          'defender2Dir','BallAirDuration','Dest_X','Dest_Y','PlayDirection','QB_X','QB_Y')],names(coverageDf)))
        }
      }
      

      if(!is.na(play$defender3Name)){
        p3Hull = findMedianCoverageHull(play$defender3X,play$defender3Y,play$defender3S*sin(play$defender3Dir * pi / 180),
                                        play$defender3S*cos(play$defender3Dir * pi / 180),play$QB_X,play$QB_Y,play$PlayDirection)
        if(nrow(p3Hull) > 0){
          if(point.in.polygon(dest_x,dest_y,p3Hull$xPts,p3Hull$yPts)){
            coverageDf <- rbind(coverageDf,setNames(play[,c('week','gameId','playId','down','yardsToGo','absoluteYardlineNumber',
                                                            'offensePlayResult','passResult','isDefensivePI',
                                                            'MatchedTargetReceiver','YAC','route','throwDistance','defender3Name','defender3X',
                                                            'defender3Y','defender3Dist','defender3Pos','defender3S','defender3A','defender3O',
                                                            'defender3Dir','BallAirDuration','Dest_X','Dest_Y','PlayDirection','QB_X','QB_Y')],names(coverageDf)))
          }
        }
      }
    }
  }
  
  if(i %% 50 == 0) {
    print('saving')
    save(coverageDf,file = 'CoverageData.Rdata')
  }
}

# i = 1
