#Expected Future Locations

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')
source('FindAirDuration.R')

load('FinalPlays.Rdata')

findExpectedFutureLoc <- function(timeInSec,currentX,currentY,currentS,currentDir,currentA,currentO){
  #Given time, current location, velocity, and acceleration, uses physics to predict future location 
  #in the certain amount of elapsed time. Assumes velocity and acceleration do not change over the time
  #which is why this is the expect location, serving more or less as the mean of future positions 
  
  #Determine direction of acceleration, assuming acceleration is in the orientation of the player, unless backpedaling
  #If angle of motion is opposite of orientation, then probably backpedaling
  if(abs(abs(currentDir - currentO) - 180) < abs(currentDir - currentO) - 30){
    #Backpedaling (probably)
    accelAngle = currentO - 180
  }
  else{
    accelAngle = currentO
  }
  
  v_x = currentS * sin(currentDir * pi / 180)
  v_y = currentS * cos(currentDir * pi / 180)
  a_x = currentA * sin(accelAngle * pi / 180)
  a_y = currentA * cos(accelAngle * pi / 180)
  
  fut_x = currentX + v_x * timeInSec + .5 * a_x * timeInSec^2
  fut_y = currentY + v_y * timeInSec + .5 * a_y * timeInSec^2
  return(list('Future_x' = fut_x,'Future_y' = fut_y))
}


# p = playsToUse[1,]
# p2 = findExpectedFutureLoc(1.101,p$defender1X,p$defender1Y,p$defender1S,p$defender1Dir,p$defender1A,p$defender1O)
# 
# plot(p$defender1X,p$defender1Y)
# points(p2$Future_x,p2$Future_y,col='red')
# arrows(p$defender1X,p$defender1Y,p$defender1X+cos(p$defender1Dir * pi/180),p$defender1Y+sin(p$defender1Dir * pi/180),length = .02)
# arrows(p$defender1X,p$defender1Y,p$defender1X+cos(p$defender1O * pi/180),p$defender1Y+sin(p$defender1O * pi/180),length = .02)




plotExpectedFutureLocs <- function(playNum,gameNum,data = w1Data){
  
  #First, just plot the play as it stands
  base = baseNFLField()
  playData = isolatePlay(playNum,gameNum,data)
  playInfo = findPlay(playNum,gameNum)
  playData[playData$displayName == 'Football','jerseyNumber'] = 'FB'
  
  
  if(playData$playDirection[1] == 'right'){
    LOS = playInfo$absoluteYardlineNumber 
    L2G = LOS + playInfo$yardsToGo
  }
  else{
    LOS = playInfo$absoluteYardlineNumber
    L2G = LOS - playInfo$yardsToGo
  }
  
  field.lines.df = data.frame(x = c(LOS,L2G),
                              ybot = rep(0,2),
                              ytop = rep(53.3,2))
  
  actualLocPlot <- base + geom_point(data = playData,aes(x = x,y = y,col = team,size = 4)) +
    geom_text(data = playData,aes(x = x,y = y,size = 4,label = jerseyNumber)) + 
    geom_segment(data=field.lines.df,
                 aes(x=x,y=ybot,xend=x,yend=ytop,linetype='a'),
                 inherit.aes = F,
                 col=c('blue','yellow')) +
    transition_time(frameId) +
    shadow_mark(alpha = .3,size = .5)
  
  #Then, lets find out where they could be
  
  #Find which players are covering who
  defAssignDf = assignDefenders(play,game,data)
  defAssignDf$Future_x = NA
  defAssignDf$Future_y = NA
  
  #Loop through the frames
  for(frame in 1:max(defAssignDf$frameId)){
  # for(frame in 1:2){
    #Find distance from potential targets to qb
    offInfo = playData[playData$frameId == frame,]
    qb_x = offInfo[offInfo$position == 'QB','x']
    qb_y = offInfo[offInfo$position == 'QB','y']

    curDefFrame = defAssignDf[defAssignDf$frameId == frame,]
    curPotTargets = unique(curDefFrame$PlayerDefending)
    #Loop through the pot targets
    for(targName in curPotTargets){
      targ_x = offInfo[offInfo$displayName == targName,'x']
      targ_y = offInfo[offInfo$displayName == targName,'y']
      
      distToQb = sqrt((targ_x - qb_x)^2 + (targ_y - qb_y)^2)
      #Find the time the ball would have to be in the air for each of those potential targets
      potAirTimes = findAirDuration(distToQb)
      
      #Just the median times right now
      medAirTime = potAirTimes[4]
      
      coveringDefRows = defAssignDf[(defAssignDf$frameId == frame) & (defAssignDf$PlayerDefending == targName),]
      for(defPlayerName in coveringDefRows$displayName){
        curCoverPlayer = coveringDefRows[coveringDefRows$displayName == defPlayerName,]
        defAssignDf[(defAssignDf$frameId == frame) &
                      (defAssignDf$PlayerDefending == targName) &
                      (defAssignDf$displayName == defPlayerName),c('Future_x','Future_y')] = 
          findExpectedFutureLoc(medAirTime,curCoverPlayer$x,curCoverPlayer$y,curCoverPlayer$s,curCoverPlayer$dir,
                                curCoverPlayer$a,curCoverPlayer$o)
      }
      # defAssignDf[(defAssignDf$frameId == frame) & (defAssignDf$PlayerDefending == targName),
      #             c('Future_x','Future_y')] = findExpectedFutureLoc(medAirTime,)
    }
  }
  futLocAddedPlot <- actualLocPlot + geom_point(data = defAssignDf,aes(x = Future_x,y = Future_y,col = team,size = 4)) +
    geom_text(data = playData,aes(x = x,y = y,size = 4,label = jerseyNumber)) + 
    transition_time(frameId)
  
  return(animate(futLocAddedPlot,height = 700,width = 840))
}


plotExpectedFutureLocs(play,game,w1Data)




