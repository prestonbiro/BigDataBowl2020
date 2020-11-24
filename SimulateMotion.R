#Simulate Motion

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')
source('FindAirDuration.R')

load('FinalPlays.Rdata')

accelerationPopulation = w1Data[w1Data$a < 10,'a']

simulateMotion <- function(curX,curY,curS,curDir,timeToArrive,destX,destY){
  numFrames = timeToArrive*10 + 1
  playerx = rep(NA,numFrames)
  playery = rep(NA,numFrames)
  playerVx = rep(NA,numFrames)
  playerVy = rep(NA,numFrames)
  playerAx = rep(NA,numFrames)
  playerAy = rep(NA,numFrames)
  adir = rep(NA,numFrames)
  times = seq(0,timeToArrive,by = .1)
  playerx[1] = curX
  playery[1] = curY
  playerVx[1] = curS * sin(curDir * pi/180)
  playerVy[1] = curS * cos(curDir * pi/180)
  a_mag_1 = sample(accelerationPopulation,1)
  a_dir_1 = atan((destX - curX)/(destY - curY))
  playerAx[1] = a_mag_1 * cos(a_dir_1)
  playerAy[1] = a_mag_1 * sin(a_dir_1)
  adir[1] = a_dir_1 * 180 / pi
  
  for(i in 2:numFrames){
    playerx[i] = playerx[i-1] + playerVx[i-1] * .1
    playery[i] = playery[i-1] + playerVy[i-1] * .1
    playerVx[i] = playerVx[i-1] + playerAx[i-1] * .1
    playerVy[i] = playerVy[i-1] + playerAy[i-1] * .1
    a_mag = sample(accelerationPopulation,1)
    # print(paste(playerx[i-1],playery[i-1]))
    a_dir= atan((destX - playerx[i-1])/(destY - playery[i-1])) + 
      ifelse(((destX - playerx[i-1]) < 0) & ((destY - playery[i-1]) < 0),-pi,
             ifelse(((destX - playerx[i-1]) < 0) & ((destY - playery[i-1]) > 0),pi,0))
    # print(a_dir * 180 / pi)
    adir[i] = a_dir * 180 / pi
    playerAx[i] = a_mag * cos(a_dir)
    playerAy[i] = a_mag * sin(a_dir)
  }
  
  moveDf = data.frame(x = playerx, y = playery,t = times,angle = adir)
  destDf = data.frame(x = destX,y = destY,t = times)

  actualLocPlot <- ggplot() + geom_label(data = moveDf,aes(x = x,y = y,label = angle)) +
    geom_point(data = destDf,aes(x=x,y=y)) +
    transition_time(t) +
    shadow_mark(alpha = .3,size = .5)

  return(animate(actualLocPlot,height = 700,width = 840))
}


a = simulateMotion(0,0,1,-120,10,10,10)
a
