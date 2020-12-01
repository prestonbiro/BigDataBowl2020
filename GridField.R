#Grid Field
# setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')
source('FindAirDuration.R')

load('FinalPlays.Rdata')

xleft = 0; xright = 120; ybot = 0; ytop = 53.3

gridField <- function(QBLOC_X,playDir){
  if(playDir == 'left'){
    xmin = ifelse(QBLOC_X-50<xleft,xleft,QBLOC_X-50); xmax = QBLOC_X
  }
  else{
    xmax = ifelse(QBLOC_X+50>xright,xright,QBLOC_X+50); xmin = QBLOC_X
  }
  xSeq = seq(xmin,xmax,length.out = ceiling((xmax-xmin)/1.75))
  ySeq = seq(ybot,ytop,length.out = 30)
  gridDf = data.frame()
  for(yi in ySeq){
    gridDf <- rbind(gridDf,data.frame(xPts = xSeq,yPts = yi))
  }
  # plotBase = baseNFLField()
  # plotBase <- plotBase + geom_point(data = gridDf,aes(x = xPts,y = yPts))
  return(gridDf)
}

# a = gridField(45,'right')

distToGrid <- function(QB_X,QB_Y,playDir){
  QBLOC_X = QB_X + 10
  griddedField = gridField(QBLOC_X,playDir)
  griddedField$DistToQB = round(sqrt((QBLOC_X - griddedField$xPts)^2 + (QB_Y - griddedField$yPts)^2),1)
  # plotBase = baseNFLField()
  # plotBase <- plotBase + geom_label(data = griddedField,aes(x = xPts,y = yPts,label = DistToQB))
  return(griddedField)
  # return(plotBase)
}

airDurToGrid <- function(QB_X,QB_Y,playDir,medOnly = F){
  gridDur = distToGrid(QB_X,QB_Y,playDir)
  if(medOnly){
    gridDur[,'50%'] = NA
    for(i in 1:nrow(gridDur)) {
      gridDur[i,'50%'] = findAirDuration(gridDur[i,'DistToQB'],medOnly = T)
    }
  }
  else{
    gridDur[,c('5%','10%','25%','50%','75%','90%','95%')] = NA
    for(i in 1:nrow(gridDur)) {
      # print(gridDur[i,'DistToQB'])
      gridDur[i,c('5%','10%','25%','50%','75%','90%','95%')] = findAirDuration(gridDur[i,'DistToQB'])
    }
  }
  
  return(gridDur)
}

# a = distToGrid(45,26,'right')
# aGrid = airDurToGrid(5,26,'left')


# plot(aGrid$DistToQB,aGrid$`95%`,lty = 2)
# points(aGrid$DistToQB,aGrid$`5%`,lty = 2,col='red')

#Source https://www.reddit.com/r/GlobalOffensive/comments/90ztdc/player_reaction_times_relative_to_major_sports/
t_react = .309
maxSpeed = quantile(w1Data[w1Data$displayName != 'Football','s'],.99)
projectFuture <- function(player_x,player_y,vel_x,vel_y,dest_x,dest_y,timeToArrive){
  #Returns true if the player could reasonable reach the destination location in the specified time given the 
  #current location and speed
  #Assumes if you're within 1 foot of destination than you've gotten there
  
  if(timeToArrive <= t_react){
    #not enough time to react, better hope its on path
    
    futureLoc_x = player_x + timeToArrive * vel_x
    futureLoc_y = player_y + timeToArrive * vel_y
    
    finalDist = sqrt((futureLoc_x - dest_x)^2 + (futureLoc_y - dest_y)^2)
    if(finalDist < 1) return(TRUE)
    else return(FALSE)
  }
  else{
    reactLoc_x =  player_x + t_react * vel_x
    reactLoc_y =  player_y + t_react * vel_y
    
    timeRespond = timeToArrive - t_react
    distRespond = sqrt((reactLoc_x - dest_x)^2 + (reactLoc_y - dest_y)^2)
    speedNeedRespond = distRespond/timeRespond
    
    if(speedNeedRespond < maxSpeed) return(TRUE)
    else return(FALSE)
  }
}

findCoverage <- function(player_x,player_y,vel_x,vel_y,qb_x,qb_y,playDir){
  gridVals = airDurToGrid(qb_x,qb_y,playDir)
  gridCover = gridVals[,c('xPts','yPts','5%','10%','25%','50%','75%','90%','95%')]
  # gridCover[,c('LowRange','MidRange','HighRange')] = NA
  gridCover$CoveragePercentage = NA
  
  # percentCheck = c('5%','10%','25%','50%','75%','90%','95%')
  percentCheck = c('5%','25%','50%','75%','95%')
  # percentCover = c(.95,.9,.75,.5,.25,.1,.05)
  percentCover = c(.95,.75,.5,.25,.05)
  for(i in 1:nrow(gridCover)){
    j = 1
    while(is.na(gridCover[i,'CoveragePercentage']) & j <= 5){
      if(projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,percentCheck[j]]))
        gridCover[i,'CoveragePercentage'] = percentCover[j]
      else j = j + 1
    }
    if(is.na(gridCover[i,'CoveragePercentage'])) gridCover[i,'CoveragePercentage'] = 0
    # gridCover[i,c('LowRange','MidRange','HighRange')] = 
    #   c(projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,'5%']),
    #     projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,'50%']),
    #     projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,'95%']))
  }
  plotBase = baseNFLField()
  plotBase <- plotBase + stat_contour(data = gridCover,aes(x = xPts,y = yPts,z = CoveragePercentage),binwidth = .2)
  plotBase <- plotBase + geom_point(inherit.aes = F,data = data.frame(xp = c(qb_x,player_x),yp = c(qb_y,player_y)),aes(x=xp,y=yp,col=c('red','blue')))
  return(plotBase)
  # return(gridCover)
}

# projectFuture(0,0,1,1,2,2,.55)

# findCoverage(20,25,0,10,25,25,'left')

findMedianCoverageHull <- function(player_x,player_y,vel_x,vel_y,qb_x,qb_y,playDir){
  gridVals = airDurToGrid(qb_x,qb_y,playDir,medOnly=T)
  gridCover = gridVals[,c('xPts','yPts','50%')]
  gridCover$CoveredBool = NA
  
  for(i in 1:nrow(gridCover)){
    gridCover[i,c('CoveredBool')] =
      projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,'50%'])
  }
  
  trueSpots = gridCover[gridCover$CoveredBool == T,c('xPts','yPts')]
  hullIx = chull(trueSpots$xPts,trueSpots$yPts)
  # return(trueSpots[hullIx,])
  return(trueSpots)
}

# aAll = findMedianCoverageHull(20,25,0,10,25,25,'left')
