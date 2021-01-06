#Grid Field
# setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')
source('FindAirDuration.R')

load('FinalPlays.Rdata')

xleft = 0; xright = 120; ybot = 0; ytop = 53.3

gridField <- function(QBLOC_X,playDir,givenMinX = NULL,givenMaxX = NULL){
  if(is.null(givenMinX)){
    if(playDir == 'left'){
      xmin = ifelse(QBLOC_X-50<xleft,xleft,QBLOC_X-50); xmax = QBLOC_X
    }
    else{
      xmax = ifelse(QBLOC_X+50>xright,xright,QBLOC_X+50); xmin = QBLOC_X
    }
  }
  else{
    xmin = givenMinX; xmax = givenMaxX
  }
  
  xSeq = seq(xmin,xmax,length.out = ceiling((xmax-xmin)/1.75))
  ySeq = seq(ybot,ytop,length.out = 30)
  gridDf = data.frame()
  for(yi in ySeq){
    gridDf <- rbind(gridDf,data.frame(xPts = xSeq,yPts = yi))
  }
  # plotBase = baseNFLField()
  # plotBase <- plotBase + geom_point(data = gridDf,aes(x = xPts,y = yPts))
  # print(plotBase)
  return(gridDf)
}

# a = gridField(45,'right')

distToGrid <- function(QB_X,QB_Y,playDir,givenMinX = NULL,givenMaxX = NULL){
  griddedField = gridField(QB_X,playDir,givenMinX,givenMaxX)
  griddedField$DistToQB = round(sqrt((QB_X - griddedField$xPts)^2 + (QB_Y - griddedField$yPts)^2),1)
  # plotBase = baseNFLField()
  # plotBase <- plotBase + geom_label(data = griddedField,aes(x = xPts,y = yPts,label = DistToQB))
  # print(plotBase)
  return(griddedField)
}

airDurToGrid <- function(QB_X,QB_Y,playDir,medOnly = F,givenGrid = NA){
  if(is.na(givenGrid))  gridDur = distToGrid(QB_X,QB_Y,playDir,givenMinX,givenMaxX)
  else gridDur = givenGrid
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

findCoverage <- function(player_x,player_y,vel_x,vel_y,qb_x,qb_y,playDir,givenGrid = NA){
  player_x = player_x #+ 10
  qb_x = qb_x #+ 10
  gridVals = airDurToGrid(qb_x,qb_y,playDir,givenGrid = givenGrid)
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
  # return(plotBase)
  return(gridCover)
}

# projectFuture(0,0,1,1,2,2,.55)
# findCoverage(p$defender1X,p$defender1Y,p$defender1S * sin(p$defender1Dir * pi / 180),p$defender1S * cos(p$defender1Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findCoverage(p$defender2X,p$defender2Y,p$defender2S * sin(p$defender2Dir * pi / 180),p$defender2S * cos(p$defender2Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findCoverage(p$defender3X,p$defender3Y,p$defender3S * sin(p$defender3Dir * pi / 180),p$defender3S * cos(p$defender3Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findCoverage(r$defender1X,r$defender1Y,r$defender1S * sin(r$defender1Dir * pi / 180),r$defender1S * cos(r$defender1Dir * pi / 180),r$QB_X,r$QB_Y,r$PlayDirection)
# findCoverage(20,25,0,10,25,25,'left')

findMedianCoverageHull <- function(player_x,player_y,vel_x,vel_y,qb_x,qb_y,playDir,givenMinX = NULL,givenMaxX = NULL){
  player_x = player_x + 10
  qb_x = qb_x + 10
  # print(paste(player_x,player_y,vel_x,vel_y,qb_x,qb_y,playDir))
  gridVals = airDurToGrid(qb_x,qb_y,playDir,medOnly=T,givenMinX = givenMinX,givenMaxX = givenMaxX)
  gridCover = gridVals[,c('xPts','yPts','50%')]
  gridCover$CoveredBool = NA
  # print(gridCover)
  for(i in 1:nrow(gridCover)){
    gridCover[i,c('CoveredBool')] =
      projectFuture(player_x,player_y,vel_x,vel_y,gridCover[i,'xPts'],gridCover[i,'yPts'],gridCover[i,'50%'])
  }
  # print(gridCover$CoveredBool)
  
  trueSpots = gridCover[gridCover$CoveredBool == T,c('xPts','yPts')]
  hullIx = chull(trueSpots$xPts,trueSpots$yPts)
  return(trueSpots[hullIx,])
  # return(trueSpots)
}

# findMedianCoverageHull(p$defender1X,p$defender1Y,p$defender1S * sin(p$defender1Dir * pi / 180),p$defender1S * cos(p$defender1Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findMedianCoverageHull(p$defender2X,p$defender2Y,p$defender2S * sin(p$defender2Dir * pi / 180),p$defender2S * cos(p$defender2Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findMedianCoverageHull(p$defender3X,p$defender3Y,p$defender3S * sin(p$defender3Dir * pi / 180),p$defender3S * cos(p$defender3Dir * pi / 180),p$QB_X,p$QB_Y,p$PlayDirection)
# findMedianCoverageHull(r$defender1X,r$defender1Y,r$defender1S * sin(r$defender1Dir * pi / 180),r$defender1S * cos(r$defender1Dir * pi / 180),r$QB_X,r$QB_Y,r$PlayDirection)

# aAll = findMedianCoverageHull(20,25,0,10,25,25,'left')

# findMedianCoverageHull()

plotCoverageHulls <- function(defenderData,allData,givenMinX = NULL,givenMaxX = NULL){
  qbx = allData[allData$position == 'QB','x']
  qby = allData[allData$position == 'QB','y']
  
  fieldGridDf = distToGrid(qbx,qby,defenderData[1,'playDirection'],givenMinX,givenMaxX)
  
  for(i in 1:nrow(defenderData)){
    p = defenderData[i,]
    a = findCoverage(p$x,p$y,p$s * sin(p$dir * pi / 180),p$s * cos(p$dir * pi / 180),qbx,qby,p$playDirection,givenMinX,givenMaxX)
    # print(a)
    
    # print(p$displayName)
    fieldGridDf[,as.character(p$displayName)] = a$CoveragePercentage
    # print(a)
    # print(fieldGridDf)
  }
  return(fieldGridDf)
}
# fullHullDf3 = plotCoverageHulls(ap3$defInfo,ap3$allInfo)
# 
# fullHullDf3$SumCol = rowSums(fullHullDf3[,4:ncol(fullHullDf3)]) 
# fullHullDf3$SumCol = fullHullDf3$SumCol/(ncol(fullHullDf3) - 3)
# 
# fullHullPlot3 <- ggplot(data = fullHullDf3) +
#   geom_contour_filled(aes(x = xPts,y = yPts,z = SumCol),binwidth = .05) +
#   geom_point(data = ap3$allInfo,aes(x = x,y = y, colour = team,size = 2)) + 
#   geom_segment(data = ap3$allInfo,
#                aes(x = x,y = y, xend = x + s * sin(dir * pi / 180), yend = y + s * cos(dir * pi / 180),colour = team),
#                arrow = arrow(length = unit(.4,"cm"))) +
#   theme(legend.position = 'none') + labs(x = '',y = '') +
#   geom_point(data = aa3$allInfo[aa3$allInfo$team == 'football',], aes(x=x, y = y),colour = 'burlywood4',size = 5)
# fullHullPlot3
# 
# fullHullDf2 = plotCoverageHulls(ap2$defInfo,ap2$allInfo)
# 
# fullHullDf2$SumCol = rowSums(fullHullDf2[,4:ncol(fullHullDf2)]) 
# fullHullDf2$SumCol = fullHullDf2$SumCol/(ncol(fullHullDf2) - 3)
# 
# fullHullPlot2 <- ggplot(data = fullHullDf2) +
#   geom_contour_filled(aes(x = xPts,y = yPts,z = SumCol),binwidth = .05) +
#   geom_point(data = ap2$allInfo,aes(x = x,y = y, colour = team,size = 2)) + 
#   geom_segment(data = ap2$allInfo,
#                aes(x = x,y = y, xend = x + s * sin(dir * pi / 180), yend = y + s * cos(dir * pi / 180),colour = team),
#                arrow = arrow(length = unit(.4,"cm"))) +
#   theme(legend.position = 'none') + labs(x = '',y = '') +
#   geom_point(data = ap2$allInfo[ap2$allInfo$team == 'football',], aes(x=x, y = y),colour = 'burlywood4',size = 5)
# fullHullPlot2
# 
# grid.arrange(fullHullPlot2,fullHullPlot3)
