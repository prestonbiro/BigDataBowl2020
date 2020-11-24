#Grid Field

setwd('F:/BigDataBowl2021')
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
}

airDurToGrid <- function(QB_X,QB_Y,playDir){
  gridDur = distToGrid(QB_X,QB_Y,playDir)
  gridDur[,c('5%','10%','25%','50%','75%','90%','95%')] = NA
  for(i in 1:nrow(gridDur)) {
    # print(gridDur[i,'DistToQB'])
    gridDur[i,c('5%','10%','25%','50%','75%','90%','95%')] = findAirDuration(gridDur[i,'DistToQB'])
  }
  return(gridDur)
}

# a = distToGrid(45,26,'right')
aGrid = airDurToGrid(5,26,'left')


# plot(aGrid$DistToQB,aGrid$`95%`,lty = 2)
# points(aGrid$DistToQB,aGrid$`5%`,lty = 2,col='red')






