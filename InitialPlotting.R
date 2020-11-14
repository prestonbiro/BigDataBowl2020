#NFL Big Data Bowl
library(gganimate)

setwd('F:/BigDataBowl2021')
source('NFLFieldPlot.R')

games = read.csv('games.csv')
players = read.csv('players.csv')
plays = read.csv('plays.csv')
w1Data = read.csv('week1.csv')
# load('Combined2018PassingData.RData')

base = baseNFLField()

isolatePlay <- function(playNum,gameNum,data = allWeeksData){
  playData = data[(data$playId == playNum) & (data$gameId == gameNum),]
  return(playData)
}

isolateFrame <- function(frameNum,playNum,gameNum,data = allWeeksData){
  playData = isolatePlay(playNum,gameNum,data)
  frameData = playData[playData$frameId == frameNum,]
  return(frameData)
}

findPlay <- function(playNum,gameNum,data = plays){
  playInfo = data[(data$playId == playNum) & (data$gameId == gameNum),]
  return(playInfo)
}

plotFrame <- function(frameNum,playNum,gameNum,data = allWeeksData){
  #Plots a specific frame of a specific play 
  base = baseNFLField()
  frameData = isolateFrame(frameNum,playNum,gameNum,data)
  frameData[frameData$displayName == 'Football','jerseyNumber'] = 'FB'
  playInfo = findPlay(playNum,gameNum)
  
  if(frameData$playDirection[1] == 'right'){
    LOS = playInfo$absoluteYardlineNumber 
    L2G = LOS + playInfo$yardsToGo
  }
  else{
    LOS = playInfo$absoluteYardlineNumber
    L2G = LOS - playInfo$yardsToGo
  }
  
  field.lines.df = data.frame(x = c(LOS,L2G),
                              ybot = rep(0.3,2),
                              ytop = rep(53,2))
  
  framePlot <- base + geom_point(data = frameData,aes(x = x,y = y,col = team,size = 4)) +
    geom_text(data = frameData,aes(x = x,y = y,size = 4,label = jerseyNumber)) + 
    geom_segment(data=field.lines.df,
                 aes(x=x,y=ybot,xend=x,yend=ytop,linetype='a'),
                 inherit.aes = F,
                 col=c('blue','yellow'))
  
  return(framePlot)
}





# for(pl in unique(w1Data[w1Data$gameId == game,'playId'])){
#   p = plotFrame(1,pl,game)
#   plot(p)
# }


plotPlay <- function(playNum,gameNum,data = allWeeksData){
  #Plots a specific play of a specific game
  base = baseNFLField()
  playData = isolatePlay(playNum,gameNum)
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
  
  framePlot <- base + geom_point(data = playData,aes(x = x,y = y,col = team,size = 4)) +
    geom_text(data = playData,aes(x = x,y = y,size = 4,label = jerseyNumber)) + 
    geom_segment(data=field.lines.df,
                 aes(x=x,y=ybot,xend=x,yend=ytop,linetype='a'),
                 inherit.aes = F,
                 col=c('blue','yellow')) +
    transition_time(frameId) +
    shadow_mark(alpha = .3,size = .5)
  
  return(animate(framePlot,height = 700,width = 840))
}



# frame = 1
# play = 75
# game = 2018090600
# 
# playRight = 320
# findPlay(play,game)
# 
# frame1_146 = isolateFrame(1,146)
# plotFrame(frame,playRight,game)
# 
# 
# play146 = isolatePlay(146,game)
# unique(play146$frameId)
# 
# plotPlay(play,game)

