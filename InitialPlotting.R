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

plotFrame <- function(frameNum,playNum,gameNum,data = allWeeksData,color = T){
  #Plots a specific frame of a specific play 
  if(color){
    base = baseNFLField()
  }
  else{
    base = noColor_baseNFLField()
  }
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


# 
# frame = 1
# play1 = 2845 #Okay
# play2 = 1425 #Pretty good
# play3 = 3784 #Pretty good
# play4 = 1737 #Good
# play5 = 877 # okay
# play6 = 2353 #Good
# play7 = 2312
# game = 2018090900
# 
# plotFrame(20,play,game,w1Data)
# 
# ap1 = findFrameAtPass(play1,game,w1Data,T)
# ap2 = findFrameAtPass(play2,game,w1Data,T)
# # ap2$plot
# ap3 = findFrameAtPass(play3,game,w1Data,T)
# # ap3$plot
# ap4 = findFrameAtPass(play4,game,w1Data,T)
# # ap4$plot
# ap5 = findFrameAtPass(play5,game,w1Data,T)
# # ap5$plot
# ap6 = findFrameAtPass(play6,game,w1Data,T)
# # ap6$plot
# ap7 = findFrameAtPass(play7,game,w1Data,T)
# # ap7$plot
# 
# findPlay(play1,game)
# findPlay(play2,game)
# findPlay(play3,game)
# findPlay(play4,game)
# findPlay(play5,game)
# findPlay(play6,game)
# findPlay(play7,game)
# 
# ap1$plot
# ap2$plot
# ap3$plot
# ap4$plot
# ap5$plot
# ap6$plot
# ap7$plot
# 
# 
# 
# aa = findFrameAtArrival(play1,game,w1Data,T)
# aa$plot
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

