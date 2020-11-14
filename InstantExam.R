#Plot at Throw
library(gridExtra)

setwd('F:/BigDataBowl2021')
source('AssignDefender.R')

play = 75
game = 2018090600

findFrameAtPass <- function(playNum,gameNum,data = w1Data,plotOn = F,returnAllRes = F,assignDef = F){
  playData = isolatePlay(playNum,gameNum,data)
  if(plotOn) returnAllRes = T
  if(returnAllRes) assignDef = T
  if(assignDef)  defData = assignDefenders(playNum,gameNum,data)
  frameThrow = playData[(playData$event == 'pass_forward') | (playData$event == 'pass_shovel'),'frameId'][1]
  
  if(plotOn){
    p = plotFrame(frameThrow,playNum,gameNum,data)
    p = p +     
      geom_segment(data = defData[(defData$frameId == frameThrow),],
                   aes(x = x,y = y,xend = DefendingPlayer_x,yend = DefendingPlayer_y))
  }
  else{
    p = 'pineapple'
  }
  
  if(returnAllRes){
    return(list(plot = p,
                defInfo = defData[(defData$frameId == frameThrow),],
                allInfo = playData[(playData$frameId == frameThrow),]))
  }
  else{
    if(assignDef) return(defInfo = defData[(defData$frameId == frameThrow),])
    else return(playData[(playData$frameId == frameThrow),])
  }
}

# findFrameAtPass(play,game)

findFrameAtArrival <- function(playNum,gameNum,data = w1Data,plotOn = F,returnAllRes = F,assignDef = F){
  playData = isolatePlay(playNum,gameNum,data)
  if(plotOn) returnAllRes = T
  if(returnAllRes) assignDef = T
  if(assignDef)  defData = assignDefenders(playNum,gameNum,data)
  frameThrow = playData[(playData$event == 'pass_arrived') | (playData$event == 'pass_outcome_caught') |
                          (playData$event == 'pass_outcome_incomplete') | 
                          (playData$event == 'pass_outcome_touchdown'),'frameId'][1]
  if(any(is.na(playData[playData$frameId == frameThrow,'x']))){
    frameThrow = playData[(playData$event == 'first_contact'),'frameId'][1]
  }
  
  if(plotOn){
    p = plotFrame(frameThrow,playNum,gameNum,data)
    p = p +     
      geom_segment(data = defData[(defData$frameId == frameThrow),],
                   aes(x = x,y = y,xend = DefendingPlayer_x,yend = DefendingPlayer_y))
  }
  else{
    p = 'pineapple'
  }
  
  if(returnAllRes){
    return(list(plot = p,
                defInfo = defData[(defData$frameId == frameThrow),],
                allInfo = playData[(playData$frameId == frameThrow),]))
  }
  else{
    return(playData[(playData$frameId == frameThrow),])
  }
}

# findFrameAtArrival(play,game)

sideBySidePassPlot <- function(playNum,gameNum,data = w1Data){
  list_throw = findFrameAtPass(playNum,gameNum,data,plotOn = T)
  list_arrive = findFrameAtArrival(playNum,gameNum,data,plotOn = T)
  
  grid.arrange(list_throw$plot,list_arrive$plot)
}

# sideBySidePassPlot(play,game)
