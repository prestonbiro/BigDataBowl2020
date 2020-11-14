#Assign Defender
# setwd('F:/BigDataBowl2021')
setwd("~/Documents/BigDataBowl2020-main")
source('InitialPlotting.R')


defPos = c('CB','SS','FS','DB','S','MLB','LB','OLB','ILB','DL','NT','DE')
skillOffPos = c('WR','RB','TE','FB','HB')

# play = 75
# game = 2018090600


# playInfo = findPlay(play,game)
# playData = isolatePlay(play,game)

assignDefenders <- function(playNum,gameNum,data = allWeeksData){
  playData = isolatePlay(playNum,gameNum,data)
  
  
  playUpdate = playData
  playUpdate$PlayerDefending = NA
  playUpdate$DistToDefPlayer = NA
  playUpdate$DefendingPlayer_x = NA
  playUpdate$DefendingPlayer_y = NA
  lastFrame = max(playData$frameId)
  numRowsFirstFrame = nrow(isolateFrame(1,playNum,gameNum,data))
  
  firstFrameInfo = isolateFrame(1,playNum,gameNum,data)
  defPlayers = firstFrameInfo[firstFrameInfo$position %in% defPos,'displayName']
  skillOffPlayers = firstFrameInfo[firstFrameInfo$position %in% skillOffPos,'displayName']
  # print(defPlayers)
  # print(skillOffPlayers)
  for(frame in 1:lastFrame){
    frameData = isolateFrame(frame,playNum,gameNum,data)
    # print(frameData)
    if(nrow(frameData) == numRowsFirstFrame){
      offPlayerLocs = frameData[!is.na(match(frameData$displayName,skillOffPlayers)),c('x','y')]
      
      for(playerName in defPlayers){
        if(playerName %in% frameData$displayName){
          curPlayerLoc = c(frameData[frameData$displayName == playerName,'x'],frameData[frameData$displayName == playerName,'y'])
          playerDists = rep(NA,nrow(offPlayerLocs))
          for(row in 1:nrow(offPlayerLocs)){
            playerDists[row] = sqrt(sum((curPlayerLoc - offPlayerLocs[row,])^2))
          }
          closeIx = which(playerDists == min(playerDists))
          
          if(sum((playUpdate$displayName == levels(skillOffPlayers)[skillOffPlayers[closeIx]]) & (playUpdate$frameId == frame)) == 1){
            playUpdate[(playUpdate$displayName == playerName) & (playUpdate$frameId == frame),
                       c('PlayerDefending','DistToDefPlayer','DefendingPlayer_x','DefendingPlayer_y')] =
              c(levels(skillOffPlayers)[skillOffPlayers[closeIx]],playerDists[closeIx],
                playUpdate[(playUpdate$displayName == levels(skillOffPlayers)[skillOffPlayers[closeIx]]) & (playUpdate$frameId == frame),c('x','y')])
          }
          else{
            playUpdate[(playUpdate$displayName == playerName) & (playUpdate$frameId == frame),
                       c('PlayerDefending','DistToDefPlayer')] =
              c(levels(skillOffPlayers)[skillOffPlayers[closeIx]],playerDists[closeIx])
          }
        }
      }
    }
    
  }
  return(playUpdate[!is.na(playUpdate$PlayerDefending),])
}


# assignDefenders(play,game,trackData)

plotPlayWithDefenders <- function(playNum,gameNum,data = allWeeksData){
  #Plots a specific play of a specific game
  base = baseNFLField()
  playData = isolatePlay(playNum,gameNum,data)
  defData = assignDefenders(playNum,gameNum,data)
  playInfo = findPlay(playNum,gameNum,data)
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
                              ybot = rep(0.3,2),
                              ytop = rep(53,2))
  
  framePlot <- base + geom_point(data = playData,aes(x = x,y = y,col = team,size = 4)) +
    geom_text(data = playData,aes(x = x,y = y,size = 4,label = jerseyNumber)) + 
    geom_segment(data=field.lines.df,
                 aes(x=x,y=ybot,xend=x,yend=ytop,linetype='a'),
                 inherit.aes = F,
                 col=c('blue','yellow')) +
    transition_time(frameId) +
    geom_segment(data = defData,aes(x = x,y = y,xend = DefendingPlayer_x,yend = DefendingPlayer_y))
  return(animate(framePlot,height = 700,width = 840))
}

# plotPlayWithDefenders(play,game)



