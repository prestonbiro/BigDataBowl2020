#Make ECA Plots
library(metR)
library(transformr)
library(gganimate)
library(ggplot2)

setwd('F:/BigDataBowl2021')
source('AssignDefender.R')
source('GridField.R')

options(warn = -1)


addNFLField <- function(oldPlot){
  ytop = 53.3
  ybot = 0
  field.lines <- data.frame(x1 = c(0,0,0,120,110,10),
                            x2 = c(120,120,0,120,110,10),
                            y1 = c(ybot,ytop,ytop,ytop,ytop,ytop),
                            y2 = c(ybot,ytop,ybot,ybot,ybot,ybot))
  
  ids = factor(c('endDef','fieldPlay','endOff'))
  
  values <- data.frame(
    id = ids,
    color = c('white','white','white')
  )
  field.shape <- data.frame(id = unlist(matrix(t(matrix(rep(ids,4),nrow=3)),nrow=1)[1,]),
                            xg = c(0,0,10,10,10,10,110,110,110,110,120,120),
                            yg = c(ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot,ybot,ytop,ytop,ybot))
  datapoly <- merge(values, field.shape, by = c("id"))
  
  fieldLines <- geom_segment(data=field.lines,aes(x=x1,y=y1,xend=x2,yend=y2,
                                                  linetype='a'),colour='white',
                             size = c(2,2,2,2,1,1))
  
  minor.lines.df <- data.frame(xvals = seq(15,105,length.out = 19),
                               ybot = rep(ybot,19),
                               ytop = rep(ytop,19))
  
  minor.field.lines <- geom_segment(data = minor.lines.df,aes(x = xvals,y = ybot,xend = xvals,yend = ytop),colour = 'white')
  
  hash.marks.df <- data.frame(xvals = rep(11:109,4),
                              ybot = c(rep(ybot + 1/3,99),rep(22.91,99),rep(29.73,99),rep(ytop - 1,99)),
                              ytop = c(rep(ybot + 1 ,99),rep(23.57,99),rep(30.39,99),rep(ytop - 1/3,99)))
  
  hash.marks <- geom_segment(data = hash.marks.df,aes(x = xvals,y = ybot,xend = xvals, yend = ytop),colour = 'white')
  
  field.numbers.bot.df <- data.frame(textVals = unlist(lapply(c(1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1,0),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ybot + 9,18))
  
  show.bot.numbers <- geom_text(data=field.numbers.bot.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'))
  
  field.numbers.top.df <- data.frame(textVals = unlist(lapply(c(0,1,0,2,0,3,0,4,0,5,0,4,0,3,0,2,0,1),toString)),
                                     #x_text = c(8,12,18,22,28,32,38,42,48,52,58,62,68,72,78,82,88,92),
                                     x_text = c(8.5,11.5,18.5,21.5,28.5,31.5,38.5,41.5,48.5,51.5,58.5,61.5,68.5,71.5,78.5,81.5,88.5,91.5)+10,
                                     y_text = rep(ytop - 9,18))
  
  show.top.numbers <- geom_text(data=field.numbers.top.df, aes(x = x_text, y = y_text, label = textVals),
                                size = 6,col='white',family = ifelse('Century' %in% fonts(),windowsFont('Century'),'Impact'),angle = 180)
  
  left.endzone.df <- data.frame(textVals = 'T E A M', x_left = 5, y_left = ytop/2)
  
  show.left.endzone <- geom_text(data = left.endzone.df, aes(x = x_left, y = y_left,label = textVals),
                                 size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = 90)
  
  right.endzone.df <- data.frame(textVals = 'T E A M', x_right = 115, y_right = ytop/2)
  
  show.right.endzone <- geom_text(data = right.endzone.df, aes(x = x_right, y = y_right,label = textVals),
                                  size = 10, col = 'white', family = ifelse('Copperplate Gothic Bold' %in% fonts(),windowsFont('Copperplate Gothic Bold'),'Georgia'),angle = -90)
  
  big_box <- data.frame(x_big_box_s = c(-1,-1,-1,121), 
                        x_big_box_e = c(121,121,-1,121),
                        y_big_box_s = c(ytop+.25,ybot-.2,ybot-.2,ybot-.2),
                        y_big_box_e = c(ytop+.25,ybot-.2,ytop+.25,ytop+.25))
  
  baseNFL <- oldPlot + 
    # geom_polygon(data = datapoly,aes(group = id)) +
    # scale_fill_identity() + 
    labs(x='',y='') +
    minor.field.lines + 
    hash.marks +
    show.bot.numbers +  show.top.numbers +
    show.left.endzone + show.right.endzone +
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.position = 'none') +
    fieldLines +
    geom_segment(data=big_box,aes(x=x_big_box_s,y=y_big_box_s,xend=x_big_box_e,yend=y_big_box_e),colour='black')
  
  baseNFL$layers <- c(geom_polygon(data = datapoly,aes(x = xg,y = yg),fill = 'black'), baseNFL$layers)
  return(baseNFL)
}

ECA_Contour <- function(fieldContours,moveData,flipColors = F){
  p <- ggplot(data = fieldContours) + 
    geom_contour_fill(data = fieldContours,aes(x = xPts,y = yPts,z = SumCol),breaks = seq(0,.7,length.out = 31)) +
    scale_fill_divergent(midpoint = .25) +
    geom_segment(data = moveData,
                 aes(x = x,y = y, xend = x + s * sin(dir * pi / 180), yend = y + s * cos(dir * pi / 180),colour = team),
                 arrow = arrow(length = unit(.4,"cm"))) +
    geom_point(data = moveData,aes(x = x,y = y, colour = team,size = 2)) + 
    labs(x = '',y = '')
  
  if(flipColors){
    p = p + scale_color_manual(values = c('#619CFF','#00BA38','#F8766D'))
  }
  else{
    p = p + scale_color_manual(values = c('#F8766D','#00BA38','#619CFF'))
  }
  return(p)
}
  
fullECAPlot_frame <- function(contourPlot){
  return(addNFLField(contourPlot))
}

findECAData <- function(frameNum,playNum,gameNum,data = w1Data,givenMinX = NULL,givenMaxX = NULL){
  frameToUse = isolateFrame(frameNum,playNum,gameNum,data)
  defData = assignDefenders(playNum,gameNum,data)
  # allInfo = playData[(playData$frameId == frameThrow),]
  defFrame = defData[defData$frameId == frameNum,]
  
  qbx = frameToUse[frameToUse$position == 'QB','x']
  qby = frameToUse[frameToUse$position == 'QB','y']
  
  fieldGridDf = distToGrid(qbx,qby,defData[1,'playDirection'],givenMinX,givenMaxX)
  
  # print(paste(givenMinX,givenMaxX))
  
  for(i in 1:nrow(defFrame)){
    oneDefender = defFrame[i,]
    covArea = findCoverage(oneDefender$x,oneDefender$y,oneDefender$s * sin(oneDefender$dir * pi / 180),
                           oneDefender$s * cos(oneDefender$dir * pi / 180),qbx,qby,oneDefender$playDirection,givenGrid = fieldGridDf[,1:3])
    
    fieldGridDf[,as.character(oneDefender$displayName)] = covArea$CoveragePercentage
  }
  
  fieldGridDf$SumCol = rowSums(fieldGridDf[,4:ncol(fieldGridDf)]) 
  fieldGridDf$SumCol = fieldGridDf$SumCol/(ncol(fieldGridDf) - 3)
  
  return(list('ContourData' = fieldGridDf,'MovementData' = frameToUse))
}

makeECAPlot_Frame <- function(frameNum,playNum,gameNum,data = w1Data,givenMinX = NULL,givenMaxX = NULL,flipColors = F){
  eca_data = findECAData(frameNum,playNum,gameNum,data = w1Data,givenMinX = givenMinX,givenMaxX = givenMaxX)
  
  contPlot = ECA_Contour(eca_data$ContourData,eca_data$MovementData,flipColors = flipColors)
  return(fullECAPlot_frame(contPlot))
}


addMovingPieces <- function(contourDf,movementDf){
  contourDf$frame <- as.integer(contourDf$frame)
  contourPlot <- ggplot() + 
    geom_contour_fill(data = contourDf,aes(x = xPts,y = yPts,z = SumCol,group = frame),breaks = seq(0,.7,length.out = 31)) +
    scale_fill_divergent(midpoint = .25) +
    transition_time(frame) +
    labs(x = '',y = '')
  fieldSupplement <- addNFLField(contourPlot)
  
  fieldSupplement <- fieldSupplement + geom_segment(data = movementDf,
                                                    aes(x = x,y = y, xend = x + s * sin(dir * pi / 180), yend = y + s * cos(dir * pi / 180),colour = team),
                                                    arrow = arrow(length = unit(.4,"cm"))) +
    geom_point(data = movementDf,aes(x = x,y = y, colour = team,size = 2))
  return(animate(fieldSupplement,height = 550,width = 840,fps = 10))
}

makeECAPlot <- function(playNum,gameNum,data = w1Data){
  p = isolatePlay(playNum,gameNum,data)
  numFrames = max(p$frameId)
  
  #Setup grid
  playDir = p[1,'playDirection']
  qbx_max = max(p[p$position == 'QB','x'])
  qbx_min = min(p[p$position == 'QB','x'])
  if(playDir == 'left'){
    xmin = ifelse(qbx_max-60<xleft,xleft,qbx_max-60); xmax = qbx_max
  }
  else{
    xmax = ifelse(qbx_max+60>xright,xright,qbx_max+60); xmin = qbx_max
  }
  # print(paste(xmin,xmax))
  firstFrame = findECAData(1,playNum,gameNum,data = w1Data,givenMinX = xmin,givenMaxX = xmax)
  
  # print(firstFrame)
  
  bigContourDf = firstFrame$ContourData
  bigMoveDf = firstFrame$MovementData
  bigContourDf$frame = 1
  for(frameNum in 2:numFrames){
    print(frameNum)
    # print(paste(xmin,xmax))
    curFrame = findECAData(frameNum,playNum,gameNum,data = w1Data,givenMinX = xmin,givenMaxX = xmax)
    curContour = curFrame$ContourData
    curContour$frame = frameNum

    bigContourDf <- rbind(bigContourDf,curContour)
    bigMoveDf <- rbind(bigMoveDf,curFrame$MovementData)
  }
  bigMoveDf$frame = bigMoveDf$frameId
  
  bigPlot <- addMovingPieces(bigContourDf,bigMoveDf)
  
  return(bigPlot)
  # return(list('contourDf' = bigContourDf,'movementDf' = bigMoveDf))
}


# #Play 2, TB/NO
# play2 = 4173
# game2 = 2018090906
# ECA_Gif_2 = makeECAPlot(play2,game2)
# 
# contDf2 = ECA_Gif_2$contourDf
# moveDf2 = ECA_Gif_2$movementDf
# 
# addMovingPieces(contDf2,moveDf2)
# makeECAPlot_Frame(38,play2,game2)
# #Arrives at 50
# 
# 
# 
# #Play 3, HOU/NE
# play3 = 1533
# game3 = 2018090905
# ECA_Gif_3 = makeECAPlot(play3,game3)
# 
# contDf3 = ECA_Gif_3$contourDf
# moveDf3 = ECA_Gif_3$movementDf
# 
# addMovingPieces(contDf3,moveDf3)
# makeECAPlot_Frame(40,play3,game3)
# # Arrives at 55

makeECAPlot_BallDest <- function(playNum,gameNum,data = w1Data,givenMinX = NULL,givenMaxX = NULL,flipColors = F){
  atPassDf = findFrameAtPass(playNum,gameNum,data)
  frameAtPass = atPassDf[1,'frameId']
  atArrivalDf = findFrameAtArrival(playNum,gameNum,data)
  # frameAtArrival = atArrivalDf[1,'frameId']
  # print(atArrivalDf)
  ecaPlot = makeECAPlot_Frame(frameAtPass,playNum,gameNum,data = w1Data,givenMinX = givenMinX,givenMaxX = givenMaxX,flipColors = flipColors)
  # print(ecaPlot)
  ecaPlot <- ecaPlot + geom_point(data=atArrivalDf[atArrivalDf$displayName == 'Football',],
                                  aes(x = x,y = y),size = 4,colour = 'burlywood4')
  playDesc = findPlay(playNum,gameNum)$playDescription
  if(!grepl('incomplete',as.character(playDesc))) playDesc = paste(strsplit(as.character(playDesc),'yards')[[1]][1],'yards.',sep = '') 
  ecaPlot <- ecaPlot + labs(title = playDesc)
  return(ecaPlot)
}

makeECAPlot_BallDest(play2,game2,givenMinX = 0,givenMaxX = 55)
makeECAPlot_BallDest(play3,game3,givenMinX = 31,givenMaxX = 91)


# makeECAPlot_Frame(40,play2,game2)

