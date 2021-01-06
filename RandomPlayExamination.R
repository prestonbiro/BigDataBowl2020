#Random Plays ECA Plots at Throw

#Around 950 is where week 2 starts, so we stick to week 1 for ease of data plotting
playIx = sample(1:950,1)


for(j in playIx){
  playNum = plays[j,'playId']
  gameNum = plays[j,'gameId']
  print(j)
  playDesc = plays[j,'playDescription']
  print(playDesc)
  p = makeECAPlot_BallDest(playNum,gameNum)
  show(p)
  p = makeECAPlot_BallDest(playNum,gameNum,flipColors = T)
  show(p)
}



