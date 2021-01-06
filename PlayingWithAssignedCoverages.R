#Coverages Data

cov1 = read.csv('coverages_week1.csv')
cov1$YdsResult = NA
cov1$PassResult = NA

for(i in 1:nrow(cov1)){
  play = cov1[i,'playId']
  game = cov1[i,'gameId']
  fp = findPlay(play,game)
  if(nrow(fp)> 0){
    cov1[i,'YdsResult'] = fp$playResult
    cov1[i,'PassResult'] = levels(fp$passResult)[fp$passResult]
  }
}

cov1[(cov1$coverage == 'Cover 2 Zone') & (cov1$PassResult == 'I'),]


#This one is pretty good, has two defenders covering the same guy, but not really covering him
#https://gamepass.nfl.com/game/buccaneers-at-saints-on-09092018?coach=true
#TB/NO Q4 4:22
play2 = 4173
game2 = 2018090906
p2 = findFrameAtPass(play2,game2,w1Data,T)
p2$plot
ap2 = findFrameAtPass(play2,game2,w1Data,T)
ap2$plot
aa2 = findFrameAtArrival(play2,game2,w1Data,T)
aa2$plot



#This might be the one
#https://gamepass.nfl.com/game/texans-at-patriots-on-09092018?coach=true
#HOU/NE Q2 7:23
play3 = 1533
game3 = 2018090905    
p3 = findFrameAtPass(play3,game3,w1Data,T)
p3$plot
ap3 = findFrameAtPass(play3,game3,w1Data,T)
ap3$plot
aa3 = findFrameAtArrival(play3,game3,w1Data,T)
aa3$plot

grid.arrange(p2$plot,p3$plot)
grid.arrange(ap2$plot,ap3$plot)
grid.arrange(aa2$plot,aa3$plot)

# #Decent followup
# play3 = 1927
# game3 = 2018090906
# ap3 = findFrameAtPass(play3,game3,w1Data,T)
# ap3$plot
# aa3 = findFrameAtArrival(play3,game3,w1Data,T)
# aa3$plot

#Decent option, post play stopped
# play3 = 2715 
# game3 = 2018091001    
# ap3 = findFrameAtPass(play3,game3,w1Data,T)
# ap3$plot
# aa3 = findFrameAtArrival(play3,game3,w1Data,T)
# aa3$plot

# play3 = 1555
# game3 = 2018090912
# ap3 = findFrameAtPass(play3,game3,w1Data,T)
# ap3$plot
# aa3 = findFrameAtArrival(play3,game3,w1Data,T)
# aa3$plot