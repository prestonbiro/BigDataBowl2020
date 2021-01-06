#AirTime Vs Distance
library(fitdistrplus)

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
source('SplitPlaysByWeek.R')

load('Plays_All_Offense_Defense_Ball.Rdata')


findAirDuration <- function(throwDistance,playData = playsToUse,medOnly = F){
  #Given a throw distance, will find the fit gamma distribution of times for 
  #how long the ball might be in the air
  
  #Finds previous data within a 1 yard range of the given distance, adds small 1e-6 to make gamma fitable with 0s
  if(throwDistance < 1) throwDistance = 1
  else if(throwDistance > 55) throwDistance = 55
  laxSpace = ifelse(throwDistance < 30,.5,ifelse(throwDistance < 40,1,ifelse(throwDistance < 50,1.5,5)))
  durationsToUse = playData[(playData$throwDistance >= throwDistance - laxSpace) & 
                         (playData$throwDistance <= throwDistance + laxSpace) &
                           (playData$BallAirDuration >= 0) & 
                           !is.na(playData$BallAirDuration),'BallAirDuration'] + 1e-6
  # print(durationsToUse)
  fit.gamma <- fitdist(durationsToUse,distr = 'gamma', method = 'mle')
  if(medOnly){
    timeQuants = qgamma(.5,fit.gamma$estimate[1],fit.gamma$estimate[2]) - 1e-6
    names(timeQuants) = .5
  }
  else{
    timeQuants = qgamma(c(.05,.1,.25,.5,.75,.9,.95),fit.gamma$estimate[1],fit.gamma$estimate[2]) - 1e-6
    names(timeQuants) = c(.05,.1,.25,.5,.75,.9,.95)
  }
  
  return(timeQuants)
}


findAirDuration(20)
# plot(trimmedPlays$throwDistance,trimmedPlays$BallAirDuration)
# points(trimmedPlays[(trimmedPlays$throwDistance >19) & (trimmedPlays$throwDistance < 21),'throwDistance'],
#        trimmedPlays[(trimmedPlays$throwDistance >19) & (trimmedPlays$throwDistance < 21),'BallAirDuration'],col='red')

# hist(trimmedPlays[(trimmedPlays$throwDistance >19) & (trimmedPlays$throwDistance < 21),'BallAirDuration'],breaks = 30)
# 
# x = trimmedPlays[(trimmedPlays$throwDistance >19) & (trimmedPlays$throwDistance < 21),'BallAirDuration'] + 1e-6
# fit.gamma <- fitdist(x,distr = 'gamma', method = 'mle')
# 
# hist(x,breaks = 30,freq=F)
# points(seq(0,4,length.out = 30),dgamma(seq(0,4,length.out = 30),shape = 8.107134,rate = 7.122155),col='blue')
# fit.gamma$estimate

# findAirDuration(24)
