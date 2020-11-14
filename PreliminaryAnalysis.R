#Preliminary Analysis

setwd('F:/BigDataBowl2021')
source('InstantExam.R')
# load('Combined2018PassingData.RData')
load('UpdatedPlays.Rdata')

targetPlays = plays[!is.na(plays$MatchedTargetReceiver),]

play = 75
game = 2018090600

sideBySidePassPlot(play,game,w1Data)
