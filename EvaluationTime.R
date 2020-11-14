#Evaluation Time

setwd('F:/BigDataBowl2021')
load('Plays_Week_Targets_YAC.Rdata')

YACPlays = plays[!is.na(plays$YAC),]


