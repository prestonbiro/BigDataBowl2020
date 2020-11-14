#CombineData
setwd('F:/BigDataBowl2021')

w1Data = read.csv('week1.csv')
w2Data = read.csv('week2.csv')
w3Data = read.csv('week3.csv')
w4Data = read.csv('week4.csv')
w5Data = read.csv('week5.csv')
w6Data = read.csv('week6.csv')
w7Data = read.csv('week7.csv')
w8Data = read.csv('week8.csv')
w9Data = read.csv('week9.csv')
w10Data = read.csv('week10.csv')
w11Data = read.csv('week11.csv')
w12Data = read.csv('week12.csv')
w13Data = read.csv('week13.csv')
w14Data = read.csv('week14.csv')
w15Data = read.csv('week15.csv')
w16Data = read.csv('week16.csv')
w17Data = read.csv('week17.csv')

allWeeksData = rbind(w1Data,w2Data,w3Data,w4Data,
                     w5Data,w6Data,w7Data,w8Data,
                     w9Data,w10Data,w11Data,w12Data,
                     w13Data,w4Data,w15Data,w16Data,w17Data)

save(allWeeksData,file = 'Combined2018PassingData.RData')

rm(list = ls())

load('Combined2018PassingData.RData')
