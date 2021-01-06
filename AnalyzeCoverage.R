#Analyze Coverage

setwd('F:/BigDataBowl2021')
load('FinalPlays.Rdata')
load('CoverageData.Rdata')
source('InitialPlotting.R')

playerAnalysis = data.frame('DefenderName' = unique(coverageDf[!is.na(coverageDf$Def_Name),'Def_Name']))
playerAnalysis$NumberPlaysEnteredCoverage = NA
playerAnalysis$NumberComplete = NA
playerAnalysis$NumberIncomplete = NA
playerAnalysis$PercentComplete = NA
playerAnalysis$AverageYAC = NA
playerAnalysis$NumberINT = NA
playerAnalysis$NumberTD = NA
playerAnalysis$TotalYards = NA
playerAnalysis$Position = NA


for(i in 1:nrow(playerAnalysis)){
  playsWithPlayer = coverageDf[coverageDf$Def_Name == playerAnalysis[i,'DefenderName'],]
  
  playerAnalysis[i,'Position'] = playsWithPlayer[1,'Def_Pos']
  playerAnalysis[i,'NumberPlaysEnteredCoverage'] = nrow(playsWithPlayer)
  playerAnalysis[i,'NumberComplete'] = sum(playsWithPlayer$PassResult == 'C')
  playerAnalysis[i,'NumberIncomplete'] = sum(playsWithPlayer$PassResult == 'I')
  playerAnalysis[i,'NumberINT'] = sum(playsWithPlayer$PassResult == 'IN')
  playerAnalysis[i,'NumberTD'] = sum(playsWithPlayer$TD)
  playerAnalysis[i,'TotalYards'] = sum(playsWithPlayer$PlayResult)
  playerAnalysis[i,'PercentComplete'] = playerAnalysis[i,'NumberComplete']/(playerAnalysis[i,'NumberIncomplete'] + playerAnalysis[i,'NumberComplete'])
  playerAnalysis[i,'AverageYAC'] = mean(playsWithPlayer[!is.na(playsWithPlayer$YAC),'YAC'])
}


min10Df = playerAnalysis[playerAnalysis$NumberPlaysEnteredCoverage >= 10,]
min10Df[order(min10Df$PercentComplete,decreasing = F),]

min20Df = playerAnalysis[playerAnalysis$NumberPlaysEnteredCoverage >= 20,]
min20Df[order(min20Df$PercentComplete,decreasing = F),]
min20Df[order(min20Df$AverageYAC,decreasing = F),]

playerAnalysis[order(playerAnalysis$NumberPlaysEnteredCoverage,decreasing = T),]

plot(min20Df[,c('AverageYAC','PercentComplete')])
# text(min20Df$AverageYAC,min20Df$PercentComplete,min20Df$DefenderName)
text(min20Df$AverageYAC,min20Df$PercentComplete,min20Df$Position)




passRating <- function(NumAtts,NumComp,TotYards,TotTDs,TotINTs){
  a = (NumComp/NumAtts - .3) * 5
  b = (TotYards/NumAtts - 3) * .25
  c = (TotTDs/NumAtts) * 20
  d = 2.375 - (TotINTs/NumAtts) * 25
  thresh = 2.375
  a = ifelse(a>thresh,thresh,a)
  b = ifelse(b>thresh,thresh,b)
  c = ifelse(c>thresh,thresh,c)
  d = ifelse(d>thresh,thresh,d)
  return((a + b + c + d)/6 * 100)
}

min20Df$PasserRating = NA
for(i in 1:nrow(min20Df)){
  min20Df[i,'PasserRating'] = passRating(NumAtts = min20Df[i,'NumberPlaysEnteredCoverage'],NumComp = min20Df[i,'NumberComplete'],TotYards = min20Df[i,'TotalYards'],
                                         TotTDs = min20Df[i,'NumberTD'],TotINTs = min20Df[i,'NumberINT'])
}

min20Df[order(min20Df$PasserRating,decreasing = F),]


ECAT = min20Df[,c("DefenderName","NumberPlaysEnteredCoverage","PercentComplete","AverageYAC" , "Position" )]
colnames(ECAT) <- c('Defender Name','ECAT Plays','CMP%','YAC/p','Position')
save(ECAT,file = 'ECATData.Rdata')

