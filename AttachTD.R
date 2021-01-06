#Attach TD Indicator

load('Plays_All_Offense_Defense_Ball.Rdata')
load('CoverageData.Rdata')

playsToUse$TD = NA
for(i in 1:nrow(playsToUse)){
  playsToUse[i,'TD'] = grepl('TOUCHDOWN',playsToUse[i,'playDescription'],fixed = T)
}

coverageDf$TD = NA
for(i in 1:nrow(coverageDf)){
  playNum = coverageDf[i,'Play']
  gameNum = coverageDf[i,'Game']
  p = findPlay(playNum,gameNum,playsToUse)
  coverageDf[i,'TD'] = p$TD
}

