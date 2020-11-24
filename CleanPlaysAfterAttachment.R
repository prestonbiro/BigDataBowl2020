#Fix Trimmed Data

setwd('F:/BigDataBowl2021')
load('Plays_All_Offense_Defense_Ball.Rdata')

droppedFrames = subset(trimmedPlays,select = -c(defender1dir,defender2dir,defender3dir))

rightClass = droppedFrames
rightClass$defender1X = as.numeric(rightClass$defender1X)
rightClass$defender1Y = as.numeric(rightClass$defender1Y)
rightClass$defender1S = as.numeric(rightClass$defender1S)
rightClass$defender1Dir = as.numeric(rightClass$defender1Dir)
rightClass$defender1A = as.numeric(rightClass$defender1A)
rightClass$defender1O = as.numeric(rightClass$defender1O)
rightClass$defender1Dist = as.numeric(rightClass$defender1Dist)

rightClass$defender2X = as.numeric(rightClass$defender2X)
rightClass$defender2Y = as.numeric(rightClass$defender2Y)
rightClass$defender2S = as.numeric(rightClass$defender2S)
rightClass$defender2Dir = as.numeric(rightClass$defender2Dir)
rightClass$defender2A = as.numeric(rightClass$defender2A)
rightClass$defender2O = as.numeric(rightClass$defender2O)
rightClass$defender2Dist = as.numeric(rightClass$defender2Dist)

rightClass$defender3X = as.numeric(rightClass$defender3X)
rightClass$defender3Y = as.numeric(rightClass$defender3Y)
rightClass$defender3S = as.numeric(rightClass$defender3S)
rightClass$defender3Dir = as.numeric(rightClass$defender3Dir)
rightClass$defender3A = as.numeric(rightClass$defender3A)
rightClass$defender3O = as.numeric(rightClass$defender3O)
rightClass$defender3Dist = as.numeric(rightClass$defender3Dist)

playsToUse = rightClass

save(playsToUse,file='FinalPlays.Rdata')
