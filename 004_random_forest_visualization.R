## June 19, 2015
## Bodo Winter
## Analysis of random forest output

## Load in summary data:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
RPT <- read.csv("RPT_summary_processed.csv")

## Load random forest output:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
load("accented_forests.RData")
load("central_forests.RData")
load("KNN_forests.RData")
load("raw_forests.RData")
load("reduced_forests.RData")

## Which one works best?

cor(raw_predictions,RPT$p_score)
cor(central_predictions,RPT$p_score)
cor(KNN_predictions,RPT$p_score)
cor(red_predictions,RPT_red$p_score)
cor(accented_predictions,RPT_accented$p_score)

## Plot KNN variable importance:

KNN_varimp_conditional <- sort(KNN_varimp_conditional)
quartz("",9,6)
par(mai=c(1,2,0.25,0.5))
plot(1,1,type="n",bty="n",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(-10,350),ylim=c(0.25,14.25))
abline(h=1:14,col="darkgrey")
points(KNN_varimp_conditional,1:14,pch=19,cex=1.5)
axis(side=1,at=seq(0,350,50),font=2,cex.axis=1.25,lwd=2,lwd.ticks=2)
axis(side=2,at=1:14,las=2,font=2,tick=F,cex.axis=1.25,labels=names(KNN_varimp_conditional))
abline(v=0,lwd=2,lty=2)
mtext(side=1,"Variable Importance",line=3.25,cex=2,font=2)

## 



## Plot variable importances ...





########################################################################
######################## Conditional inference trees:
########################################################################

library(party)

## LogFrequency:

xtree = ctree(p_score ~ AccentPosition + AccentType + MeanPitch + MaxPitch + PitchRangeST + PitchSlopeST + POS_class +
	SyllableDur + VowelDur + Vowel + RMS_amplitude + LogFreq + LastArgument + Focused,RPT)
plot(xtree)

xtree = ctree(p_score ~ AccentPosition + AccentType + MeanPitch + MaxPitch + RangeST + SlopeST + POS_class +
	SyllableDur + VowelDur + Vowel + RMS_amplitude + Frequency + LastArgument + FocusParticle,RPT)
plot(xtree)



