## Bodo Winter
## April 25, 2015; Added new analyses: June 19, 2015
## Visualizations of Rapid Prosody Transcription Data


########################################################################
######################## Load in data:
########################################################################

## Path for main analysis:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load:

RPT <- read.csv("RPT_individual_processed.csv")

## Load in libraries needed:

library(lme4)



########################################################################
######################## Coding predictor variables:
########################################################################

## Take absolute variables for slope, treating strongly falling...
## ... and strongly rising the same:

RPT$PitchRangeST_abs <- abs(RPT$PitchRangeST)
RPT$PitchSlopeST_abs <- abs(RPT$PitchSlopeST)

## Z-transform variables:

RPT$MeanPitch_z <- scale(RPT$MeanPitch)
RPT$MaxPitch_z <- scale(RPT$MaxPitch)
RPT$RMS_amplitude_z <- scale(RPT$RMS_amplitude)
RPT$NSyll_z <- scale(RPT$NSyll)
RPT$SyllableDur_z <- scale(RPT$SyllableDur)
RPT$VowelDur_z <- scale(RPT$VowelDur)
RPT$LogFreq_z <- scale(RPT$LogFreq)
RPT$PitchRangeST_z <- scale(RPT$PitchRangeST)
RPT$PitchSlopeST_z <- scale(RPT$PitchSlopeST)
RPT$PitchRangeST_abs_z <- scale(RPT$PitchRangeST_abs)
RPT$PitchSlopeST_abs_z <- scale(RPT$PitchSlopeST_abs)

## Create contrast coded predictors:

RPT$Vowel_c <- RPT$Vowel
contrasts(RPT$Vowel_c) <- contr.sum(2)

RPT$POS_c <- RPT$POS
contrasts(RPT$POS_c) <- contr.sum(length(levels(RPT$POS)))

RPT$POS_class_c <- RPT$POS_class
contrasts(RPT$POS_class_c) <- contr.sum(2)

RPT$AccentPosition_c <- RPT$AccentPosition
contrasts(RPT$AccentPosition_c) <- contr.sum(length(levels(RPT$AccentPosition)))

RPT$AccentType_c <- RPT$AccentType
contrasts(RPT$AccentType_c) <- contr.sum(length(levels(RPT$AccentType)))

RPT$ListenerGender_c <- RPT$ListenerGender
contrasts(RPT$ListenerGender_c) <- contr.sum(2)

RPT$SpeakerGender_c <- RPT$SpeakerGender
contrasts(RPT$SpeakerGender_c) <- contr.sum(2)

## For Accent Type recode so that -1 = the middle prominence ("falling"):

contrasts(RPT$AccentType_c)[1,] <- c(-1,-1,-1,-1)
contrasts(RPT$AccentType_c)[5,] <- c(1,0,0,0)
# (this is not necessary for AccentPosition because -1 happened to be the most middle catgory anyway)

## Create a "word nested within sentence" variable:

RPT$WordWithinSentence <- paste(RPT$Word,RPT$Sentence,sep=":")



########################################################################
######################## Mixed model analysis:
########################################################################

## First, let's test the gender effects:

xmdl.Gender <- glmer(Prominence ~ ListenerGender*SpeakerGender +
	(1|Listener) + (0+SpeakerGender|Listener) +
	(1|Speaker) + (0+ListenerGender|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")

## MaxPitch model:

xmdl.MaxPitch <- glmer(Prominence ~ MaxPitch_z + 
	(1|Listener) + (0+MaxPitch_z|Listener) + 
	(1|Speaker) + (0+MaxPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.MaxPitch.Null <- glmer(Prominence ~ 1 + 
	(1|Listener) + (0+MaxPitch_z|Listener) + 
	(1|Speaker) + (0+MaxPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## MeanPitch model:

xmdl.MeanPitch = glmer(Prominence ~ MeanPitch_z +
	(1|Listener) + (0+MeanPitch_z|Listener) + 
	(1|Speaker) + (0+MeanPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.MeanPitch.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+MeanPitch_z|Listener) + 
	(1|Speaker) + (0+MeanPitch_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## RMS_amplitude:

xmdl.RMS_amplitude = glmer(Prominence ~ RMS_amplitude_z +
	(1|Listener) + (0+RMS_amplitude_z|Listener) + 
	(1|Speaker) + (0+RMS_amplitude_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.RMS_amplitude.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+RMS_amplitude_z|Listener) + 
	(1|Speaker) + (0+RMS_amplitude_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## VowelDur:

xmdl.VowelDur = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.VowelDur.Null = glmer(Prominence ~ VowelDur_z +
	(1|Listener) + (0+VowelDur_z|Listener) + 
	(1|Speaker) + (0+VowelDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## SyllableDur:

xmdl.SyllableDur = glmer(Prominence ~ SyllableDur_z +
	(1|Listener) + (0+SyllableDur_z|Listener) + 
	(1|Speaker) + (0+SyllableDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.SyllableDur.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+SyllableDur_z|Listener) + 
	(1|Speaker) + (0+SyllableDur_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## SyllableDur:

xmdl.NSyll = glmer(Prominence ~ NSyll_z +
	(1|Listener) + (0+NSyll_z|Listener) + 
	(1|Speaker) + (0+NSyll_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.NSyll.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+NSyll_z|Listener) + 
	(1|Speaker) + (0+NSyll_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## RangeST:

xmdl.RangeST_abs = RPT[complete.cases(RPT$PitchRangeST_abs),]
xmdl.RangeST = glmer(Prominence ~ PitchRangeST_abs_z +
	(1|Listener) + (0+PitchRangeST_abs_z|Listener) + 
	(1|Speaker) + (0+PitchRangeST_abs_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.RangeST.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+PitchRangeST_abs_z|Listener) + 
	(1|Speaker) + (0+PitchRangeST_abs_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## SlopeST:

RPT.SlopeST = RPT[complete.cases(RPT$PitchSlopeST_abs),]
xmdl.SlopeST = glmer(Prominence ~ PitchSlopeST_abs_z +
	(1|Listener) + (0+PitchSlopeST_abs_z|Listener) + 
	(1|Speaker) + (0+PitchSlopeST_abs_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.SlopeST.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+PitchSlopeST_abs_z|Listener) + 
	(1|Speaker) + (0+PitchSlopeST_abs_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## Log frequency:

xmdl.Freq = glmer(Prominence ~ LogFrequency_z +
	(1|Listener) + (0+LogFrequency_z|Listener) + 
	(1|Speaker) + (0+LogFrequency_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.Freq.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+LogFrequency_z|Listener) + 
	(1|Speaker) + (0+LogFrequency_z|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## Phonological Vowel Duration:

xmdl.Vowel = glmer(Prominence ~ Vowel_c +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.Vowel.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+Vowel_c|Listener) + 
	(1|Speaker) + (0+Vowel_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## Part of Speech class:

xmdl.POS_class = glmer(Prominence ~ POS_class_c +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.POS_class.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_class_c|Listener) + 
	(1|Speaker) + (0+POS_class_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## Part of Speech:

xmdl.POS = glmer(Prominence ~ POS_c +
	(1|Listener) + (0+POS_c|Listener) + 
	(1|Speaker) + (0+POS_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.POS.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+POS_c|Listener) + 
	(1|Speaker) + (0+POS_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## AccentType:

xmdl.AccentType = glmer(Prominence ~ AccentType_c +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.AccentType.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentType_c|Listener) + 
	(1|Speaker) + (0+AccentType_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")

## AccentPosition:

xmdl.AccentPosition = glmer(Prominence ~ AccentPosition_c +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
xmdl.AccentPosition.Null = glmer(Prominence ~ 1 +
	(1|Listener) + (0+AccentPosition_c|Listener) + 
	(1|Speaker) + (0+AccentPosition_c|Speaker) + 
	(1|WordWithinSentence) + (1|Sentence),
	RPT,family="binomial")
save.image("mixed_models.RData")




########################################################################
######################## Make plots:
########################################################################

## Write a function that computes fitted values and standard errors based on a single fixed effect object:

predict.logisticLMM = function(fit,min,max){
	xvals = seq(min,max,0.1)
	newdata = data.frame(xvals)
	names(newdata) = names(fixef(fit)[2])
	newdata$Prominence = 0
	mm = model.matrix(terms(fit),newdata)
	newdata$Prominence = predict(fit,newdata,re.form=NA)
	pvar1 = diag(mm %*% tcrossprod(vcov(fit),mm))
	newdata$UB = logit.inv(newdata$Prominence + 1.96*sqrt(pvar1))
	newdata$LB = logit.inv(newdata$Prominence - 1.96*sqrt(pvar1))
	return(newdata)
	}

predict.logistic_cat = function(fit,data,var="AccentPosition"){
	newdata = data.frame(unique(data[,var]))
	names(newdata) = var
	newdata$Prominence = 0
	mm = model.matrix(terms(fit),newdata)
	newdata$Prominence = predict(fit,newdata,re.form=NA)
	pvar1 = diag(mm %*% tcrossprod(vcov(fit),mm))
	newdata$UB = logit.inv(newdata$Prominence + 1.96*sqrt(pvar1))
	newdata$LB = logit.inv(newdata$Prominence - 1.96*sqrt(pvar1))
	return(newdata)
	}

## Write a function that creates nice plots for logistic models:

cont.PP = function(newdata,xlims,xaxis,xlabel){
	quartz("",9,6);par(mai=c(1.25,1.5,0.75,0.5))
	plot(1,1,type="n",xlim=xlims,ylim=c(-0.1,1.1),xaxs="i",xaxt="n",yaxt="n",xlab="",ylab="")
	axis(side=2,at=c(0,1),labels=c("not\nprominent","prominent"),las=2,font=2,lwd=4,lwd.ticks=4,cex.axis=1.25)
	axis(side=1,at=xaxis,labels=xaxis,font=2,cex.axis=1.25,lwd=4,lwd.ticks=4)
	mtext(side=1,line=3,text=xlabel,font=2,cex=1.5)
	polygon(c(newdata[,1],rev(newdata[,1])),
		c(newdata$UB,rev(newdata$LB)),col=rgb(0,0,0,0.3),border=F)
	points(newdata[,1],logit.inv(newdata$Prominence),lwd=5,type="l",col="darkred")
	abline(h=0,lty=2,lwd=4)
	abline(h=1,lty=2,lwd=4)
	box(lwd=4)
	}

cat.PP = function(newdata,xlabel){
	quartz("",9,6);par(mai=c(1.25,1.5,0.75,0.5))
	plot(1,1,type="n",
		xlim=c(1-0.5,nrow(newdata)+0.5),ylim=c(-0.1,1.1),
		xaxs="i",xaxt="n",yaxt="n",xlab="",ylab="")
	axis(side=2,at=c(0,1),labels=c("not\nprominent","prominent"),las=2,font=2,lwd=4,lwd.ticks=4,cex.axis=1.25)
	axis(side=1,at=1:nrow(newdata),labels=newdata[,1],font=2,cex.axis=1.25,lwd=4,lwd.ticks=4)
	mtext(xlabel,sid=1,line=3,font=2,cex=1.5)
	points(1:nrow(newdata),logit.inv(newdata$Prominence),pch=19,cex=1.5)
	arrows(1:nrow(newdata),y0=newdata$LB,y1=newdata$UB,angle=90,code=3,length=0.15,lwd=2)
	abline(h=0,lty=2,lwd=4)
	abline(h=1,lty=2,lwd=4)
	box(lwd=4)
	}


## MeanPitch:

xmdl.MeanPitch = glmer(Prominence ~ MeanPitch +
	(1+MeanPitch|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.MeanPitch,80,380)
cont.PP(newdata,xlims=c(80,380),xaxis=seq(80,380,20),xlabel="Mean Pitch (Hz, by Word)")

## MaxPitch:

xmdl.MaxPitch = glmer(Prominence ~ MaxPitch +
	(1+MaxPitch|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.MaxPitch,80,480)
cont.PP(newdata,xlims=c(80,480),xaxis=seq(80,480,80),xlabel="Max Pitch (Hz, by Word)")

## RangeST:

ind$AbsRang = abs(ind$RangeST)
indRange = ind[complete.cases(ind$RangeST),]
xmdl.AbsRang = glmer(Prominence ~ AbsRang +
	(1+AbsRang|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.AbsRang,0,15)
cont.PP(newdata,xlims=c(0,15),xaxis=seq(0,15,5),xlabel="Absolute Range (semitones, by Word)")

## RMS_amplitude:

xmdl.RMS = glmer(Prominence ~ RMS_amplitude +
	(1+RMS_amplitude|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.RMS,50,100)
cont.PP(newdata,xlims=c(50,100),xaxis=seq(50,100,25),xlabel="RMS Amplitude")

## Vowel Dur:

xmdl.VowelDur = glmer(Prominence ~ VowelDur +
	(1+VowelDur|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.VowelDur,0,250)
cont.PP(newdata,xlims=c(0,250),xaxis=seq(0,250,50),xlabel="Vowel Duration")

## LogFrequency:

xmdl.Freq = glmer(Prominence ~ LogFrequency +
	(1+LogFrequency|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.Freq,0,15)
cont.PP(newdata,xlims=c(0,15),xaxis=seq(0,15,2.5),xlabel="Log Frequency")

## Repetition:

xmdl.Rep = glmer(Prominence ~ WordRepetition +
	(1+WordRepetition|Speaker),
	ind,family="binomial")
newdata = predict.logisticLMM(xmdl.Rep,0,20)
cont.PP(newdata,xlims=c(0,10),xaxis=seq(0,10,2.5),xlabel="Word Repetition")

## CATGORICAL: Accent Position:

xmdl.Accent = glmer(Prominence ~ AccentPosition +
	(1|Speaker),
	ind,family="binomial")
newdata = predict.logistic_cat(xmdl.Accent,ind,"AccentPosition")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Accent Position")

## CATGORICAL: Accent Position:

xmdl.AccentType = glmer(Prominence ~ AccentType +
	(1|Speaker),
	ind,family="binomial")
newdata = predict.logistic_cat(xmdl.AccentType,ind,"AccentType")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Accent Type")

## CATGORICAL: Accent Position:

xmdl.POS = glmer(Prominence ~ POS_class +
	(1|Speaker),
	ind,family="binomial")
newdata = predict.logistic_cat(xmdl.POS,ind,"POS_class")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Part of Speech Type")

## CATGORICAL: Accent Position:

xmdl.Vowel = glmer(Prominence ~ Vowel +
	(1|Speaker),
	ind,family="binomial")
newdata = predict.logistic_cat(xmdl.Vowel,ind,"Vowel")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Phonological Vowel Length")



## This is the full model structure that I want to use when I have time:

xmdl.MeanPitch = glmer(Prominence ~ MeanPitch +
	(1+MeanPitch|Listener) + (1+MeanPitch|Speaker) +
	(1|NewSentenceIdentifiers) + (1|Word),
	ind,family="binomial")

