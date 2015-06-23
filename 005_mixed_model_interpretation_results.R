## Bodo Winter
## June 20, 2015
## Mixed model interpretation and analysis:

## Load mixed models:

load("/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/mixed_models_converged.RData")

## Do likelihood ratio tests:

anova(xmdl.MeanPitch.bobyqa,xmdl.MeanPitch.Null.bobyqa,test="Chisq")
anova(xmdl.MaxPitch.bobyqa,xmdl.MaxPitch.Null.bobyqa,test="Chisq")
anova(xmdl.RMS_amplitude,xmdl.RMS_amplitude.Null,test="Chisq")
anova(xmdl.VowelDur.bobyqa,xmdl.VowelDur.Null.bobyqa,test="Chisq")
anova(xmdl.SyllableDur.bobyqa,xmdl.SyllableDur.Null.bobyqa,test="Chisq")
anova(xmdl.NSyll,xmdl.NSyll.Null,test="Chisq")
anova(xmdl.RangeST,xmdl.RangeST.Null,test="Chisq")
anova(xmdl.SlopeST,xmdl.SlopeST.Null,test="Chisq")
anova(xmdl.Freq,xmdl.Freq.Null,test="Chisq")
anova(xmdl.Vowel.bobyqa,xmdl.Vowel.Null.bobyqa,test="Chisq")			# n.s
anova(xmdl.POS_class.bobyqa,xmdl.POS_class.Null.bobyqa,test="Chisq")

## Plot output:

summary(xmdl.MeanPitch.bobyqa)
summary(xmdl.MaxPitch.bobyqa)
summary(xmdl.RMS_amplitude)
summary(xmdl.VowelDur.bobyqa)
summary(xmdl.SyllableDur)
summary(xmdl.NSyll)
summary(xmdl.RangeST)
summary(xmdl.SlopeST)
summary(xmdl.Freq)
summary(xmdl.Vowel.bobyqa)
summary(xmdl.POS_class.bobyqa)

## Dunnsidak pvals:

dunnsidak = function(P,N){1 - ((1 - P) ^ N)}
dunnsidak(anova(xmdl.MeanPitch.bobyqa,xmdl.MeanPitch.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.MaxPitch.bobyqa,xmdl.MaxPitch.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.RMS_amplitude,xmdl.RMS_amplitude.Null,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.VowelDur.bobyqa,xmdl.VowelDur.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.SyllableDur.bobyqa,xmdl.SyllableDur.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.NSyll,xmdl.NSyll.Null,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.RangeST,xmdl.RangeST.Null,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.SlopeST,xmdl.SlopeST.Null,test="Chisq")$'Pr(>Chisq)',14)			# n.s.
dunnsidak(anova(xmdl.Freq,xmdl.Freq.Null,test="Chisq")$'Pr(>Chisq)',14)
dunnsidak(anova(xmdl.Vowel.bobyqa,xmdl.Vowel.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)		# n.s.
dunnsidak(anova(xmdl.POS_class.bobyqa,xmdl.POS_class.Null.bobyqa,test="Chisq")$'Pr(>Chisq)',14)


RPT <- read.csv("/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data/RPT_individual_processed.csv")

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

## Write a function that computes fitted values and standard errors based on a single fixed effect object:

predict.logisticLMM = function(fit,min,max){
	xvals = seq(min,max,0.1)
	newdata = data.frame(xvals)
	names(newdata) = names(fixef(fit)[2])
	newdata$Prominence = 0
	mm = model.matrix(terms(fit),newdata)
	newdata$Prominence = predict(fit,newdata,re.form=NA)
	pvar1 = diag(mm %*% tcrossprod(vcov(fit),mm))
	newdata$UB = plogis(newdata$Prominence + 1.96*sqrt(pvar1))
	newdata$LB = plogis(newdata$Prominence - 1.96*sqrt(pvar1))
	newdata$Prominence = plogis(newdata$Prominence)
	return(newdata)
	}

## Make a scatterplot of Frequency:

newdata <- predict.logisticLMM(xmdl.Freq,-2,2)

p <- ggplot(RPT,aes(LogFreq_z,jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nLogFrequency (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=LogFreq_z , ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=LogFreq_z,y=Prominence*100),color="black",size=1.5)

## Make a scatterplot of Amplitude:

newdata <- predict.logisticLMM(xmdl.RMS_amplitude,-2,2)

p <- ggplot(RPT,aes(jitter(RMS_amplitude_z,factor=0.25),jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nRMS Amplitude (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=RMS_amplitude_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=RMS_amplitude_z,y=Prominence*100),color="black",size=1.5)

## Make a scatterplot of Vowel Duration:

newdata <- predict.logisticLMM(xmdl.VowelDur.bobyqa,-2,2)

p <- ggplot(RPT,aes(jitter(VowelDur_z,factor=0.25),jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nVowel Duration (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=VowelDur_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=VowelDur_z,y=Prominence*100),color="black",size=1.5)

## Make a scatterplot of Syllable Duration:

newdata <- predict.logisticLMM(xmdl.SyllableDur.bobyqa,-2,2)

p <- ggplot(RPT,aes(jitter(SyllableDur_z,factor=0.25),jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nSyllable Duration (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=SyllableDur_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=SyllableDur_z,y=Prominence*100),color="black",size=1.5)

## Make a scatterplot of Range:

newdata <- predict.logisticLMM(xmdl.RangeST,-2,2)

p <- ggplot(RPT,aes(jitter(PitchRangeST_abs_z,factor=0.25),jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nAbsolute Pitch Range (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=PitchRangeST_abs_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=PitchRangeST_abs_z,y=Prominence*100),color="black",size=1.5)


## Make a scatterplot of Slope:

newdata <- predict.logisticLMM(xmdl.SlopeST,-2,2)

p <- ggplot(RPT,aes(jitter(PitchSlopeST_abs_z,factor=0.25),jitter(Prominence,factor=0.15)*100)) + 
	geom_point(shape=16,color="grey50",size=4,alpha=1/50) +
	coord_cartesian(ylim=c(-10,110),xlim=c(-2,2)) + 
	labs(y="Prominence",x="\nAbsolute Pitch Slope (z-scored)") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + 
geom_ribbon(data = newdata, aes(x=PitchSlopeST_abs_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=PitchSlopeST_abs_z,y=Prominence*100),color="black",size=1.5)





