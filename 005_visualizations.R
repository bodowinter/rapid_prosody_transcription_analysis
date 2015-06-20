## Bodo Winter
## April 29, 2015; Major overhaul June 19, 2015
## Analysis of already computed mixed model data

########################################################################
######################## Preliminaries
########################################################################

## Load required packages:

library(lme4)
library(ggplot2)
library(RColorBrewer)

## Path for main analysis:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load in RPT summary data:

RPT <- read.csv("RPT_summary_processed.csv")

## Load in extra function:

setwd("..")
source("summarySE_function.R")



########################################################################
######################## Plots requested by Stefan #1: AccentType*AccentPosition
########################################################################

## Define data frame:

subs <- filter(RPT,AccentPosition %in% c("prenuclear","nuclear_ip","nuclear_IP"))
subs <- summarySE(subs,measurevar="p_score",groupvars=c("AccentPosition","AccentType"))
subs$AccentType <- factor(as.character(subs$AccentType),
	levels=c("low","falling","high","rising"))
subs$AccentPosition <- factor(as.character(subs$AccentPosition),
	levels=c("prenuclear","nuclear_ip","nuclear_IP"))
levels(subs$AccentPosition) = c("Prenuclear","Nuclear ip","Nuclear IP")

## The plot:

quartz("",9,5)
p <- ggplot(subs,
	aes(x=AccentType,y=p_score,fill=AccentType))
p + geom_bar(stat="identity",width=1,color="black") + facet_wrap(~AccentPosition) +
	geom_errorbar(aes(ymin=p_score-se,ymax=p_score+se),width=0.2) + 
	scale_fill_brewer(palette=7) + coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nAccent Position") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))



########################################################################
######################## Plots requested by Stefan #2: All POS
########################################################################

## Define data frame:

subs <- summarySE(RPT,measurevar="p_score",groupvars=c("POS","POS_class"))
subs <- arrange(subs,p_score)
subs$POS <- factor(as.character(subs$POS),
	levels=as.character(subs$POS))

## The plot:

quartz("",9,5)
ggplot(subs,
	aes(x=POS,y=p_score)) + geom_bar(stat="identity",width=1,color="black",fill="goldenrod3") + 
	geom_errorbar(aes(ymin=p_score-se,ymax=p_score+se),width=0.2) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nPart of Speech") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))



########################################################################
######################## Mixed model plots:
########################################################################

## Load in data:

load("mixed_model_results.RData")

########################################################################
######################## For reporting, check models:
########################################################################

summary(xmdl.MeanPitch)
summary(xmdl.MaxPitch)
summary(xmdl.RangeST)
summary(xmdl.SlopeST)
summary(xmdl.RMS_amplitude)
summary(xmdl.SyllableDur)
summary(xmdl.VowelDur)
summary(xmdl.Freq)

## Models with categorical fixed effects:

summary(xmdl.POS)
summary(xmdl.Vowel)
summary(xmdl.AccentPosition)
summary(xmdl.AccentType)


########################################################################
######################## Write functions for making plots:
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
	abline(h=0,lty=2,lwd=4)
	abline(h=1,lty=2,lwd=4)
	axis(side=2,at=c(0,1),labels=c("not\nprominent","prominent"),las=2,font=2,lwd=4,lwd.ticks=4,cex.axis=1.25)
	axis(side=1,at=xaxis,labels=xaxis,font=2,cex.axis=1.5,lwd=4,lwd.ticks=4)
	mtext(side=1,line=4.5,text=xlabel,font=2,cex=1.5)
	polygon(c(newdata[,1],rev(newdata[,1])),
		c(newdata$UB,rev(newdata$LB)),col=colors()[38],border=F)
	points(newdata[,1],logit.inv(newdata$Prominence),lwd=6,type="l",col="darkred")
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
	points(1:nrow(newdata),logit.inv(newdata$Prominence),pch=15,cex=1.5)
	arrows(1:nrow(newdata),y0=newdata$LB,y1=newdata$UB,angle=90,code=3,length=0.15,lwd=2)
	abline(h=0,lty=2,lwd=4)
	abline(h=1,lty=2,lwd=4)
	box(lwd=4)
	}


########################################################################
######################## Make the plots:
########################################################################

## MeanPitch:

newdata = predict.logisticLMM(xmdl.MeanPitch,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Mean Pitch\n(z-scores)")

## MaxPitch:

newdata = predict.logisticLMM(xmdl.MaxPitch,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Max Pitch\n(z-scores)")

## RangeST:

newdata = predict.logisticLMM(xmdl.RangeST,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Absolute Pitch Range\n(z-scores)")

## SlopeST:

newdata = predict.logisticLMM(xmdl.SlopeST,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Absolute Pitch Slope\n(z-scores)")

## RMS amplitude:

newdata = predict.logisticLMM(xmdl.RMS_amplitude,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="RMS Amplitude\n(z-scores)")

## Vowel Duration:

newdata = predict.logisticLMM(xmdl.VowelDur,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Vowel Duration\n(z-scores)")

## Syllable Duration:

newdata = predict.logisticLMM(xmdl.SyllableDur,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Syllable Duration\n(z-scores)")

## Log Frequency:

newdata = predict.logisticLMM(xmdl.Freq,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="SUBTLEX Log Frequency\n(z-scores)")

## Repetition (non-significant):

newdata = predict.logisticLMM(xmdl.WordRepetition,-2,2)
cont.PP(newdata,xlims=c(-2,2),xaxis=seq(-2,2,1),xlabel="Word Repetition\n(z-scores)")

## CATEGORICAL: Accent Position:

newdata = predict.logistic_cat(xmdl.AccentPosition,xdata,"AccentPosition_c")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Accent Position")

## CATEGORICAL: Accent Type:

newdata = predict.logistic_cat(xmdl.AccentType,xdata,"AccentType_c")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Accent Type")

## CATEGORICAL: POS:

newdata = predict.logistic_cat(xmdl.POS,xdata,"POS_class_c")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Part of Speech Type")

## CATEGORICAL: Phonological Vowel Length:

newdata = predict.logistic_cat(xmdl.Vowel,xdata,"Vowel_c")
newdata = newdata[order(newdata$Prominence),]
cat.PP(newdata,"Phonological Vowel Length")


########################################################################
######################## Make a table with Z-Scores of the continuous variables:
########################################################################

z_vals = c(fixef(xmdl.MeanPitch)[2],fixef(xmdl.MaxPitch)[2],
	fixef(xmdl.RangeST)[2],fixef(xmdl.SlopeST)[2],fixef(xmdl.RMS_amplitude)[2],
	fixef(xmdl.VowelDur)[2],fixef(xmdl.SyllableDur)[2],fixef(xmdl.Freq)[2],
	fixef(xmdl.WordRepetition)[2])
variable_names = names(z_vals)
variable_names = gsub("_z","",variable_names)

## Put this in a table:

summary_res = data.frame(variable=variable_names,z_vals)

## Sort by z-score:

summary_res = summary_res[order(summary_res$z_vals,decreasing=T),]

## Create an odds column:

summary_res$Odds = exp(summary_res$z_vals)

## Write this into a table:

write.table(summary_res,"summary_results_continuous_variables.csv",sep=",",row.names=F)



########################################################################
######################## Conditional inference trees:
########################################################################

library(party)
setwd("/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/summary_analysis_random_forests_pscore/")
RPT = read.csv("RPT_summary.csv")

## LogFrequency:

RPT$Frequency = log(RPT$Frequency+1)

xtree = ctree(p_score ~ AccentPosition + AccentType + MeanPitch + MaxPitch + RangeST + SlopeST + POS_class +
	SyllableDur + VowelDur + Vowel + RMS_amplitude + Frequency + LastArgument + FocusParticle,RPT)
plot(xtree)

xtree = ctree(p_score ~ AccentPosition + AccentType + MeanPitch + MaxPitch + RangeST + SlopeST + POS_class +
	SyllableDur + VowelDur + Vowel + RMS_amplitude + Frequency + LastArgument + FocusParticle,RPT)
plot(xtree)

