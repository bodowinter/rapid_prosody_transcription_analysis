## Bodo Winter
## April 29, 2015; Major overhaul June 19, 2015
## Analysis of already computed mixed model data

########################################################################
######################## Preliminaries
########################################################################

## Load required packages:

library(lme4)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

## Path for main analysis:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load in RPT summary data:

RPT <- read.csv("RPT_summary_processed.csv")



########################################################################
######################## Descriptive plots for presentation:
########################################################################

## Make a boxplot of just Accent Type:

RPT$AccentType = factor(RPT$AccentType,levels=c("no_accent","low","falling","high","rising"))
quartz("",9,5)
ggplot(RPT,aes(AccentType,p_score,fill=AccentType)) + geom_boxplot() +
	scale_fill_brewer(palette=7) + coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nAccent Type") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))
ggsave("plot1.png")

## Make a boxplot of just Accent Position:

RPT$AccentPosition = factor(RPT$AccentPosition,levels=c("no_accent","postnuclear","prenuclear","nuclear_ip","nuclear_IP"))
quartz("",9,5)
ggplot(RPT,aes(AccentPosition,p_score,fill=AccentPosition)) + geom_boxplot() +
	scale_fill_brewer(palette=7) + coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nAccent Position") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))
ggsave("plot2.png")

## Make a boxplot of number of syllables:

subs<-filter(RPT,NSyll<5)
subs$NSyll = factor(subs$NSyll)
quartz("",9,5)
ggplot(subs,aes(NSyll,p_score,fill=NSyll)) + geom_boxplot() +
	scale_fill_brewer(palette=7) + coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nNumber of syllables") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x=element_text(size=20,face="bold",vjust=2))
ggsave("plot3.png")

## Make a boxplot of Last Argument:

RPT$LastArgument <- factor(ifelse(as.character(RPT$LastArgument)=="not_last","no","yes"))

quartz("",5,5)
ggplot(RPT,aes(LastArgument,p_score,fill=LastArgument)) + geom_boxplot() +
	scale_fill_manual(values=c("#f6b884","#dc7331")) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nLast Argument") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave("plot4.png")

## Make a boxplot of Phonological Vowel Length:

RPT$Vowel = factor(RPT$Vowel,levels=c("short","long"))

quartz("",5,5)
ggplot(RPT,aes(Vowel,p_score,fill=Vowel)) + geom_boxplot() +
	scale_fill_manual(values=c("#f6b884","#dc7331")) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nPhonological Vowel Length") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave("plot5.png")

## Make a boxplot of Focused:

RPT$Focused <- ifelse(as.character(RPT$Focused)=="focus_particle","Focused","Not Focused")
RPT$Focused <- factor(RPT$Focused,levels=c("Not Focused","Focused"))

quartz("",5,5)
ggplot(RPT,aes(Focused,p_score,fill=Focused)) + geom_boxplot() +
	scale_fill_manual(values=c("#f6b884","#dc7331")) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nWord follows focus particle") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave("plot6.png")


########################################################################
######################## Plots requested by Stefan #1: AccentType*AccentPosition
########################################################################

## Define data frame:

filter(RPT,AccentPosition %in% c("prenuclear","nuclear_ip","nuclear_IP")) %>%
	group_by(AccentPosition,AccentType) %>%
	summarise(SD=sd(p_score),p_score=mean(p_score),count=n()) %>%
	mutate(SE=SD/sqrt(count)) -> subs
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
	geom_errorbar(aes(ymin=p_score-SE,ymax=p_score+SE),width=0.2) + 
	scale_fill_brewer(palette=7) + coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nAccent Position") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))
ggsave("plot7.png")


########################################################################
######################## Plots requested by Stefan #2: All POS
########################################################################

## Define data frame:

group_by(RPT,POS) %>%
	summarise(SD=sd(p_score),p_score=mean(p_score),count=n()) %>%
	mutate(SE=SD/sqrt(count)) -> subs
subs <- arrange(subs,p_score)
subs$POS <- factor(as.character(subs$POS),
	levels=as.character(subs$POS))

## The plot:

quartz("",9,5)
ggplot(subs,
	aes(x=POS,y=p_score,fill=POS)) + geom_bar(stat="identity",width=1,color="black") +
	scale_fill_manual(values=colorRampPalette(c("#fdf0e0","#f28c3a","#a5361d"))(12)) +
	geom_errorbar(aes(ymin=p_score-SE,ymax=p_score+SE),width=0.2) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nPart of Speech") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x=element_text(size=20,face="bold",vjust=2),
		legend.position="none")
ggsave("plot8.png")

########################################################################
######################## Plots requested by Stefan #3: Focus particle
########################################################################

filter(RPT,AccentPosition %in% c("postnuclear","nuclear_IP")) %>%
	group_by(AccentPosition,Focused) %>%
	summarise(SD=sd(p_score),p_score=mean(p_score),count=n()) %>%
	mutate(SE=SD/sqrt(count)) -> subs
subs$Focused <- ifelse(as.character(subs$Focused)=="focus_particle","Focused","Not Focused")
subs$AccentPosition <- ifelse(as.character(subs$AccentPosition)=="nuclear_IP","Nuclear IP","Postnuclear")
subs$AccentPosition <- factor(subs$AccentPosition,levels=c("Postnuclear","Nuclear IP"))
subs$Focused <- factor(subs$Focused,levels=c("Not Focused","Focused"))

## The plot:

quartz("",9,5)
p <- ggplot(subs,
	aes(x=Focused,y=p_score,fill=AccentPosition))
p + geom_bar(stat="identity",width=1,color="black",position="dodge") + facet_wrap(~AccentPosition) +
	geom_errorbar(aes(ymin=p_score-SE,ymax=p_score+SE),width=0.2) + 
	scale_fill_manual(values=c("#f6b884","#dc7331")) + 
	coord_cartesian(ylim=c(0,100)) + 
	labs(y="Prominence Score",x="\nAccent Position") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2),
		legend.position="none")


