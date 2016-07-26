## Bodo Winter
## By-individual analyses
## June 22, 2015

## Load mixed models:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)
load("../mixed_models.RData")

library(ggplot2)
library(grid)

## Generate a matrix with all the listeners' random effects:

allsubs <- data.frame(Subject=rownames(coef(xmdl.MeanPitch.bobyqa)$Listener),
	Accented=coef(xmdl.Accented.bobyqa)$Listener[,2],		# for more accented
	MeanPitch=coef(xmdl.MeanPitch.bobyqa)$Listener[,2],
	MaxPitch=coef(xmdl.MaxPitch.bobyqa)$Listener[,2],
	Amplitude=coef(xmdl.RMS_amplitude)$Listener[,2],
	VowelDur=coef(xmdl.VowelDur.bobyqa)$Listener[,2],
	SyllableDur=coef(xmdl.SyllableDur.bobyqa)$Listener[,2],
	NSyll=coef(xmdl.NSyll)$Listener[,2],
	Range=coef(xmdl.RangeST)$Listener[,2],
	Slope=coef(xmdl.SlopeST)$Listener[,2],
	Freq=coef(xmdl.Freq)$Listener[,2],
	Vowel=coef(xmdl.Vowel.bobyqa)$Listener[,2],
	POS=coef(xmdl.POS_class.bobyqa)$Listener[,1],			# for content
	Focused=coef(xmdl.Focused.bobyqa)$Listener[,1],			# for focus particle
	Argument=coef(xmdl.argument.bobyqa)$Listener[,1]			# for last argument
	)

## Generate correlation matrix:

cor(allsubs[,-1])
round(cor(allsubs[,c("Accented","Focused","Argument","POS")]),2)
round(cor(allsubs[,c("POS","MaxPitch","Amplitude","VowelDur")]),2)

## Plot pitch against duration:

p <- ggplot(allsubs,aes(x=MeanPitch,y=SyllableDur)) + 
	labs(x="\nPitch coefficient",y="Duration coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot9.png")

summary(lm(MeanPitch ~ SyllableDur,allsubs))

## Plot POS against amplitude:

p <- ggplot(allsubs,aes(x=POS,y=Amplitude)) + 
	labs(x="\nPOS coefficient",y="Amplitude coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot10.png")

summary(lm(Amplitude ~ POS,allsubs))

## Plot POS against range:

p <- ggplot(allsubs,aes(x=POS,y=Range)) + 
	labs(x="\nPOS coefficient",y="Pitch Range coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot11.png")

## Plot Focused against Last Argument:

p <- ggplot(allsubs,aes(x=Focused,y=Argument)) + 
	labs(x="\nFocus particle coefficient",y="Last Argument coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot12.png")

## Plot accented vs. argument:

p <- ggplot(allsubs,aes(x=Accented,y=Argument)) + 
	labs(x="\nAccented or not coefficient",y="Last Argument coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot13.png")


## Plot accented vs. argument:

p <- ggplot(allsubs,aes(x=Accented,y=Focused)) + 
	labs(x="\nAccented or not coefficient",y="Focus particle coefficient\n") + 
	theme_minimal() + theme(axis.text.y=element_text(face="bold"),
		axis.title.y=element_text(face="bold",size=20),
		axis.title.x=element_text(face="bold",size=20),
		axis.text.x=element_text(face="bold"),
		axis.title.x=element_text(face="bold"),
		strip.text.x = element_text(size=20,face="bold",vjust=2))

quartz("",9,5)
p + geom_smooth(method="lm",fill="#dc7331",color="black") + geom_point(shape=16,size=4)
ggsave("plot14.png")


## Multidimensional scaling

d <- dist(allsubs[,-1])
fit <- cmdscale(d,eig=T,k=2)
x <- fit$points[,1]
y <- fit$points[,2]
quartz("",8,6);par(mai=c(1.5,1.5,0.5,0.5))
plot(x,y,main="Metric MDS",type="n",xlab="",ylab="",xlim=c(-1.5,4),ylim=c(-2.5,1.5),xaxt="n",yaxt="n")
axis(side=1,seq(-1.5,4,0.5),font=2,lwd.ticks=3,cex=1.15)
axis(side=2,seq(-2.5,1.5,0.5),font=2,lwd.ticks=3,las=2,cex=1.15)
mtext(side=1,line=3,font=2,"Coordinate 1",cex=2)
mtext(side=2,line=3,font=2,"Coordinate 2",cex=2)
text(x,y,labels=allsubs$Subject,font=2,cex=1.15)
box(lwd=3)

## Cluster:

set.seed(42)

## Determine how many clusters are needed:

all_res = c()
for(i in 2:10){
	all_res = c(all_res,sum(kmeans(allsubs[,-1],centers=i)$withinss))
	}
quartz("",8,6);plot(2:10,all_res,type="b",xlab="Number of clusters")		# not a particularly good fit

## Make a plot of that:

myk <- kmeans(allsubs[,-1],centers=2,nstart=1000)




