## Bodo Winter
## By-individual analyses
## June 22, 2015

## Load mixed models:

load("/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/mixed_models_converged.RData")

## Generate a matrix with all the listeners' random effects:

allsubs <- data.frame(Subject=rownames(coef(xmdl.MeanPitch.bobyqa)$Listener),
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
	POS=coef(xmdl.POS_class.bobyqa)$Listener[,2]
	)

## Generate correlation matrix:

cor(allsubs[,-1])

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






summary(lm(MeanPitch ~ SyllableDur,allsubs))




quart
ggplot(allsubs,aes(x=MeanPitch,y=SyllableDur)) + geom_point(shape=16) + geom_smooth(method="lm")


library(mclust)
sub.Mclust <- Mclust(allsubs[,-1])

summary(sub.Mclust)
allsubs$Classification <- sub.Mclust$classification

select(allsubs,-Subject) %>% group_by(by=Classification) %>% summarise_each(funs(mean))




plot(sub.Mclust)

sub.k <- kmeans(allsubs[,-1])







geom_ribbon(data = newdata, aes(x=PitchSlopeST_abs_z, ymin=LB*100, ymax=UB*100 ), alpha=0.4 , fill="#dc7331") +
geom_line(data=newdata,aes(x=PitchSlopeST_abs_z,y=Prominence*100),color="black",size=1.5)






d <- dist(allsubs[,-1])
fit <- cmdscale(d,eig=T,k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x,y,xlab="Coordinate 1",ylab="Coordinate 2",
	main="Metric MDS",type="n")
text(x,y,allsubs$Classification)


plot(x,y,xlab="Coordinate 1",ylab="Coordinate 2",
	main="Metric MDS",type="n")
text(x,y,round(allsubs$POS,2))


names(d) <- names(allsubs)[-1]
hc <- hclust(d)
plot(hc,hang=-1)




coef(xmdl.MeanPitch.bobyqa)

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



