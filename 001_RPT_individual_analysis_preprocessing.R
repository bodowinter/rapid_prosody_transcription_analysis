## Bodo Winter
## March 12, 2015; Major overhaul June 19, 2015
## Preprocessing script

########################################################################
######################## Load in data:
########################################################################

## Load in libraries:

library(reshape2)
library(dplyr)
library(xlsx)

## Path for main analysis:

mainDir <- "/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data"
setwd(mainDir)

## Load in individual data (wide format):

wide <- read.xlsx("rpt_Einzelwerte-25juli2014-1.xls",
	sheetIndex=2,stringsAsFactors=F)

## Load in summary data:

RPT <- read.xlsx("rpt-Daten-15juli2015.xls",
	sheetIndex=1,
	stringsAsFactors=F)

## Set working directory to additional information folder and load everything in:

setwd(file.path(mainDir,"additional_information"))
blocks <- read.csv("block_order_information.csv")
listener_gender <- read.csv("listener_gender_info.csv")
speaker_gender <- read.csv("speaker_gender_info.csv")



########################################################################
######################## Cleaning of "RPT":
########################################################################

## Delete last row (is just empty):

RPT <- RPT[-nrow(RPT),]

## Make column names English:

RPT <- rename(RPT,
	Word=Wort,
	NSyll=lex..Wortlänge,
	NSyllRealized=reale.Wortlänge,
	Sentence=Satz,
	Speaker=Sprecher,
	AccentPosition=Akzentposition,
	AccentType=Akzenttyp,
	MeanPitch=mean.pitch,
	MaxPitch=max.pitch,
	PitchRangeST=range.in.st,
	PitchSlopeST=slope.st.s,
	POS_class=POS_Klasse,
	SyllableDur=Dauer_Silbe,
	VowelDur=Dauer_Vokal,
	Vowel=Vokal,
	RMS_amplitude=RMS,
	Freq=Worthäufigkeit,
	LastArgument=letztesArgument,
	Focused=Fokuspartikel)

## Put in more descriptive labels for "LastArgument" and "FocusParticle" column:

RPT$LastArgument = as.factor(ifelse(as.numeric(RPT$LastArgument),"last","not_last"))
RPT$Focused = as.factor(ifelse(as.numeric(RPT$Focused),"focus_particle","no_focus_particle"))

## Get rid of "--undefined--" tags:

RPT[RPT$MeanPitch=="--undefined--",]$MeanPitch <- NA
RPT[RPT$MaxPitch=="--undefined--",]$MaxPitch <- NA

## Add speaker gender info:

speaker_gender$gender <- toupper(speaker_gender$gender)
RPT$SpeakerGender <- speaker_gender[match(RPT$Speaker,speaker_gender$speaker),]$gender

## Log-transform frequency data:

RPT$LogFreq <- log10(as.numeric(RPT$Freq)+1)



########################################################################
######################## Cleaning of "wide":
########################################################################

## The first row just contains the block order information, delete this:

wide <- wide[-1,]

## The last column is just NA's:

wide <- wide[,-ncol(wide)]

## Make column names English:

wide <- rename(wide,
	Sentence=Satz,
	Word=Wort)



########################################################################
######################## Create unique sentence identifiers:
########################################################################

## Create a conglomerate block/sentence ID variable ... 
## ... we will loop through that variable and check whether there ...
## ... is any sentence that has the same words (spoken by a different speaker):

RPT$BlockSent <- paste(RPT$Block,RPT$Sentence,sep=":")

## Create a set of sentence identifiers to assign to unique sentences later in the loop:

NewSentenceID <- paste0(rep(LETTERS,3),1:(26*3))

## Create an empty sentence ID column:

RPT$NewSentenceID <- rep(NA,nrow(RPT))

## Cycle through each sentence and see whether that sentence is a repeat:

for(i in 1:length(unique(RPT$BlockSent))){
	
	## Pick the block sentence variable:
	
	thisBlockSent <- unique(RPT$BlockSent)[i]

	## Pick all words in this sentence:

	unlist(filter(RPT,BlockSent==thisBlockSent) %>% select(Word)) -> thisWordBag
	
	## Pick all sentences that ar not the current sentence:
	
	allOtherBlockSent <- unique(RPT$BlockSent)[-i]

	## If it's filled with NAs, that means that the sentence has not been assigned yet:
	## Make that a Boolean variable, only those that are full of NA's have be dealt with:

	NAcondition <- all(is.na(RPT[RPT$BlockSent == thisBlockSent,]$NewSentenceID))
	
	if(NAcondition){
		## Pick the next sentence identifier from the list of identifiers:
		
		RPT[RPT$BlockSent == thisBlockSent,]$NewSentenceID <- NewSentenceID[i]

		## Check all other sentences whether there's any exact match.
		## If yes, they get the same identifier:

		for(j in 1:length(allOtherBlockSent)){
			thisComparisonBlockSent <- allOtherBlockSent[j]
			thisComparisonWordBag <- RPT[RPT$BlockSent == thisComparisonBlockSent,]$Word
			if(all(thisWordBag %in% thisComparisonWordBag)){
				RPT[RPT$BlockSent == thisComparisonBlockSent,]$NewSentenceID <- NewSentenceID[i]
			}
		}
	}
}

## How many did each sentence occur in the experiment?

apply(table(RPT$NewSentenceID,RPT$Speaker),1,FUN=function(x){sum(x!=0)})



########################################################################
######################## Make "wide" into long format and append info:
########################################################################

## Melt wide file into long format:

long <- melt(wide,id.vars=c("Word","Sentence","Block"))

## Rename:

long <- rename(long,
	Listener=variable,
	Prominence=value)

## Clean listener gender information:

listener_gender$Names <- colnames(wide)[-c(1:3)]		# they appear in order of columns
listener_gender$gender <- toupper(listener_gender$gender)

## Add listener gender information to long data frame:

long$ListenerGender <- listener_gender[match(long$Listener,listener_gender$Names),]$gender

## Create a pasted Block/Sentence/Word variable for merging with the RPT information:

long$MatcherID <- paste(paste(long$Block,RPT$Sentence,sep=":"),long$Word,sep=":")
RPT$MatcherID <- paste(paste(RPT$Block,RPT$Sentence,sep=":"),RPT$Word,sep=":")

## Merge:

not_these_columns <- c("Block","Word","Sentence","MatcherID")
not_these_columns <- colnames(RPT) %in% not_these_columns
long <- cbind(long,RPT[match(long$MatcherID,RPT$MatcherID),!not_these_columns])

## Get rid of the MatcherID column that was in the long file:

long <- select(long,-MatcherID)

## "Zu" occurs twice in Sentence 5, Block 3...
## ... but the "match" function only takes the first pick...
## ... so we need to override that:

this_zu <- which(RPT$Word=="zu")[3]
long_zus <- which(long$Word=="zu")
long_zus <- long_zus[seq(along=long_zus)%%3==0]
long[long_zus,7:ncol(long)] <- RPT[this_zu,!not_these_columns]



########################################################################
######################## Re-ordering all columns:
########################################################################

RPT <- select(RPT,
	Block,Speaker,SpeakerGender,Sentence,NewSentenceID,Word,
	POS,POS_class,NSyll,NSyllRealized,Vowel,Freq,LogFreq,
	AccentPosition,AccentType,
	LastArgument,Focused,	
	MeanPitch,MaxPitch,PitchRangeST,PitchSlopeST,
	RMS_amplitude,
	SyllableDur,VowelDur,p_score)

long <- select(long,
	Listener,ListenerGender,
	Block,Speaker,SpeakerGender,Sentence,NewSentenceID,Word,
	POS,POS_class,NSyll,NSyllRealized,Vowel,Freq,LogFreq,
	AccentPosition,AccentType,
	LastArgument,Focused,	
	MeanPitch,MaxPitch,PitchRangeST,PitchSlopeST,
	RMS_amplitude,
	SyllableDur,VowelDur,Prominence)
	


########################################################################
######################## Write data:
########################################################################

setwd(mainDir)
write.table(RPT,"RPT_summary_processed.csv",sep=",",row.names=F)
write.table(long,"RPT_individual_processed.csv",sep=",",row.names=F)


