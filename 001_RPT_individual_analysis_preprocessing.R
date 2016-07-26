## Bodo Winter
## March 12, 2015; Major overhaul June 19, 2015
## July 26, 2016: Finishing brushes and incorporation of spectral tilt
## Preprocessing script

##------------------------------------------------------------------
## Load in data and packages:
##------------------------------------------------------------------

## Load in libraries:

library(reshape2)
library(dplyr)
library(xlsx)

## Define function for pasting strings together with ':':

pasteCol <- function(x, y) paste(x, y, sep = ':')

## Path for main analysis:

mainDir <- '/Users/teeniematlock/Desktop/research/rapid_prosody_transcription/analysis/data'
setwd(mainDir)

## Set options:

options(stringsAsFactors = F)

## Load in summary data:

RPT <- read.xlsx('rpt-Daten-31juli2015_spectral_tilt.xls',
	sheetIndex = 1)

## Load in individual data (wide format):

wide <- read.xlsx('rpt_Einzelwerte-25juli2014-1.xls',
	sheetIndex = 2)

## Set working directory to additional information folder and load everything in:

setwd(file.path(mainDir, 'additional_information'))
blocks <- read.csv('block_order_information.csv')
listener_gender <- read.csv('listener_gender_info.csv')



##------------------------------------------------------------------
## Cleaning of 'RPT' data frame:
##------------------------------------------------------------------

## Delete last row (is just empty):

RPT <- RPT[-nrow(RPT), ]

## Make column names English:

RPT <- rename(RPT,
	Word = Wort,
	NSyll = lex..Wortlänge,
	NSyllRealized = reale.Wortlänge,
	Sentence = Satz,
	Speaker = Sprecher,
	AccentPosition = Akzentposition,
	AccentType = Akzenttyp,
	MeanPitch = mean.pitch,
	MaxPitch = max.pitch,
	PitchRangeST = range.in.st,
	PitchSlopeST = slope.st.s,
	POS_class = POS_Klasse,
	SyllableDur = Dauer_Silbe,
	VowelDur = Dauer_Vokal,
	Vowel = Vokal,
	RMS_amplitude = RMS,
	Freq = Worthäufigkeit,
	LastArgument = letztesArgument,
	Focused = Fokuspartikel,
	SpectralEmphasis = spectralEmphasis,
	H1A2 = H1.A2..db.,
	H1A3 = H1.A3..db.,
	SpeakerGender = gender_Sprecher,
	PScore = p_score)

## Put in more descriptive labels for 'LastArgument' and 'FocusParticle' column:

RPT <- mutate(RPT,
	LastArgument = ifelse(as.numeric(LastArgument), 'last', 'not_last'),
	Focused = ifelse(as.numeric(Focused), 'focus_particle', 'no_focus_particle'),
	LastArgument = as.factor(LastArgument),
	Focused = as.factor(Focused))

## Get rid of '--undefined--' tags:

RPT <- mutate(RPT,
	MeanPitch = replace(MeanPitch, MeanPitch == '--undefined--', NA),
	MaxPitch = replace(MaxPitch, MaxPitch == '--undefined--', NA))

## Make gender tags into upper case:

RPT <- mutate(RPT,
	SpeakerGender = toupper(SpeakerGender))

## Log-transform frequency data:

RPT <- mutate(RPT,
	LogFreq = log10(as.numeric(Freq) + 1))



##------------------------------------------------------------------
## Cleaning of 'wide' data frame:
##------------------------------------------------------------------

## The first row just contains the block order information, delete this:

wide <- wide[-1, ]

## The last column is just NA's:

wide <- wide[, -ncol(wide)]

## Make column names English:

wide <- rename(wide,
	Sentence = Satz,
	Word = Wort)



##------------------------------------------------------------------
## Create unique sentence identifiers:
##------------------------------------------------------------------

## Create a conglomerate block/sentence ID variable ... 
## ... we will loop through that variable and check whether there ...
## ... is any sentence that has the same words (spoken by a different speaker):

RPT$BlockSent <- pasteCol(RPT$Block, RPT$Sentence)

## Create a set of sentence identifiers to assign to unique sentences later in the loop:

NewSentenceID <- paste0(rep(LETTERS, 3), 1:(26 * 3))

## Create an empty sentence ID column:

RPT$NewSentenceID <- rep(NA, nrow(RPT))

## Cycle through each sentence and see whether that sentence is a repeat:

for(i in 1:length(unique(RPT$BlockSent))){
	
	## Pick the block sentence variable:
	
	thisBlockSent <- unique(RPT$BlockSent)[i]

	## Pick all words in this sentence:

	unlist(filter(RPT, BlockSent == thisBlockSent) %>% select(Word)) -> thisWordBag
	
	## Pick all sentences that ar not the current sentence:
	
	allOtherBlockSent <- unique(RPT$BlockSent)[-i]

	## If it's filled with NAs, that means that the sentence has not been assigned yet:
	## Make that a Boolean variable, only those that are full of NA's have be dealt with:

	NAcondition <- all(is.na(RPT[RPT$BlockSent == thisBlockSent, ]$NewSentenceID))
	
	if(NAcondition){
		## Pick the next sentence identifier from the list of identifiers:
		
		RPT[RPT$BlockSent == thisBlockSent,]$NewSentenceID <- NewSentenceID[i]

		## Check all other sentences whether there's any exact match.
		## If yes, they get the same identifier:

		for(j in 1:length(allOtherBlockSent)){
			thisComparisonBlockSent <- allOtherBlockSent[j]
			thisComparisonWordBag <- RPT[RPT$BlockSent == thisComparisonBlockSent, ]$Word
			if(all(thisWordBag %in% thisComparisonWordBag)){
				RPT[RPT$BlockSent == thisComparisonBlockSent, ]$NewSentenceID <- NewSentenceID[i]
			}
		}
	}
}

## How many did each sentence occur in the experiment?

apply(table(RPT$NewSentenceID, RPT$Speaker), 1, FUN = function(x){sum(x != 0)})



##------------------------------------------------------------------
## Make 'wide' data frame into long format and append info:
##------------------------------------------------------------------

## Melt wide file into long format:

long <- melt(wide, id.vars = c('Word', 'Sentence', 'Block'))

## Rename:

long <- rename(long,
	Listener = variable,
	Prominence = value)

## Clean listener gender information:

colnames(listener_gender) <- c('Listener', 'ListenerGender')
listener_gender$Listener <- colnames(wide)[-c(1:3)]		# they appear in order of columns
listener_gender <- mutate(listener_gender,
	ListenerGender = toupper(ListenerGender))

## Add listener gender information to long data frame:

long <- left_join(long, listener_gender, by = 'Listener')	# safe to ignore coercion warning

## Create a pasted Block/Sentence/Word variable for merging with the RPT information:

long <- mutate(long,
	MatcherID = pasteCol(Block, Sentence),
	MatcherID = pasteCol(MatcherID, Word))
RPT <- mutate(RPT,
	MatcherID = pasteCol(Block, Sentence),
	MatcherID = pasteCol(MatcherID, Word))
	
## Merge:

not_these_columns <- c('Block', 'Word', 'Sentence', 'MatcherID')
not_these_columns <- colnames(RPT) %in% not_these_columns
long <- cbind(long, RPT[match(long$MatcherID, RPT$MatcherID), !not_these_columns])

## Get rid of the MatcherID column that was in the long file:

long <- select(long, -MatcherID)

## 'Zu' occurs twice in Sentence 5, Block 3...
## ... but the 'match' function only takes the first pick...
## ... so we need to override that:

this_zu <- which(RPT$Word == 'zu')[3]
long_zus <- which(long$Word == 'zu')
long_zus <- long_zus[seq(along = long_zus) %% 3 == 0]
long[long_zus, 7:ncol(long)] <- RPT[this_zu, !not_these_columns]



##------------------------------------------------------------------
## Re-ordering all columns:
##------------------------------------------------------------------

RPT <- select(RPT, 
	Block, Speaker, SpeakerGender, Sentence, NewSentenceID, Word, 
	POS, POS_class, NSyll, NSyllRealized, Vowel, Freq, LogFreq, 
	AccentPosition, AccentType, 
	LastArgument, Focused, 	
	MeanPitch, MaxPitch, PitchRangeST, PitchSlopeST, 
	RMS_amplitude,
	SpectralEmphasis, H1A2, H1A3,
	SyllableDur, VowelDur, PScore)

long <- select(long, 
	Listener, ListenerGender, 
	Block, Speaker, SpeakerGender, Sentence, NewSentenceID, Word, 
	POS, POS_class, NSyll, NSyllRealized, Vowel, Freq, LogFreq, 
	AccentPosition, AccentType, 
	LastArgument, Focused, 	
	MeanPitch, MaxPitch, PitchRangeST, PitchSlopeST, 
	RMS_amplitude, 
	SpectralEmphasis, H1A2, H1A3,
	SyllableDur, VowelDur, Prominence)
	


##------------------------------------------------------------------
## Write data:
##------------------------------------------------------------------

setwd(mainDir)
write.table(RPT, 'RPT_summary_processed.csv', sep = ',', row.names = F)
write.table(long, 'RPT_individual_processed.csv', sep = ',', row.names = F)


