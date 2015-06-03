#### This script was written to preprocess InWordDB and compute effect sizes 
#### For further information and questions, see inworddb.acristia.org or mail to inwordb@gmail.com
#### Author: Christina Bergmann & Alejandrina Cristia
#### Date: 01.06.2015
#### Licensed under GPLv3

# Ensure that you set the working directory to where all your scripts and files are, possibly using setwd("Your local directory")

#Read in raw datafile
db<-read.csv("InWordDB.csv")

#########################################################
#### Exclusion of conditions with nonnative material #### 
#########################################################
# For simplicitly all tests where infants were exposed to a language that is not their native language (or a nonnative accent thereof) are excluded from first analyses.
db[db$TestLanguage=="native",]->db


######################
####Preprocessing #### 
######################


db$NativeLanguage=factor(db$NativeLanguage)

#Fix infant type, bilinguals not noted
db$InfantType<-ifelse(db$NativeLanguage=="Mandarin and English","bilingual",as.character(db$InfantType))
db$InfantType<-ifelse(db$NativeLanguage=="Catalan and Spanish","bilingual",as.character(db$InfantType))
db$InfantType=factor(db$InfantType)

#Add some info: proportion of babies included, a variable to code infant group (to track repeated measures), and a handful of language categories
db$propIncl=db$Included/(db$Excluded+db$Included)
db$unique=paste(db$DOI,db$InfantGroup,sep="_")

#How to handle missing information: NA strings
db$meanAge=as.numeric(as.character(db$meanAge))
db$exact.t=as.numeric(as.character(db$exact.t))
db$proportionFamPref=as.numeric(as.character(db$proportionFamPref))
db$LTFam=as.numeric(as.character(db$LTFam))
db$LTNov=as.numeric(as.character(db$LTNov))

#Correlations of infant behavior are crucial for estimating effect size weights. These correlations are almost never reported. 
#We obtained correlations from the original authors of some database entries.
#Impute missing correlations using a randomly selected value.
library(Hmisc)

#set a random seed for reproducibility. Now impute will always yield the same result, unless the seed is changed.
set.seed(111)
db$correlationFamNov.imputed<-impute(db$correlationFamNov,fun="random")

###################################################
#### Estimate linguistic difficulty of a task #####
###################################################

# Create a numeric value that accumulates all factors that might contribute to more challenging tasks for infants.
ling_difficulty <-ifelse(db$Linguistic=="match",0,1) #This indicates whether the target word matches exactly in phonological form across familiarization and test 
align_difficulty <- ifelse(db$EdgeAlignSimple==1,0,1) #This indicates whether the target word is always aligned with a sentence edge, in which case difficulty is 0
align_difficulty[is.na(db$EdgeAlignSimple)] <- 1 #When edge alignment was not available assume that the target was NOT aligned with a sentence edge in all cases
index_difficulty[db$Indexical=="not.manipulated"]<-1 #all studies where indexical properties were not controlled for get a 1
index_difficulty <- ifelse(db$Indexical=="match",0,2) #in studies where indexical properties (pitch, voice, affect) are manipulated, the conditions having a precise match get zero, those with a mismatch get a 2
index_difficulty[is.na(db$Indexical)]<-1 #all studies where indexical properties were not controlled for get a 1

#Calculate total difficulty score. 
db$difficulty<- ling_difficulty + align_difficulty + index_difficulty + db$WdMisAlignMajorStress


###########################
####Estimate Effect Size### 
###########################

#This is a repeated measure, akin to a pre-post contrast
#therefore, we use the 'standardized mean gain' set of formulas whenever possible
db$LTDif<-ifelse(db$formula2use=="means&SD",db$LTFam-db$LTNov, NA)

#Compute pooled SD to use in Effect Size measures.
#lipsey & wilson, page 44 - from Becker 1988
db$PooledSD<-ifelse(db$formula2use=="means&SD",sqrt((db$SD.LTFam^2+db$SD.LTNov^2)/2), NA)

#Compute Effect Size based on means and SD of dependent variables where possible.
#lipsey & Wilson, formula 3.14
db$ES<-ifelse(db$formula2use=="means&SD",db$LTDif/db$PooledSD,NA)

#This is an approximation of the same ES using exact t values
#We do NOT use formula 2/3 from Table B10 in lipsey wilson because that is based on independent samples
#Instead, we follow Dunlap et al. 1996 p 171
db$ES<-ifelse(db$formula2use=="tValue",db$exact.t*sqrt(2*(1-db$correlationFamNov.imputed)/db$Included),as.numeric(as.character(db$ES)))

#Since many of these samples are smaller than 20, we calculate Hedges' g
#Morris 2010, p. 21
db$ESg = db$ES*(1-(3/(4*db$Included-5)))

#lipsey & wilson, formula 3.15; notice that others divide by N-1 in the last term
db$ES.SE<-sqrt((2*(1-db$correlationFamNov.imputed)/db$Included)+(db$ES^2)/(2*db$Included))

db$ESg.SE<-sqrt((2*(1-db$correlationFamNov.imputed)/db$Included)+(db$ESg^2)/(2*db$Included))

#Compute the weights
#lipsey & wilson, formula 3.16
db$ES.W=1/(db$ES.SE^2)
db$ESg.W=1/(db$ESg.SE^2)


#####################################################
###### Estimate p-values based on t, if possible ####
#####################################################

db$p_estim<-ifelse(!is.na(db$exact.t), pt(-abs(db$exact.t), db$Included-1), NA)

# Write everything to a file, this file now contains all effect sizes. 
write.csv(db,"InWordDB_ESg.csv",row.names=F)
