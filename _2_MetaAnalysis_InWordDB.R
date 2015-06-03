#### This script replicates all analyses we conducted to investigate the effect of age on infant word segmentation
#### For further information and questions, see inworddb.acristia.org or mail to inwordb@gmail.com
#### Author: Christina Bergmann & Alejandrina Cristia
#### Date: 01.06.2015
#### Licensed under GPLv3


# Ensure that you set the working directory to where all your scripts and files are, possibly using setwd("Your local directory")

# Make sure those two libaries are installed. 
library(meta)
library(metafor)

##############################################
####Preliminary operations with Effect Size### 
##############################################

# Read in the datafile that contains effect sizes.
read.csv("InWordDB_ESg.csv",header=T)->db


#Remove all records where no effect sizes could be calculated.
db <- db[!is.na(db$ESg),]

#Go through each unique infant group (defined as the reference
#concatenated with the infant group tested) and store a median ES
#for the independent ES calculations
db.noRM<-NULL

for(eachunique in levels(factor(db$unique))){
  subset(db,unique==eachunique)->nowdoing
    nowdoing$ES<-median(nowdoing$ESg, na.rm=T)
    nowdoing$ES.SE<-median(nowdoing$ESg.SE, na.rm=T)
    nowdoing$ES.W<-median(nowdoing$ESg.W, na.rm=T)
    nowdoing$ESg<-median(nowdoing$ESg, na.rm=T)
    nowdoing$ESg.SE<-median(nowdoing$ESg.SE, na.rm=T)
    nowdoing$ESg.W<-median(nowdoing$ESg.W, na.rm=T)
    
    nowdoing$proportionFamPref<-median(nowdoing$proportionFamPref, na.rm=T)

    for(i in 1:dim(db)[2]) if(length(levels(factor(nowdoing[,i])))>1) nowdoing[,i]<-NA
    db.noRM <-rbind(db.noRM,nowdoing[1,])
  } # Closing FOR-loop
  
#exclude ES that are more than +-3SD from mean ES; this affects only 1 datapoint: 3.2 neutral_word in Singh, 2008 
db.noRM$include<-ifelse(db.noRM$ESg > mean(db.noRM $ESg,na.rm=TRUE)
									+3*sd(db.noRM$ESg,na.rm=TRUE)    | 
						db.noRM $ESg < mean(db.noRM $ESg,na.rm=TRUE)
									-3*sd(db.noRM$ESg,na.rm=TRUE) ,F,T)

db.noOutnoRM= db.noRM[db.noRM$include,]

write.csv(db.noOutnoRM,"InWordDBnoOutnoRM_ESg.csv",row.names=F)

############################
####Preliminary Analyses####
############################

# General random effects model with no moderators trimming out the outliers and taking a single effect size per infant group (no repeated measures)
grlmodel <-rma(ESg, sei=ESg.SE, data=db.noOutnoRM, weighted=TRUE)
# The estimate significantly above zero, and in the .2 region, a small but significant effect 
# Heterogenity is significant, green light to look at moderators
summary(grlmodel)

funnel(grlmodel,main="Excluding one outlier, without repeated measures")

# NB: Linear regressions on funnel plot asymmetry are not appropriate to interpret this, because there is significant heterogeneity in the model above


#############
#### Age ####
#############

# We look at the impact of age on segmentation
# To this end we create a centered version of the age regressor
db.noOutnoRM$meanAge.C= db.noOutnoRM$meanAge-mean(db.noOutnoRM$meanAge,na.rm=T)

# Is the relationship non-linear?
# To find out, we do a polinomial regression (cf., Hunter & Ames, 1988)
db.noOutnoRM$age.q <- db.noOutnoRM $meanAge.C^2
db.noOutnoRM$age.c <- db.noOutnoRM $meanAge.C^3
polin <-rma(ESg, sei=ESg.SE, mods=~meanAge.C+age.q+age.c,data=db.noOutnoRM, weighted=TRUE)
summary(polin)


# Analyses including difficulty / task 

# First center familiarization criterion, which is neither evenly distributed nor has many samples in the extremes (20s; 100s)
db.noOutnoRM$famCrit.C = db.noOutnoRM$famCriterion-mean(db.noOutnoRM$famCriterion,na.rm=T)

# This is the actual analysis accounting for task difficulty
diff <- rma(ESg, sei=ESg.SE, mods =~ meanAge.C + as.ordered(difficulty) + Words2Passage + famCrit.C + Method, data=db.noOutnoRM, weighted=TRUE)
summary(diff)

# Create groups of languages with at least 10 entries for the final analysis controlling for language background, which might interact with infant age.
db.noOutnoRM$lang=NA #Exclude all those for which we do not have enough samples, such as Mandarin
db.noOutnoRM$lang[db.noOutnoRM$NativeLanguage=="American English"]<-"AE"
db.noOutnoRM$lang[db.noOutnoRM$NativeLanguage=="African American English" |
          db.noOutnoRM$NativeLanguage=="British English" |
					db.noOutnoRM$NativeLanguage=="Canadian English" |
					db.noOutnoRM$NativeLanguage=="Dutch" |
					db.noOutnoRM$NativeLanguage=="German"
					]<-"Other Germanic"
db.noOutnoRM$lang[db.noOutnoRM$NativeLanguage=="Canadian French"]<-"Can Fr"
db.noOutnoRM$lang[db.noOutnoRM$NativeLanguage=="Parisian French"]<-"EU Fr"

# Finally conduct the same analysis, now with native language as an added factor.
diffLg <- rma(ESg, sei=ESg.SE, mods =~ meanAge.C*lang + as.ordered(difficulty) + Words2Passage + famCrit.C + Method, data=db.noOutnoRM, weighted=TRUE)
summary(diffLg)


# Restrict to data where multiple age groups are tested and compare the younger and the older age group. 
rma(ESg, sei=ESg.SE, mods=~as.ordered(MultipleAgeGroupsYO),data= db.noOutnoRM, weighted=TRUE, subset=c(MultipleAgeGroups==1))->MultiAge
summary(MultiAge)

