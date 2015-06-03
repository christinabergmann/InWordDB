#### This script computes several measures of a bias in publishing novelty preferences
#### For further information and questions, see inworddb.acristia.org or mail to inwordb@gmail.com
#### Author: Christina Bergmann & Alejandrina Cristia
#### Date: 01.06.2015
#### Licensed under GPLv3

# Ensure that you set the working directory to where all your scripts and files are, possibly using setwd("Your local directory")

# Read in the database and remove missing entries
read.csv("InWordDBnoOutnoRM_ESg.csv",header=T)->db.noOutnoRM
db.noOutnoRM <- db.noOutnoRM[!is.na(db.noOutnoRM$ESg),]

# Make sure this package is installed
library(metafor)


####################################################################################
#### Publication Bias: Relative frequency of novelty preferences over the years ####
####################################################################################

### Novelty preferences (ESg below -.1 to ensure a small to moderate effect), divided by reported as significant and not reported as significant
### Note: Increase significant when calculating for ALL and only for those with p<.05.
### But there are only very few samples! Even if the same number of twice the number was held back, we would not have a negative ES. 

# not the most elegant solution, but it works
percent_novel <- 0
percent_novel_sig <- 0
percent_novel_nonsig <-0

# Compute the percentage per *publication* year in the database
x <-1
for (i in levels(factor(db.noOutnoRM$JnlYear))) {
  percent_novel[x]<-(length(which(db.noOutnoRM$JnlYear==i & db.noOutnoRM$ESg <= -.1)))*100/(length(which(db.noOutnoRM$JnlYear==i)))
  percent_novel_sig[x]<-(length(which(db.noOutnoRM$JnlYear==i & db.noOutnoRM$ESg <= -.1 & (db.noOutnoRM$p_val_reported <=0.05))))*100/(length(which(db.noOutnoRM$JnlYear==i)))
  percent_novel_nonsig[x]<-(length(which(db.noOutnoRM$JnlYear==i & db.noOutnoRM$ESg <= -.1& (db.noOutnoRM$p_val_reported >=0.05))))*100/(length(which(db.noOutnoRM$JnlYear==i)))
  x=x+1}


# Statistical testing:
cor.test(percent_novel,as.numeric(levels(factor(db.noOutnoRM$JnlYear))), method = "spearman")


# Plot everything to obtain an idea of the 
pdf("Additional_figure_Novelty_preferences_vs_age.pdf",width=7,height=5)
par(mar=c(4,4,2,1.5))
plot(ESg[ESg<0]~meanAge[ESg<0],xlab="Age (days)",ylab="Effect size Hedges' g",type="n", data=db.noOutnoRM)
points(ESg~meanAge, subset = c(p_estim <0.05), pch = 8, data=db.noOutnoRM)
points(ESg~meanAge, subset = c(is.na(p_estim) & p_val_reported <0.05), pch = 8, data=db.noOutnoRM)
points(ESg~meanAge, subset = c(p_estim >0.05), pch = 1, data=db.noOutnoRM)
points(ESg~meanAge, subset = c(is.na(p_estim) & p_val_reported >0.05), pch = 1, data=db.noOutnoRM)
points(ESg~meanAge, subset = c(is.na(p_estim) & is.na(p_val_reported)), pch = 2, col = "gray", data=db.noOutnoRM)

lines(c(0,800),c(-.2, -.2),col="gray")
lines(c(0,800),c(-.5, -.5),col="gray")

#legend("bottomright", c("Significant with p < .05", "Not significant", "p-Value not reported"), pch=c(8, 1, 2))
dev.off()


pdf("Additional_figure_Novelty_preferences_vs_year.pdf",width=7,height=5)
par(mar=c(4,4,2,1.5))
plot(ESg[ESg<0]~JnlYear[ESg<0],xlab="Year of publication",ylab="Effect size Hedges' g",type="n", data=db.noOutnoRM)
points(ESg~JnlYear, subset = c(p_estim <0.05), pch = 8, data=db.noOutnoRM)
points(ESg~JnlYear, subset = c(is.na(p_estim) & p_val_reported <0.05), pch = 8, data=db.noOutnoRM)
points(ESg~JnlYear, subset = c(p_estim >0.05), pch = 1, data=db.noOutnoRM)
points(ESg~JnlYear, subset = c(is.na(p_estim) & p_val_reported >0.05), pch = 1, data=db.noOutnoRM)
points(ESg~JnlYear, subset = c(is.na(p_estim) & is.na(p_val_reported)), pch = 2, col = "gray", data=db.noOutnoRM)

lines(c(1998, 2014),c(-.2, -.2),col="gray")
lines(c(1998, 2014),c(-.5, -.5),col="gray")

legend("bottomleft", c("Significant with p < .05", "Not significant", "p-Value not reported"), pch=c(8, 1, 2))
dev.off()



####################################
#### Publication Bias: P-curving ###
####################################

#To use p-curve app at p-curve.com:
db.noOutnoRM$exact.t[!is.na(db.noOutnoRM$exact.t)] ->ts
db.noOutnoRM$Included[!is.na(db.noOutnoRM$exact.t)] ->dfs
for (it in seq(length(ts))){print(paste("t(", dfs[it], ") = ", ts[it]))}

#copy into excel, remove " and [1] and then copy to app.

# To separately p-curve positive and negative, simply adjust the directon of the comparison to 0
db.noOutnoRM$exact.t[!is.na(db.noOutnoRM$exact.t) & db.noOutnoRM$exact.t <0] ->ts
db.noOutnoRM$Included[!is.na(db.noOutnoRM$exact.t) & db.noOutnoRM$exact.t <0] ->dfs
for (it in seq(length(ts))){print(paste("t(", dfs[it], ") = ", ts[it]))}


#########################################################################################################################
#### Publication Bias: Simulate the number of unpublished effects necessary to observe a switch for older age groups ####
#########################################################################################################################

##### Magic numbers for the simulation

# Set random seed to reproduce effect
set.seed(111)
# Sample 1000 times, adjust it if you want to sample more or less.
number_of_iterations = 1000
# Age in months that is the lower bound of the sample (from which age on would a switch to novelty be expected)
MinAge = 12

##### Initialize everything
subset(db.noOutnoRM, meanAge>(MinAge*30.42))->db_simulation
db_simulation<-db_simulation[,c("ESg","ESg.SE")]
rma(ESg, sei=ESg.SE, data=db_simulation, weighted=TRUE)-> baseline
estimate=as.numeric(baseline[1])
pval=as.numeric(baseline[4])
originalN=dim(db_simulation)[1]

# Starting point: the data in the database
print(paste("We start with",dim(db_simulation)[1], "observations,",sum(db_simulation$ESg< 0, na.rm=T),"of which were negative, with a median ESg",estimate,"p=",pval))


# Initialize variables to record the outcomes
estimates = c()
pvals = c()
nobservations = c()
nnegESgs = c()


estimates_0 = c()
pvals_0 = c()
nobservations_0 = c()
nnegESg_0 = c()

for(iterations in seq(number_of_iterations)){
  
  #Initialize the simulated data
  fakedata<-db_simulation
  
  #First estimate how many additional observations are necessary to reach a null result, indicating the transition from familiarity to novelty.
  while(sign(estimate)* pval< .05) {
    #step 1: repeat an observation selected randomly 
    fakedata<-rbind(fakedata,fakedata[sample(1:originalN, 1),])
    
    #step 2: flip the sign of that observation
    fakedata[dim(fakedata)[1],"ESg"] <- fakedata[dim(fakedata)[1],"ESg"]*(-1)
    #step 3: calculate the key parameters
    rma(ESg, sei=ESg.SE, data=fakedata, weighted=TRUE)-> fit
    estimate = as.numeric(fit[1])
    pval = as.numeric(fit[4])
  }
  
  # Append results of this simulation to the dummy variables
  estimates_0 = c(estimates, estimate)
  pvals_0 = c(pvals_0, pval)
  nobservations_0 = c(nobservations_0, dim(fakedata)[1]-dim(db)[1])
  nnegESg_0 = c(nnegESg_0, sum(fakedata$ESg<0, na.rm=T)-sum(db$ESg<0, na.rm=T))
  
  
  # Now estimate how many observations lead to an overall novelty preference in infants above the cutoff age.
  while(pval > .05) {
    #step 1: repeat an observation selected randomly 
    fakedata<-rbind(fakedata,fakedata[sample(1:originalN, 1),])
    
    #step 2: flip the sign of that observation
    fakedata[dim(fakedata)[1],"ESg"] <- fakedata[dim(fakedata)[1],"ESg"]*(-1)
    #step 3: calculate the key parameters
    rma(ESg, sei=ESg.SE, data=fakedata, weighted=TRUE)-> fit
    estimate=as.numeric(fit[1])
    pval=as.numeric(fit[4])
  }
  
  # record the outcome
  numbernegative = sum(fakedata$ESg<0, na.rm=T)
  
  nobservations = c(nobservations, dim(fakedata)[1]-dim(db_simulation)[1])
  nnegESgs = c(nnegESgs, numbernegative)
  estimates = c(estimates, estimate)
  pvals = c(pvals, pval)
  
  
  
}

#Report the mean outcome
print(paste("We have had to add",mean(nobservations), "(SD =", sd(nobservations),  ") observations,",mean(nnegESgs), "(SD =", sd(nnegESgs),  ") of which were negative, to get a significant negative median effect size, yielding the median ESg = ",median(estimates),", p =",mean(pvals)))





############################################################################################################
#### Supplementary analysis: Are there more novelty preferences for studies familiarizing with passages? ###
############################################################################################################


#Create binary variable coding whether there was a novelty preference or not.
db.noOutnoRM$nov<-ifelse(db.noOutnoRM$ESg<0,1,0)

#Tabulate the result
table(db.noOutnoRM$nov,db.noOutnoRM$Words2Passage)

# Statistical testing
summary(table(db.noOutnoRM$nov,db.noOutnoRM$Words2Passage))