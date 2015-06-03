#### This script reproduces all figures 
#### For further information and questions, see inworddb.acristia.org or mail to inwordb@gmail.com
#### Author: Christina Bergmann & Alejandrina Cristia
#### Date: 01.06.2015
#### Licensed under GPLv3

# Ensure that you set the working directory to where all your scripts and files are, possibly using setwd("Your local directory")

# Make sure those three libaries are installed. 
library(meta)
library(metafor)

#Makes nicer plots
library(ggplot2)

#Read in the data
read.csv("InWordDBnoOutnoRM_ESg.csv",header=T)->db.noOutnoRM



####################
#### Funnel Plot ### 
####################

#Caption: Funnel plot, showing standard error of the effect size as a function of effect size.
pdf("FunnelPlot.pdf",width=5,height=5)
par(mar=c(4,4,2,1.5))
grlmodel <-rma(ESg, sei=ESg.SE, data=db.noOutnoRM, weighted=TRUE)
funnel(grlmodel,xlab="Effect size Hedges' g",pch = 20)
dev.off()


#####################################
#### Weighted Effect Size vs. Age ### 
#####################################



pdf("Age_EffectSize_Weighted.pdf",width=6,height=6)
par(mar=c(4,4,2,1.5))
p <- ggplot(db.noOutnoRM, aes(meanAge, ESg))
p + geom_line(x=db.noOutnoRM$meanAge, y= 0, linetype="dotted")+ geom_point(size=db.noOutnoRM$ESg.W/18) + geom_smooth(method = "glm", aes(weight = db.noOutnoRM$ESg.W), colour = "black") + theme(text = element_text(size=20)) + xlab("Age in days") + ylab("Effect size Hedge's g")

dev.off()


##############################
#### Supplementary Figure ####
##############################

# Overall forest plot
pdf("ForestPlot_InWordDB_perInfantGroup.pdf", height=38, width = 13)
forest(metagen(ESg, ESg.SE, paste(Authors, JnlYear, InfantGroup, sep = ", "), 
               data = db.noOutnoRM))
dev.off()

# Create specified forest plots using "byvar = XXX" for a grouping variable and "bylab = c("XXX")" for a label


