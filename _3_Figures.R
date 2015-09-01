#### This script reproduces all figures 
#### For further information and questions, see inworddb.acristia.org or mail to inwordb@gmail.com
#### Author: Christina Bergmann & Alejandrina Cristia
#### Date: 01.06.2015
#### Licensed under GPLv3

# Ensure that you set the working directory to where all your scripts and files are, possibly using setwd("Your local directory")


####################################
#### Hunter & Ames illustration ####
####################################
fam2novO=function(x) {(1/3) * x^3 -2 * x^2 + (8/3) * x}
fam2novY=function(x) {(1/24) * x^3 -(1/2) * x^2 + (4/3) * x}


pdf("hna_graph.pdf",width=4.25,height=4.25)

par(mar=c(1.6,3.5,1.25,1)+0.1)
plot(1~1,type="n",xaxt="n",yaxt="n",xlim=c(0,6),ylim=c(-1,1),xlab="",ylab="",lwd=2)

mtext("Familiarization time",side=1, line=.7,cex=1.25)
axis(1,at=c(0),labels=c(""),line=-.5,lwd.tick=3)
text(-.2,-1,"Study starts",pos=4, font=3, cex=.8)

mtext("Preference",side=2, line=2,cex=1.25)
axis(2,at=c(-1,1),labels=c("Novelty","Familiarity"))

lines(c(-1,7),c(0,0),lty=2)

points(fam2novO((0:3000)/1000)~c((0:3000)/1000),pch=18,col="gray")
points(rep(-1,3200)~c((3001:6200)/1000),pch=18,col="gray")

points(fam2novY((0:6000)/1000)~c((0:6000)/1000),pch=20)
points(rep(-1,200)~c((6001:6200)/1000),pch=20)
lines(c(3,3),c(-1.5,1.5),lty=3,col="gray")
dev.off()


####################
#### Preparation ### 
####################


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



pdf("Age_EffectSize_Weighted.pdf",width=8,height=8)
par(mar=c(4,4,2,1.5))
p <- ggplot(db.noOutnoRM, aes(meanAge, ESg))
print(p + geom_line(x=db.noOutnoRM$meanAge, y= 0, linetype="dotted")+ geom_point(size=db.noOutnoRM$ESg.W/13) + geom_smooth(method = "glm", aes(weight = db.noOutnoRM$ESg.W), colour = "black") + theme(text = element_text(size=20)) + xlab("Age in days") + ylab("Effect size Hedges' g"))

dev.off()


##############################
#### Supplementary Figure ####
##############################

# Overall forest plot
pdf("ForestPlot_InWordDB_perInfantGroup.pdf", height=42, width = 13)
forest(metagen(ESg, ESg.SE, paste(Authors, JnlYear, InfantGroup, sep = ", "), 
               data = db.noOutnoRM))
dev.off()

# Create specified forest plots using "byvar = XXX" for a grouping variable and "bylab = c("XXX")" for a label


