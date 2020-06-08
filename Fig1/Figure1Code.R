# Install required packages (needs internet connection)
### 1. Set desired repository  
repos <- "https://stat.ethz.ch/CRAN/"

# install packages once per computer (run by deleting '##')
# install.packages("lme4", repos= repos,  dependencies=TRUE)
# install.packages("lattice", repos=repos,  dependencies=TRUE)
# install.packages("ggplot2", repos=repos,  dependencies=TRUE)
# install.packages("gridExtra", repos=repos,  dependencies=TRUE)

# load these packages each session
library(lme4)
library(lattice)
library(ggplot2)
library(gridExtra)


print(sessionInfo())

getwd()
setwd("") # set your working directory, location of folder containing data

# Load plot data (DVFC_logged_forest_census_1996-2016.csv, from EIDC)
data <- read.table('Philipson20_PlotData.csv')
head(data)
str(data)
summary(data)


unique(data$Dataset)
## 553 rows for 553 plots. (20 'INDFORSUS' plots each measured twice; 32 'conventional logging' plots from a RIL experiement 'RIL_PinardLincoln' each measured 3 times; and 205 'FACE' plots each measured twice (7 measuured a third time))
(20*2)+(32*3)+(205*2)+7
20+205+32+7

## Seperate Logged forest plots from unlogged plots
Logged <- subset(data, Forest=="Logged")
UnLogged <- subset(data, Forest=="UnLogged")

length(levels(Logged$Plot))

################################################
####### Long term plot analysis. Figure 1. #####
################################################
head(Logged)
summary(Logged)
str(Logged)

m1 <- lmer(ACD~  YearsSinceLogging  * FACE + (1|Plot)  + (1|LoggingMethod:Coupe) , REML=1 , data=Logged , na.action=na.fail) 
dotplot(ranef(m1, condVar=T))$Plot
dotplot(ranef(m1, condVar=T))$'LoggingMethod:Coupe'
anova(m1)
summary(m1)
confint(m1)

# Bootstrap errors 
## CI on restored slope
Logged$FACE <- relevel(Logged$FACE, ref="ProjectScenario")
levels(Logged$FACE)
m1 <- lmer(ACD~  YearsSinceLogging  * FACE + (1|Plot)  + (1|LoggingMethod:Coupe) , REML=1 , data=Logged , na.action=na.fail) 
round(fixef(m1),1) 
m1_mer <- bootMer(m1, nsim=1000, FUN=fixef, ncpus=8)
round(apply(m1_mer$t, 2, quantile, c(0.025, 0.975)), 2)

## CI on Natural regen slope
Logged$FACE <- relevel(Logged$FACE, ref="Baseline")
factor(Logged$FACE)
m1 <- lmer(ACD~  YearsSinceLogging  * FACE + (1|Plot)  + (1|LoggingMethod:Coupe) , REML=1 , data=Logged , na.action=na.fail) 
round(fixef(m1),1) 
m1_mer <- bootMer(m1, nsim=1000, FUN=fixef, ncpus=8)
round(apply(m1_mer$t, 2, quantile, c(0.025, 0.975)), 2)


#  Variance attributable to plot size
#  adding a random effect for each dataset accounts for variance attributable to plot size
#  adding '+(1| Dataset)' to FullModel above does not alter the conclusions from the model (recovery slope is the same)
#  m1_b <- lmer(ACD ~  YearsSinceLogging  * FACE + (1|Plot)  + (1|LoggingMethod:Coupe) +(1| Dataset), data=Logged , na.action=na.fail) 
#  summary(m1_b)
#  summary(m1)
#  dotplot(ranef(m1_b, condVar=T))$Dataset



### 1. Plot predictions from model (m1)
#########################################################
(NEWDATa<-expand.grid(YearsSinceLogging =seq(from=3, to=30, by=1), FACE=levels(Logged$FACE)[1]))
(NEWDATb<-expand.grid(YearsSinceLogging =seq(from=8, to=35, by=1), FACE=levels(Logged$FACE)[2]))
NEWDAT <- rbind(NEWDATa, NEWDATb)
NEWDAT$ACD<-0

mm <- model.matrix(terms(m1),NEWDAT)
length(fixef(m1))
NEWDAT$ACD <- mm %*% fixef(m1)
pvar1 <- diag(mm %*% tcrossprod(vcov(m1),mm))
(NEWDAT <- data.frame(
    NEWDAT
    , plo = NEWDAT$ACD-2*sqrt(pvar1)
    , phi = NEWDAT$ACD+2*sqrt(pvar1)))
head(NEWDAT)

## To do, add to original data
NEWDAT$FACE <- factor(NEWDAT$FACE)
levels(NEWDAT$FACE)[levels(NEWDAT$FACE)=="Baseline"] <- "A: Natural regeneration"
levels(NEWDAT$FACE)[levels(NEWDAT$FACE)=="ProjectScenario"] <- "B: With active restoration"

levels(Logged$FACE)[levels(Logged$FACE)=="Baseline"] <- "A: Natural regeneration"
levels(Logged$FACE)[levels(Logged$FACE)=="ProjectScenario"] <- "B: With active restoration"

levels(NEWDAT$FACE)
levels(Logged$FACE)

## add line indicating timing of restoration
RestorLine <- data.frame(x=2, y=399, xend=23, yend=399, FACE="B: With active restoration")

Fig1 <- ggplot(data= Logged, aes(YearsSinceLogging, ACD, FACE))+
xlab("Years Since Logging")+
ylab(expression(paste("Aboveground Carbon Density", " ","(Mg h", a^-1, sep = "", ")")))+
scale_y_continuous(limits=c(0,399))+
facet_grid(~FACE)+ # , scales="free_x", space="free"
# geom_point(col="grey25", pch="O")+ # grey30 grey20
geom_line(data= Logged,aes(YearsSinceLogging,ACD, group=Plot),  size=0.5, colour= "grey")+ # lines per plot
geom_line(data=NEWDAT,aes(YearsSinceLogging, ACD, group=FACE),  linetype=1, size=1.2, colour= "blue")+
geom_line(data=NEWDAT,aes(YearsSinceLogging,plo, group=FACE), size=1, linetype =3, colour= "blue")+
geom_line(data=NEWDAT,aes(YearsSinceLogging,phi, group=FACE),size=1, linetype =3,  colour= "blue")+
theme_bw()+ theme(strip.text.y = element_text( size = 12, hjust = .5), strip.text.x = element_text( size = 12, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.x = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.y = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(axis.title.y = element_text(size = 14, angle = 90))+
theme(axis.title.x = element_text(size = 14, angle = 00))+
theme(axis.text = element_text(size = 14, colour = "black"))+labs(title="")+
geom_segment(data= RestorLine, mapping=aes(x=x, y=y, xend=xend, yend=yend),inherit.aes=FALSE, arrow=arrow(angle=65, ends="both", length=unit(0.1, "inches")), size=1, color="darkgreen")
Fig1


#### Single panel version for supplement  (easier to compare slopes)
Fig_S2 <- ggplot(data= Logged, aes(YearsSinceLogging, ACD, FACE))+
xlab("Years Since Logging")+
ylab(expression(paste("Aboveground Carbon Density", " ","(Mg h", a^-1, sep = "", ")")))+
scale_y_continuous(limits=c(0,399))+
geom_line(data= Logged,aes(YearsSinceLogging,ACD, group=Plot, colour =FACE),  size=0.5, alpha=0.2)+ # lines per plot
geom_line(data=NEWDAT,aes(YearsSinceLogging, ACD, colour =FACE),  linetype=1, size=1.5)+
geom_line(data=NEWDAT,aes(YearsSinceLogging,plo, colour =FACE), size=1, linetype =2)+
geom_line(data=NEWDAT,aes(YearsSinceLogging,phi, colour =FACE),size=1, linetype =2)+
theme_bw()+ theme(strip.text.y = element_text( size = 12, hjust = .5), strip.text.x = element_text( size = 12, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.x = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.y = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(axis.title.y = element_text(size = 14, angle = 90))+
theme(axis.title.x = element_text(size = 14, angle = 00))+
theme(axis.text = element_text(size = 14, colour = "black"))+labs(title="")+
scale_color_manual(values=c("blue", "red"))+ 
theme(legend.position = "none")
Fig_S2


## Unlogged forest for comparison
head(UnLogged)
Prime <- lmer(ACD ~ 1 + (1|MeasureTime), data= UnLogged)
Prime
Prime_mer <- bootMer(Prime, nsim=1000, FUN=fixef, ncpus=8)
summary(Prime)
round(fixef(Prime),0)
round(apply(Prime_mer$t, 2, quantile, c(0.025, 0.975)), 0)
(PrimeCIs <- round(apply(Prime_mer$t, 2, quantile, c(0.025, 0.975)), 0))


### Unlogged / Primary forest
##### right hand panel (carbon potential)
head(UnLogged)
UnLogged$FACE <- "Unlogged"
PrimaryReference <- ggplot(data= UnLogged, aes(YearsSinceLogging, ACD, FACE))+
xlab("")+
ylab("")+
scale_y_continuous(limits=c(0,399), labels =NULL)+  
geom_point(col="grey25", pch="O")+
geom_segment(mapping=aes(x=0, y=PrimeCIs[1], xend=0, yend=PrimeCIs[2]), arrow=arrow(angle=90, ends="both", length=unit(0.1, "inches")), size=0.5, color="blue") + 
geom_point(x=0, y=round(fixef(Prime),0),  col="blue", pch="O",size=5)+
theme_bw()+ theme(strip.text.y = element_text( size = 12, hjust = .5), strip.text.x = element_text( size = 12, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.x = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(strip.text.y = element_text(size = 14, hjust = .5),strip.background=element_rect(colour="black", fill="white"))+
theme(axis.title.y = element_text(size = 14, angle = 90))+
theme(axis.title.x = element_text(colour="white", size = 14, angle = 00))+
theme(axis.text = element_text(size = 14, colour = "white"))+
theme(axis.ticks.x=element_line(colour = "white"))+
theme(panel.grid.minor.x = element_blank())+
labs(title="")+ theme(panel.grid.major.x = element_blank())

(PrimaryReferenceF1 <- PrimaryReference + facet_grid(~FACE))


# tiff(filename = 'Fig1.tiff',width=30, height=15, units="cm",res=300,pointsize=8, compression="lzw")
grid.arrange(Fig1, PrimaryReferenceF1, nrow = 1, ncol=2, widths=c(6,0.8))
# dev.off()


# tiff(filename = 'FigS2.tiff',width=16, height=15, units="cm",res=300,pointsize=8, compression="lzw")
grid.arrange(Fig_S2, PrimaryReference, nrow = 1, ncol=2, widths=c(3,0.6))
# dev.off()


######################################
#### End of plot data analysis and figs
######################################
