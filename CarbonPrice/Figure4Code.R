##########################################################################################
#### Break even carbon price based depending cost of restoration #########################
#### Estimate for a range of senarious ###################################################
##########################################################################################

# Install required packages (needs internet connection)
### 1. Set desired repository  
repos <- "https://stat.ethz.ch/CRAN/"

# install.packages("ggplot2", repos= repos, dependencies = TRUE)
# install.packages("reshape", repos= repos,  dependencies = TRUE)
# install.packages("cowplot", repos= repos,  dependencies = TRUE)


library(ggplot2)
library(reshape)
library(cowplot)

######################################################
## Five Yearly Accounting interest rates
######################################################
(dats<- data.frame(t=seq(from=5, to=30, by=5), rate=NA))

for (i in c(1:6))
	{
dats$rate[i] <- (1/((1+ 0.05)^dats$t[i]))
	}

1/dats$rate

## Project accounting every five years, assuming 5% discount rate
rm(ints)
ints <- expand.grid(Cost_T1 =seq(0,5000, by=10), rate = 0.05) 
ints$CP<-  (with(ints,  Cost_T1/(5*3.67*1.5*(sum(dats$rate)))))
ints$CP_low<-  (with(ints,  Cost_T1/(5*3.67* 0.4*(sum(dats$rate)))))
ints$CP_High<-  (with(ints,  Cost_T1/(5*3.67* 2.6*(sum(dats$rate)))))
tail(ints)

# Holl & Zahawi 2014, growth rates
# "Mean above-ground C sequestration rate was Plantation(Pl)=2.88 Mg ha-􏰀1 yr-􏰀1, Islands(Is)=1.57 & Control= 0.40” 
2.48 # Mg #growth added for planting the full area.  
# subtract the control estimate of 0.4 to get 2.48 (2.88-0.4)
1.17 # Mg for the planted islands 1.17 (1.57-0.4)

ints$BE_Holl_Is<-  (with(ints,  Cost_T1/(5*3.67* 1.17*(sum(dats$rate)))))
ints$BE_Holl_Pl<-  (with(ints,  Cost_T1/(5*3.67* 2.48*(sum(dats$rate)))))

# Wheeler2016 (forEcolManag)
## 40.6 in 2013, minus 9.5 Mg AGB in 2005 (+/-2.9), and minus 5.1 in the grassland /2 for carbon
round((((40.6-9.5)-5.1) / (2013-2005))/2, 2) # 1.62
round(((((40.6-7.7)-9.5)-5.1) / (2013-2005))/2, 2) #1.14 # lower 
round(((((40.6+7.7)-9.5)-5.1) / (2013-2005))/2, 2) #2.11 # upper
ints$CPWheelerEst <-  (with(ints,  Cost_T1/(5*3.67* 1.62*(sum(dats$rate)))))
ints$CPWheelerlow <-  (with(ints,  Cost_T1/(5*3.67* 1.14*(sum(dats$rate)))))
ints$CPWheelerhigh <-  (with(ints,  Cost_T1/(5*3.67* 2.11*(sum(dats$rate)))))

## illustrating potential range, hopefully there is more data out there
ints$CP_0.1<-  (with(ints,  Cost_T1/(5*3.67* 0.1*(sum(dats$rate)))))
ints$CP_0.5<-  (with(ints,  Cost_T1/(5*3.67* 0.5*(sum(dats$rate)))))
ints$CP_1  <-  (with(ints,  Cost_T1/(5*3.67* 1*(sum(dats$rate)))))
ints$CP_2  <-  (with(ints,  Cost_T1/(5*3.67* 2*(sum(dats$rate)))))
ints$CP_3  <-  (with(ints,  Cost_T1/(5*3.67* 3*(sum(dats$rate)))))
ints$CP_4  <-  (with(ints,  Cost_T1/(5*3.67* 4*(sum(dats$rate)))))
ints$CP_5  <-  (with(ints,  Cost_T1/(5*3.67* 5*(sum(dats$rate)))))

head(ints)
tail(ints)
################################################
### add in range of restoration costs seen in literature [ Table S1]
(CostExamples <- data.frame(
	Costs=c(2500, 9695, 2067, 2158, 994, 1100, 297, 6370, 17920, 1200,
	1275, 943, 1395, 1450, 1024, 1435, 1964, 1231, 1539, 1518, 1047, 1447, 1463, 1025, 1435,
	429,2000, 3682, 7794, 4308, 406, 1158, 2922, 875, 1355, 824, 849, 526, 1625, 1500, 2429,
	6477,1625, 2096, 8625, 3306, 1625, 2000, 2875, 1313),
	Locations=c( "TM","NH", "Q","CV", "ET", "CBC_PL", "CBC_IS", "AU","AU", "KNP", 
	"EK","EK","EK","EK","EK","EK","EK","EK","EK","EK","EK","EK","EK","EK","EK",
	"SBK", "IFP", "IDN", "IDN", "IDN", 
	"LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", "LDF_MYR", 	"LDF_MYR", "LDF_MYR", 
	"MGF_ST","MGF_S", "MGF_P", "PSF_PM", "PSF_S", "FF_S", "FF_S", "FF_S", "FF_S")))

length(CostExamples[,1])

###################################################
## Holl 2011 (Restoration Ecology), Holl & Zahawi 2014
# Range for restoration costs
# 400 - 600 for planting; 500-700 for maintainence, 27% of that cost for island planting
400+500  # 900, FullAreaCost lower 
700+600 # 1300 full area planting cost upper
(400+500)*0.27 # 243, lower cost for planting islands
(700+600)*0.27 # 351, upper for islands 
###################################################

cash <- ggplot(ints, aes(x=Cost_T1, y= CP))+
xlab(expression(paste("Restoration cost ", " ","(US$ h", a^-1, sep = "", ")")))+
ylab(expression(paste("Break even point carbon price ", " ","(US$ per tC", O[2], sep = "", e,sep="", ")")))+
theme_bw()+
theme(legend.position="none")+
scale_y_log10(limits=c(0.1,max(ints$CP_0.1)), breaks=c(2,10,20,50, 200,1000))+ 
theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
panel.border = element_blank(), panel.grid.minor = element_blank())+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_0.1,ymax=CP_0.5), alpha=0.1,fill="darkgreen")+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_0.5,ymax=CP_1), alpha=0.2, fill="darkgreen")+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_1,ymax=CP_2), alpha=0.3, fill="darkgreen")+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_2,ymax=CP_3), alpha=0.4, fill="darkgreen")+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_3,ymax=CP_4), alpha=0.5, fill="darkgreen")+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_4,ymax=CP_5), alpha=0.6, fill="darkgreen")+
# Add data from this study (corresponds to Fig 3)
geom_line(data=subset(ints, Cost_T1>=1500 & Cost_T1<=2500), aes(x=Cost_T1, y=CP), size=1.2, colour="blue")+
geom_segment(aes(x=2000, xend= 2000, y= ints$CP_low[201], yend=ints$CP_High[201]), size=1.2, colour="blue")+
## Add wheeler 2016
geom_segment(aes(x=1200, xend= 1200, y= ints$CPWheelerlow[121], yend=ints$CPWheelerhigh[121]), size=1.2, colour="red")+
geom_point(aes(x=1199, y=ints$CPWheelerEst[121]),colour="red", pch=1, size=3)+
## Add Holl 2011 (Restoration Ecology), Holl & Zahawi 2014
geom_line(data=subset(ints, Cost_T1>=243 & Cost_T1<=351), aes(x=Cost_T1, y= BE_Holl_Is), size=1.2,colour="purple")+
geom_line(data=subset(ints, Cost_T1>=900 & Cost_T1<=1300), aes(x=Cost_T1, y= BE_Holl_Pl), size=1.2, colour="purple")
cash

###################################################
#### With Legend
###################################################  
##### Making dataset long for version with legend
intsHigh <- ints[,-c(2:11)]
intsLongH <- melt(intsHigh, id=c("Cost_T1"))
colnames(intsLongH)<-c(colnames(intsLongH)[1], "GrowthLow", "CP_l")
intsLow <- ints[,-c(2:10, 17)]
intsLongL <- melt(intsLow, id=c("Cost_T1"))
colnames(intsLongL)<-c(colnames(intsLongL)[1], "GrowthHigh", "CP_h")
intsLong<-data.frame(intsLongL, intsLongH[,2:3])

cashLeg <- ggplot(intsLong, aes(x=Cost_T1, y= CP_l))+
xlab(expression(paste("Restoration cost ", " ","(US$ h", a^-1, sep = "", ")")))+
ylab(expression(paste("Break even point carbon price ", " ","(US$ per tC", O[2], sep = "", e,sep="", ")")))+
theme_bw()+
scale_y_log10(limits=c(0.1,max(intsLong$CP_h)), breaks=c(2,10,20,50, 200,1000))+ 
theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),panel.border = element_blank(), panel.grid.minor = element_blank())+
geom_ribbon(data=intsLong,aes(x=Cost_T1, ymin=CP_l,ymax= CP_h, alpha=GrowthLow),fill="darkgreen")+
# Add our data line
geom_line(data=subset(ints, Cost_T1>=1500 & Cost_T1<=2500), aes(x=Cost_T1, y=CP), size=1.2, colour="blue")+
geom_segment(aes(x=2000, xend= 2000, y= ints$CP_low[201], yend=ints$CP_High[201]), size=1.2, colour="blue")+
## Add wheeler 2016
geom_segment(aes(x=1200, xend= 1200, y= ints$CPWheelerlow[121], yend=ints$CPWheelerhigh[121]), size=1.2, colour="red")+
geom_point(aes(x=1199, y=ints$CPWheelerEst[121]),colour="red", pch=1, size=3)+
## Add Holl data line
geom_line(data=subset(ints, Cost_T1>=243 & Cost_T1<=351), aes(x=Cost_T1, y= BE_Holl_Is), size=1.2,colour="purple")+
geom_line(data=subset(ints, Cost_T1>=900 & Cost_T1<=1300), aes(x=Cost_T1, y= BE_Holl_Pl), size=1.2, colour="purple")
cashLeg

# Improve legend details
Vals = c("CP_0.5" = "0.1", "CP_1"= "0.2", "CP_2" = "0.4", "CP_3" = "0.6", "CP_4" = "0.8", "CP_5" = "1")
Labs = c("0.1 - 0.5", "0.5 - 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5")

(cashLeg + scale_alpha_manual(name = expression(atop(NA, atop(NA, atop("Additional ACD accumulation", atop("due to restoration ", paste("(Mg h", a^-1," " , year^-1, ")")))))), values = Vals, labels=Labs) +theme(legend.justification = "top", legend.title=element_text(size = 20), legend.text = element_text(size = 10),  legend.margin = margin(5, 0, 0, 0)))

# legend title slightly out of panel alligns better with density plot
(cashLeg <- cashLeg + scale_alpha_manual(name = expression(atop(NA, atop(NA, atop("Additional ACD growth", atop("due to restoration ", paste("(Mg h", a^-1," " , year^-1, ")")))))), values = Vals, labels=Labs) +theme(legend.justification = "top", legend.title=element_text(size = 20), legend.text = element_text(size = 10),  legend.margin = margin(-40, 0, 0, 0)))

(CostDensity <- ggplot(CostExamples, aes(x= Costs), box=F)+
					   geom_density(alpha=0.4)+ theme_bw()+
	                  geom_rug(aes(x = Costs, y = 0))+
		     		   theme(panel.grid.minor = element_blank())+
					   scale_y_continuous(breaks = NULL))

# not including costs above 5000 (not much data), harder to see visually
(CostDensity <- ggplot(subset(CostExamples, Costs<= 5000), aes(x= Costs), box=F)+
					   geom_density(alpha=0.4)+ theme_bw()+
	                  geom_rug(aes(x = Costs, y = 0))+
		     		   theme(panel.grid.minor = element_blank())+
					   scale_y_continuous(breaks = NULL))

# "AU" # Australia costs off the scale - hard to see plot - added in text 
length(subset(CostExamples, Costs<= 5000)$Costs)
length(subset(CostExamples)$Costs)

######################################################
## average restoration costs
head(CostExamples)
m1<- lm(Costs~1, data=subset(CostExamples))
par(mfrow=c(2,2)); plot(m1)
m1
confint(m1)
m2<- lm(Costs~1, data=CostExamples, Costs<= 5000)
par(mfrow=c(2,2)); plot(m2)
m2
confint(m2)
######################################################

37+7+17+10

###########################
(cashLeg = cashLeg  + theme(plot.margin = unit(c(0.5, 0.5, 0, 0), "cm")))
# only remove upper x-axis when graph finally alligned
(CostDensityNoAxis <- CostDensity+ xlim(0,5000)+
					    theme(axis.title.x=element_blank(),
                        axis.title.y=element_blank(),
                        axis.text=element_blank(),
                        axis.line=element_blank(),
                        axis.ticks=element_blank(),
                        panel.border = element_blank())+ #+
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
						theme(plot.margin = unit(c(0, 4.4, 0.5, 1.5), "cm")))

# Combine all plots together and crush graph density with rel_heights
(combined = plot_grid(CostDensityNoAxis, cashLeg, ncol = 1, rel_heights = c(1, 3)))


setwd("")
setwd("~/Google Drive/PApers In Prep/Carbon Recovery/Science2020/CarbonRecovery_Code4Git/CarbonPrice/")

tiff(filename = "Fig4.tiff",width=15, height=15, units="cm",res=200,pointsize=8, compression="lzw")
combined
dev.off()
###########################
# End
###########################
