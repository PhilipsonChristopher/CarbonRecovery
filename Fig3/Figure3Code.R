############################################################
#### Calculate Break-even carbon price and plot Fig 3 ######
############################################################ 
# install.packages("ggplot2", dependencies = TRUE)

library(ggplot2)

## Project accounting every five years, assuming 5% discount rate
(dats<- data.frame(t=seq(from=5, to=30, by=5), rate=NA))
for (i in c(1:6))
	{
dats$rate[i] <- (1/((1+ 0.05)^dats$t[i]))
	}
dats

# Or what we are paying in interest..
1/dats$rate # cumulative
sum(dats$rate)

## Five Yearly Accounting rates
rm(ints)
ints <- expand.grid(Cost_T1 =seq(0,4000, by=10), 
		  AddGR_Restoration = 1.5, 
		  AddGR_RestorationLow = 0.4, 
		  AddGR_RestorationHigh = 2.6, 
		  Years= 30) 
head(ints)

# Equation4 in manuscript, 
ints$CP<-  (with(ints,  Cost_T1/(5*3.67*AddGR_Restoration*(sum(dats$rate)))))

# Upper and lower bounds (of Equation4) ,propagated from the ACD recovery model  
ints$CP_low<-  (with(ints,  Cost_T1/(5*3.67* AddGR_RestorationLow*(sum(dats$rate)))))
ints$CP_High<-  (with(ints,  Cost_T1/(5*3.67* AddGR_RestorationHigh*(sum(dats$rate)))))
tail(ints)


cash <- ggplot(ints, aes(x=Cost_T1, y= CP, AddGR_Restoration))+
geom_line(aes(x=Cost_T1, y=CP, linetype = factor(AddGR_Restoration)))+
theme_bw()+
theme(legend.position="none")+
xlab(expression(paste("Restoration cost ", " ","(US$ h", a^-1, sep = "", ")")))+
ylab("Break even point carbon price ($)")+
ylab(expression(paste("Break even point carbon price ", " ","(US$ per tC", O[2], sep = "", e, sep="", ")")))+
scale_y_log10(breaks=c(2,10,100))+
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_low,ymax=CP_High), fill="grey", alpha=0.5)+ 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
# geom_hline(yintercept=2, lty=1,col="blue")+
# geom_hline(yintercept=10, lty=1, col="blue")+
geom_vline(xintercept=1500, lty=1, col="darkgreen")+
geom_vline(xintercept=2500, lty=1, col="darkgreen")+
annotate("text", x = c(2000, 2000), y = c(445, 355), label = c( "Restoration costs", "in Sabah") , color=c("darkgreen","darkgreen"), size=2.8 , angle=0, fontface="bold")+
annotate("segment", y = 280, yend = 280, x = 1550, xend = 2450, colour = "darkgreen", size=1, alpha=0.6, arrow=arrow(ends="both", length=unit(0.1, "inches")));cash


## Can add lines to highlight current prices and 
#  required to fulfil the Paris Agreement 
(cash2<- cash+ geom_rect(mapping=aes(xmin=-5, ymin=2, xmax=2000, ymax=10), col="red", fill=NA )+
geom_rect(mapping=aes(xmin=-5, ymin=40, xmax=4100, ymax=80), col="blue", fill=NA ))


# setwd("") # Specify where you would like the file
getwd() # location where you will save if you dont alter working directory
tiff("Fig3.tiff", width=13, height=13,units="cm",res=300, compression="lzw")
cash2
dev.off()
########################################################



#############
## Fig S3. Break-even at 1% and 10% discount rate
#############

# 1% 
rm(dats)
(dats<- data.frame(t=seq(from=5, to=30, by=5), rate=NA))

for (i in c(1:6))
	{
dats$rate[i] <- (1/((1+ 0.01)^dats$t[i]))
	}

1/dats$rate # Cumulative rate
sum(dats$rate)

## Project accounting every five years, assuming 1% discount rate
rm(ints, ints1)
ints1 <- expand.grid(Cost_T1 =seq(0,4000, by=10), 
		   rate =    0.01, 
		  AddGR_Restoration = 1.5, 
		  AddGR_RestorationLow = 0.4, 
		  AddGR_RestorationHigh = 2.6, 
		  Years= 30) 
ints1$CP<-  (with(ints1,  Cost_T1/(5*3.67*AddGR_Restoration*(sum(dats$rate)))))
ints1$CP_low<-  (with(ints1,  Cost_T1/(5*3.67* AddGR_RestorationLow*(sum(dats$rate)))))
ints1$CP_High<-  (with(ints1,  Cost_T1/(5*3.67* AddGR_RestorationHigh*(sum(dats$rate)))))
tail(ints1)


# 10% 
rm(dats)
(dats<- data.frame(t=seq(from=5, to=30, by=5), rate=NA))

for (i in c(1:6))
	{
dats$rate[i] <- (1/((1+ 0.1)^dats$t[i]))
	}
1/dats$rate # Cumulative rate
sum(dats$rate)

## Project accounting every five years, assuming 10% discount rate
rm(ints10)
ints10 <- expand.grid(Cost_T1 =seq(0,4000, by=10), 
		   rate =    0.10, 
		  AddGR_Restoration = 1.5, 
		  AddGR_RestorationLow = 0.4, 
		  AddGR_RestorationHigh = 2.6, 
		  Years= 30) 
ints10 $CP<-  (with(ints10,  Cost_T1/(5*3.67*AddGR_Restoration*(sum(dats$rate)))))
ints10 $CP_low<-  (with(ints10,  Cost_T1/(5*3.67* AddGR_RestorationLow*(sum(dats$rate)))))
ints10 $CP_High<-  (with(ints10,  Cost_T1/(5*3.67* AddGR_RestorationHigh*(sum(dats$rate)))))
tail(ints10)
tail(ints1)
rm(ints)
ints<-rbind(ints1, ints10)

head(ints)
ints$rate<- with(ints, ifelse(rate==0.01, "1% Discount rate" , "10% Discount rate"))

quartz(); 
(cash <- ggplot(ints, aes(x=Cost_T1, y= CP, AddGR_Restoration))+
geom_line(aes(x=Cost_T1, y=CP, linetype = factor(AddGR_Restoration)))+
facet_grid(cols=vars(rate))+
theme_bw()+
theme(legend.position="none")+
xlab(expression(paste("Restoration cost ", " ","(US$ h", a^-1, sep = "", ")")))+
ylab("Break even point carbon price ($)")+
ylab(expression(paste("Break even point carbon price ", " ","(US$ per tC", O[2], sep = "", e, sep="", ")")))+
scale_y_log10(breaks=c(2,10,100))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"))+
geom_vline(xintercept=1500, lty=1, col="darkgreen")+
geom_vline(xintercept=2500, lty=1, col="darkgreen")+ 
geom_ribbon(data=ints,aes(x=Cost_T1, ymin=CP_low,ymax=CP_High),fill="darkgrey", alpha=0.5)+
annotate("text", x = c(2000), y = c(445), label = c("Restoration costs") , color=c("darkgreen"), size=2.8 , angle=0, fontface="bold")+
annotate("text", x = c(2000), y = c(355), label = c("in Sabah") , color=c("darkgreen"), size=2.8 , angle=0, fontface="bold")+
annotate("segment", y = 280, yend = 280, x = 1550, xend = 2450, colour = "darkgreen", size=1, alpha=0.6, arrow=arrow(ends="both", length=unit(0.1, "inches"))))




## Can add lines to highlight current prices and 
#  required to fulfil the Paris Agreement 
(cash2<- cash+ geom_rect(mapping=aes(xmin=-5, ymin=2, xmax=2000, ymax=10), col="red", fill=NA )+
geom_rect(mapping=aes(xmin=-5, ymin=40, xmax=4100, ymax=80), col="blue", fill=NA ))


# setwd("") # Specify where you would like the file
getwd() # location where you will save if you dont alter working directory
tiff("FigS3.tiff", width=26, height=13,units="cm",res=300, compression="lzw")
cash2
dev.off()

