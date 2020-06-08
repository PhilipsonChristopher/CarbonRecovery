# Install required packages (needs internet connection)
### 1. Set desired repository  
repos <- "https://stat.ethz.ch/CRAN/"


# Install packages once then has out
install.packages("raster", repos= repos, dependencies=TRUE)
install.packages("rgeos", repos= repos, dependencies=TRUE)
install.packages("viridis", repos= repos, dependencies=TRUE)
install.packages("lme4", repos= repos, dependencies=TRUE)
install.packages("ggplot2", repos= repos, dependencies=TRUE)
install.packages("lattice", repos= repos, dependencies=TRUE)
install.packages("ggbeeswarm", repos= repos, dependencies=TRUE)
# install.packages("grid", repos= repos, dependencies=TRUE)

library(raster)
library(rgeos)
library(lme4)
library(ggplot2)
library(viridis)
library(lattice)
library(ggbeeswarm)
library(grid)

print(sessionInfo())


# Set your working directory
setwd("")
# Files Needed in this folder:
# 'ALLCoupes' indicates logged forest study area
# 'Danum' indicates conservation area
# 'Planting' for restored area


# GAO carbon map 
# Primary_ACD.csv 
# Restored_ACD.csv
# NaturalRegen_ACD.csv

 
# 'CRUISE_HA' from setups

#######################
## Load files #########
#######################

###########
## Shapefile of main area of study
###########


#1. Load shapefiles of logging coupes 
## This is the main area of study (all logged forest) including one unlogged area the dvfc 'water catchment'
AllCoupes <- shapefile("Coupes.shp")
AllCoupes <- spTransform(AllCoupes,  CRS=CRS("+init=epsg:32650"))
AllCoupes@proj4string
plot(AllCoupes)

## We here will only use Coupe 'Year' for random effect levels 
AllCoupes@data

# Year is year of logging, and 'NA' is correct for Watercatchment level 
AllCoupes@data[1,3] <- 1 # Here we change to 1 for Watercatchment level 
# We rasterise the area for easier manipulation (which requires numeric)

## Coupes 1000-1005 (unclear history, potentially not logged) get 0 for random effect level ID
AllCoupes@data$YEAR
AllCoupes@data$YEAR<- ifelse(AllCoupes@data$YEAR=="NA", 0, AllCoupes@data$YEAR)
AllCoupes@data
###########
## Shapefile # DVCA Conservation area 
Danum <- shapefile("Danum.shp")
Danum <- spTransform(Danum,  CRS=CRS("+init=epsg:32650"))
crs(Danum)
plot(Danum)
Danum@proj4string

## Make danum data allign logged forest file
Danum@data
Danum@data[1:4] <- c("DVCA", 10, 2, "UnLogged") 
Danum@data <- Danum@data[1:4]
colnames(Danum@data)<- colnames(AllCoupes@data)
# adding '2' for logging Year for danum (technically an NA)
# this column only used for random effect level


## Seperate Primary forest and logged-over forest
Primary<- rbind(Danum, subset(AllCoupes, Name=="WaterCatchment"))
Logged <- subset(AllCoupes, Name!="WaterCatchment")
plot(gUnaryUnion(subset(AllCoupes, Name!="WaterCatchment")), lty=2)
plot(gUnaryUnion(Primary), add=T, col='green')

###############################
## Areas of INFAPRO planting ##
###############################
Planting<- shapefile("Planting.shp")
Planting@data <- Planting@data[2:3]
crs(Planting)
Planting@data
plot(Planting)

Planting <- spTransform(Planting,  CRS=CRS("+init=epsg:32650"))
plot(gUnaryUnion(Planting), col='red')

Planting@data[,3:4] <-NA
names(Planting)<- names(AllCoupes)
AllCoupes@data


##########################################################################
### Fig2 (left panel) Carbon Map for our area from Asner Sabah Map #######
##########################################################################

## Code to cut area from Asner (2018) carbon map is hashed out. Included here for clarity
## See Asner (2018), doi.org/10.1016/j.biocon.2017.10.020
#GAO_Sabah <- raster("GAO_Sabah_ACD_30m_Masked.tif")
#str(GAO_Sabah)
#head(GAO_Sabah)
#GAO_Sabah@data
#proj4string(GAO_Sabah)<-CRS("+init=epsg:32650")
### Define full Map area 
#MapArea <- gUnaryUnion(rbind(Danum, Planting, AllCoupes))
#plot(MapArea)
#GAO_Map <- mask(crop(GAO_Sabah, MapArea),MapArea) 
#GAO_Map
# GAO_MapPoints <- rasterToPoints(GAO_Map)
# head(GAO_MapPoints)
# write.csv(GAO_MapPoints, row.names=FALSE, "GAO_Map.csv")




## Load Carbon map data for our area
GAO_MapPoints <- read.table("GAO_Map.txt", header=TRUE)
head(GAO_MapPoints)
tail(GAO_MapPoints)
GAO_Map <- rasterFromXYZ(as.data.frame(GAO_MapPoints)[, c("x", "y", "CAO_Sabah_ACD_30m_Masked")], crs=CRS)
GAO_Map
proj4string(GAO_Map)<-CRS("+init=epsg:32650")
plot(GAO_Map)

PrimaryOutline <- gUnaryUnion(Primary)
plot(PrimaryOutline, add=T)
NaturalOutline <- gUnaryUnion(Logged) 
plot(NaturalOutline, add=T)
RestoredOutline <- gUnaryUnion(Planting)
plot(RestoredOutline, add=T)

# define Figure 2 map extent
bbox <- as(raster::extent(AllCoupes), "SpatialPolygons") # , 520000, 575000, extent(c2)[3]
crs(bbox) <- crs(Logged)

# crop all to Figure 2 map extent
GAO_Map_F3 <- crop(GAO_Map, bbox)
NaturalOutline_F3 <- crop(NaturalOutline, bbox)
PrimaryOutline_F3 <- crop(PrimaryOutline, bbox)
RestoredOutline_F3<- crop(RestoredOutline, bbox)

## Graphical parameters
RestCol<-'#56B1F7'; NatCol<-"red" ; PrimeCol<- hcl(h = 130, c = 100, l = 70) 
lwd=1.5 # 1.5 for the tiff
lwd=3
mycols=inferno(10) #viridis(10)
cex4points <- 0.8
pch4points<-16
xlim=c(575000,690000) # extent(c2)[2]
# ylim=c(535000,571500) # extent(c2)[2]
par(mfrow=c(1,2), oma=c(0,1,0,3))
cex=1

#############################
##### CarbonMap###############
##############################
### All sized according to the following save to tiff code # tiff()
#tiff(filename = "DanumCarbonMap.tiff",width=13, height=13, units="cm",res=100,pointsize=8, compression="lzw")
par(mfrow=c(1,1))
plot(GAO_Map_F3,col=mycols,legend=F,ylim=c(538500, 575000), xlim=c(575000, 608500), xlab="Eastings UTM Zone 50N (m)", ylab="Northings UTM Zone 50N (m)", cex.lab=1.3,cex.axis=1.3,lwd=lwd, zlim=c(0,400))
plot(NaturalOutline_F3, border=NatCol, add=T,lwd= lwd) # 
plot(RestoredOutline_F3, border=RestCol, add=T,lwd= lwd)
plot(PrimaryOutline_F3, border=PrimeCol, add=T,lwd= lwd)
plot(GAO_Map_F3, legend.only=TRUE, legend.width=2.5, col=mycols, legend.shrink=1,legend.args=list(text=expression(paste("Aboveground Carbon Density", " ","(Mg h", a^-1, sep = "", ")")), side=4, font=1, line=3, cex=1), zlim=c(0,400), axis.args=list( cex.axis=1))
legend("topleft", legend = c( "Natural regeneration", "Active restoration", "Primary forest"), cex=1, border=c(NatCol, RestCol, PrimeCol) ,fill="NA")
# dev.off() # close device if saving to tiff


######################################################
### data organising for analysis
#########################################################
AllCoupes@data
Primary@data
Logged@data

## some patches with uncertain providence, currently zeros in dataset potentially mean officially unlogged, 
# data suggest there may have been considerable encroachment in some of these areas.
## I leave all these areas in, and show analysis with and without 
## some areas are riperian

dat <- rbind(Logged, Primary)
dat@data
plot(dat)

## Add in the small planted patch area outside original INFAPRO boundary
## all INFAPRO area is only once logged, 
## This area outside possibly logged more than once
## Doesnt change main results
## missingBit <- gDifference(Planting, dat)
## plot(missingBit)
## dataNAs<- dat@data[1,] ; dataNAs[1,]<-NA
## dat2 <- SpatialPolygonsDataFrame(missingBit, dataNAs) 
## dat<- rbind(dat, dat2)
## dat@data[31,]<-c(0,0, 0, "outside")


plot(GAO_Map)
plot(dat, add=T)
dat@data$COUPE_ID <- as.numeric(dat@data$COUPE_ID)
dat@data$YEAR <- as.numeric(dat@data$YEAR)
## Seperate coupeID code for each coupe, 'max' helps avoid errors at boundaries
Coupe <-  rasterize(dat, GAO_Map, 'YEAR', max)
plot(Coupe) # shows riparian and potentially unlogged areas
plot(GAO_Map)
plot(Coupe, add=T, alpha=0.5)

# Primary Area Carbon
plot(Primary)
PrimaryACD = mask(crop(GAO_Map, Primary), Primary)
plot(PrimaryACD, add=T)
CoupePrimary = mask(crop(Coupe, Primary), Primary)
plot(CoupePrimary, add=T, alpha=0.5)
plot(CoupePrimary) ## slight 'spillage' at boundary removed later

## Planted Area Carbon
Planting@data
Planting@data$SP_ID <- as.numeric(Planting@data$SP_ID)
Restored  <- rasterize(Planting, GAO_Map, 'SP_ID', max) # max PLANTING_Y
# If warning crs comment is lost, can ignore

RestoredACD = mask(crop(GAO_Map, Restored), Restored)
plot(RestoredACD) ## 
plot(Restored, add=T)
plot(Planting, col="red", add=T)
str(RestoredACD)
cellStats(RestoredACD, stat='mean', na.rm=TRUE)
CoupeRestored = mask(crop(Coupe, Restored), Restored)
plot(CoupeRestored, add=T, alpha=1)

# UnPlanted Area Carbon
plot(Logged)
Logged@data
plot(Planting)
Planting@data
Planting@data
NaturalRegen <- gDifference(Logged, Planting)
NaturalRegenACD = mask(crop(GAO_Map, NaturalRegen), NaturalRegen)
plot(NaturalRegenACD) # 'Spillage' narrow lines of data where overlap between carbon map and polygons. Cut out later
plot(NaturalRegen, add=T)
cellStats(NaturalRegenACD, stat='mean', na.rm=TRUE) # Lower without RIL coupe, and some small patches of unclear providence
CoupeNatRegen = mask(crop(Coupe, NaturalRegen), NaturalRegen)
plot(CoupeNatRegen, add=T, alpha=1.5)


plot(NaturalRegenACD)
plot(RestoredACD, add=T, legend=F)
plot(PrimaryACD, add=T, legend=F)


##########################################
##### Analysis & Violin plots  	##########
##########################################
# Map appears to show differences in stocks between areas. analysis to confirm

## Restored area to dataframe for analysis, with Coupe for ranef
plot(RestoredACD)
RestoredACD_dat <- data.frame(rasterToPoints(RestoredACD))
colnames(RestoredACD_dat)[3]<-"ACD"
RestoredACD_dat$treatment <-rep("Active restoration")
CoupeRestored_dat<-data.frame(rasterToPoints(CoupeRestored))
RestoredACD_dat<- merge(RestoredACD_dat, CoupeRestored_dat)
head(RestoredACD_dat)
colnames(RestoredACD_dat)[5]<-"Coupe"

## Natural regeneration area
plot(NaturalRegenACD)
NatRegenACD_dat <- data.frame(rasterToPoints(NaturalRegenACD))
NatRegenACD_dat$treatment <-rep("Natural regeneration")
colnames(NatRegenACD_dat)[3]<-"ACD"
CoupeNatRegen_dat<-data.frame(rasterToPoints(CoupeNatRegen))
NaturalRegenACD_dat <- merge(NatRegenACD_dat, CoupeNatRegen_dat)
dim(NaturalRegenACD_dat)
colnames(NaturalRegenACD_dat)[5]<-"Coupe"
head(NaturalRegenACD_dat)

# Primary forest area
PrimaryACD_dat <- data.frame(rasterToPoints(PrimaryACD))
PrimaryACD_dat$treatment <-rep("Primary forest")
colnames(PrimaryACD_dat)[3]<-"ACD"
CoupePrimary_dat<- data.frame(rasterToPoints(CoupePrimary))
PrimaryACD_dat <- merge(PrimaryACD_dat, CoupePrimary_dat)
colnames(PrimaryACD_dat)[5]<-"Coupe"

## some minor spillar to chop out
aggregate(PrimaryACD_dat$Coupe, by=list(PrimaryACD_dat$Coupe), function(x) length(x))
# there should be no coupe 1988, 1989, or 1991 in primary # Spillage!
head(PrimaryACD_dat)
PrimaryACD_dat <- subset(PrimaryACD_dat)
PrimaryACD_dat <- subset(PrimaryACD_dat, !(Coupe %in% c(1988, 1989, 1991)))
str(PrimaryACD_dat)
# there should be no coupe 1988, 1989, or 1991 in primary! # Spillage!

### 
head(RestoredACD_dat)
aggregate(RestoredACD_dat$Coupe, by=list(RestoredACD_dat$Coupe), function(x) length(x))
RestoredACD_dat <- subset(RestoredACD_dat, !(Coupe %in% c(1, 1986, 1992)))
# there should be no coupe 1986, 1992 or 1 in restored! # Spillage!

head(NaturalRegenACD_dat)
aggregate(NaturalRegenACD_dat $Coupe, by=list(NaturalRegenACD_dat $Coupe), function(x) length(x))
NaturalRegenACD_dat <- subset(NaturalRegenACD_dat, !(Coupe %in% c(2, 1983, 1987)))
# there should be no coupe 1986, 1992 or 1 in nat regen! # Spillage!

Dat<-rbind(PrimaryACD_dat, RestoredACD_dat, NaturalRegenACD_dat)
Dat$treatment <- factor(Dat$treatment, levels=c("Natural regeneration", "Active restoration", "Primary forest"))
str(Dat)
summary(Dat)

############################################################################
### Analysis  ##############################################################
############################################################################
mem1 <- lmer(ACD ~ treatment + (1|Coupe), data= Dat)
# plot(mem1)
# qqnorm(resid(mem1))
dotplot(ranef(mem1, condVar=TRUE))
mem1 ## We report this model in manuscript
round(fixef(mem1))
135+29 # restored estimate
135+59 # primary estimate

############################################################################
#### infered Lidar growth rates - Final revisions end May2020
41.12 # intercept from plots model (= post logging estimate), see Fig1 file
(41-17.85) # in project scenario (planted areas), see Fig1 file

### Average time since logging, page 28, line 20
## Average logging year for areas of known logging year, not including unlogged areas
Dat$treatment <- relevel(Dat$treatment, ref="Natural regeneration")
time_m1 <- lm(Coupe ~ treatment, data=subset(Dat, !(Coupe %in% c(0, 2, 1))))
time_m1
# natural regen
round(2016-coef(time_m1)[1],2)
round(2016 - confint(time_m1)[1,],2)

levels(Dat$treatment)
# restored
Dat$treatment <- relevel(Dat$treatment, ref="Active restoration")
time_m1 <- lm(Coupe ~ treatment, data=subset(Dat, !(Coupe %in% c(0, 2, 1))))
time_m1
round(2016-coef(time_m1)[1],2)
round(2016 - confint(time_m1)[1,],2)

####################################
# All areas (including RIL and patches)
# lmer, All areas including RIL, ## We report this model for entire area in the main text
round(fixef(mem1))
135+31 # 166
round(((166 - (41-17.85)) / 29.7), 1)
round(((135 - 41) / 26.8), 1)
############################################################################


############################
## excluding both RIL and the small patches (IDs 1000) that may not have been logged
mem1_noRILorPatchs <- lmer(ACD ~ treatment + (1|Coupe), data= subset(Dat, !(Coupe %in% c(1993, 0))))
round(fixef(mem1_noRILorPatchs))
dotplot(ranef(mem1_noRILorPatchs, condVar=TRUE))
Dat$treatment <- relevel(Dat$treatment, ref="Natural regeneration")
time_m1 <- lm(Coupe ~ treatment, data=subset(Dat, !(Coupe %in% c(1993,0, 2, 1))))
round(2016-coef(time_m1)[1],2) # natural regen average time
round(2016- (coef(time_m1)[1]+coef(time_m1)[2]),2) # restored average time
round(fixef(mem1_noRILorPatchs))
161-32
round(((161 - (41-17.85)) / 29.67), 1)
round(((129 - 41) / 27.23), 1)
############################








######################################
### Violin plots 
######################################
head(Dat)
# tiff(filename = "~/Google Drive/Papers In Prep/Carbon Recovery/SwarmInferno.tiff",width=24, height=12, units="cm",res=300,pointsize=8, compression="lzw")

Dat$treatment <- factor(Dat$treatment, levels=c("Natural regeneration", "Active restoration", "Primary forest"))
levels(Dat$treatment)

## This plots all data points so takes some time to plot. Could use sampling to speed up
Swarm <- ggplot(Dat, aes(x= treatment,y=ACD, color= ACD))+
scale_color_viridis(option="inferno")+
geom_quasirandom()+
xlab("")+
scale_y_continuous(limits=c(0, 400), expand = c(0, 0),name=expression(paste("\nAboveground Carbon Density", " ","(Mg h", a^-1, sep = "", ")")), position="right")+
theme_bw()+
theme(axis.text = element_text(size = 10, colour = "black"))+
theme(axis.text.y.right = element_text(margin = margin(t = 10, r = 10, b = 10, l = 5)),legend.position = "none")
# Swarm


### 1. Add predictions from model (mem1) to the plot
#########################################################
(NEWDAT<-expand.grid(treatment =levels(Dat$treatment)))
NEWDAT$ACD<-0
mm <- model.matrix(terms(mem1),NEWDAT)
length(fixef(mem1))
NEWDAT$ACD <- mm %*% fixef(mem1)
pvar1 <- diag(mm %*% tcrossprod(vcov(mem1),mm))
(NEWDAT <- data.frame(
    NEWDAT
    , plo = NEWDAT$ACD-2*sqrt(pvar1)
    , phi = NEWDAT$ACD+2*sqrt(pvar1)))
NEWDAT[,c(2:4)] <- round(NEWDAT[,c(2:4)])
NEWDAT

## requires more memory to run than above, similar estimates
# m1_mer <- bootMer(mem1,  nsim=1000, FUN=fixef, ncpus=8)
# round(fixef(mem1),1) 
# round(apply(m1_mer$t, 2, quantile, c(0.025, 0.975)), 1)


RestCol<-'#56B1F7'; NatCol<-"red" ; PrimeCol<- hcl(h = 130, c = 100, l = 70) 
SwarmMeans <- Swarm + geom_pointrange(data= NEWDAT, mapping=aes(x= treatment, y=ACD, ymin= plo, ymax= phi),  size=1 , fill="white", shape=22,colour=c(NatCol, RestCol, PrimeCol)) 
# SwarmMeans 
######

# tiff(filename = 'Fig2b.tiff',width=13, height=12, units="cm",res=300,pointsize=8, compression="lzw")
# SwarmMeans 
# dev.off()


## Plot next to map
#tiff(filename = 'Fig2.1.tiff',width=22.3, height=12, units="cm",res=300,pointsize=8, compression="lzw")
vp.BottomRight <- viewport(height=unit(0.85, "npc"), width=unit(0.45, "npc"), 
                           just=c("left","center"), 
                           y=0.475, x=0.52)
par(mfrow=c(1,2))
plot(GAO_Map_F3,col=mycols,legend=F,ylim=c(538500, 575000), xlim=c(575000, 608500), xlab="Eastings UTM Zone 50N (m)", ylab="Northings UTM Zone 50N (m)", cex.lab=1.3,cex.axis=1.3,lwd=lwd, zlim=c(0,400))
plot(NaturalOutline_F3, border=NatCol, add=T,lwd= lwd) # 
plot(RestoredOutline_F3, border=RestCol, add=T,lwd= lwd)
plot(PrimaryOutline_F3, border=PrimeCol, add=T,lwd= lwd)
plot(GAO_Map_F3, legend.only=TRUE, legend.width=2.5, col=mycols, legend.shrink=1,legend.args=list(text="", side=4, font=1, line=3, cex=1), zlim=c(0,400), axis.args=list( cex.axis=1))
legend("topleft", legend = c( "Natural regeneration", "Active restoration", "Primary forest"), cex=1, border=c(NatCol, RestCol, PrimeCol) ,fill="NA")
# plot violins
print(SwarmMeans+ theme(axis.text = element_text(size = 8, colour = "black")), vp=vp.BottomRight)
#dev.off()


########################################################################
## End of Main text ####################################################
########################################################################


