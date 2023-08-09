#libraries

library(rgdal)
library(raster)
library(prioritizr)
library(gurobi)
library(dplyr)

####datasets
BeeData<-read.csv("ManagedBeeData.csv", stringsAsFactors = F)
speciesstack<-stack("speciesstack.tif")
canada <- raster("CanadaPolyRaster5km.grd") # cost file
canada[canada > 0] <- 1 # set cost of each pixel to be 1
CPA<-raster("CPA30.tif")
CensusBounds<-readOGR(dsn=".",layer="AgCensusBounds") #Ag census boundary shapefile

BeeData<-BeeData %>% rename(year= ï..year ) %>% filter(year==2016) #removes 2011 data
BeeData2<-BeeData[grep("CCS",BeeData$GEO),]
BeeData2<-BeeData2 %>% mutate(CensusCode=substr(DGUID, 10,16)) #making a column that matches
#the agricultural census polygon name so the managed bee data can be linked to the polygon
BeeData2<-subset(BeeData2, Unit.of.measure=="Number")
HoneyBeeData<-subset(BeeData2, Bees=="Colonies of honeybees")
OtherBeeData<-subset(BeeData2, Bees=="Gallons of other pollinating bees")

PollinatedCrops<-raster("reclassed.tif") #Pollinated = 2, not pollinated = 1
Crops5K<-raster("crops5km.tif")
HoneyBeeLayer<-raster("HoneyBeeLayer.tif")
OtherBeeLayer<-raster("OtherBeeLayer.tif")

totalrisk<-raster("totalrisk.tif")
eastrisk<-raster("eastrisk.tif")
westrisk<-raster("westrisk.tif")

landcover<-stack("newlandstack.tif")

#######
### Data prepping
#######

#attaching managed bee data to ag census polygons

HoneyBeeBounds<-merge(CensusBounds, HoneyBeeData, by.x="CCSUID", by.y="CensusCode", duplicateGeoms = T)
OtherBeeBounds<-merge(CensusBounds, OtherBeeData, by.x="CCSUID", by.y="CensusCode", duplicateGeoms = T)

### binning managed bee data

#Honey bee data

HoneyBeeData2 <- HoneyBeeData 
HoneyBeeData2[is.na(HoneyBeeData2) | HoneyBeeData2 < 0] <- 0
quantile(HoneyBeeData2$VALUE)
#trying to make levels for amount of honey bees, but there are lots of zeros and high values
#one bin will be zero for sure so look at data without zeros
HoneyBeeNoZero <- subset(HoneyBeeData2, VALUE >0)
quantile(HoneyBeeNoZero$VALUE) #level 1 = up to 56, level 2 = 180, level 3 = 891, level 4 >31702
HoneyBeeData3 <- HoneyBeeData2 %>% mutate(new_bin = cut(VALUE, breaks=c(0, 1, 56, 180, 891,50000)-1)) %>% 
mutate(new_bin = as.numeric(new_bin)-1)
  
HoneyBeeData3$new_bin <- as.numeric(as.factor(HoneyBeeData3$new_bin)) 

#other bee data
OtherBeeData2 <- OtherBeeData 
OtherBeeData2[is.na(OtherBeeData2) | OtherBeeData2 < 0] <- 0
#trying to make levels for amount of Other bees, but there are lots of zeros and high values
#one bin will be zero for sure so look at data without zeros
OtherBeeNoZero <- subset(OtherBeeData2, VALUE >0)
quantile(OtherBeeNoZero$VALUE) #level 1 = up to 34, level 2 = 159, level 3 = 948, level 4 >948
OtherBeeData3 <- OtherBeeData2 %>% mutate(new_bin = cut(VALUE, breaks=c(0, 34, 159, 948), 
                                                        include.lowest=T))
OtherBeeData3$new_bin <- as.numeric(as.factor(OtherBeeData3$new_bin)) 

##  attaching managed bee data to ag census polygons

HoneyBeeBounds<-merge(CensusBounds, HoneyBeeData3, by.x="CCSUID", by.y="CensusCode", duplicateGeoms = T)
OtherBeeBounds<-merge(CensusBounds, OtherBeeData3, by.x="CCSUID", by.y="CensusCode", duplicateGeoms = T)

##  turning the BeeBounds polygon into a raster
emptyRaster <- raster(ncols=ncol(CPA), nrows=nrow(CPA), crs=crs(HoneyBeeBounds), ext=extent(HoneyBeeBounds))
HoneyBeeLayer<-rasterize(HoneyBeeBounds, emptyRaster, field="new_bin", fun="max", na.rm=T)
HoneyBeeLayer <- projectRaster(HoneyBeeLayer, CPA, method="ngb")

emptyRaster <- raster(ncols=ncol(CPA), nrows=nrow(CPA), crs=crs(OtherBeeBounds), ext=extent(OtherBeeBounds))
OtherBeeLayer<-rasterize(OtherBeeBounds, emptyRaster, field="new_bin", fun="max", na.rm=T)
OtherBeeLayer <- projectRaster(OtherBeeLayer, CPA, method="ngb")

writeRaster(HoneyBeeLayer, "HoneyBeeLayer.tif", overwrite=T)
writeRaster(OtherBeeLayer, "OtherBeeLayer.tif", overwrite=T)

PollinatedCrops<-raster("reclassed.tif") #Pollinated = 2, not pollinated = 1

Crops5K<-projectRaster(Crops5K, CPA) ###OLD STUFF trying new stuff
writeRaster(Crops5K, "crops5km.tif")

Crops5K<-raster("crops5km.tif")
aggregated_raster <- aggregate(PollinatedCrops, fact = 166, fun = function(x, na.rm = T) {
  if (any(!is.na(x) & x == 2)) {
    return(1)
  } else {
    return(0)
  }
})
writeRaster(aggregated_raster, "crops5km.tif", overwrite=T)
Crops5K <- raster("crops5km.tif")

#####  Determining risk

# first make plots of each risk layer individually for possible figures in the paper
HoneyBeeLayer.df <- as.data.frame(HoneyBeeLayer, xy=T) %>%  #ggplot2 needs raster as dataframe
  na.omit()
library(ggplot2)
hbmap <- ggplot(data=HoneyBeeLayer.df) +
  geom_raster(aes(x=x, y=y, fill = HoneyBeeLayer)) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  theme_minimal() 
hbmap


Crops5K<-projectRaster(Crops5K, HoneyBeeLayer)
totalrisk <- (HoneyBeeLayer + OtherBeeLayer + Crops5K) #summing risk

library(dplyr)


#split longitude -95.16 
totalrisk<-raster("totalrisk.tif")
eastrisk<-crop(totalrisk, extent(-95.16, -52.625, 41.66667, 83.125))
westrisk<-crop(totalrisk, extent(-141, -95.16, 41.66667, 83.125))

writeRaster(eastrisk, "easkrisk.tif")
writeRaster(westrisk, "westrisk.tif")

#####
###Make plot with conservation priority areas
#####

#total risk plot
totalrisk[totalrisk==0]<-NA
plot(CPA, legend=F, xlim=c(-150,-50), col = c("#E4E1DD", "#6E7DAB"))
col <- hcl.colors(5000, "Inferno", rev=T, alpha=0.85)
col[1] <- "NA"
plot(totalrisk, add=T, xlim=c(-150,-50), col=col)

#Eastern Canada plot
pdf("eastrisk.pdf", width=9, height=12)
eastrisk[eastrisk==0]<-NA
plot(CPA, legend=F, xlim=c(-96,-50), col = c("#E4E1DD", "#517664"))
col <- hcl.colors(5000, "Inferno", rev=T, alpha=0.6)
col[1] <- "NA"
plot(eastrisk, add=T, xlim=c(-96,-50), col=col)
devoff.()


############
#Data analysis
############

###summarizing most at risk areas

#what landcovers are in areas where risk and CPA overlap
riskpoly<-rasterToPolygons(totalrisk, function(x){x>=1}, dissolve=TRUE)
riskCPA<-mask(CPA, riskpoly)
riskCPApoly<-rasterToPolygons(riskCPA, function(x){x>=1}, dissolve=TRUE)

landrisk<-extract(landcover, riskCPApoly)
do.call(function(x)colSums(x, na.rm=T), landrisk)
write.csv(do.call(function(x)colSums(x, na.rm=T), landrisk), "landcoverriskCPA.csv")

#how much of the CPAs are within at-risk areas? 
protected<-length(CPA[CPA==1]) #117869
riskCPArast<-rasterize(riskCPApoly, CPA)
overlap<-length(riskCPArast[riskCPArast>=1]) #27709
overlap/protected #0.235083

#how much of the at-risk areas have CPAs?
total<-length(totalrisk[totalrisk>=1]) #78563
overlap/total #0.3526978
  
#rank the worst CCS by province, by Canada
censusrisk<-extract(totalrisk, CensusBounds)
sumna <-function(x)sum(x,na.rm=T)
CensusBounds$CSSrisk <- unlist(lapply(censusrisk, sumna))
write.csv(CensusBounds, "censusriskccs.csv", row.names=F)

#which CCS have CPAs
censusCPAS<-extract(CPA, CensusBounds)
sumna <-function(x)sum(x,na.rm=T)
CensusBounds$CSSCPAnum <- unlist(lapply(censusCPAS, sumna))
write.csv(CensusBounds, "censusCPRACCS.csv", row.names=F)

#Which CCS have CPAs that are at risk?
censusCPArisk<-extract(riskCPArast, CensusBounds)
sumna <-function(x)sum(x,na.rm=T)
CensusBounds$CPAnum <- unlist(lapply(censusCPArisk, sumna))
write.csv(CensusBounds, "censusCPRArisk.csv", row.names=F)

###Census area plots

#map of census areas with three colours
#one colour for having CPAs = green
#one colour for having at-risk areas = yellow
#one colour if they combine = purple
#if none of the above = white

censusdata<-read.csv("censusCPRArisk.csv")

colours<-ifelse(censusdata$CPAnum > 0, "#EC9F05",
                ifelse(censusdata$CSSCPAnum  > 0, "#F3F9D2",
                ifelse(censusdata$CSSrisk > 0, "#4B543B", "white")))
plot(CensusBounds, col=colours)

##Make plot of landcovers

landdata<-read.csv("landcoverriskCPA.csv")
names(landdata)[1:2]<-c("landcover","pixelarea")

keeps <-landdata %>% group_by(landcover) %>% summarize(n=min(pixelarea)) %>% filter(n>1)
landdata2 <- landdata %>% filter(landcover %in% keeps$landcover)

#making plot
library(ggplot2)
ggplot(data=landdata2, aes(x=reorder(landcover, -pixelarea), y=pixelarea))+
  geom_bar(stat="identity",  color="black", position=position_dodge())  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Landcover Class")+ ylab("pixel area")+scale_y_continuous(expand=c(0,0))

polyarea<-area(CensusBounds, na.rm=T)
censusdata$area<-polyarea
write.csv(censusdata, "censusdataarea.csv", row.names=F)
