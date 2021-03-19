#R script for bumble bee pathogen monitoring project

##libraries
library(raster)
library(rgdal) #for spTransform

##datafiles
CanadaReclass<-raster("reclassed.tif")
poly<-shapefile("CanadaPolygon.shp")

##data prep

#Need to merge individual rasters from the Canada Agricultural Resource Inventory together. Currently separated by province
provs<-c("aci_2019_bc.tif", "aci_2019_ab.tif", "aci_2019_sk.tif", "aci_2019_mb.tif", "aci_2019_on.tif",
            "aci_2019_qc.tif", "aci_2019_nl.tif", "aci_2019_pe.tif","aci_2019_nb.tif", "aci_2019_ns.tif"  ) #make a list of all province rasters
#names(provs) <-c ("BC", "AB", "SK", "MN", "ON", "QC", "NL", "PEI", "NB", "NS")
provsRast<-lapply(provs, raster) #applying raster function to list of provinces
CanadaAll<- do.call(merge, provsRast) #merge all provincial rasters
writeRaster(CanadaAll, "CanadaAllRasters.tif") #writing a raster file for all Ag data in Canada

#reclassify crop classes as pollinated by managed crops or not
reclass<-read.csv("CropReclassify.csv") #file to reclassify crop data as pollinated by managed bees
newCanada<-reclassify(CanadaAll, reclass, filename="reclassed.tif") #reclassifies the crop classifications as 1 (for pollinated by managed bees) or 2 (for not pollinated by managed bees)

##getting polygon of Canada for mapping

CanadaPoly<-getData('GADM',country='CAN',level=1) #accessing shapefile for Canada
shapefile(CanadaPoly, filename="CanadaPolygon.shp") #wriitng Canada polygon shapefile

#forgot to reproject before saving polygon need to reproject the shapefile to match the ag raster 
polynew<-spTransform(poly, crs(CanadaReclass))
shapefile(polynew, filename="CanadaPolygon.shp", overwrite=TRUE)


## Making plots

#raw plot of where the managed crops are
plot(CanadaReclass, col=c("#FFFFFFFF", "#533A71"), breaks=c(0,1,2), legend=FALSE)
plot(poly, add=T)

#making less fine resolution image of the map
crop100<-raster::aggregate(CanadaReclass, fact=30) #900 x 900 m resolution, still speckled 
crop10k<-raster::aggregate(crop100, fact=10) #9000 x 9000 resolution
writeRaster(crop10k, "CanadaLowRes.tif")
