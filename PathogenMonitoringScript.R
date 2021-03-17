#R script for bumble bee pathogen monitoring project

##libraries
library(raster)

##datafiles
CanadaAll<-raster("CanadaAllRasters.tif") #created in data prep below

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