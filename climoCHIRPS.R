# build CHIRPS climo for Sky Island region
# adapted from PRISMCumPrecip.R
# MAC 04/15/23

library(raster)

# SI boundary
SIbounds <- rgdal::readOGR(dsn = "./shapes", layer = "SkyIslandBoundary")
ext<-extent(SIbounds)
  ext@xmin<-ext@xmin-0.25
  ext@ymax<-ext@ymax+0.25
  ext@xmax<-ext@xmax+0.25
  ext@ymin<-ext@ymin-0.25

# loop through each and create growing stack of cumulative precip - does not work with webservice ----
# write to file
cumPrecipAll <- stack()
for(year in 1981:2022){
  # create current date
  dateStart=paste0(year,"-06-01")
  dateEnd= paste0(year,"-09-30")
  
  # generate dates -- keep with PRISM date
  #allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
  
  # download summer precip
  download.file(paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.nc?precip%5B(",dateStart,"):1:(",dateEnd,")%5D%5B(",ext[3],"):1:(",ext[4],")%5D%5B(",ext[1],"):1:(",ext[2],")%5D"),
                destfile = "temp.nc")
  # load to stack
  gridStack<-stack("temp.nc")
  
  # set 0 and neg to NA
  gridStack[gridStack < 0] <- NA
  ##
  
  print("processing raster stack")
  # get cumulative total precip
  # parallell calc
  ptm <- proc.time()
  beginCluster(6)
    tempGrid <- clusterR(gridStack, calc, args=list(fun=cumsum))
  endCluster()
  proc.time() - ptm
  
  # write to file
  #writeRaster(tempGrid,filename=paste0("./data/cumPrecip/SI_CHIRPS_June_Sept_",year,"_cumPrecip.grd"), overwrite=TRUE)
  
  cumPrecipAll <- stack(cumPrecipAll , tempGrid)
  print(year)
}
# ----

# get doy index
ndays<-length(seq(as.Date("1981-06-01"),as.Date("1981-09-30"),by="day"))
doyIdx<-rep(seq(1,ndays,1),nlayers(cumPrecipAll)/ndays)

# develop climo
meanDailyCumPrecip<- stackApply(cumPrecipAll, doyIdx, fun = mean)

# write to file
writeRaster(meanDailyCumPrecip,filename=paste0("./data/SI_CHIRPS_June_Sept_meanDailyCumPrecip_1981_2022.grd"), overwrite=TRUE)



