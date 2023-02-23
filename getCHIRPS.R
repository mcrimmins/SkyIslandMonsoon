# daily CHIRPS precip for Sky Island monsoon tracker
# MAC 02/22/23

# CHIRPS daily precip
# https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.html
# https://upwell.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=precipitation
library(raster)

SIbounds <- rgdal::readOGR(dsn = "./shapes", layer = "SkyIslandBoundary")
ext<-extent(SIbounds)

dateStart<-"1981-09-01"
dateEnd<-"1981-09-10"

# download.file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.nc?precip%5B(2022-06-01):1:(2022-09-30)%5D%5B(28.524994):1:(33.024994)%5D%5B(-111.975006):1:(-107.975006)%5D",
#               destfile = "temp.nc")

download.file(paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.nc?precip%5B(",dateStart,"):1:(",dateEnd,")%5D%5B(",ext[3],"):1:(",ext[4],")%5D%5B(",ext[1],"):1:(",ext[2],")%5D"),
              destfile = "temp.nc")


test<-stack("temp.nc")

plot(test)
