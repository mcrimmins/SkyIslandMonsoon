# daily CHIRPS precip for Sky Island monsoon tracker
# MAC 02/22/23

# CHIRPS daily precip
# https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.html
# https://upwell.pfeg.noaa.gov/erddap/search/index.html?page=1&itemsPerPage=1000&searchFor=precipitation
library(raster)

SIbounds <- rgdal::readOGR(dsn = "./shapes", layer = "SkyIslandBoundary")
ext<-extent(SIbounds)
  lons<-c(ext@xmin, ext@xmax)
  lats<-c(ext@ymin, ext@ymax)
  

dateStart<-"2023-03-01"
dateEnd<-"2023-03-31"

# download.file("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.nc?precip%5B(2022-06-01):1:(2022-09-30)%5D%5B(28.524994):1:(33.024994)%5D%5B(-111.975006):1:(-107.975006)%5D",
#               destfile = "temp.nc")

download.file(paste0("https://upwell.pfeg.noaa.gov/erddap/griddap/chirps20GlobalDailyP05.nc?precip%5B(",dateStart,"):1:(",dateEnd,")%5D%5B(",ext[3],"):1:(",ext[4],")%5D%5B(",ext[1],"):1:(",ext[2],")%5D"),
              destfile = "temp.nc")

gridStack<-stack("temp.nc")

# get total precip
totalPrecipAll<-(calc(gridStack, sum, na.rm=TRUE))/25.4

# load cumulative climo
allCumSum<-stack("./data/SI_CHIRPS_June_Sept_meanDailyCumPrecip_1981_2022.grd")
# calculate percent of cumulative average
percPrecip<-totalPrecipAll/((allCumSum[[nlayers(gridStack)]])/25.4)

# test out leaflet
# library(leaflet)
# library(leafem)
# r<-totalPrecipAll
# pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
#                     na.color = "transparent")
# 
# 
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(r, colors = pal, opacity = 0.8) %>%
#   addLegend(pal = pal, values = values(r),
#             title = "Perc Precip")%>%
#   addMouseCoordinates() %>%
#   addImageQuery(r)
# 
# lid<-"totPrecip"
# 
# leaflet() %>% addTiles() %>%
#   addRasterImage(r, project = TRUE, group = lid,
#                  layerId = lid) %>%
#   addImageQuery(r, project = TRUE,
#                 layerId = lid,position="bottomleft",prefix = "") %>%
#   addLayersControl(overlayGroups = lid)


##### ggplot maps -----
library(ggplot2)
library(rasterVis)
library(scales)

# map layers
SI_df<-fortify(SIbounds)

# total precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,20,2)
precLabs<-as.character(seq(0,20,2))
precLabs[11]<-">20"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(totalPrecipAll) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
                       name="inches", limits=c(0,20),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = lons, ylim = lats, expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=SI_df, aes(x=long, y=lat),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Total Precipitation  - PRISM")+
  ggtitle(paste0("Total Precipitation (in.): ",dateStart," to ",dateEnd, " (CHIRPS)"))
  # labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
  #                     "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: NOAA MPE Analysis\nhttps://water.weather.gov/precip/"))+
  # theme(plot.title=element_text(size=14, face = "bold"))

# write out file
png(paste0("./figs/SkyIslandRegion_Monsoon_TotalPrecip.png"),
    width = 8, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

