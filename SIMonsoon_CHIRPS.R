# get recent CHIRPS from UCSB
# MAC 04/28/27

# to do: get other boundary layers, automatic date selection between 06-01 and 09-30, perc of avg
# add blue polygon to gulf of CA

library(raster)
#library(ncdf4)
library(sf)

# load layers
# SIbounds <- rgdal::readOGR(dsn = "/home/crimmins/RProjects/SkyIslandMonsoon/shapes", layer = "SkyIslandBoundary")
#   ext<-extent(SIbounds)
#   lons<-c(ext@xmin-0.25, ext@xmax+0.25)
#   lats<-c(ext@ymin-0.25, ext@ymax+0.25)
# gulfCA <- rgdal::readOGR(dsn = "/home/crimmins/RProjects/SkyIslandMonsoon/shapes/gulfCA", layer = "Gulf_of_CA_poly")

#### NEW SF CODE ####
SIbounds <- st_read(dsn = "/home/crimmins/RProjects/SkyIslandMonsoon/shapes", layer = "SkyIslandBoundary")
  ext<-extent(SIbounds)
  lons<-c(ext@xmin-0.25, ext@xmax+0.25)
  lats<-c(ext@ymin-0.25, ext@ymax+0.25)
gulfCA <- st_read(dsn = "/home/crimmins/RProjects/SkyIslandMonsoon/shapes/gulfCA", layer = "Gulf_of_CA_poly")
st_crs(gulfCA) <- 4326  # WGS84

#### NEW MAP LAYERS CODE
us <- geodata::gadm(country = "USA", level = 0, path = tempdir(), resolution = 2)
  us<- st_as_sf(us)
states <- geodata::gadm(country = "USA", level = 1, path = tempdir(), resolution = 2)
  states <- st_as_sf(states)
mx <- geodata::gadm(country = "MEX", level = 1, path = tempdir(), resolution = 2)
  mx <- st_as_sf(mx)

# map layers
#states <- getData('GADM', country='United States', level=1)
#us <- getData('GADM', country='United States', level=0)
#mx <- getData('GADM', country='Mexico', level=1)
#goc <- rgdal::readOGR(dsn="/home/crimmins/RProjects/StateMonsoonMaps/shps", layer="goc")
#goc <- ggplot2::fortify(goc)
stateLab<-rbind.data.frame(c(33,-111.8,"Arizona"),
                c(32.5,-108.65,"New Mexico"),
                c(30,-111.5,"Sonora"),
                c(28.35,-111.9,"Gulf of CA"))
colnames(stateLab)<-c("lat","lon","lab")
  stateLab$lat<-as.numeric(as.character(stateLab$lat))
  stateLab$lon<-as.numeric(as.character(stateLab$lon))
  stateLab$lab<-as.character(stateLab$lab)
  

# set date ranges
 # yr<-2022
 # dateStart<-as.Date("2022-06-01")
 # dateEnd<-as.Date("2022-09-30")
# real time
yr<-as.numeric(format(Sys.Date(),"%Y"))
dateStart<-as.Date(paste0(yr,"-06-01"))
dateEnd<-Sys.Date()

# load cumulative climo; create grid from climoCHIRPS.R
allCumSum<-stack("/home/crimmins/RProjects/SkyIslandMonsoon/data/SI_CHIRPS_June_Sept_meanDailyCumPrecip_1981_2022.grd")

# download current year file
#yr<-format(Sys.Date(),"%Y")
#yr<-2022
download.file(paste0("https://data.chc.ucsb.edu/products/CHIRPS-2.0/prelim/global_daily/netcdf/p05/chirps-v2.0.",yr,".days_p05.nc"),
              destfile = "/home/crimmins/RProjects/SkyIslandMonsoon/temp.nc")
# current grids in stack
gridStack<-stack("/home/crimmins/RProjects/SkyIslandMonsoon/temp.nc")
# crop to SI region
gridStack<-crop(gridStack, extent(allCumSum))
# extract dates from layers
dates<-as.Date(names(gridStack),format="X%Y.%m.%d")
# subset to current date range
gridStack<-gridStack[[which(dates>=dateStart & dates<=dateEnd)]]
# set 0 and neg to NA
gridStack[gridStack <= 0] <- NA
# get total precip
totalPrecipAll<-(calc(gridStack, sum, na.rm=TRUE))/25.4
totalPrecipAllw0<-totalPrecipAll
totalPrecipAll[totalPrecipAll <= 0] <- NA
# calculate percent of cumulative average based on current June - Sep date range
percPrecip<-(totalPrecipAllw0/((allCumSum[[nlayers(gridStack)]])/25.4))*100

# get actual dates from layers present
gridDates<-as.Date(names(gridStack),"X%Y.%m.%d")

##### Make Maps #####
##### ggplot maps 
library(ggplot2)
library(rasterVis)
library(scales)
library(magick)

# map layers
SI_df<-fortify(SIbounds)
states<-fortify(states)
mx<-fortify(mx)
us<-fortify(us)
gulfCA<-fortify(gulfCA)

# total precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,24,2)
precLabs<-as.character(seq(0,24,2))
precLabs[13]<-">24"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

# Convert raster to dataframe for ggplot
prec_df <- as.data.frame(totalPrecipAll, xy = TRUE, na.rm = FALSE)
colnames(prec_df) <- c("x", "y", "value")  # Adjust if needed

p <- ggplot() +
  geom_raster(data = prec_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours = precipCols, na.value = "burlywood4", 
                       name = "inches", limits = c(0, 24), oob = squish, 
                       breaks = precBreaks, labels = precLabs) +
  guides(fill = guide_colorbar(barheight = 15, nbin = 500, raster = FALSE)) +
  # add layers
  geom_sf(data = SIbounds, colour = "black", fill = NA, size = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = mx, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = us, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = gulfCA, color = "black", fill="darkcyan", size = 0.25)+
  geom_text(data = stateLab, aes(x = lon, y = lat, label = lab),
            size = 4, col = "black", fontface = "bold", nudge_y = 0)+
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude")+
  ggtitle(paste0("Sky Island Region Total Precip (in.): ",gridDates[1]," to ",gridDates[length(gridDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: CHIRPS\nhttps://www.chc.ucsb.edu/data/chirps"))+
  theme(plot.title=element_text(size=14, face = "bold"))

##### OLD CODE -----
# p<-gplot(totalPrecipAll) + geom_tile(aes(fill = value)) +
#   #scale_fill_gradient2(low = 'white', high = 'blue') +
#   #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
#   #                     name="inches", limits=c(0,20),oob=squish)+
#   
#   scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
#                        name="inches", limits=c(0,24),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
#   guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
#   
#   coord_sf(xlim = lons, ylim = lats, expand = FALSE)+
#   xlab("Longitude") + ylab("Latitude") 
# 
# p<-p +  
#   #geom_sf(data=SIbounds,colour="black", fill=NA, size=0.25 )+
#   #geom_polygon( data=SI_df, aes(x=long, y=lat),colour="black", fill=NA, size=0.25 )+
#   #scale_x_continuous(breaks = c(-120,-140))+
#   #ggtitle("Total Precipitation  - PRISM")+
#   ggtitle(paste0("Sky Island Region Total Precip (in.): ",gridDates[1]," to ",gridDates[length(gridDates)]))+
#   labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
#                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: CHIRPS\nhttps://www.chc.ucsb.edu/data/chirps"))+
#   theme(plot.title=element_text(size=14, face = "bold"))
# p <- p + 
#   geom_sf(data = states, color = "black", fill = NA, size = 0.25)
# 
# p<-p+geom_path(data = states, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = 0.25)
# 
# p<-p+geom_path(data = mx, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = .25)
# 
# p<-p+geom_path(data = us, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = 1)
# 
# p<-p +  geom_polygon(data=gulfCA, aes(x=long, y=lat, group=group),colour="black", fill="darkcyan", size=0.25 )
#   
# p<-p+geom_text(data = stateLab, aes(x = lon, y = lat, label = lab),
#                size = 4, col = "black", fontface = "bold", nudge_y = 0)
######

# write out file
png(paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_TotalPrecip.png"),
    width = 7, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_TotalPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/SkyIslandMonsoon/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+80+2165")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_TotalPrecip.png"))


##### PERCENT OF AVERAGE MAP
# PERCENT of AVG Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("darkgoldenrod4", "white", "darkgreen","blue1","deepskyblue"))(50)
precBreaks<-seq(0,400,50)
precLabs<-as.character(seq(0,400,50))
precLabs[9]<-">400"
precLabs[1]<-"0"
#precBreaksmin<-seq(1,19,2)

# Convert raster to dataframe for ggplot
prec_df <- as.data.frame(percPrecip, xy = TRUE, na.rm = FALSE)
colnames(prec_df) <- c("x", "y", "value")  # Adjust if needed

p <- ggplot() +
  geom_raster(data = prec_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
                       name="% of avg", limits=c(0,400),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  # add layers
  geom_sf(data = SIbounds, colour = "black", fill = NA, size = 0.25) +
  geom_sf(data = states, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = mx, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = us, color = "black", fill = NA, size = 0.25)+
  geom_sf(data = gulfCA, color = "black", fill="darkcyan", size = 0.25)+
  geom_text(data = stateLab, aes(x = lon, y = lat, label = lab),
            size = 4, col = "black", fontface = "bold", nudge_y = 0)+
  coord_sf(xlim = lons, ylim = lats, expand = FALSE) +
  xlab("Longitude") + ylab("Latitude")+
  labs(title=paste0("Sky Island Region Percent of Avg Precip: \n ",gridDates[1]," to ",gridDates[length(gridDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: CHIRPS\nhttps://www.chc.ucsb.edu/data/chirps"))+
  theme(plot.title=element_text(size=14, face = "bold"))

##### OLD CODE -----
#theme_set(theme_bw())
# p<-gplot(percPrecip) + geom_tile(aes(fill = value)) +
#   #scale_fill_gradient2(low = 'white', high = 'blue') +
#   #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
#   #                     name="inches", limits=c(0,20),oob=squish)+
#   
#   scale_fill_gradientn(colours = precipCols, na.value="burlywood4", 
#                        name="% of avg", limits=c(0,400),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
#   guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
#   
#   coord_equal(xlim = lons, ylim = lats, expand = FALSE)+
#   xlab("Longitude") + ylab("Latitude") 
# 
# p<-p +  geom_polygon( data=SI_df, aes(x=long, y=lat),colour="black", fill=NA, size=0.25 )+
#   #scale_x_continuous(breaks = c(-120,-140))+
#   #ggtitle("Total Precipitation  - PRISM")+
#   #ggtitle(paste0("Sky Island Region Percent of Avg: ",gridDates[1]," to ",gridDates[length(gridDates)]))+
#   labs(title=paste0("Sky Island Region Percent of Avg Precip: \n ",gridDates[1]," to ",gridDates[length(gridDates)]))+
#   labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
#                       "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: CHIRPS\nhttps://www.chc.ucsb.edu/data/chirps"))+
#   theme(plot.title=element_text(size=14, face = "bold"))
# 
# p<-p+geom_path(data = states, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = 0.25)
# 
# p<-p+geom_path(data = mx, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = .25)
# 
# p<-p+geom_path(data = us, 
#                aes(x = long, y = lat, group = group),
#                color = 'black', size = 1)
# 
# p<-p +  geom_polygon(data=gulfCA, aes(x=long, y=lat, group=group),colour="black", fill="darkcyan", size=0.25 )
# 
# p<-p+geom_text(data = stateLab, aes(x = lon, y = lat, label = lab),
#                size = 4, col = "black", fontface = "bold", nudge_y = 0)
######

# write out file
png(paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_PercPrecip.png"),
    width = 7, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_PercPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/SkyIslandMonsoon/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=95,height = 95))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+80+2165")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/SkyIslandMonsoon/figs/SkyIslandRegion_Monsoon_PercPrecip.png"))


unlink('/home/crimmins/RProjects/SkyIslandMonsoon/temp.nc')

# push bullet notify
#source('/home/crimmins/RProjects/SkyIslandMonsoon/pushNotify.R')

