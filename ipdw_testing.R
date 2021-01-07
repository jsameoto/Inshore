library(raster)
library(stars)
library(ipdw)
library(spatstat)


#Create SpatialPointsDataframe
ScallopSurv.sp <- ScallopSurv %>%
  filter(year == survey.year) #Use only data from survey year (specified earlier)
coordinates(ScallopSurv.sp) <- c("lon", "lat")
proj4string(ScallopSurv.sp) <- CRS("+init=epsg:4326")

#Create Cost Raster using land layer - allows for in-water interpolation rather than Euclidean distances
land <- st_read("Y:/INSHORE SCALLOP/BoF/Coastline/AtlCan_1M_BoundaryPolygons_modified/AC_1M_BoundaryPolygons_wAnnBasin_StJohnR.shp") %>%
  #filter(POL_DIV == c("Nova Scotia", "New Brunswick", "Maine")) %>% 
  as("Spatial") #Convert to sp

costras <- costrasterGen(ScallopSurv.sp, land, projstr = crs(land))
 
r <- raster(extent(ScallopSurv.sp), resolution = 0.07, crs = crs(land)) #Create raster at fine resolution with extent of data.
costras <- resample(costras, r, method = "bilinear") # increse resolution of costras to raster with specified resolution.


#find average nearest neighbbour
W              <- owin(range(coordinates(ScallopSurv.sp)[,1]), range(coordinates(ScallopSurv.sp)[,2]))
kat.pp         <- ppp(coordinates(ScallopSurv.sp)[,1], coordinates(ScallopSurv.sp)[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

test <- ipdw(ScallopSurv.sp, costras, range = mean.neighdist, "com", overlapped = FALSE)

plot(test, main = "Commercial")
plot(land, add = TRUE)

ipdw.com <- st_as_stars(test) %>% #convert to stars object
  st_transform(crs = 4326) %>% #crop to area around data points
  st_as_sf(test, as_points = FALSE, merge = FALSE) #convert to sf object.. in order to integrate into pecjector function.

plot(ipdw.com)


##########
#Set aesthetics for plot
breaks <- c(1,5,10,50,100,200,300,400,500) #Set breaks
n.breaks <- length(unique(ipdw.com$layer)) 
col <- brewer.pal(length(breaks),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.6))

#basemap with data contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = ipdw.com %>% #Set breaks
                                                                                                                                                     mutate(brk = cut(layer, breaks = breaks, dig.lab = 10)) %>% 
                                                                                                                                                     dplyr::select(brk), size = 1, fill = "cfd", color = NA))

p +
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.1) +
  labs(title = "BoF Density (>= 80mm)",
       x = "Longitude",
       y = "Latitude") +
  theme_void() +
  theme(legend.position = "bottom")