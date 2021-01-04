library(raster)
library(stars)

##Setting up for plotting data contours... (re-vist another time for alternatives?)
b_box <- ScallopSurv %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com) %>%  
  st_buffer(0.08) %>% #create sf object around data points
  st_union()

ScallopSurv.sf <- ScallopSurv %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com)

r <- raster(extent(ScallopSurv.sf), resolution = 0.07999, crs= crs(ScallopSurv.sf)) #create raster with extent of scallop survey data

ScallopSurv.grid <- rasterize(ScallopSurv.sf, r, "com", mean) #rasterize sf point objects using means with the resolution in raster template.
#plot(ScallopSurv.grid)
ScallopSurv.grid.sf <- st_as_stars(ScallopSurv.grid)

ScallopSurv.gstat <- gstat(id = "com", formula = com ~ 1, data = ScallopSurv.sf, 
                           nmax = 8, set = list(idp = 0.5))

z <- predict(ScallopSurv.gstat, ScallopSurv.grid.sf)
plot(z)

S.idw <- mask(z, b_box)

cont <- st_contour(z, na.rm = FALSE, breaks = c(1,5,10,50,100,200,300,400,500), contour_lines = FALSE)
plot(cont)


#######################################################################################
library(sp)
library(sf)
library(stars)

data.idw <- function(data = data, var = "com")
  
  b_box <- ScallopSurv %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com) %>%  
  st_buffer(0.06) %>% #create sf object around data points
  st_union()

ScallopSurv.sp <- ScallopSurv
coordinates(ScallopSurv.sp) <- c("lon", "lat")
grd <- as.data.frame(spsample(ScallopSurv.sp, "regular", n = 15000))
names(grd) <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

proj4string(ScallopSurv.sp) <- CRS("+init=epsg:4326")
proj4string(grd) <- proj4string(ScallopSurv.sp)

S.idw <- gstat::idw(com ~ 1, ScallopSurv.sp, newdata=grd, idp = 2.5)

S.idw.sf <- st_as_stars(S.idw) %>% #convert to stars object
  st_transform(crs = 4326) %>% #crop to area around data points
  dplyr::select(var1.pred) %>% 
  st_as_sf(S.idw.sf, as_points = FALSE, merge = FALSE) %>%  #convert to sf object.. in order to integrate into pecjector function.
  st_intersection(b_box) %>%
  mutate(brk = cut(var1.pred, breaks = brk, dig.lab = 10)) %>% 
  dplyr::select(brk)

plot(S.idw.sf)
class(S.idw.sf)
###########################################################################

idw.output <- as.data.frame(S.idw)
idw.output$Longitude <- idw.output$lon
idw.output$Latitude <- idw.output$lat

brk <- c(1,5,10,50,100,200,300,400,500)

idw.output <- st_as_sf(idw.output, coords = c("lon", "lat"), crs = 4326, as_points=F,merge=F) %>% 
  st_intersection(b_box) %>% 
  dplyr::select(var1.pred) %>% #, Longitude, Latitude) %>% 
  st_as_stars() %>% 
  #mutate(brk = cut(var1.pred, breaks = brk, dig.lab = 10)) %>% 
  #dplyr::select(brk)
  
  #n.breaks <- length(unique(idw.output$brk)) 
  
  
  plot(idw.output)

class(idw.output)

col <- 
  nbin <- 20
ggplot(data=idw.output, aes(x=Longitude, y=Latitude)) + 
  geom_tile(aes(fill=var1.pred),color=NA) +
  scale_fill_manual(values = col[1:n.breaks])+
  stat_contour(aes(z=var1.pred), bins=nbin, color="#999999") +
  scale_fill_gradient2(low="blue",mid="white",high="red", midpoint=mean(idw.output$var1.pred)) +
  coord_equal()




#######################################################################################

##Setting up for plotting data contours... (re-vist another time for alternatives?)
ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon","lat"), crs = 4326) #convert to sf
com.contours.sf <- ScallopSurv.sf %>%
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com)

#########################################Requires contour.gen function, and PBSMapping ##################################################
#from original script using ScallopMap function and PBSMapping objects
com.contours <- contour.gen(subset(ScallopSurv,year==survey.year,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

########################################################################################################################################
#Convert PBSMapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly)
totCont.poly.sf <- totCont.poly.sf %>% 
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("MULTIPOLYGON") %>% #Convert multilines to polygons
  st_join(com.contours.sf[3]) %>% #combine with selected ScallopSurv data
  st_make_valid() %>% 
  st_buffer(0) #takes a while....otherwise we get the error "Hole lies outside shell at or near point -65.199470000000005 44.998051272292109 at -65.199470000000005 44.998051272292109" when plotting with pecjector.

plot(totCont.poly.sf)



#############################################################
#Example of plotting with p. (Freya's)
#p  + #geom_sf(data=shpf,aes(fill= Details))    +  scale_fill_manual(values = cols) + 
#     geom_sf(data=shpf,aes(linetype = `Number of Tows`))  + 
#     geom_sf(data=shpf,aes(colour = `Area (km^2)`))  +
#     new_scale("fill") + geom_sf(data=shpf,aes(fill= ID))    +  
#     geom_point(data=surv,aes(lon, lat, shape=`Tow type`),size=2) + scale_shape_manual(values = shp) + 
#     #taking advantage of OTHER aes types and then overriding them with fill (hacky but it works):
#     scale_fill_manual(values = cols, guide=guide_legend(override.aes = list(fill= cols)))  +
#     scale_colour_manual(values = rep("black", length(cols)), guide=guide_legend(override.aes = list(fill= cols)))  +
#     scale_linetype_manual(values = rep("solid", length(cols)), guide=guide_legend(override.aes = list(fill= cols)))  +
#     theme(legend.position = 'right',legend.direction = 'vertical',
#           legend.justification = 'left',legend.key.size = unit(.5,"line"))
# } # end  if(banks[i] %in% c("BBn" ,"BBs","Sab", "GBb", "GBa"))