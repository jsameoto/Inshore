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
land <- st_read("C:/Users/WILSONB/Documents/1_GISdata/Shapefiles/AtlanticCanada/AC_1M_BoundaryPolygons_wAnnBasin_StJohnR.shp") %>%
  #filter(POL_DIV == c("Nova Scotia", "New Brunswick", "Maine")) %>% 
  as("Spatial") #Convert to sp

costras <- costrasterGen(ScallopSurv.sp, land, projstr = crs(land))
 
r <- raster(extent(ScallopSurv.sp), resolution = 0.03, crs = crs(land)) #Create raster at fine resolution with extent of data.
costras <- resample(costras, r, method = "bilinear") # increse resolution of costras to raster with specified resolution.

costras[160:170,1:80] <- 10000 #inserting contguous barrier ***?***



#find average nearest neighbbour
W              <- owin(range(coordinates(ScallopSurv.sp)[,1]), range(coordinates(ScallopSurv.sp)[,2]))
kat.pp         <- ppp(coordinates(ScallopSurv.sp)[,1], coordinates(ScallopSurv.sp)[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

test <- ipdw(ScallopSurv.sp, costras, range = mean.neighdist * 10, "com", overlapped = FALSE)

plot(test, main = "Commercial")
plot(land, add = TRUE)



grd <- as.data.frame(spsample(ScallopSurv.sp, "regular", n = 15000))
names(grd) <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object