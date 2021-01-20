
#BREAKOUT SCRIPT - will need to run BOFSpatialPlotsofSurveyData_2020.r to run these



###################################################
#Solution 1 - Raster grid, stars, using gstat and cropping to bbox
###################################################
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

r <- raster(extent(ScallopSurv.sf), resolution = 0.03, crs= crs(ScallopSurv.sf)) #create raster with extent of scallop survey data

ScallopSurv.grid <- rasterize(ScallopSurv.sf, r, "com", mean) #rasterize sf point objects using means with the resolution in raster template.
#plot(ScallopSurv.grid)
ScallopSurv.grid.sf <- st_as_stars(ScallopSurv.grid, crs = st_crs(4326))

ScallopSurv.gstat <- gstat(id = "com", formula = com ~ 1, data = ScallopSurv.sf, 
                           nmax = 8, set = list(idp = 3.5))

z <- predict(ScallopSurv.gstat, ScallopSurv.grid.sf)
plot(z)

z
cont <- st_contour(z, na.rm = FALSE, breaks = c(1,5,10,50,100,200,300,400,500), contour_lines = TRUE) %>% 
  st_make_valid()
plot(cont)


###########################################################################
#Solution 2 - sp, grid, stars, sf, using idw, and cropping to bounding box
###########################################################################
library(sp)
library(sf)
library(stars)

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

S.idw <- gstat::idw(com ~ 1, ScallopSurv.sp, newdata=grd, idp = 3.5)
#plot(S.idw)

lvls <- c(1,5,10,50,100,200,300,400,500)

S.idw.sf <- st_as_stars(S.idw) %>% #convert to stars object
  st_transform(crs = 4326) %>% #crop to area around data points
  dplyr::select(var1.pred) %>% 
  st_as_sf(S.idw.sf, as_points = FALSE, merge = FALSE) %>%  #convert to sf object.. in order to integrate into pecjector function.
  st_intersection(b_box) %>%
  #mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% 
  mutate(brk = cut(var1.pred, breaks = brk)) %>% 
  dplyr::select(brk) %>% 
  mutate(col = brewer.pal(length(lvls),"YlGn"), level = lvls)

plot(S.idw.sf)
class(S.idw.sf)


#####################################################
#Solution 3 - using old method and converting to sf
#####################################################

#from original script using ScallopMap function and PBSMapping objects
com.contours <- contour.gen(subset(ScallopSurv,year==survey.year,c('ID','lon','lat','com')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)
lvls=c(1,5,10,50,100,200,300,400,500) #levels to be color coded
CL <- contourLines(com.contours$image.dat,levels=lvls) #breaks interpolated raster/matrix according to levels so that levels can be color coded
CP <- convCP(CL)
totCont.poly <- CP$PolySet

#Convert PBSMapping object to sf
totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid() %>% 
  mutate(col = brewer.pal(length(lvls),"YlGn"), level = lvls) #Adding in the colour palette and levels into the shapefile

plot(totCont.poly.sf)

#####################################################
#Solution 4 - trying ipdw package instead of gstat
#####################################################

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
W <- owin(range(coordinates(ScallopSurv.sp)[,1]), range(coordinates(ScallopSurv.sp)[,2]))
kat.pp <- ppp(coordinates(ScallopSurv.sp)[,1], coordinates(ScallopSurv.sp)[,2], window = W)
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

#########################################################################
#Working Solution - Pulling script from contour.gen function for testing
#########################################################################

#Dataset up:
contour.dat <- ScallopSurv %>%
  filter(year == survey.year) %>% 
  dplyr::select(lon, lat, com) # ("X","Y","Z")

datapoints1 <- contour.dat %>%
  mutate(EID = 1:nrow(contour.dat)) %>% 
  dplyr::select(EID, lon, lat)

#######Image.prep#######
#Parameters defined in image.prep function inside contour.gen.R function
aspr <- 1.345640
res<- 0.02
subscale <- 0.01

#subset.poly? - check old plot script for this argument... Xs and Ys are defined differently
#Xs<-seq(min(subset.poly$X)-subscale,max(subset.poly$X)+subscale,res*aspr)
#Ys<-seq(min(subset.poly$Y)-subscale,max(subset.poly$Y)+subscale, res)
Xs<-seq(min(contour.dat$lon)-subscale,max(contour.dat$lon)+subscale,res*aspr)
Ys<-seq(min(contour.dat$lat)-subscale,max(contour.dat$lat)+subscale, res)

tow.xy <- ScallopSurv %>% 
  dplyr::select(lon, lat)

poly <- tow.xy[chull(tow.xy), ]
grid.dat <- with(poly, expand.grid(lon = Xs, lat = Ys))

#gstat idw interpretation
Z.gstat <- gstat(id = "com", formula = com ~ 1, locations = ~ lon + lat, data = contour.dat, maxdist=Inf, nmax = 8, set = list(idp = 3.5))
Z.dat<- predict(Z.gstat, grid.dat)
#plot(Z.dat)

#######################
#working on idw object
#Z.dat <- st_as_sf(Z.dat, coords = c("lon", "lat"), crs = 4326) %>% #convert to sf object
  #st_transform(crs = 4326) %>% #crop to area around data points
  #st_as_sf(Z.dat, as_points = FALSE, merge = FALSE) %>% 
#  st_intersection(b_box) %>% 
  #mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% 
#  mutate(brk = cut(com.pred, breaks = brk)) %>% 
#  dplyr::select(com.pred) %>% 
#  st_as_stars()

#st_contour(Z.dat, contour_lines=TRUE, breaks = lvls)
plot(Z.dat)


image.data<-makeTopography(Z.dat[c('lon','lat','com.pred')],digits=5)
spatial.model<-Z.gstat

#image.data <- as.data.frame(image.data)
#image.data <- st_as_sf(image.data, coords = c("x","y"), crs = 4326)

CL <- contourLines(com.contours$image.dat,levels=lvls)#### Line from old script


###########################################################################
# ConvCP function from PBSmapping package - Contour lines to spatial object
############################################################################
vData <- unlist(CL) #vector contour lines


levelsIdx <- which(names(vData) == "level");
levelsIdx <- c(levelsIdx, length(vData) + 1);
## 'nVerts': # of vertices for each polyline
nVerts <- (diff(levelsIdx) - 1) / 2;
## 'levelsIdx': indices to each 'level' element in vector 'vData'
levelsIdx <- levelsIdx[-length(levelsIdx)];

#Longitude
n <- vector();
n[seq(1, by=2, length=length(nVerts))] <- nVerts;
n[seq(2, by=2, length=length(nVerts))] <- nVerts + 1;
n <- c(1, n[-length(n)], n[length(n)-1]);
b <- rep(c(FALSE, TRUE), length.out=length(n));
x <- vData [rep(b, n)];

#Latitude
n <- vector();
n[seq(1, by=2, length=length(nVerts))] <- nVerts + 1;
n[seq(2, by=2, length=length(nVerts))] <- nVerts;
b <- rep(c(FALSE, TRUE), length.out=length(n));
y <- vData [rep(b, n)];

## 'dupLevel': Boolean vector; TRUE when a level is a duplicate
dupLevel <- duplicated(vData[levelsIdx]);

## create the PID column
pid <- vector()
pid[which(!dupLevel)] <- 1:length(unique(vData[levelsIdx]));
pid[which(dupLevel)] <-
  rep(pid[!dupLevel], diff(c(which(!dupLevel), length(levelsIdx)+1))-1);
PolyData <- data.frame(PID=pid);
pid <- rep(PolyData$PID, nVerts);

## create the SID column
d <- diff(c(which(!dupLevel), length(levelsIdx)+1));
maxSid <- max(d);
sid <- rep(seq(from=1, to=maxSid), times=length(d));
n <- vector();
n[seq(1, by=2, length.out=length(d))] <- d;
n[seq(2, by=2, length.out=length(d))] <- maxSid - d;
b <- rep(c(TRUE, FALSE), length.out=length(n));
PolyData$SID <- sid[rep(b, n)];
sid <- rep(PolyData$SID, nVerts);

## create the POS column
maxPos <- max(nVerts);
pos <- rep(seq(from=1, to=maxPos), times=length(nVerts));
n <- vector();
n[seq(1, by=2, length.out=length(nVerts))] <- nVerts;
n[seq(2, by=2, length.out=length(nVerts))] <- maxPos - nVerts;
b <- rep(c(TRUE, FALSE), length.out=length(n));
pos <- pos[rep(b, n)];

## add level column to 'PolyData'
PolyData$level <- vData[levelsIdx];

## put the data frames together in a list
PolySet <- data.frame(PID=pid, SID=sid, POS=pos, X=x, Y=y);

### added in so that i could plot it to make sense of things
data <- merge(PolyData, PolySet, by = "PID")
data <- st_as_sf(data, coords = c("X", "Y"), crs = 4326)
plot(data)

#############################################################################################################
