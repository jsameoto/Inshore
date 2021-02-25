
#BREAKOUT SCRIPT - will need to run BOFSpatialPlotsofSurveyData_2020.r to run these

################################
## Option - written by Freya?##
###############################

### Prep the contours 
### Pre-rec Survey Distribution (0-64 mm) ###

com.contours<-contour.gen(subset(ScallopSurv,year==2019,c('ID','lon','lat','pre')),ticks='define',nstrata=7,str.min=0,place=2,id.par=3.5,units="mm",interp.method='gstat',key='strata',blank=T,plot=F,res=0.01)

contour_ras <- raster(com.contours$image.dat)
# To convert a raster to a spatial polygon.is easy..
contour_sp <- as(contour_ras, "SpatialPolygonsDataFrame") 
# Make it an sf object
contour_sf <- st_as_sf(contour_sp,as_points=F,merge=F) 
# Now we need to convert to the coordinate system you want
contour_sf <- st_transform(contour_sf,crs = 4326)

lvls=c(1,5,10,50,100,200,300,400,500)

# REPEAT ABOVE FOR ALL CONTOUR GROUPINGS (Rec, Com, biomass, clappers etc...)
################### PLOT IT! ###############################
## do this for each contour grouping. change contour_sf object to match above object name.

p <- pecjector(area = list(x=c(-67.2, -64.3), y=c(43.6, 45.6), crs=4326), 
               add_layer = list(land="beige", 
                                bathy = c(50, 'c', 200)),#, 
               #survey=c("inshore", "outline")), # don't add these here because it puts too many on.
               # add the contours here: 
               add_custom = list(obj=contour_sf, 
                                 scale=list(breaks = lvls, 
                                            scale='d', 
                                            palette=brewer.pal(length(lvls), "YlGn"),
                                            leg.name = "#/tow")), 
               legend=T) +
  # moving the legend
  theme(legend.position = c(0.99, 0.01), 
        legend.background = element_rect(fill="white"), 
        legend.key.height = unit(0.25, "in"),
        legend.justification = c(1,0))

# manual corrections to bathy colour and transparency (alpha)
p$layers[[2]]$aes_params$colour <- "blue"
p$layers[[2]]$aes_params$alpha <- 0.25
# relabelling the legend
p$scales$scales[[3]]$labels <- c("1-5", "5-10", "10-50", "50-100", "100-200", "200-300", "300-400", "400-500", "500+")

p <- p + geom_sf(data=strata, fill=NA)

p <- p + geom_sf(data=VMSpoly, linetype="dashed", size=0.5)+
  geom_sf(data=inVMS, linetype="dashed", size=0.5)

p


########################################################################################################################################
#Solution 1 - sp, grid, stars, sf, using idw, and cropping to bounding box  **Really close to old plot, but still needs some tweeking**
########################################################################################################################################

library(sp)
library(sf)
library(stars)
library(smoothr)

b_box <- ScallopSurv %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com) %>%  
  st_buffer(0.06) %>% #create sf object around data points
  st_union() %>% 
  st_as_sf()

ScallopSurv.sp <- ScallopSurv %>% #Save data as new object
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com, lon, lat) %>% 
  filter(com != 0, !is.na(com)) #Remove NAs and zero values #Check.

coordinates(ScallopSurv.sp) <- c("lon", "lat")
grd <- as.data.frame(spsample(ScallopSurv.sp, "regular", n = 15000))
names(grd) <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object
proj4string(ScallopSurv.sp) <- CRS("+init=epsg:4326")
proj4string(grd) <- proj4string(ScallopSurv.sp)

S.idw <- gstat::idw(com ~ 1, ScallopSurv.sp, newdata=grd, idp = 3.5)
plot(S.idw)

S.idw <- st_as_stars(S.idw)

lvls <- c(1,5,10,50,100,200,300,400,500)

z.crop <- S.idw  %>%
  st_as_sf(as_points = FALSE, merge = FALSE) %>% 
  #st_as_stars() %>% 
  st_transform(crs = 4326) %>%
  st_intersection(b_box) %>%
  mutate(brk = cut(var1.pred, breaks = lvls, right = FALSE)) %>%
  group_by(brk) %>%
  summarise(brk = paste(unique(brk))) %>%
  mutate(col = brewer.pal(length(brk),"YlGn"), level = lvls) %>%
  mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% 
  mutate(brk = fct_reorder(brk, level)) %>%
  dplyr::select(brk) %>% 
  smooth(method = "ksmooth", smoothness = 2)

plot(z.crop)


#####################################
#With contours? Work in progress - trying to get closed contours using b_box..
cont <- S.idw %>%
  st_contour(na.rm = FALSE, breaks = c(1,5,10,50,100,200,300,400,500), contour_lines = TRUE) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_intersection(b_box)
########################################

#lvls <- c(1,5,10,50,100,200,300,400,500)
n.breaks <- length(lvls)
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.6), name = expression(frac(N,tow))) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = z.crop, size = 1, fill = "cfd", color = NA))

#Final plot with survey data and custom legend
p +
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.1) +
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  #theme_void() +
  theme(plot.title = element_text(size = 15, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 15, face = "bold"), 
        legend.text = element_text(size = 12),
        legend.position = c(.85,.25), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(10, 30, 10, 10))


#############################################################################################################
#Solution 2 - Raster grid, stars, using gstat and cropping to bbox - not working as well... still in progress
#############################################################################################################
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
  st_transform(crs = 4326) %>% 
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com)

r <- raster(extent(ScallopSurv.sf), resolution = 0.03, crs= crs(ScallopSurv.sf)) #create raster with extent of scallop survey data
#grid.sf <- rasterize(ScallopSurv.sf, r, "com", mean) #rasterize sf point objects using means with the resolution in raster template.
#plot(ScallopSurv.grid)
grid.sf <- st_as_stars(grid.sf, crs = 4326) %>%  
  st_transform(4326)

ScallopSurv.gstat <- gstat(id = "com", formula = com ~ 1, data = ScallopSurv.sf, 
                           nmax = 8, set = list(idp = 3.5))

z <- predict(ScallopSurv.gstat, grid.sf)
plot(z)

z.crop <- z %>%
  st_crop(b_box) %>% 
  st_transform(crs = 4326) %>% 
  st_as_sf(as_points = FALSE, merge = FALSE) %>% 
  mutate(brk = cut(com.pred, breaks = lvls, right = FALSE)) %>%
  group_by(brk) %>%
  summarise(brk = paste(unique(brk))) %>%
  mutate(col = brewer.pal(length(brk),"YlGn"), level = lvls) %>%
  mutate(brk = c(paste(lvls[-length(lvls)],'-',lvls[-1],sep=''),paste(lvls[length(lvls)],'+',sep=''))) %>% 
  mutate(brk = fct_reorder(brk, level)) %>%
  dplyr::select(brk) %>% 
  smooth(method = "ksmooth", smoothness = 2)

#str(z.crop)
plot(z.crop)

#####
lvls <- c(1,5,10,50,100,200,300,400,500)
n.breaks <- length(lvls)
col <- brewer.pal(length(lvls),"YlGn") #set colours
cfd <- scale_fill_manual(values = alpha(col[1:n.breaks], 0.6), name = expression(frac(N,tow))) #set custom fill arguments for pecjector.

#Pecjector with custom contour layer
p <- pecjector(area = "bof",repo ='github',c_sys="ll", gis.repo = 'github', plot=F,plot_as = 'ggplot',
               add_layer = list(land = "grey", bathy = "ScallopMap", survey = c("inshore", "outline"), scale.bar = c('tl',0.5)), add_custom = list(obj = z.crop, size = 1, fill = "cfd", color = NA))

#Final plot with survey data and custom legend
p +
  geom_spatial_point(data = ScallopSurv %>% 
                       filter(year == survey.year), #survey.year defined in beginning of script
                     aes(lon, lat), size = 0.1) +
  labs(title = paste(survey.year, "", "BoF Density (>= 80mm)"), x = "Longitude",
       y = "Latitude") +
  #theme_void() +
  theme(plot.title = element_text(size = 15, hjust = 0.5), #plot title size and position
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 15, face = "bold"), 
        legend.text = element_text(size = 12),
        legend.position = c(.85,.25), #legend position
        legend.box.background = element_rect(colour = "white", fill= alpha("white", 0.7)), #Legend bkg colour and transparency
        legend.box.margin = margin(10, 30, 10, 10))

cont <- z.crop %>% 
  st_contour(na.rm = FALSE, breaks = c(1,5,10,50,100,200,300,400,500), contour_lines = TRUE) %>%
  st_transform(crs = st_crs(4326)) %>%
cont <- st_difference(b_box, st_union(cont))
plot(cont)


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

#######################
#working on idw object
#require(stars)
Z.dat <- st_as_stars(Z.dat, coords = c("lon", "lat")) %>% #convert to sf object
  st_as_sf(Z.dat, as_points = FALSE, merge = FALSE) %>% 
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
