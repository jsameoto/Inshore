
#Dataset up:
contour.dat <- ScallopSurv %>%
  filter(year == survey.year) %>% 
  dplyr::select(lon, lat, com) # ("X","Y","Z")

datapoints1 <- contour.dat %>%
  mutate(EID = 1:nrow(contour.dat)) %>% 
  dplyr::select(EID, lon, lat)

#######Image.prep#######
#Parameters defined in image.prep function
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
str(Z.dat)

as.data.frame(com.contours$image.mod)

image.data<-makeTopography(Z.dat[c('lon','lat','com.pred')],digits=5)
spatial.model<-Z.gstat
plot(Z.dat)


# Contour lines to spatial object
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

data <- merge(PolyData, PolySet, by = "PID")

data.sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) %>%
  
  
  
  st_cast("MULTIPOINT") %>% #Convert multilines to polygons
  st_cast("MULTILINESTRING")


#st_join(ScallopSurv.sf[3]) %>% #combine with selected ScallopSurv data
#st_make_valid() %>% 
#st_buffer(0)

ScallopSurv.sf <- st_as_sf(ScallopSurv, coords = c("lon","lat"), crs = 4326) %>%  #convert to sf
  filter(year == survey.year) %>%  #filters out survey year, formerly defined as xx in contour.gen() function.
  dplyr::select(year, ID, com)

totCont.poly <- as.PolySet(totCont.poly,projection = "LL") #assuming you provide Lat/Lon data and WGS84
totCont.poly <- PolySet2SpatialLines(totCont.poly) # Spatial lines is a bit more general (don't need to have boxes closed)
totCont.poly.sf <- st_as_sf(totCont.poly) %>%
  st_transform(crs = 4326) %>% #Need to transform (missmatch with ellps=wgs84 and dataum=wgs84)
  st_cast("POLYGON") %>% #Convert multilines to polygons
  st_make_valid()

plot(totCont.poly.sf)

data <- merge(cont.data, totCont.poly, by = "PID")
data <- merge(data, CP$PolyData, by = "PID")

data.sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) %>%
  #st_make_valid() %>% 
  #group_by(level) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("MULTILINESTRING") %>% 
  plot()

#st_join(ScallopSurv.sf[3]) %>% #combine with selected ScallopSurv data
#st_make_valid() %>% 
#st_buffer(0)