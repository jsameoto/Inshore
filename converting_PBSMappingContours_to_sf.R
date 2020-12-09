

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