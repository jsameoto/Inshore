#	source("Y:/INSHORE SCALLOP/Survey/2010/r/fn/alloc.poly.r")

# ALLOCATION by STRATIFIED RANDOM DESIGN
# based on bottom type (or another polygon based non-continuous stratifying variable)

alloc.poly<-function(poly.lst,bounding.poly,ntows,bank.plot=NULL,mindist=1,pool.size=4,repeated.tows=NULL){
	
	options(warn=-1)
	# create pool of random points
	if(missing(ntows))ntows<-sum(poly.lst[[2]]$allocation)
	npool=ntows*pool.size
  
  if(!missing(bounding.poly))surveyed.polys<-joinPolys(poly.lst[[1]],bounding.poly,operation="INT")#,maxVerts=1e+06)
	
	if(missing(bounding.poly)){
    surveyed.polys<-poly.lst[[1]]
    bounding.poly<-poly.lst[[1]][chull(poly.lst[[1]]$X,poly.lst[[1]]$Y),]
		bounding.poly$POS<-1:nrow(bounding.poly)
		bounding.poly$SID<-1
	}
	
	source("Y:/Inshore/Survey/2010/r/fn/genran.r",local=T)
	pool.EventData<-genran(npool,bounding.poly,mindist=mindist)
	#browser()	
	
	
	Poly.ID<-unique(poly.lst[[2]]$PID)
	strata<-as.character(unique(poly.lst[[2]]$PName))
	strataTows.lst<-list(NULL)
	
	# allocation provided
	if("allocation"%in%names(poly.lst[[2]])){
		towsi<-with(poly.lst[[2]],tapply(allocation,PName,unique))
		strataPolys.dat<-surveyed.polys
		attr(strataPolys.dat,"projection")<-"LL"
		strataArea<-calcArea(strataPolys.dat,1)
		
	}else{
		# calculate area and proportional allocation
		strataPolys.lst<-list(NULL)
		strataArea<-c()
		towsi<-c()
	
		for(i in 1:length(strata)){
			strataPIDS<-poly.lst[[2]]$PID[poly.lst[[2]]$PName==strata[i]]
			tmp<-subset(surveyed.polys,PID%in%strataPIDS)
			if(nrow(tmp)>0){
				tmp$PName<-strata[i]
				strataPolys.lst[[i]]<-combinePolys(tmp)
				attr(strataPolys.lst[[i]],"projection")<-"LL"
				strataArea[i]<-calcArea(strataPolys.lst[[i]],1)$area
				names(strataArea)[i]<-strata[i]
				print(strata[i])
			}
		}
		strataArea<-na.omit(strataArea)
		strataPolys.dat<-do.call("rbind",strataPolys.lst)
		towsi<-round(strataArea/sum(strataArea)*ntows)
		towsi<-towsi[towsi>0]
		towsi[1]<-ntows-sum(towsi[-1])
		strata<-names(towsi)
	}
	
	for(i in 1:length(strata)){
	
		LocSet<-findPolys(pool.EventData,subset(strataPolys.dat,PID==Poly.ID[i]))
		strataTows.lst[[i]]<-data.frame(subset(pool.EventData,EID%in%LocSet$EID)[1:towsi[strata[i]],c("EID","X","Y")],Poly.ID=Poly.ID[i],STRATA=strata[i])

	}
	
	#browser()	
	Tows<-do.call("rbind",strataTows.lst)
	Tows$EID<-1:sum(towsi)
	rownames(Tows)<-1:sum(towsi)
	attr(Tows,"projection")<-"LL"
	
	# randomly selects stations from last years	survey (repeated.tows)
	if(!is.null(repeated.tows)){
		#browser()	
    names(repeated.tows)<-c("EID","X","Y","Poly.ID")
		repeat.str<-poly.lst[[2]][!is.na(poly.lst[[2]]$repeats),]
		repeated.lst<-list(NULL)
		for(i in 1:length(repeat.str$PID)){
			str.tows<-subset(repeated.tows,Poly.ID==repeat.str$PID[i])
	
			repeated.lst[[i]]<-str.tows[sample(1:nrow(str.tows),repeat.str$repeats[repeat.str$PID==repeat.str$PID[i]]),]
			repeated.lst[[i]]$STRATA<-repeat.str$PName[repeat.str$PID==repeat.str$PID[i]]
		}
		repeated.tows<-do.call("rbind",repeated.lst)
		
		Tows<-list(new.tows=Tows, repeated.tows=repeated.tows)
	}
	

	if(!is.null(bank.plot)){
		source("Y:/Inshore/Survey/2010/r/fn/ScallopMap.r",local=T)
		ScallopMap(bank.plot,poly.lst=list(surveyed.polys,poly.lst[[2]]))
#		addPolys(bounding.poly,lwd=2,border='yellow')
		bg.col<-tapply(poly.lst[[2]]$col,poly.lst[[2]]$PName,unique)
		addPoints(Tows,pch=21, cex=1,bg=bg.col[as.character(Tows$STRATA)])
		legend('bottomright',legend=names(bg.col[unique(as.character(Tows$STRATA))]),pch=21,pt.bg=bg.col[unique(as.character(Tows$STRATA))],bty='n',cex=1.2, inset = .02)
#		legend('bottomleft',legend=poly.lst[[2]]$PName[order(as.numeric(rownames(poly.lst[[2]])),decreasing = T)],pch=21,pt.bg=poly.lst[[2]]$col[order(as.numeric(rownames(poly.lst[[2]])),decreasing = T)],bty='n',cex=1.2, inset = .02)
#		legend('bottomleft',legend=poly.lst[[2]]$PName[order(as.numeric(rownames(poly.lst[[2]])))],pch=21,pt.bg=poly.lst[[2]]$col[order(as.numeric(rownames(poly.lst[[2]])))],bty='n',cex=1.2, inset = .02)

	}
	
	options(warn=0)
	#return(list(Tows=Tows,Areas=strataArea))
	return(list(Tows=Tows))	
}
