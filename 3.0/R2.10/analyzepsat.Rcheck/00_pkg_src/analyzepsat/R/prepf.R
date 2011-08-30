
prepf <- function(tag,xmin=-100,xmax=0,ymin=10,ymax=55,keepall=F,sst.depth=NULL, use.minmax = F){       
    dim1=dim(tag$T)[1];
	dim2=dim(tag$T)[2]
    Temp=tag$T[1:(dim1),]
    Z=tag$Z[1:(dim1),]
	loc.init=c(date.mdy(tag$day0)$year,date.mdy(tag$day0)$month,date.mdy(tag$day0)$day,(tag$x0))
	loc.last=c(date.mdy(tag$dayT)$year,date.mdy(tag$dayT)$month,date.mdy(tag$dayT)$day,(tag$xT))
    #locs=rbind(loc.init,tag$MWTxy)
	locs=tag$MWTxy
	len=length(locs[,4])
	locs[1,1:5]=loc.init
	locs[len,1:5]=loc.last
	locs[,4:5]=rev(locs[,4:5])
    dates=rev(locs[,1:3])
	if(is.null(sst.depth)==F) Temp[Z<=(sst.depth)]=NaN
    maxz=apply(Z,1,min,na.rm=T)
    maxz[!is.finite(maxz)]=0
	maxt=apply(Temp,1,max,na.rm=T)
	
	if(use.minmax){
		mdates = as.POSIXct(trunc(ISOdate(tag$MWTxy[,1],tag$MWTxy[,2],tag$MWTxy[,3]),'day'))
		tdates = tag$mmt[,1]
		pdates = tag$mmz[,1]
		tidx = match(tdates, mdates)
		pidx = match(pdates, mdates)
		maxz = maxt = numeric(nrow(tag$MWTxy))
		maxz[pidx] = tag$mmz[,3]
		maxt[tidx] = tag$mmt[,3]
		maxz2 = apply(Z,1,min,na.rm = T)
		maxz2[!is.finite(maxz2)]=0
		maxt2 = apply(Temp,1,max,na.rm = T)
		maxz[is.na(maxz)] = maxz2[is.na(maxz)]
		maxt[is.na(maxt)] = maxt2[is.na(maxt)]
	}
	
    dat=as.data.frame(cbind(dates,(locs[,4:5]), maxt[1:len],maxz[1:len])) #sapply(1:dim(Temp)[1], function(i) max(Temp[i,],na.rm=T))
    names(dat)[4:7]=c('Lon','Lat','SST','maxD')
	#dat[1,4:5]=rev(dat[1,4:5])
	if(keepall){
			dat=dat[!is.na(dat[,5]),]
			dat=dat[!is.na(dat[,4]),]
		}else{
	dat[is.nan(dat[,5]),5]=NA
	dat[is.nan(dat[,4]),4]=NA
	dat[!is.na(dat[,5])&dat[,5]<ymin,5]=NA
	dat[!is.na(dat[,5])&dat[,5]>ymax,5]=NA
	dat[!is.na(dat[,4])&dat[,4]>xmax,4]=NA
	dat[!is.na(dat[,4])&dat[,4]<(xmin),4]=NA
	dat=dat[!is.na(dat[,5]),]
	dat=dat[!is.na(dat[,4]),]
	dat[is.na(dat[,6]),6]=-1
    }
	rm(Temp,maxz,dates,locs,Z,dim1,dim2,tag)
    attr(dat,'Header') = "#Input data frame for kftrack/kfsst/ukfsst"
	dat
}


.interp.lon <-
function(xtrack, ukf = F, span = .25, fill.sst = F, span.sst = .25){
	if(ukf) xtrack[,4] = xtrack[,4]+360
		span = span
		len = length(xtrack[, 1])
		vec = 1:len
		fidx = is.na(xtrack[, 5])
		xtrack[fidx, 4] = NA
		if (any(fidx == T)) {
			ltmp = loess(xtrack[, 4] ~ vec, span = span)
			xtrack[fidx, 4] = predict(ltmp, newdata = vec[fidx])
			resids = xtrack[,4]-(predict(ltmp, newdata = 1:len))
			bound = 2*sd(resids, na.rm = T)
			ridx = resids < bound*-1 |  resids > bound
			xtrack[ridx,4] = NA 
					if (any(ridx == T)) {
						 ltmp = loess(xtrack[, 4] ~ vec, span = span)
						 xtrack[ridx, 4] = predict(ltmp, newdata = vec[ridx])
					 }
		}
		
	if(fill.sst) xtrack[,6] = fill.vals(xtrack[,6], span = span)
	xtrack
}

