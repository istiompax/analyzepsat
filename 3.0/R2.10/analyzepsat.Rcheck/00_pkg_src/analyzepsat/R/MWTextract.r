

.gmtok = function () 
{
    .testgmt <- tempfile("testgmt")
    on.exit(unlink(.testgmt))
    return(.kfsys(paste("gmtdefaults -L 1>", .testgmt)) == 0)
}

MWTextract = function (tagID, tyear, xlsfile, taglocfile, delta=F, minmax=F) 
{
    #psatcon = odbcConnectExcel(xlsfile, readOnly = T)
    print("Reading tagging data")
    x0 = .getTaggingLoc(taglocfile, as.numeric(substr(tagID, 
        1, 5)), tyear)
    x0 = rev(x0)
    day0 = .getTaggingDay(taglocfile, as.numeric(substr(tagID, 
        1, 5)), tyear)
    day0b = ISOdatetime(date.mdy(day0)[[3]], date.mdy(day0)[[1]], 
        date.mdy(day0)[[2]], 0, 0, 0)
    print("Reading ARGOS data")
    adata = .GetArgosPSAT(xlsfile) 
    dayT = .GetLastDay(adata)
    dayTb = .GetDayTb(dayT)
    xT = .GetLastLoc(adata)
    print("Reading MWT produced locations")
    MWTxy = .getMWTxy(xlsfile, x0, xT, day0, dayT)
    fulldates = seq(day0, dayT)
    print("Reading sunset/sunrise times")
     SRSS = .getSRSS(xlsfile, day0b, dayTb)
    print("Reading temperature data")
	if(delta==T){ 
	print("Removing delta limited Values")
		dataT = .getPSATtemp(xlsfile, day0b, dayTb, delta=T)
	 }else{
		dataT = .getPSATtemp(xlsfile, day0b, dayTb, delta=F)
		}
    print("Reading pressure data")
	if(delta==T){ 
	print("Removing delta limited Values")
		dataP = .getPSATdepth(xlsfile, day0b, dayTb, delta=T)
	}else{
		dataP = .getPSATdepth(xlsfile, day0b, dayTb, delta=F)
	}
	if(minmax==T){
	print("Reading daily minimum and maximum depth and temperature")
		mmp = .getminmaxp(xlsfile, day0b, dayTb, MWTxy)
		mmt = .getminmaxt(xlsfile, day0b, dayTb, MWTxy)
		dataout = list(tagID = tagID, x0 = x0, day0 = day0, day0b = day0b, xT = xT, 
			dayT = dayT, dayTb = dayTb, fulldates = fulldates,SRSS = SRSS,  
			MWTxy = MWTxy, T = dataT, Z = dataP, mmt = mmt, mmz = mmp, Argos = adata)
		}else{
			dataout = list(tagID = tagID, x0 = x0, day0 = day0, day0b = day0b, xT = xT, 
			dayT = dayT, dayTb = dayTb, fulldates = fulldates,SRSS = SRSS,  
			MWTxy = MWTxy, T = dataT, Z = dataP, Argos = adata)
		}
#     odbcClose(psatcon) #
    class(dataout) = c('MWTpsat', 'list')
	dataout
}

.getTaggingLoc = function (TagLocfile, tagID, tyear) {
    require(date)
    tab = read.table(TagLocfile, header = F)
    i = tab[(tab[, 1] == tagID) & (tab[, 2] == tyear), ]
    i = as.numeric(i)
    day0 = mdy.date(i[3], i[4], i[2])
    x0 = rev(i[5:6])
    return(x0)
}

.getTaggingDay = function (TagLocfile, tagID, tyear) 
{
    require(date)
    tab = read.table(TagLocfile, header = F)
    i = tab[(tab[, 1] == tagID) & (tab[, 2] == tyear), ]
    i = as.numeric(i)
    day0 = mdy.date(i[3], i[4], i[2])
    return(day0)
}

.GetArgosPSAT = function (xlsfile) {
    # res = sqlFetch(psatcon, "Argos Data")
    res = read.xls(xlsfile, "Argos Data",skip=1, head=T)
    adata = res[, 1:4]
    names(adata) = c("Date-Time", "LC", "Lat", "Lon")
    res = res[!is.na(res[, 1]), ]
    res = res[!is.na(res[, 2]), ]
    d1 = dim(res)[1]
    d2 = dim(res)[2]
    adates = as.character(res[, 1])
    adates = strptime(adates, format = '%m/%d/%Y %H:%M')
    adata[,1] = as.POSIXct(adates)
    adata
}
.GetLastDay = function(argos){
    adates = argos[,1]
    a1 = min(adates, na.rm = T)
    a1 = as.POSIXlt(a1)
    aa = unclass(unlist(a1))
    dayT = mdy.date(aa[5] + 1, aa[4], aa[6] + 1900)
    dayT
}
.GetDayTb = function(dayT){
  dayTb = ISOdatetime(date.mdy(dayT)[[3]], date.mdy(dayT)[[1]], 
        date.mdy(dayT)[[2]], 0, 0, 0)
 dayTb
}

.GetLastLoc = function(argos){
    res = argos
    i = 1
    while ((res[i, 2] == "A") || (res[i, 2] == "B") || (res[i, 
        2] == "Z")) {
        i = i + 1
    }
    xT = c(as.numeric(res[i, 3:4]))
    xT[2] = -1*xT[2]
    if (i > 5) {
        print("Pop-off location may be incorrect")
    }
    xT
}

.getMWTxy = function (xlsfile, x0, xT, day0, dayT) 
{
    # MWTxy = sqlFetch(psatcon, "Lat&Long")
    MWTxy = read.xls(xlsfile, sheet = "Lat&Long", skip=1, head=T)
    MWTxy = MWTxy[!is.na(MWTxy[, 1]), 1:3]
    MWTxy[,1] = as.character(MWTxy[,1])
    MWTxy[,1] = as.POSIXct(strptime(MWTxy[,1], format="%b %d,%Y"))
    names(MWTxy) = c("Date", "Lat", "Long")
    fulldates = seq(day0, dayT)
    len = length(fulldates)
    MWTdata = as.data.frame(array(NA, c(len, 5)))
    Years = as.numeric(format.Date(MWTxy$Date, '%Y'))
    Months = as.numeric(format.Date(MWTxy$Date, '%m'))
    Days = as.numeric(format.Date(MWTxy$Date, '%d'))
    MWTdates = mdy.date(Months, Days, Years)
    didx = match(MWTdates, fulldates)
    didx = didx[!is.na(didx)]
    MWTdata[, 1:3] = cbind(date.mdy(fulldates)[[3]], date.mdy(fulldates)[[1]], 
        date.mdy(fulldates)[[2]])
    MWTdata[didx, 4:5] = MWTxy[, 2:3]
    MWTdata[, 5] = -1 * (abs(MWTdata[, 5]))
    MWTdata[1, 4:5] = (x0)
    MWTdata[len, 4:5] = xT
    names(MWTdata) = c("Year", "Month", "Day", "Lat", "Lon")
    MWTdata
}


.getSRSS = function (xlsfile, day0b, dayTb) 
{
#     res = sqlFetch(psatcon, "Sunrise and Sunset Times")
    res = read.xls(xlsfile, sheet = "Sunrise and Sunset Times", skip=1, head=T)
    res = res[!is.na(res[, 1]), c(1, 2, 4)]
    len = length(res[, 1])
    srssdates = as.POSIXct(strptime(res[,1], "%b %d, %Y"))

    SR = strptime(paste(res[,1], res[,2], sep=" "), "%b %d, %Y %H:%M:%S")
    SR = SR$hour * 60 + SR$min
    SS = strptime(paste(res[,1], res[,3], sep=" "), "%b %d, %Y %H:%M:%S")
    SS = SS$hour * 60 + SS$min

    tidx = srssdates>=day0b&srssdates<=dayTb

    SRSS = data.frame(Date = srssdates[tidx], SR = SR[tidx], 
        SS = SS[tidx])
    SRSS[which(SRSS[, 3] < 500), 3] = SRSS[which(SRSS[, 3] < 
        500), 3] + 1400
    SRSS[which(SRSS[, 2] > 1400), 3] = SRSS[which(SRSS[, 2] > 
        1400), 2] - 500
    SRSS
}

.getPSATtemp = function (xlsfile, day0b, dayTb, delta=F) 
{
#     tres = sqlFetch(psatcon, "Temp Data")
    tres = read.xls(xlsfile, "Temp Data", head=T, skip =1)
    tres = tres[2:length(tres[, 1]), ]
    tdates = as.POSIXct(strptime(as.character(tres[, 1]), "%m/%d/%y %H:%M"))
    tidx = tdates <= (dayTb) & tdates >= day0b
    tres = tres[tidx, ]
#     tdates = tres[, 1]
    ttimes = seq(day0b, dayTb, 86400/96)
    ttidx = match(tdates, ttimes)
    ttidx = ttidx[!is.na(ttidx)]
    dataT = matrix(NaN, 24 * 4, length(seq(day0b, dayTb, 86400)))
	if(delta){
		 delt = as.numeric(as.character(tres[,4]))
		 delt[is.na(delt)] = 0
		 delt=abs(delt)>=31
		 tres[delt,3] = NA
	}
    dataT[ttidx] = tres[, 3]
    dataT = t(dataT)
    dataT
}


.getPSATdepth = function (xlsfile,day0b, dayTb, delta=F) 
{
#     pres = sqlFetch(psatcon, "Press Data")
    pres = read.xls(xlsfile, "Press Data", head=T, skip =1)
    pres = pres[2:length(pres[, 1]), ]
    pdates = as.POSIXct(strptime(as.character(pres[, 1]), "%m/%d/%y %H:%M"))
    pidx = pdates <= (dayTb) & pdates >= day0b
    pres = pres[pidx, ]
    ptimes = seq(day0b, dayTb, 86400/96)
    ptidx = match(pdates, ptimes)
    ptidx = ptidx[!is.na(ptidx)]
    dataP = matrix(NaN, 24 * 4, length(seq(day0b, dayTb, 86400)))
	if(delta){
		 pres[,4] = as.numeric(as.character(pres[,4]))
		 deltap = as.numeric(as.character(pres[,5]))
		 deltap[is.na(deltap)]=0
		 deltap=abs(deltap)>=31
		 pres[deltap,4] = NA
	}
    if (min(pres[, 3], na.rm = T) > 0) {
        dataP[ptidx] = pres[, 4]
    }else{ dataP[ptidx] = pres[, 3]
	}
    dataP = t(dataP)
    dataP
}

.getminmaxp <- function(xlsfile, day0b, dayTb, MWTxy){
  pres = read.xls(xlsfile, "Press Data (MinMax)", head=T, skip =1)	
  mmpdates = as.POSIXct(strptime(as.character(pres[, 1]), "%m/%d/%y"), tz = "GMT")
  mmpdates = as.POSIXct(trunc(mmpdates,'day'))
  alldates = ISOdate(MWTxy[,1], MWTxy[,2], MWTxy[,3])
  alldates = as.POSIXct(trunc(alldates,'day'))
  pidx = mmpdates <= (dayTb) #& mmpdates >= day0b
  pres = pres[pidx, ]
  didx = match(mmpdates,alldates)
  didx=didx[!is.na(didx)]
  mmp = as.data.frame(matrix(NA, nrow=length(alldates), ncol=3))
  mmp[,1] = alldates
  mmp[didx,2] = pres[,4]
  mmp[didx,3] = pres[,5]
  names(mmp) = c('Date','minz','maxz')
 mmp
}


.getminmaxt <- function(xlsfile,  day0b, dayTb, MWTxy){
  tres = read.xls(xlsfile, "Temp Data (MinMax)", head=T, skip =1)	
  mmtdates = as.POSIXct(strptime(as.character(tres[, 1]), "%m/%d/%y"), tz = "GMT")
  mmtdates = as.POSIXct(trunc(mmtdates,'day'))
  alldates = ISOdate(MWTxy[,1], MWTxy[,2], MWTxy[,3])
    alldates = as.POSIXct(trunc(alldates,'day'))
  pidx = mmtdates <= (dayTb) #& mmtdates >= day0b
  tres = tres[pidx, ]
  didx = match(mmtdates,alldates)
  didx=didx[!is.na(didx)]
  mmt = as.data.frame(matrix(NA, nrow=length(alldates), ncol=3))
  mmt[,1] = alldates
  mmt[didx,2] = tres[,4]
  mmt[didx,3] = tres[,5]
  names(mmt) = c('Date','mint','maxt')
 mmt
}

print.MWTpsat<-function(x,...){ 
  # headvec<-strsplit(x$header, split="\n")[[1]]
  "%+%"<-function(s1, s2)paste(s1, s2, sep="")
  out<-"\n\n#Microwave Telemetry PSAT\n"%+%x$tagID%+%"\n"%+%
	   "\n#Days at Liberty: "%+%c(x$dayT-x$day0+1)%+%"\n"%+%
       "#Number of observations: "%+%nrow(x$MWTxy)%+%"\n"%+%
       "\n#Tagging Date: "%+%x$day0b%+%"\n"%+% 
	   "#Tagging Latitude "%+%x$x0[1]%+%"\n"%+%
	   "#Tagging Longitude "%+%x$x0[2]%+%"\n"%+%
       "\n#Report Date: "%+%x$dayTb%+%"\n"%+%
	   "#Report Latitude "%+%x$xT[1]%+%"\n"%+%
	   "#Report Longitude "%+%x$xT[2]%+%"\n"
	cat(out)
    # cat("Parameters:\n")
    # print(rbind("Estimates:"=x$estimates,"Std. dev.:"=x$std.dev))

     cat("\nThis object contains the following sub-items:\n")
     print(names(x))  
}


