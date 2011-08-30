merge.tz <-
function(btrack, psat, tagID = NULL){
require(maptools)
#=================================================================================#
# First, combine all temperature and depth readings and determine day/night
#=================================================================================#
allTZ = NULL
if(class(btrack)!='list') btrack = list(btrack)
if(names(psat)[1]=="x0"){ psat = list(psat); names(psat) = tagID}

for(j in 1:length(psat)){
print(paste("Now merging tag# ", names(psat)[j], sep=""))
day0 = psat[[j]]$day0
dayT = psat[[j]]$dayT
dim1 = dim(psat[[j]]$T)[1]
dim2 = dim(psat[[j]]$T)[2]

tdates = as.POSIXct(round(seq(psat[[j]]$day0b, psat[[j]]$dayTb, length = dim1*dim2), 'mins'), tz='GMT')
depth = as.vector(t(psat[[j]]$Z))
Ext_T = as.vector(t(psat[[j]]$T))
tzdat=data.frame(Date=tdates, depth, Ext_T)

# using civil twilight for SR and SS
mlat = mean(btrack[[j]][,9], na.rm=T)
mlon = mean(btrack[[j]][,8], na.rm=T)
mpos =  matrix(c(mlon, mlat),nrow=1)  # mean of all estimated positions.. these fish stayed near the US most of the time

sr = crepuscule(mpos, tdates, solarDep=6, direction="dawn", POSIXct.out=T)$time
ss = crepuscule(mpos, tdates, solarDep=6, direction="dusk", POSIXct.out=T)$time
dn = numeric(nrow(tzdat))

for(i in 1:nrow(tzdat)){
 if(tzdat[i,1]>sr[i]&tzdat[i,1]<ss[i]){
dn[i] = 'Day'
}else{
 dn[i] = 'Night'
 }
}
diel = data.frame(sr,ss,tzdat$Date,dn)
subdat = cbind(tzdat, diel, rep(names(psat)[j],nrow(tzdat)))
allTZ = rbind(allTZ, subdat)
}

allTZ$dn = as.character(allTZ$dn)
allTZ[,8] = as.character(allTZ[,8])

#----- Deal with the positive depth readings...-----------------#
tidx=which(allTZ$depth>1 )
allTZ$depth[tidx] = allTZ$depth[tidx]*-1
names(allTZ)[8] = 'tagID'
allTZ
}

