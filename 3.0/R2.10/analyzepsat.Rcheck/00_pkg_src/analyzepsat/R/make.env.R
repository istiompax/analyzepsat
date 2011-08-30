make.env <-
function(allhv, fixz = F, plot = F, mbz = c(-250), mbt = c(25), log = T, mmar = c(4,6,2,6), mcex = 1.2){

if(fixz){
#====================================================================================#
#====================================================================================#
#  Take a step back... and remove depth changes greater than |86m|
#====================================================================================#
#====================================================================================#
allhv$hour = as.numeric(format(allhv$Date, '%H'))
utags = unique(allhv$tagID)
alltemp = NULL
sumidx = sumgidx = 0
for(i in utags){
tagidx = allhv$tagID==i
temp = allhv[tagidx,]
Ztmp = temp$depth
zdiff = c(0, diff(Ztmp))
gidx = temp$hour==0|temp$hour==6|temp$hour==12|temp$hour==18  # assuming these are the hours with actual measurements... this may be different for older tags
# gidx = temp$hour==0|temp$hour==8|temp$hour==16

#====================================================================================#
# values are relatvie to the same value one hour before i.e. 14:15 is relative to 13:15
# par(mfrow=c(2,1))
# plot(c(0,0,0,diff(temp$depth,4))[1:100])  # this one should be correct
# plot(c(0,0,0,diff(temp$depth,2))[1:100])
#====================================================================================#
# Index of depth values w more than 86m change--not counting those from the 'good times' index. Dyno-mite.
delta = c(0,0,0,diff(temp$depth,4))
bidx = which(abs(delta[gidx==F])>85)
sumidx = sumidx+length(bidx)
sumgidx = sumgidx+sum(gidx==T)+length(which(abs(delta[gidx==F])<=85))
temp$depth[bidx] = NaN
alltemp = rbind(alltemp,temp)
}
allhv = alltemp
}

dbin = allhv$depth
dbin[dbin<(mbz)] = mbz-1 
brks = (seq(mbz-5,0,5))
dbin = brks[findInterval(round(dbin), brks)]
allhv$dbin = dbin
rm(dbin)


par(mar = mmar)
# env = table(as.vector(round(alltemp$Ext_T)), round(as.vector(alltemp$dbin)))
env = table(as.vector(round(allhv$Ext_T)), round(as.vector(allhv$dbin)))
if(log){env = log(env)}# use log of frequency counts...
env[!is.finite(env)] = 0
env.x = as.numeric(attributes(env)$dimnames[[1]])
env.y = as.numeric(attributes(env)$dimnames[[2]])

if(plot){
require(fields)
#====================================================================================#
# Plot
#====================================================================================#
par(cex = mcex)
if(log){
image(env.x, env.y, env, zlim = c(.1, 8), col = jet.colors(100), xlab = 'Temperature(C)', ylab = 'depth (m)', axes=T, ylim = c(mbz,0), xlim=c(0,mbt))
abline(h=seq(mbz,0,50),v=seq(5,mbt,5),col='grey90', lty=2)
image(env.x, env.y, env, zlim = c(.1, 8), col = jet.colors(100), xlab = '', ylab = '', axes=F, ylim = c(mbz,0), xlim=c(0,mbt),add=T)
box()
image.plot(env, zlim=c(0,8), add=F,legend.only = T, horizontal = F, legend.shrink=1, legend.args=list(text = "ln(frequency)", side = 4, line = 1.5, cex = 1.2))
}else{
image(env.x, env.y, env, zlim = c(min(env),max(env)), col = jet.colors(100), xlab = 'Temperature(C)', ylab = 'depth (m)', axes=T, ylim = c(mbz,0), xlim=c(0,mbt))
abline(h=seq(mbz,0,50),v=seq(5,mbt,5),col='grey90', lty=2)
image(env.x, env.y, env, zlim = c(min(env),max(env)), col = jet.colors(100), xlab = '', ylab = '', axes=F, ylim = c(mbz,0), xlim=c(0,mbt),add=T)
box()
image.plot(env, zlim=c(min(env),max(env)), add=F,legend.only = T, horizontal = F, legend.shrink=1, legend.args=list(text = "frequency", side = 4, line = 1.5, cex = 1.2))
}
}

list(env.x, env.y, env)
}

