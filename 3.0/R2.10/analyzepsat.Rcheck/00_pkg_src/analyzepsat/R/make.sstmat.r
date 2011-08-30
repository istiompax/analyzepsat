make.sstmat <- function(sstfile, type=c("Reynolds", "Blended")){
 require(ncdf)
 require(matlab)
 sstfile = open.ncdf(sstfile)
 if(type == "Reynolds"){
  land <- get.var.ncdf(sstfile, varid="land")
  lon = as.numeric(get.var.ncdf(sstfile, 'lon'))-360
  lat = as.numeric(sort(get.var.ncdf(sstfile, 'lat')))
  sst = get.var.ncdf(sstfile, 'sst')
  for(i in 1:dim(sst)[3]) sst[,,i] = fliplr(sst[,,i]*land)
  sdate = as.Date("1800-01-01") + get.var.ncdf(sstfile, varid="time")
  sstmat = list(lon=lon, lat=lat, sstdates=sdate, DATA=sst)
 }
 
 if(type == "Blended"){
  lon = as.numeric(get.var.ncdf(sstfile, 'longitude'))-360
  lat = as.numeric(sort(get.var.ncdf(sstfile, 'latitude')))
  sst = get.var.ncdf(sstfile, 'sst')
  sstdates = as.POSIXct(get.var.ncdf(sstfile, 'time'), origin='1970-1-1')
  sstmat = list(lon=lon, lat=lat, sstdates=sstdates, DATA=sst)
 }
sstmat
}
