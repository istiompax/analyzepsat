plot.btrack <- function (btrack, map, cex = 1.5, ci = F, bathy = NULL, add = F, 
    bathlevels = c(-100, -200), alpha = 0.15, bymonth = T, pch = 21, 
    bg = 4, offset = 360) 
{
    len = length(btrack[, 1])
	btrack = btrack[!is.na(btrack[,8]),]
	btrack = btrack[!is.na(btrack[,9]),]
    xlims = c(min(btrack[, 8], na.rm=T) - 2, max(btrack[, 8], na.rm = T) + 2) + offset
    ylims = c(min(btrack[, 9], na.rm = T) - 2, max(btrack[, 9], na.rm = T) + 2)
    if (add == F) 
        plot(map2$SP, col = "khaki", pbg = "azure2", xlim = xlims, 
            ylim = ylims, xaxs = "i", yaxs = "i", axes = TRUE)
    if (!is.null(bathy)) {
        bathy$lon = bathy$lon + offset
        contour(bathy$lon, bathy$lat, t(bathy$data), levels = bathlevels, 
            drawlabels = F, add = T, col = bath.colors(length(bathlevels)))
    }
    S = SpatialPointsDataFrame(btrack[, 8:9], btrack)
    if (btrack[1, 8] < 0) 
        S@coords[, 1] = S@data[, 8] = btrack[, 8] = S@coords[, 
            1] + offset
    proj4string(S) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
    if (ci) {
        sapply(1:len, function(i) .makeCI(as.numeric(btrack[i, 
            4:9]), col = rgb(0.7, 0.7, 0.7, alpha = alpha), border = 0))
        plot(map2$SP, col = "khaki", pbg = "azure2", xaxs = "i", 
            yaxs = "i", axes = F, add = T)
    }
    points(S, pch = pch, bg = bg, typ = "o")
    if (bymonth) 
        .plot.by.month(S@data, cex = cex, pch = pch)
    lines(btrack[1, 8], btrack[1, 9], typ = "p", pch = 21, col = 1, 
        bg = 3, cex = 1.8)
    lines(btrack[len, 8], btrack[len, 9], typ = "p", pch = 24, 
        col = 1, bg = 2, cex = 1.8)
    box(lwd = 2)
}

.plot.by.month <-
function(ttrack, saveplot=F, filename=NULL, cex=2, pch = 21){
    attach(ttrack)

month.colors=cbind(c(8:12,1:7),
  c(rgb(115/255,0,76/255),
    rgb(0,76/255,115/255),
    rgb(0,92/255,230/255),
    rgb(115/255,223/255,1),
    rgb(190/255,232/255,1),
    rgb(1,1,190/255),
    rgb(230/255,230/255,0),
    rgb(1,170/255,0),
    rgb(1,120/255,0),
    rgb(1,0,197/255),
    rgb(1,0,0),
    rgb(168/255,0,0)
  )
)

    mons=unique(Month)
    for(i in 1:length(mons)){
      lines(ttrack[Month==mons[i],8:9],typ='o',cex=cex,pch=pch,bg=month.colors[month.colors[,1]==mons[i],2])
      
     if(saveplot){
        savePlot(filename,type='eps')
        savePlot(filename,type='png',res=100)
        }
    }
   detach(ttrack)
  }


