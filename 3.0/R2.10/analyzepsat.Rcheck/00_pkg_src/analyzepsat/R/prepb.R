 make.fmat = function (kfit, prepf, fill.sst = F, span = NULL) 
{
    if (fill.sst) {
		if(is.null(span)) span=.75
        prepf[,6] = .fill.vals(prepf[,6], span = span)
    }
    fmat = as.data.frame(cbind(prepf[, rev(1:3)], kfit$var.most.prob.track, 
        kfit$most.prob.track, prepf[, c(7, 6)]))
    names(fmat) = c("Year", "Month", "Day", "V11", "V12", "V21", 
        "V22", "Lon_E", "Lat_N", "max_depth", "SST")
	fmat$Lon_E = fmat$Lon_E-360	
	#attr(fmat,'Header') = "#Output data frame from kftrack/kfsst/ukfsst #Includes Max daily depth for Bathymetric correction"
    fmat
}

