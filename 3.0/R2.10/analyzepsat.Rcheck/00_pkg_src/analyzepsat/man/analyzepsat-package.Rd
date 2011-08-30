\name{analyzepsat-package}
\Rdversion{2.0}
\alias{analyzepsat-package}
\alias{analyzepsat}
\docType{package}
\title{Functions for Microwave Telemtry PSAT analysis}
\description{
Functions for reading Microwave Telemetry PSAT data into R,  analyzing it via state space methods,  and producing useful output. Emphasis is placed on data management i.e. structures used in state space analysis and efficient plotting. 
}
\details{
\tabular{ll}{
Package: \tab analyzepsat\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2010-12-03\cr
License: \tab GPL version 2 \cr
LazyLoad: \tab yes\cr
}
Package requires gdata, ukfsst, maptools, ellipse, ncdf
}
\author{
Benjamin Galuardi
Large Pelagics Research Center
University of Massachusetts, Amherst

Maintainer:  <galuardi@eco.umass.edu>

Contributions from Chi Hin Lam, PhD
}
\references{
Many thanks to Francois Royer who created the basis for many functions herein as well as the logic involved. 
}

\keyword{ Longitude matching}
\keyword{sst}
\keyword{Kalman Filter}
\keyword{kftrack}
\keyword{package}

\seealso{
\code{\link{kftrack}}
\code{\link{ukfsst}}
\code{\link{gdata}}
\code{\link{maptools}}
\code{\link{GenKern}}
\code{\link{ncdf}}
\code{\link{maptools}}
\code{\link{adehabitat}}
}
\examples{
# Example script

library(analyzepsat)

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste(Mydir, '/',tyear, '/',tagID, '/',tagID, '.xls', sep = "")		#  If you follow this syntax you can easily do more than one tag within a loop, change years etc. 

tagdata = MWTextract(tagID,  tyear,  xlsfile,  taglocfile, minmax = F)		# This creates a list object filled with PSAT data. Change minmax flag to T if your fish report has these sheets. Use the 'try' command in case something doesn't work..

 xtrack = prepf(tagdata, xmin=-100, xmax=0, ymin=10, ymax=55)
 fit = kftrack(xtrack[,1:5])			# Kalman filter without SST
 
 #		Load the bathymetry and map polygons for the N. Atlantic
bathy = get.bath.data(-100, 0, 0, 60, folder = getwd(), seaonly = T, res = 2)
data(ATL)

fmat = prepb(fit,xtrack,fill.sst = T)		# makes a data frame for bathymetric and/or longitudinal SST correction		
btrack = make.btrack(fmat, bathy)						# Bathymetric correction 

plot.btrack(bfit,bathy=bathy,add=F)
 
 

#  Not Run
#  for many tags in a series

#	for(i in 1:length(files)){
#	   tagID = files[i]
# 	  print(tagID)
# 	  psat[[i]] = try(MWTextract(tagID,  tyear,  xlsfile,  taglocfile) )		
# 	  names(psat)[[i]] = as.character(files[i])
#	}
#
#-----------------------------------------------------------------------------#
# 	To make the color ramps used in this package, use the following
#       jet.colors is of this is built into the functions but the others are not. monames is a truncated version of the \code{\link{month.names}} function
#-----------------------------------------------------------------------------#

# Mimics the jet colors from matlab. Good for SST
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# nice blue cascade for bathymetry
bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

#  ***OR***  use this if you get tired of all blues
bath.colors <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))

# nice earthy colors for land
land.colors<-  colorRampPalette(c("darkgreen", "greenyellow","burlywood4"))

# Also included via data(monames)
monames=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec')

}
