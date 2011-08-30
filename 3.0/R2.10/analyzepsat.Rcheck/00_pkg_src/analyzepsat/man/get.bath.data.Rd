\name{get.bath.data}
\Rdversion{2.0}
\alias{get.bath.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download bathymetry
}
\description{
Downloads bathymetry at three resolutions: 2 minute (ETOPO2), 1 minute (ETOPO1) and 30 second (SRTM). 
}
\usage{
get.bath.data(lonlow, lonhigh, latlow, lathigh, folder = tempdir(), seaonly = T, res = c(.5,1,2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lonlow}{
Longitude minimum
}
  \item{lonhigh}{
Longitude maximum
}
  \item{latlow}{
Latitude minimum
}
  \item{lathigh}{
Latitude maximum
}
  \item{folder}{
storage location is desired
}
  \item{res}{
Resolution, in minutes
}

}
\value{
Bathymetry object. Here, this is a list containing 3 objects: Longitude vector, Latitude vector, matrix of depth values. 
}

\references{
http://www.ngdc.noaa.gov/mgg/global/etopo2.html
http://www.ngdc.noaa.gov/mgg/global/global.html
http://coastwatch.pfeg.noaa.gov/erddap/info/usgsCeSrtm30v6/index.html
}
\author{
Benjamin Galuardi
}

\examples{

# nice blue cascade for bathymetry
bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))

#***OR****#
# bath.colors <- colorRampPalette(c("grey10", "gray40", "gray50", "gray70", "lightsteelblue4","lightsteelblue3","lightsteelblue2","lightsteelblue1","lemonchiffon1"))


 par(mfrow=c(1,3))
 bath2 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 2)
 image(bath2[[1]],(bath2[[2]]),t(bath2[[3]]),col=bath.colors(100), zlim=c(-10000,0))
 bath3 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 1)
 image(bath3[[1]],(bath3[[2]]),t(bath3[[3]]),col=bath.colors(100), zlim=c(-10000,0))
 bath4 = get.bath.data(-100,-90,20,30, folder = tempdir(), seaonly = T, res = .5)
 image(bath4[[1]],(bath4[[2]]),t(bath4[[3]]),col=bath.colors(100), zlim=c(-10000,0))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}
