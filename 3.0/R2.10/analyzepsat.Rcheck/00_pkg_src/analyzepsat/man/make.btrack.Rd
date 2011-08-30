\name{make.btrack}
\Rdversion{1.1}
\alias{make.btrack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bathymetric Correction
}
\description{
Uses bathymetry to correct a kftrack style estiamted PSAT track. 
}
\usage{
make.btrack(fmat, bathy, save.samp = F, mintype = 2, ci = 0.95, npoints = 300, fulldist = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmat}{
data frame containing the following columns:
Day 	Month 	Year      V11         V12         V21     	V22   	Lon_E  Lat_N 	max_depth 	 maxTemp

}
  \item{bathy}{
Bathymetry object. Here, this is a list containing 3 objects: Longitude vector, Latitude vector, matrix of depth values. The data provided in this package is the E-Topo 2 minute bathymetry for the North Atlantic Ocean. 
}
  \item{save.samp}{
This function samples bathymetry at each estimated point. If you want to save all these sampled points, set this to 'T'
}
  \item{mintype}{
Minimization type. Possible choices are, 2, 3, and 4. This function weights each sample with the weighted mean of the sample and the previous days final estimated point. By default, this is option 2. Option 3 weights with these and the next days estimated location as well. Option 4 weights based on the weighted mean of the sample and the next days final estimated location. Avoid using this under normal circumstances.
}
  \item{ci}{
Confidence level used for the bathymetric sampling.
}
  \item{npoints}{
number of points in the sample
}
  \item{fulldist}{
This function may be used in a manner where bathymetric correction is only performed when the maximum daily depth is shallower than the bathymetry at that point in the most probable track. In this case, set this parameter to 'T'. 
}
}
\details{
This function adjusts a kftrack fitted track based on bathymetric sampling within the estimated covariance (confidence ) intervals. 
}
\value{
Data frame of the same form as fthe fmat input. If save.samp = T, then a list is returned where the second element is all samples fro mthe estimation.
}
\references{
http://www.ngdc.noaa.gov/mgg/global/etopo2.html
}
\author{
Benjamin Galuardi
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{get.samp}},\code{\link{denselect}},\code{\link{get.bath}}
}
\examples{
bath2 = get.ss.bath(-100,-90,20,30, folder = tempdir(), seaonly = T, res = 2)
map2 <- data(ATL)

Mydir = 'C:/MYDATA/';   # Data arranged by year in the DATA folder
tyear = 2008;						  # Year fish was tagged 
taglocfile = paste(Mydir, 'Tagginglocations.txt', sep = "");	# ASCII file containing Tag ID,  year,  month,  day,  longitude and latitude of tagging locations. This is neccesary in order to properly read in the data.
tagID = '12345'
tyear = 2008
xlsfile = paste('Mydir', tyear, tagID, '/', tagID, '/', '.xls', sep = "")

tagdata <- MWTextract(tagID, tyear, xlsfile, taglocfile)

xdata=make.xtrack(tagdata)
fit=kftrack(xdata[,1:5])
fmat <- as.data.frame(cbind(xdata[,1:3],fit$var.most.prob.track,fit$most.prob.track,xdata[,c(7:6)]))

btrack <- make.btrack(fmat,bathy,mintype=2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{bathymetry}

