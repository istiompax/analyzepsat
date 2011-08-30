\name{plot.btrack}
\Rdversion{1.1}
\alias{plot.btrack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots an estimated track by month.
}
\description{
Plots an estimated track by month. May include confidence intervals and bathymetric contours
}
\usage{
plot.btrack(btrack, map, cex = 1.5, ci = F, bathy = NULL, add = F, bathlevels = c(-100, -200),  alpha  =  0.15, bymonth = T, pch = 21, bg = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{btrack}{
Estimated track. Track is typically estimated using make.btrack
}
  \item{map}{
Map object to be used. May be ghssh object created through maptools, or a ghssh coastline extracted through plotmap.r in the kftrack family of libraries 
}
  \item{cex}{
Size of points
}
  \item{ci}{
Should confidence interval be plotted? 
}
  \item{bathy}{
Should a bathymtric contour be added? The default is to plot the -100m and -200m contours. Color will be drawn from the bath.colors palette
}
  \item{add}{
Should this plot be added to the existing plot?
}
  \item{bathlevels}{
Bathymetric contours to be drawn
}
  \item{alpha}{
If ci = T, then this refers to the degree of transparency in the error ellipses. This is useful for eyeballing heavily used areas.
}
   \item{bymonth}{
 Indicates if the colors of points plotted represent months. This will use \code{\link{plot.by.month}}
}
   \item{pch}{
 The point type  
}   
   \item{bg}{
 Background color for the point. This is only used if bymonth = F
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A plot of an estimated track.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Benjamin Galuardi
}
\note{

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.by.month}}
}
\examples{
data(ATL)
data(fake.b)
bathy = get.ss.bath(-80, -50, 30, 50, folder = getwd(), seaonly = T, res = 1)
plot.btrack(fake.b, bathy = bathy)
}

