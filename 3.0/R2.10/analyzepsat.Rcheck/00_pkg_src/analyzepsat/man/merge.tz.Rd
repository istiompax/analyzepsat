\name{merge.tz}
\alias{merge.tz}
\title{
Merge temperature and depth records from individual tags 
}
\description{
Build a single data frame of all temperature and depth records for a group of tags.
Also determines sunrise and sunset on a macro scale.  
}
\usage{
merge.tz(btrack, psat, tagID = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{btrack}{
Estimated fit from ukfsst or trackit. This is only needed 
}
  \item{psat}{
List of psat tag data. Each item follows the data structure built using \code{\link{MWTextract}}
}
  \item{tagID}{
If using a single psat record (not a list), you need to specify a tagID.
}
}
\value{
A dataframe of temperature, depth, day night designation
}

\author{
Benjamin Galuardi
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{merge.hv}}, \code{\link{MWTextract}}
}

