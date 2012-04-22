\name{xymean}

\docType{methods}

\alias{xymean}
\alias{xymean,SpatialPolygons-method}
\alias{xymean,SpatialPoints-method}
\alias{xymean,RasterLayer-method}

\alias{xymedian}
\alias{xymedian,SpatialPolygons-method}
\alias{xymedian,SpatialPoints-method}
\alias{xymedian,RasterLayer-method}

\alias{xyvar}
\alias{xyvar,SpatialPolygons-method}
\alias{xyvar,SpatialPoints-method}

\title{ 
mean, median, and variance for locations.
}

\description{
mean, median, and variance for points or polygons. For polygons, the centroid is used. Mean and median can be weighted.
}

\usage{
\S4method{xymean}{SpatialPoints}(x, w)
\S4method{xymedian}{SpatialPoints}(x, w, converge)
\S4method{xyvar}{SpatialPoints}(x, simple=TRUE)
}

\arguments{
  \item{x}{SpatialPoints or SpatialPolygons object}
  \item{w}{weights (vector of numeric values, with a length that is equal to the number of spatial features in \code{x}}
  \item{converge}{xxx}
  \item{simple}{xxx}
}

\value{
Numeric (coordiate pair for xymean and xymedian)
}

\author{Robert J. Hijmans}

\references{
Rogerson, P.A., 2010. Statistical methods for xygraphy. A student's guide. Sage
}

\examples{
x <- c(.8616, .1277, .3093, .4623, .4657, .2603, .6680, .2705, .1981, .8891)
y <- c(.1781, .4499, .5080, .3419, .3346, .0378, .3698, .1659, .1372, .1192)
p <- SpatialPoints(cbind(x, y))
xymean(p)
xymean(p, w=1:10)
xymedian(p, converge=0.0001)
}


\keyword{methods}
\keyword{spatial}

