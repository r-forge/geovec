\name{sptools-package}

\alias{sptools}

\docType{package}

\title{ 
Manipulation of geographic object data (points, lines, polygons) 
}

\description{

The \code{sptools} package has functions to manipulate geographic vector data. It builds on the functionality provided by the \code{rgeos} package. \code{rgeos} has functions to manipulate geometries ("spatial features"). \code{sptools} makes it easier to do for complex polygons with attribute data. Attribute data are kept, or combined. It might be that some of these functions will become obsolete as rgeos develops.


Comparison of terminology for polygon manipulation methods used in ArcGIS and in this package (sptools)

\tabular{rll}{
  \tab \bold{ArcGIS}    \tab  \bold{sptools} \cr
  \tab Erase     \tab  \code{\link{erase}} or "-" \cr
  \tab Identity  \tab  \code{\link{ident}} \cr
  \tab Intersect \tab  \code{\link{intersect}} or "\*" \cr
  \tab Union     \tab  \code{\link{union}} or "+" \cr
  \tab Update    \tab  \code{\link{update}} \cr
  \tab Symmetrical Difference \tab  \code{\link{symdif}} \cr 
  \tab Append    \tab  \code{\link{combine}} \cr
  \tab Dissolve  \tab  \code{\link{aggregate}}  \cr
  \tab Explode   \tab  \code{\link[sp]{disaggregate}} (in the \code{sp} package) \cr
  \tab Join table \tab \code{\link[sp]{merge}} (in the \code{sp}  package) \cr
  \tab Clip      \tab  \code{\link[raster]{crop}} (in the \code{raster} package) \cr
  \tab Select    \tab  \code{\link[raster]{select}} (in the \code{raster} package) \cr
  \tab "i" (identify) \tab  \code{\link[raster]{click}} (in the \code{raster} package) \cr
}

For spatial-join and other spatial queries, see \code{\link[sp]{over}} in package the \code{sp} package or \code{\link[raster]{extract}} in the \code{raster} package
}


\keyword{package}
\keyword{spatial}
