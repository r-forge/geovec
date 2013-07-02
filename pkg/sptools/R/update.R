# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


setMethod('update', signature(object='SpatialPolygons'), 
function(object, ...) {

	yy <- list(...)
	if (is.null(yy)) {
		return(object)
	}

	i <- which(sapply(yy, function(x) inherits(x, 'SpatialPolygons')))
	if (length(i)==0) {
		stop('additional arguments should be of class SpatialPolygons')
	} else if (length(i) < length(yy)) {
		warning('additional arguments that are not of class SpatialPolygons are ignored')
		yy <- yy[i]
	} 

	haswarned <- FALSE
	for (y in yy) {
		if (! identical(proj4string(x), proj4string(y)) ) {
			if (!haswarned) {
				warning('non identical CRS')
				haswarned <- TRUE
			}
			y@proj4string <- x@proj4string
		}	
		subs <- gIntersects(x, y, byid=TRUE)
		if (!any(subs)) {
			next
		} else {
			int <- .cropSpatPolys(y, x)
			x <- erase(x, int)
			x <- combine(x, int)
		}
	}
	x
} 
)


.cropSpatPolys <- function(x, y, ...) {
	
		y <- gUnaryUnion(y)
		row.names(y) <- '1'
		rnx <- row.names(x)
		row.names(x) <- as.character(1:length(rnx))
		
		if (.hasSlot(x, 'data')) {
			
			# to keep the correct IDs
			# in future versions of rgeos, this intermediate step won't be necessary
			i <- as.vector( gIntersects(x, y, byid=TRUE) )
			if (sum(i) == 0) {
				return(NULL)
			}
			y <- gIntersection(x[i,], y, byid=TRUE)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			if (is.null(y)) { return(y) }
			
			ids <- strsplit(row.names(y), ' ') 
			ids <- as.numeric(do.call(rbind, ids)[,1])
			row.names(y) <- as.character(rnx[ids])
			data <- x@data[ids, ,drop=FALSE]
			rownames(data) <- rnx[ids]
			
			return( SpatialPolygonsDataFrame(y, data) )
		} else {
			y <- gIntersection(x, y)
			if (inherits(y, "SpatialCollections")) {
				y <- y@polyobj
			}
			return(y)
		}
}





