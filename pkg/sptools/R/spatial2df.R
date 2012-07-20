

if (!isGeneric("spatialToDf")) {
	setGeneric("spatialToDf", function(x, ...)
		standardGeneric("spatialToDf"))
}	


setMethod("spatialToDf", c('SpatialPointsDataFrame'),
	function(x, data=TRUE, ...) {
		if (data) {
			data.frame(coordinates(x), x@data)
		} else {
			data.frame(coordinates(x))		
		}
	}
)


setMethod("spatialToDf", c('SpatialPolygons'),
	function(x, ...) {
		spatialToDf(SpatialPolygonsDataFrame(x, data.frame(ID1 = 1:length(x))), data=FALSE)
	}
)


setMethod("spatialToDf", c('SpatialPolygonsDataFrame'),
	function(x, data=TRUE, ...) {
		sl <- as(x, 'SpatialLinesDataFrame')
		sl$ID1 <- 1:nrow(sl)
		sld <- disaggregate(sl)
		sld$ID2 <- 1:nrow(sld)
		sp <- as(sld, 'SpatialPointsDataFrame')
		
		
	
	
} )
