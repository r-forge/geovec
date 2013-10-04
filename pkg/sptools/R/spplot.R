# spplot for Spatial object that have no data.frame

setMethod('spplot', signature(obj='SpatialPolygons'), 
function(obj, ...) {
	obj <- SpatialPolygonsDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})



setMethod('spplot', signature(obj='SpatialLines'), 
function(obj, ...) {
	obj <- SpatialLinesDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})



setMethod('spplot', signature(obj='SpatialPoints'), 
function(obj, ...) {
	obj <- SpatialPointsDataFrame(obj, data.frame(ID=1:length(obj)))
	spplot(obj, ...)
})



