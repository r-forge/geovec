

if (!isGeneric("safeTransform")) {
	setGeneric("safeTransform", function(x, crs, ...)
		standardGeneric("safeTransform"))
}	


setMethod ('safeTransform', c('SpatialPolygonsDataFrame', 'CRS'),
	function(x, crs, ...) {

	crs <- projection(crs)
	sl <- as(x, 'SpatialLinesDataFrame')
	sl$ID1 <- 1:nrow(sl)
	sld <- disaggregate(sl)
	sld$ID2 <- 1:nrow(sld)
	sp <- as(sld, 'SpatialPointsDataFrame')
	z <- project(coordinates(sp), projection(crs))
	z[!is.finite(z)] <- NA
	xyz <- na.omit(cbind(z, ID1=sp$ID1, ID2=sp$ID2))
	zz <- unique(xyz[,3])

	List <- lapply(zz, function(i) {
		b <- subset(xyz, xyz[, 3] == i)
		pls <- lapply( unique(b[, 4]), function(j) {
			bb <- subset(b[, 1:2], b[, 4] == j)
			if (nrow(bb) > 2) {	
				test <- isTRUE(all.equal(bb[1,], bb[nrow(bb), ]) )
				if (! test) {
					bb <- rbind(bb, bb[1,])
				}			
				Polygon(bb) 
			}
		} )
		pls <- pls[!sapply(pls, is.null)]
		if (!is.null(pls)) {
			pls <- Polygons(pls, as.character(i))
		}
		pls
	} )
	spps <- SpatialPolygons(List, proj4string = CRS(crs))
	dat <- sl@data
	dat <- dat[dat$ID1 %in% row.names(spls), ]
	rownames(dat) <- dat$ID1
	dat$ID1 <- NULL
	SpatialPolygonsDataFrame(spps, dat)
})




setMethod ('safeTransform', c('SpatialLinesDataFrame', 'CRS'),
	function(x, crs, ...) {

	crs <- projection(crs)
	x$ID1 <- 1:nrow(x)
	sld <- disaggregate(x)
	sld$ID2 <- 1:nrow(sld)
	sp <- as(sld, 'SpatialPointsDataFrame')
	z <- project(coordinates(sp), projection(crs))
	z[!is.finite(z)] <- NA
	xyz <- na.omit(cbind(z, ID1=sp$ID1, ID2=sp$ID2))
	zz <- unique(xyz[,3])

	List <- lapply(zz, function(i) {
		b <- subset(xyz, xyz[, 3] == i)
		lns <- lapply( unique(b[, 4]), function(j) {
			bb <- subset(b[, 1:2], b[, 4] == j)
			if (nrow(bb) > 1) {	
				Line(bb) 
			}
		} )
		lns <- lns[!sapply(lns, is.null)]
		if (!is.null(lns)) {
			lns <- Lines(lns, as.character(i))
		}
		lns
	} )
	spls <- SpatialLines(List, proj4string = CRS(crs))
	dat <- x@data
	dat <- dat[dat$ID1 %in% row.names(spls), ]
	rownames(dat) <- dat$ID1
	dat$ID1 <- NULL
	splsdf <- SpatialLinesDataFrame(spls, dat)
})


