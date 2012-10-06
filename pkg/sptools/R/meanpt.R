# Author: Robert J. Hijmans
# February 2012
# version 1
# license GPL3

if (!isGeneric("xymean")) {
	setGeneric("xymean", function(x, w=NULL)
		standardGeneric("xymean"))
	
}

setMethod("xymean", signature(x='SpatialPolygons'),
	function(x, w=NULL){
		if (isTRUE(!is.projected(x))) {
			.xymeanLL(coordinates(x), w=w)
		} else {
			.xymean(coordinates(x), w=w)
		}
	}
)	


setMethod("xymean", signature(x='SpatialPoints'),
	function(x, w=NULL){
		if (isTRUE(!is.projected(x))) {
			.xymeanLL(coordinates(x), w=w)
		} else {
			.xymean(coordinates(x), w=w)
		}
	}
)	


setMethod("xymean", signature(x='RasterLayer'),
	function(x, w=NULL){
		if (is.null(w)) {
			w <- getValues(x)
		}
		if (isTRUE(!is.projected(x))) {
			.xymeanLL(coordinates(x), w=w)
		} else {
			.xymean(coordinates(x), w=w)
		}
	}
)	


.xymean <- function(xy, w=NULL) {
	if (is.null(w)) {
		xy <- na.omit(xy)
		res <- apply(xy, 2, mean)
	} else {
		if (length(w) != nrow(xy)) {
			stop('length of weights not correct. It should be: ', nrow(xy))
		}
		xy <- cbind(xy, w)
		xy <- na.omit(xy)
		sw <- sum(xy[,3])
		x <- sum(xy[,1] * xy[,3]) / sw
		y <- sum(xy[,2] * xy[,3]) / sw
		res <- cbind(x, y)
	}
	res
}



.xymeanLL <- function(xy, w=NULL) {
	xy <- na.omit(xy) 
	xy[,1] <- xy[,1] + 180
	xy <- xy * pi / 180 
	if (is.null(w)) {
		w <- 1
	} else if (length(w) != nrow(xy)) {
			stop('length of weights not correct. It should be: ', nrow(xy))
	}
	w <- w / sum(w)
		
	Sx <- mean(sin(xy[,1]) * w)
	Cx <- mean(cos(xy[,1]) * w)
	x <- atan2(Sx, Cx)
	x <- x %% (2 * pi) - pi
		
	Sy <- mean(sin(xy[,2]) * w)
	Cy <- mean(cos(xy[,2]) * w)
	y <- atan2(Sy, Cy)
		
	cbind(x,y) * 180 / pi
}


if (!isGeneric("xymedian")) {
	setGeneric("xymedian", function(x, w=NULL, converge)
		standardGeneric("xymedian"))
	
}


setMethod("xymedian", signature(x='SpatialPolygons'),
	function(x, w=NULL, converge){
		if (isTRUE(!is.projected(x))) {	
			warning('this function should only be used with planar coordinates') 
		}
		.xymedian(coordinates(x), w=w, converge=converge)
	}
)	

setMethod("xymedian", signature(x='SpatialPoints'),
	function(x, w=NULL, converge){
		if (isTRUE(!is.projected(x))) {
			warning('this function should only be used with planar coordinates') 
		}
		.xymedian(coordinates(x), w=w, converge=converge)
	}
)	


setMethod("xymedian", signature(x='RasterLayer'),
	function(x, w=NULL, converge){
		if (isTRUE(!is.projected(x))) {	
			warning('this function should only be used with planar coordinates') 
		}
		if (is.null(w)) {
			w <- getValues(x)
		}
		.xymedian(coordinates(x), w=w, converge=converge)
	}
)	


.xymedian <- function(xy, w=NULL, converge){
	if (missing(converge)) {
		converge = 0.01
	}
	if (is.null(w)) {
		xyp <- cbind(x=mean(xy[,1]), y=mean(xy[,2]))
		dif <- dold <- Inf
		d <- sqrt((xy[,1] - xyp[,1])^2 + (xy[,2] - xyp[,2])^2)
		while (dif > converge) {
			xyp <- cbind(sum(xy[,1]/d) / sum(1/d), sum(xy[,2]/d) / sum(1/d))
			d <- sqrt((xy[,1] - xyp[,1])^2 + (xy[,2] - xyp[,2])^2)
			dif <- sum(abs(dold - d))
			dold <- d
		}
		res <- xyp
	} else {
		xy <- cbind(xy, w)
		xy <- na.omit(xy)
		xyp <- .xymean(xy[, 1:2], w=w)
		dif <- dold <- Inf
		d <- sqrt((xy[,1] - xyp[,1])^2 + (xy[,2] - xyp[,2])^2)
		while (dif > converge) {
			sw <- xy[,3]/d
			sw[!is.finite(sw)] <- 0
			xyp <- cbind(sum(xy[,1]*sw)/sum(sw), sum(xy[,2]* sw)/sum(sw))
			d <- sqrt((xy[,1] - xyp[,1])^2 + (xy[,2] - xyp[,2])^2)
			dif <- sum(abs(dold - d))
			dold <- d
		}
		res <- xyp
	}
	res
}
	



if (!isGeneric("xyvar")) {
	setGeneric("xyvar", function(x, simple=TRUE)
		standardGeneric("xyvar"))
	
}

setMethod("xyvar", signature(x='SpatialPolygons'),
	function(x, simple=TRUE){
		if (isTRUE(!is.projected(x))) {
			warning('this function should only be used with planar coordinates') 
		}
		x <- coordinates(x)
		.xyvar(x, simple=simple)
	}
)	


setMethod("xyvar", signature(x='SpatialPoints'),
	function(x, simple=TRUE){
		if (isTRUE(!is.projected(x))) {
			warning('this function should only be used with planar coordinates') 
		}
		.xyvar(coordinates(x), simple=simple)		
	}
)	

.xyvar <- function(xy, simple=TRUE) {
	m <- .xymean(xy)
	d <- sqrt((xy[,1] - m[,1])^2 + (xy[,2] - m[,2])^2)
	if (simple) {
		stdev <- sum(d) / nrow(xy)	
	} else {
		stdev <- sqrt(sum(d^2) / nrow(xy))
	}
	stdev
}


