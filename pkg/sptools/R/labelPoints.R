
# author: Karl Ove Hufthammer 
# http://r-sig-geo.2731867.n2.nabble.com/Better-label-placement-for-polygons-td7580550.html

labelPoints <- function(x) {

  # Prepopulate the label point matrix with the centroid values
	coords <- coordinates(x)
	sarea <- sqrt(gArea(x)/pi)/10
		
  # For each polygon in x, calculate the appropriate label point
	for(i in seq_len(length(x))) {

    # First fetch the polygon to process
		p <- x[i,]

		init <- 0             # Initial amount to shrink
		estep <- sarea[i]     # Additional amount to shrink for each step

    # Try repeatedly shrinking the polygon until we’re left
    # with a polygon whose convex hulls fits inside
		repeat {
			repeat {
				r <- init + estep               # Amount to shrink
				p2 <- gBuffer(p, width = -r)    # Shrink the polygon
				if( gArea(p2) <= 0 ) {          # If the shrunken polygon is empty ...
					estep <- estep/2 
				} else {
					break   # ... try again with a smaller value
				}
			}

		  # If we’re left with more than one polygon, choose the largest one
			areas <- sapply(p2@polygons[[1]]@Polygons, function(x) x@area)
			if(length(areas) > 1) {
			# Note that we create a *new* SpatialPolygon containing the largest polygon.
			# I guess in theory we *could* have just replaced the @Polygons slot of p2,
			# but then gArea seems to crash R ... :(
				ind.max <- which.max(areas)
				p2 <- SpatialPolygons(list(Polygons(list(p2@polygons[[1]]@Polygons[ind.max][[1]]),
											 ID="middle")), proj4string=CRS(proj4string(p2)))
			}

		  # Calculate the convex hull of the inner polygon.
		  # If this is wholly contained in the original polygon,
		  # break out of the loop and set the label point to
		  # the centroid of the inner polygon.
			if( gContains(p, gConvexHull(p2)) ) {
				break 
			} else {
				init <- init + estep
			}
		}
		coords[i,] <- coordinates(p2)
	}
	coords
}

