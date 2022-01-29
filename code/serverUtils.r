################################
# shared functions among servers
################################
domainOffset <- 5
# set up probability mass function (PMF)
makePMF <- function (domain, start_guess, weight = 4, floor = 0.001) {
  glaucomaPMF <- rep(0.001,length(domain))
  # bimodal prior
  glaucomaPMF[1:10] <- c(rep(0.001, 4), 0.2, 0.3, 0.2, 0.15, 0.1, 0.02)
  healthyPMF <- function(normalModePoint) {
    temp <- c(rep(0.001, 100), 0.001, 0.03, 0.05, 0.1, 0.2, 0.3, 0.2, 0.05, 0.025, 0.01, rep(0.001, 100))
    mode <- which.max(temp)
    return(temp[(mode - normalModePoint + domain[1]):(mode - normalModePoint + domain[length(domain)])])
  }
  makeBiModalPMF <- function(normalModePoint, weight, pdf.floor) {
    npdf <- healthyPMF(normalModePoint)
    cpdf <- npdf * weight + glaucomaPMF
    cpdf[which(cpdf < pdf.floor)] = pdf.floor 
    return(cpdf)
  }
  # define prior PMF, minimum stimulus and maximum stimulus for luminance test
  # bimodal Prior PMF
  prior_pmf <- makeBiModalPMF(start_guess, weight, floor)
  # normalise PMF
  prior_pmf <- prior_pmf / sum(prior_pmf)
  return(prior_pmf)
}

# create table of neighboring locations
findNeighbors <- function(locs) {
  # Function which creates Voronoi tessellation tiles from grid locations
  voroni_to_polys <- function(dd) {
    # tile.list extracts the polygon data from the deldir computation
    vor_desc <- tile.list(dd)
    # gets us the points for the polygons but we still have to close them, hence the need for the rbind
    vor_polygons <- lapply(1:(length(vor_desc)), function(i) {
      tmp <- cbind(vor_desc[[i]]$x, vor_desc[[i]]$y)
      tmp <- rbind(tmp, tmp[1,])
      Polygons(list(Polygon(tmp)), ID = i) # now we can make the Polygons
    })
    # create location IDs
    xdf <- data.frame(id=sapply(slot(SpatialPolygons(vor_polygons), 'polygons'), slot, 'ID'))
    rownames(xdf) <- xdf$id
    SpatialPolygonsDataFrame(SpatialPolygons(vor_polygons), data=xdf)
  }
  #create lookup table of neighbors
  vpoly <- voroni_to_polys(deldir(locs$x,locs$y))
  neighbors <- gTouches(vpoly, byid = TRUE)
  return(neighbors)
}

# open up new locations to test in the growth pattern
openNewLocs <- function (neighbors, loc, locs, unfinished, finished) {
  # check locations to open up for the growth algorithm
  wave <- locs$wave[loc] # find wave of reference location
  # find neighbors that are in the next wave
  new_locations <- intersect(which(neighbors[loc,] == TRUE),
                             which(locs$wave == wave + 1))
  # check that new locations aren't already in unfinished queue as they could have opened up earlier
  # also check that locations have not already terminated
  if (length(new_locations) > 0)
    new_locations <- setdiff(new_locations, c(unfinished, finished))
  return(new_locations)
}

# read command from the client
readCommand <- function(q) {
  # get all messages received
  msg <- q$pop()
  if(msg$title != "CMD")
    stop("Expecting a command. There is an error in logic")
  return(msg$message)
}

# send results to client
writeResults <- function(q, res) {
  q$push(title   = rep("VAL", 9),
         message = c(res$type, res$x, res$y, res$level, res$th, as.character(res$seen),
                     res$time, res$respWin, as.character(res$done)))
}