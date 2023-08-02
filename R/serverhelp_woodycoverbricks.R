# purely for canopyfromlatlon() using the THREDDS server

woody_vals_buffer <- function(roi, pts, years, buffer){
  suppressMessages(iscovered <- (vapply(sf::st_covered_by(pts, roi), length, FUN.VALUE = 1.1) >= 1))
  stopifnot(all(iscovered))
  out <- fetch_woody_cover_meanbuffer(pts, years, buffer)
  return(out[[1]][, -1]) # to make compatible with original outputs
}

fetch_woody_cover_meanbuffer <- function(pts, years, buffers){
  pts_3577 <- sf::st_transform(pts, 3577)
  bufferroi <- sf::as_Spatial(sf::st_buffer(pts_3577, max(buffers)))
  
  pts_sp <- sf::as_Spatial(pts_3577)
  
  woody_b <- fetch_woody_cover_brick(bufferroi, years) #in epsg:3577, which is GDA94
  woody_b[is.na(woody_b)] <- 0 #treat the NA values as 0, as fetch_brick_Albers returns pixel values for tiles outside mainland as NA
  
  # do special thing to particular values
  woody_b[woody_b == 157] <- 0
  woody_b[woody_b > 100] <- NA # I don't know what the other values above 100 mean, and if there are any
  wcfproportions_l <- lapply(buffers, function(buff){
    out <- terra::extract(woody_b, sf::st_buffer(pts_3577, buff), 
                           fun = mean,
                           touches = FALSE, na.rm = TRUE, raw = FALSE)
    cbind(buffer = buff, out[, -1, drop = TRUE]) #-1 removes the ID column
  })
  names(wcfproportions_l) <- buffers
  return(wcfproportions_l)
}

fetch_woody_cover_brick <- function(spobj, years, rootdir = "[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC"){
  
  b <- fetch_brick_Albers(spobj, years,
                          get_tile_filenames = function(tilecode, years){
                            filelist <- get_tile_filenames_WCF(tilecode, years, rootdir = rootdir)
                            return(filelist)}, 
                          tilereader = tilereader_WCF)
  return(b)
}

get_tile_filenames_WCF <- function(tilecode, years, rootdir = NULL){
  filelist <-               paste0(rootdir, "/",
                                  tilecode, "/",
                                  paste0("fc_metrics_", tilecode, "_"),
                                  years, 
                                  # ".nc#fillmismatch", #this code at the end get around the data type and fill value mismatch errors
                                  ".nc")
  return(filelist)
}

tilereader_WCF <- function(filename){
  ras <- withCallingHandlers(raster_wcflike(filename, varname = "WCF"),
                             warning = function(w){
                               if (grepl("cannot process these parts of the CRS", w$message))
                                 tryInvokeRestart("muffleWarning") 
                             })
  return(ras)
}

#' @title Extract a brick of Australian Albers Tiles.
#' @description Uses the files stored at the given location, and assumes the tiles are saved as EPSG:3577, which is GDA94.
#' @param spobj Spatial* or sf object that informs extents of the raster to extract
#' @param years Years of data to extract
#' @param get_tile_filenames A function with arguments (tilecode, years, ...). 
#' For a given tilecode and years it must return a list of filenames. Each filename is for the tile for each year provided.
#' @param tilereader A function that accepts a single argument, 'filename', and returns a raster object for that file.
#' @return A raster brick with extent equal or larger than \code{extent(spobj)}, snapped to the cells of the raster data.
#' The projection of the returned raster is EPSG:3577, which is GDA94.
#'  Extent of the returned value is a rectangle, and pixel values outside `spobj` are included.
fetch_brick_Albers <- function(spobj, years, get_tile_filenames = get_bggwtile_filenames, tilereader = bggwtilereader){
  spobj <- sf::st_as_sf(spobj)
  spobj <- sf::st_transform(spobj, crs = 3577)
  roi <- terra::ext(spobj)
  
  #tile codes:
  tilecodes <- get_tilecodes(spobj)
  austiles <- unlist(read.csv("./data/austilecodes.txt"))
  missingtiles <- setdiff(tilecodes, austiles)
  tilecodes <- intersect(tilecodes, austiles)
  if (length(tilecodes) == 0){stop("No data for this location.")}
  if (length(missingtiles) > 0){
    warning(paste("The following tiles are not available due to being outside of Australia mainland:",
                  paste(missingtiles, collapse = " "),
                  ". The returned raster will have NA values for locations in these tiles."))
  }
  
  #build brick for each tile
  brickfortile <- function(tilecode){
    filelist <- get_tile_filenames(tilecode, years)
    r.l <- lapply(filelist, tilereader)
    
    names(r.l) <- years
    r.l_crop <- lapply(r.l, terra::crop, y = roi, snap = "out")
    
    # warning: I think the following bricks get saved to rasterOptions()$tmpdir when RAM runs out
    bs <- terra::rast(r.l_crop)
    names(bs) <- years
    return(bs)}
  b.l <- lapply(tilecodes, brickfortile) 
  
  # merge bricks
  b <- Reduce(terra::merge, b.l)
  names(b) <- years
  terra::crs(b) <- "epsg:3577"
  
  # if missing tiles, extend raster with zero values
  if (length(missingtiles) > 0){
    b <- terra::extend(b, roi, fill = NA, snap = "out")    
  }
  return(b)
}


#' @title Intersecting Albers Australian Tile Codes
#' @description Given a spatial object returns the tile codes for Albers Tiles used by Geoscience Australia and others
#' @param sfobj an sp object
#' @return a named vector of tile codes
#' @export
get_tilecodes <- function(sfobj){
  sfobj <- sf::st_as_sf(sfobj)
  sfobj <- sf::st_transform(sfobj, 3577) #transform to the correct projection
  roi <- sf::st_bbox(sfobj)
  
  tilestep <- 100000
  lxmin <- floor(roi$xmin / tilestep) * tilestep #lowest xmin
  xmins <- seq(lxmin, -1 + ceiling(roi$xmax / tilestep) * tilestep,
               by = tilestep)
  lymin <- floor(roi$ymin / tilestep) * tilestep #lowest ymin
  ymins <- seq(lymin, -1 + ceiling(roi$ymax / tilestep) * tilestep,
               by = tilestep)
  
  xmin_v_ymin <- expand.grid(xmin = xmins, ymin = ymins)
  tilecodes <- apply(xmin_v_ymin / tilestep, 1, function(x) paste(x, collapse = "_"))
  names(tilecodes) <- tilecodes
  return(tilecodes)
}

