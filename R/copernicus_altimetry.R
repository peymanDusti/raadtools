#' Altimetry products. 
#' 
#' Functions `read_sla_daily` and so on for "ugosa, adt, ugos, sla, vgos, vgosa, err". 
#'
#' sla is sea level anomaly, for the raw files see `raadfiles::altimetry_daily_files`
#' 
#' @inheritParams raadtools
#'
#' @return
#' @export
#' @name read_ugosa_daily
#' @examples
#' a <- read_adt_daily(sort(Sys.Date() - 1:50), 
#' xylim = extent(100, 150, -70, -40))
#' \dontrun{
#' animate(a, pause = 0, 
#' col = colorRampPalette(c("dodgerblue", "white", "firebrick"))(21), 
#' breaks = c(seq(min(cellStats(a, min)), 0, length = 11), 
#'            seq(0.001, max(cellStats(a, max)), length = 10)))
#' }
read_ugosa_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "ugosa"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_adt_daily
#' @export
read_adt_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "adt"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_ugos_daily
#' @export
read_ugos_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "ugos"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_sla_daily
#' @export
read_sla_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "sla"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}
#' @name read_vgos_daily
#' @export
read_vgos_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "vgos"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}


#' @name read_vgosa_daily
#' @export
read_vgosa_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "vgosa"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}

#' @name read_err_daily
#' @export
read_err_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  varname <- "err"
  read_copernicus_daily(date = date, xylim = xylim, latest  = latest, returnfiles = returnfiles, varname = varname, ..., inputfiles = inputfiles)
}



read_copernicus_daily <- function(date, xylim = NULL, latest = TRUE, returnfiles = FALSE, varname, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- raadfiles::altimetry_daily_files()
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) {
      date <- max(files$date)
    } else {
      date <- min(files$date)
    }  
  }
    date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")
  read0 <- function(x, varname) raster(x)
  
  nfiles <- nrow(files)
  ## progress
  pb <- progress::progress_bar$new(
    format = "  extracting [:bar] :percent in :elapsed",
    total = nfiles, clear = FALSE, width= 60)
  pb$tick(0)
  read_fun <- function(xfile, ext, msk, rot, varname = "", band = 1) {
    pb$tick()
    mask_if_needed(crop_if_needed(rotate_if_needed(raster(xfile, varname = varname, band = band), rot), ext), msk)
  }
  
  msk <- NULL
  rot <- FALSE
  files$band <- 1
  op <- options(warn = -1)
  r0 <- brick(stack(lapply(seq_len(nrow(files)), function(xi) 
    read_fun(files$fullname[xi], ext = xylim, msk = msk, rot = rot, varname = varname, band = files$band[xi]))), ...)
  options(op)
  r0 <- setZ(r0, files$date)
 r0  
  }
  

#' @examples
#' files <- raadfiles::altimetry_daily_files()
#' ex <- extent(100, 150, -70, -40)
#' dts <- sort(Sys.Date() - 1:50)
#' 
#' ## 3.8s
#' system.time({a <- read_adt_daily(dts, xylim = ex, inputfiles = files)})
#' 
#' ## 1.5s
#' system.time({b <- tidycopern(dts, varname = "adt", xylim = ex, inputfiles = files)})
#' 
#' dts <- head(files$date, 100)
#' ex <- extent(170, 220, -10, 40)
#' ## 8.1s
#' system.time({a <- read_adt_daily(dts, xylim = ex, inputfiles = files)})
#' 
#' ## 4s
#' system.time({b <- tidycopern(dts, varname = "adt", xylim = ex, inputfiles = files)})
#' 
tidycopern <- function(date, varname, rescale = TRUE, xylim = NULL, latest = TRUE, returnfiles = FALSE, ..., inputfiles = NULL) {
  if (is.null(inputfiles)){
    files <- raadfiles::altimetry_daily_files()
  } else {
    files <- inputfiles
  }
  if (returnfiles) return(files)
  if (missing(date)) {
    if (latest) {
      date <- max(files$date)
    } else {
      date <- min(files$date)
    }  
  }
  date <- timedateFrom(date)
  files <- .processFiles(date, files, "daily")
  read0 <- function(x, varname) raster(x)
  
  nfiles <- nrow(files)
  ## progress
  #pb <- progress::progress_bar$new(
  #  format = "  extracting [:bar] :percent in :elapsed",
  #  total = nfiles, clear = FALSE, width= 60)
  #pb$tick(0)
  ## the metadata
  tnc <- tidync::tidync(files$fullname[1])
  ## the transforms
  trn <- tidync::hyper_filter(tnc)
  #unique(diff(trn$longitude$longitude))  0.25
  dx <- diff(trn$longitude$longitude[1:2])
  dy <- diff(trn$latitude$latitude[1:2])
  yflip <- dy > 0
  xflip <- dx < 0
  if (!is.null(xylim)) {
   trn$longitude <-  trn$longitude %>% 
      dplyr::filter(longitude >= raster::xmin(xylim) & longitude <= raster::xmax(xylim))
   trn$latitude <- trn$latitude %>% 
      dplyr::filter(dplyr::between(latitude, raster::ymin(xylim), raster::ymax(xylim)))
  }
  tin <- tidync::hyper_index(trn)
  
  out_extent <- raster::extent(range(trn$longitude$longitude) + c(-1, 1) * abs(dx)/2, 
                               range(trn$latitude$latitude) + c(-1, 1) * abs(dy)/2)
  out_crs <- "+proj=longlat +rf=298.257 +a=6378136.3 "
  
  ## extent is transform
  ## mask is <not needed>
  ## rotation is <later> use the transforms
  ## varname is input
  read_fun <- function(xfile, index, varname, raw_datavals = FALSE) {
   con <- ncdf4::nc_open(xfile)
   on.exit(ncdf4::nc_close(con))
   ncdf4::ncvar_get(con, varname, start = index$start, count = index$count, raw_datavals = raw_datavals)
 }  
  arr <- gbind(lapply(files$fullname, read_fun, index = tin, varname = varname, raw_datavals = !rescale))
  if (yflip) arr <- arr[,ncol(arr):1, ]
  r0 <- setExtent(brick(arr, crs = out_crs, transpose = TRUE), out_extent)
  r0 <- setZ(r0, files$date)
  r0  
}

