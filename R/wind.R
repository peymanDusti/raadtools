
##' NCEP2 wind files
##'
##' Files containing NCEP2 wind vector data
##' @title Files containing NCEP2 wind vector data
##' @param data.source ignored, reserved for future use
##' @param time.resolution  time resolution data to read, daily only for now
##' @param ... reserved for future use, currently ignored
##' @param fromCache load file catalog from cache or rebuild
##' @return \code{data.frame} of file names and dates
##' @export
windfiles <-
  function(data.source = "", time.resolution = c("daily"), fromCache = TRUE, ...) {
    datadir <- getOption("default.datadir")
    time.resolution <- match.arg(time.resolution)
    ##      fromCache <- TRUE
    
    if (fromCache) {
      load(file.path(datadir, "cache", sprintf("%s_windfiles.Rdata", time.resolution)))
      wf$ufullname <- file.path(datadir,  wf$ufile)
      wf$vfullname <- file.path(datadir,  wf$vfile)
      return(wf)
    }
    
    
    cfiles <- list.files(file.path(datadir, "wind", "ncep2", time.resolution), pattern = ".nc$", full.names = TRUE)
    cfiles <- file.path("wind", "ncep2", time.resolution, basename(cfiles))
    ufiles <- grep("uwnd", cfiles, value = TRUE)
    vfiles <- grep("vwnd", cfiles, value = TRUE)
    
    ##datepart <- sapply(strsplit(basename(cfiles), "_"), function(x) x[length(x)-1])
    dates <- as.POSIXlt(as.Date(basename(ufiles), "uwnd.10m.gauss.%Y"))
    dates$mday <- 1
    dates$mon <- 0
    dates <- as.POSIXct(dates)
    wf <- data.frame(ufile = ufiles, vfile = vfiles, date = dates, stringsAsFactors = FALSE)
    wfU <- .expandFileDateList(file.path(datadir, wf$ufile))
    wfV <- .expandFileDateList(file.path(datadir, wf$vfile))
    wf <- data.frame(ufile = gsub("^/", "", gsub(datadir, "", wfU$file)), vfile = gsub("^/", "", gsub(datadir, "", wfV$file)), date = wfU$date, band = wfU$band, stringsAsFactors = FALSE)
    save(wf, file = file.path(datadir, "cache", sprintf("%s_windfiles.Rdata", time.resolution)))
    wf
    
  }

##' Read wind
##'
##' Read wind data
##' @title readwind
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resolution to read
##' @param magonly return just the magnitude from the U and V
##' components
##' @param dironly return just the direction from the U and V, in degrees N=0, E=90, S=180, W=270
##' @param uonly return just the horizontal component of velocity, U
##' @param vonly return just the vertical component of velocity, V
##' @param returnfiles ignore options and just return the file names and dates
##' @param xylim crop
##' @param lon180 Pacific or Atlantic
##' @param ... arguments passed to \code{\link[raster]{brick}}, i.e. \code{filename}
##' @return raster object
##' @examples
##' \dontrun{
##'  dts <- seq(as.Date("2000-01-01"), by = "1 days", length = 350)
##'  library(animation)
##'  ani.start(ani.width = 800, ani.height = 800)
##'  for (i in seq_along(dts)) {
##'     x <- readwind(dts[i]);
##'     if (i == 1L) crds <- coordinates(x[[1]])
##'     plot(sqrt(x[[1]]^2 + x[[2]]^2), xlim = c(40, 180), ylim = c(-90, -20));
##' x1 <- crds[,1]
##' y1 <- crds[,2]
##' x2 <- crds[,1] + values(x[[1]])/4
##' y2 <- crds[,2] + values(x[[2]])/4
##'     arrows(x1, y1, x2, y2, length = 0.06);
##'     plot(m, add = TRUE)
##' }
##' ani.stop()
##'
##'
##'
##' }
##' @export
readwind <- function(date, time.resolution = c("daily"), xylim = NULL, lon180 = TRUE,
                     magonly = FALSE, dironly = FALSE,
                     uonly = FALSE,
                     vonly = FALSE,
                     returnfiles = FALSE, ...) {
  
  time.resolution <- match.arg(time.resolution)
  if ((magonly + dironly + uonly + vonly) > 1) stop("only one of magonly, dironly, uonly or vonly may be used, exiting")
  files <- windfiles()
  if (returnfiles) return(files)
  
  if (missing(date)) date <- min(files$date)
  date <- timedateFrom(date)
  ## findex <- .processDates(date, files$date, time.resolution)
  files <- .processFiles(date, files, time.resolution)
  
  
  nfiles <- nrow(files)
  if (nfiles > 1L & !magonly & !dironly & !uonly & !vonly) {
    warning("only one time index can be read at once unless 'magonly', 'dironly', 'uonly' or 'vonly' is TRUE")
    files <- files[1L,]
    nfiles <- 1L
  }
  
  
  if (!(magonly | dironly))
    rasterfun <- function(x1, x2) {
      x <- brick(x1, x2)
      names(x) <- c("U", "V")
      x
    }
  if (magonly)
    rasterfun <- function(x1, x2) sqrt(x1 * x1 + x2 * x2)
  if (dironly)
    rasterfun <- function(x1, x2) (90 - atan2(x2, x1) * 180/pi)%%360
  
  if (!(magonly | dironly)) {
    if (uonly) rasterfun <- function(x1, x2) x1
    if (vonly) rasterfun <- function(x1, x2) x2
  }
  
  
  cropit <- FALSE
  if (!is.null(xylim)) {
    cropit <- TRUE
    cropext <- extent(xylim)
  }
  
  r <- vector("list", nfiles)
  for (ifile in seq_len(nfiles)) {
    r1 <- raster(files$ufullname[ifile], band = files$band[ifile])
    r2 <- raster(files$vfullname[ifile], band = files$band[ifile])
    r0 <- rasterfun(r1, r2)
    if (lon180)     r0 <- suppressWarnings(.rotate(r0))
    if (cropit) r0 <- crop(r0, cropext)
    r[[ifile]] <- r0
    
  }
  r <- brick(stack(r), ...
  )
  if (magonly | dironly | uonly | vonly)  {
    r <- setZ(r, files$date)
    names(r) <- sprintf("wind_%s", format(files$date, "%Y%m%d"))
  } else {
    
    r <- setZ(r, rep(files$date, 2L))
    names(r) <- sprintf("%swind_%s", c("U", "V"), format(files$date, "%Y%m%d"))
  }
  
  
  ## get alignment right (put this in raster?)
  extent(r) <- extent(c(xmin(r) + res(r)[1]/2, xmax(r) + res(r)[1]/2,
                        ymin(r), ymax(r)))
  
  r
  
}


