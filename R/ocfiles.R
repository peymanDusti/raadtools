##' Load metadata and location of files of ocean colour data products.
##'
##' This function loads the latest cache of stored files for
##' NASA ocean colour products.
##' @param time.resolution daily or monthly files?
##' @param product choice of ocean colour sensor
##' @param varname which variable (or set of variables)
##' @param type which level of data
##' @param bz2.rm ignore files that are compressed
##' @param ext which extension, e.g. "nc" or "main"
##' @param ... reserved for future use, currently ignored
##' @export
##' @return data.frame of \code{file} and \code{date}
ocfiles <- function(time.resolution = c("daily", "weekly", "monthly", "weekly32"),
                    product = c("MODISA", "SeaWiFS", "VIIRS"), 
                    varname = c("RRS", "CHL", "POC", "KD490", "NPP_PAR", "SNPP_CHL", "SNPP_RRS"), 
                    type = c("L3b", "L3m"),
                    bz2.rm = TRUE, 
                    ext = c("nc", "main"), 
                    ...) {
  datadir <- getOption("default.datadir")
  ftx <- .allfilelist()
  time.resolution <- match.arg(time.resolution)
  product <- match.arg(product)
  ext <- match.arg(ext)
  varname <- varname[1L]
  type <- match.arg(type)
  
  time <- switch(time.resolution, 
                 daily = "DAY", 
                 weekly = "8D", 
                 monthly = "MO", 
                 weekly32 = "R32")
  
  ## don't forget those ST93c files!  
  ## see here: http://oceancolor.gsfc.nasa.gov/DOCS/FormatChange.html
  #Note: ST92 is the test set designation for the SeaWiFS test run, similarly 
  #AT108 and AT109 are the MODIS-Aqua test set designators.  These designations 
  # will NOT be part of the reprocessing filenames.  
  mtag <- sprintf(paste0("%s.*\\.", ext), paste(type, time, varname, sep = "_"))
  #print(mtag)
  ##cfiles1 <- sapply(product, function(x) file.path("oceandata.sci.gsfc.nasa.gov", x)
  cfiles1 <- grep(file.path("oceandata.sci.gsfc.nasa.gov", product), ftx, value = TRUE)
  cfiles2 <- grep(mtag, cfiles1, value = TRUE)
  cfiles <- if (bz2.rm)  grep(paste0(ext, "$"), cfiles2, value = TRUE) else cfiles2
  tokens <- .filetok(basename(cfiles))
  if (length(cfiles) < 1) stop("no files found for ", paste(product, varname, type, time.resolution, collapse = ", "))
  dates <- as.POSIXct(strptime(paste0(tokens$year, tokens$jday), "%Y%j", tz = "GMT"))
  cfs <- data.frame(file = gsub(paste(datadir, "/", sep = ""), "", cfiles), fullname = cfiles, 
                    date = dates, stringsAsFactors = FALSE)[order(dates), ]
  cfs
  
}


## This function is from roc
.filetok <- function(x) {
  sensortok <- substr(x, 1, 1)
  yeartok <- substr(x, 2, 5)
  jdaytok <- substr(x, 6, 8)
  ## Note: Aquarius is *versioned* so we need some extra handling here
  ## or we'll just smash them all together (might be ok since the files have the version name)
  
  list(sensor = sensortok, year = yeartok, jday = jdaytok)
}
