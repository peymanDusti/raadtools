.ssbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <GeoTransform>0.0, 1855.325, 0.0000000000000000e+000, 15987768, 0.0000000000000000e+000,-1850.436</GeoTransform>
  <SRS>PROJCS["unnamed",
  GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1" subClass="VRTRawRasterBand">
  <Metadata />
  <SourceFilename relativeToVRT="1">ssbinfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <ImageOffset>0</ImageOffset>
  <PixelOffset>2</PixelOffset>
  <LineOffset>43200</LineOffset>
  <ByteOrder>MSB</ByteOrder>
  </VRTRasterBand>
  </VRTDataset>'
}

.ssatlanticbintext <- function() {
  '<VRTDataset rasterXSize="21600" rasterYSize="17280">
  <SRS>PROJCS["unnamed",
    GEOGCS["Normal Sphere (r=6370997)",
  DATUM["unknown",
  SPHEROID["sphere",6370997,0]],
  PRIMEM["Greenwich",0],
  UNIT["degree",0.0174532925199433]],
  PROJECTION["Mercator_1SP"],
  PARAMETER["central_meridian",0],
  PARAMETER["scale_factor",1],
  PARAMETER["false_easting",0],
  PARAMETER["false_northing",0]]</SRS>
  <GeoTransform>-20037510.000, 1.8553250000000000e+003, 0.0000000000000000e+000, 1.5987768000000000e+007, 0.0000000000000000e+000,-1.8504359999999999e+003</GeoTransform>
  <Metadata />
  <VRTRasterBand dataType="Int16" band="1">
  <Metadata />
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  <SimpleSource>
  <SourceFilename relativeToVRT="1">ssvrtfile</SourceFilename>
  <SourceBand>1</SourceBand>
  <SourceProperties RasterXSize="21600" RasterYSize="17280" DataType="Int16" BlockXSize="21600" BlockYSize="1" />
  <SrcRect xOff="0" yOff="0" xSize="10800" ySize="17280" />
  <DstRect xOff="10800" yOff="0" xSize="10800" ySize="17280" />
  </SimpleSource>
  </VRTRasterBand>
  </VRTDataset>'
}


.smithsandwellraw <- function(candidatefiles) {
  tail(sort( grep("/topo_.*\\.img$", candidatefiles, value = TRUE)), 1)
}

.smithsandwellvrt <- function(lon180 = FALSE) {
  all_files <- allfiles() %>% mutate(fullname = file.path(root, file)) %>% dplyr::pull(fullname)
  binfile <- .smithsandwellraw(all_files)
  vrtfile1 <- file.path(dirname(binfile), ".vrt", gsub(".img$", ".vrt", basename(binfile)))
  vrtfile2 <- file.path(dirname(binfile), ".vrt",gsub(".img$", "_atlantic.vrt", basename(binfile)))

  vrtext1 <- gsub("ssbinfile", file.path("..", basename(binfile)), .ssbintext())
  vrtext2 <- gsub("ssvrtfile", basename(vrtfile1), .ssatlanticbintext())

  if (!file.exists(vrtfile1)) writeLines(vrtext1, vrtfile1)
  if (!file.exists(vrtfile2)) writeLines(vrtext2, vrtfile2)
  if (lon180) vrtfile2 else vrtfile1
}

#' Functions to provide topographic (bathymetry and/or topography) data.
#'
#' Use \code{readtopo} (or its alias \code{readbathy}) to read data
#' from the chosen data set. The function \code{topofile} is used to
#' find the full file name.
#' \code{xylim} is expected to be consistent with \code{lon180}
#' The following data sets are available using the argument \code{topo}.
#' \describe{
#' \item{gebco_08}{The GEBCO_08 Grid, a global 30 arc-second grid largely generated by combining quality-controlled ship depth soundings with interpolation between sounding points guided by satellite-derived gravity data. \url{http://www.gebco.net/data_and_products/gridded_bathymetry_data/}}
#' \item{ibcso}{IBCSO bathymetry data, resolution 1min, use argument \code{polar = TRUE} to return the projected version (polar stereographic with true scale at 71S, WGS84), 500m resolution. \url{http://www.ibcso.org/data.html}. Default is Ice Surface 'ibcso_is', use 'ibcso_bed' for Bedrock.}
#' \item{etopo1}{ETOPO1 is a 1 arc-minute global relief model of Earth's surface that integrates land topography and ocean bathymetry. \url{http://www.ngdc.noaa.gov/mgg/global/global.html}}
#' \item{etopo2}{Historic and deprecated prior version of ETOPO1. \url{http://www.ngdc.noaa.gov/mgg/global/etopo2.html}}
#' \item{kerguelen}{Kerguelen Plateau Bathymetric Grid, GeoScience Australia}
#' \item{george_v_terre_adelie}{A bathymetric Digital Elevation Model (DEM) of the George V and Terre Adelie continental shelf and margin - 100, 250, and 500 metre resolution. \url{http://data.aad.gov.au/aadc/metadata/metadata_redirect.cfm?md=AMD/AU/GVdem_2008}}
#' \item{smith_sandwell}{Global seafloor topography from satellite altimetry and ship depth soundings. \url{http://topex.ucsd.edu/WWW_html/mar_topo.html}}
#' \item{cryosat2}{Antarctica CryoSat-2 Digital Elevation Model (DEM). \url{https://earth.esa.int/web/guest/missions/esa-operational-eo-missions/cryosat}}
#' \item{lake_superior}{Bathymetry of Lake Superior \url{https://www.ngdc.noaa.gov/mgg/greatlakes/superior.html}}
#' \item{ramp}{Radarsat Antarctic digital elevation model V2 \url{https://github.com/AustralianAntarcticDivision/blueant#radarsat-antarctic-digital-elevation-model-v2}}
#' \item{ga_srtm}{Digital Elevation Model (DEM) of Australia with 1 Second Grid}
#' \item{rema_1km, rema_200m, rema_100m, rema_8m}{Reference Elevation Model of Antartica (REMA) for the peninsula, see `read_rema_tiles` for the index}
#' \item{gebco_19}{GEBCO_2019 Grid https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}
#' }
#' @title Topography data
#' @name readtopo
#' @aliases readtopo topofile readbathy read_rema_tiles
#' @param topo Data source, see Details.
#' @param lon180 Flag for returning data in Atlantic [-180, 180] rather than Pacific [0, 360] view.
#' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
#' @param polar Flag for returning the polar version of the IBCSO data.
#' @param returnfiles Return just the relevant file name
#' @param ... reserved for future use, ignored currently
#' @return
#' \describe{
#' \item{}{\code{topofile} returns a character string of the full path to a file name}
#' \item{}{\code{readtopo} and \code{readbathy} return the requested data as a RasterLayer (these are aliases)}
#' }
#' @export
readtopo <- function(topo = c("gebco_08", "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell", "gebco_14",
                              "macrie1100m", "macrie2100m",
                              "cryosat2",
                              "lake_superior",
                              "ramp", "ibcso_is", "ibcso_bed",
                              "ga_srtm",
                              "rema_1km",
                              "rema_200m",
                              "rema_100m",
                              "rema_8m",
                              "srtm", "gebco_19"),
                     polar = FALSE,
                     lon180 = TRUE,
                     xylim = NULL,
                     returnfiles = FALSE,
                     ...) {
  topo <- match.arg(topo)

  if (!lon180 & topo %in% c("geboc_08", "ibcso", "etopo2")) {
    tfile <- topofile(topo = topo, polar = FALSE, ...)
    if (returnfiles) return(tfile)
    if (is.null(xylim)) res <- .rotate(raster(tfile))
  } else {
    tfile <- topofile(topo = topo, polar = polar, lon180 = lon180, ...)
    if (returnfiles) return(tfile)
    res <- raster(tfile)
  }

  ## sources with missing CRS metadata
  llprojs <- c("etopo2", "kerguelen", "george_v_terre_adelie", "lake_superior")
  if (topo %in% llprojs)  projection(res) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  ## self-describing, if you always work in lon-lat ...
  if (topo == "ibcso_bed") projection(res) <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

  if (topo == "cryosat2") projection(res) <- "+proj=stere +lat_0=-90 +lat_ts=-71 +datum=WGS84 +x_0=0 +y_0=0"

  if (!is.null(xylim) && topo == "rema_8m") {
    stop("'xylim' is not supported for rema_8m, discuss with local experts for other options (there are many)")
  }
  if (!is.null(xylim)) res <- crop(res, xylim)
  res
}


##' @rdname readtopo
##' @export
readbathy <- readtopo



##' @rdname readtopo
##' @export
topofile <- function(topo = c("gebco_08",  "ibcso",
                              "etopo1", "etopo2",
                              "kerguelen", "george_v_terre_adelie",
                              "smith_sandwell", "gebco_14", "macrie1100m", "macrie2100m", "cryosat2",
                              "lake_superior",
                              "ramp", "ibcso_is", "ibcso_bed",
                              "ga_srtm",
                              "rema_1km",
                              "rema_200m",
                              "rema_100m",
                              "rema_8m",
                              "srtm", "gebco_19"),
                     polar = FALSE,
                     lon180 = TRUE,
                     ...) {
  topo <- match.arg(topo)
  if (topo == "ibcso") topo <- "ibcso_is" ## ??

  if (topo == "rema_8m") {
    r8m_files <- raadfiles::rema_8m_files()
    warning(sprintf("rema_8m is a very large **virtual** raster consisting of many (%i) files on disk,\n beware of making subsets that will pull a lot of data into memory", nrow(r8m_files)))
  }
  allf <- allfiles()
  if (topo == "smith_sandwell") {
    topopath <- if (lon180) raadfiles::smith_sandwell_lon180_files()$fullname else raadfiles::smith_sandwell_files()$fullname
  } else {

   topopath <-  switch(topo,
           gebco_08 = raadfiles::gebco08_files()$fullname,
           gebco_14 = raadfiles::gebco14_files()$fullname,
           gebco_19 = raadfiles::gebco19_files()$fullname,
           ibcso_is = dplyr::filter(raadfiles::ibcso_files(all = TRUE),
                                    grepl("_is.*tif$", basename(.data$fullname)))$fullname,
           ibcso_bed = dplyr::filter(raadfiles::ibcso_files(all = TRUE),
                                    grepl("_bed.*grd$", basename(.data$fullname)))$fullname,
           etopo1 = raadfiles::etopo1_files()$fullname,
           etopo2 = raadfiles::etopo2_files()$fullname,
           kerguelen = raadfiles::kerguelen_files()$fullname,
           george_v_terre_adelie = raadfiles::george_v_terre_adelie_250m_files()$fullname,
           macrie1100m = raadfiles::macquarie100m_57S_files()$fullname,
           macrie2100m = raadfiles::macquarie100m_58S_files()$fullname,
           cryosat2 = raadfiles::cryosat2_files()$fullname,
           lake_superior = raadfiles::lakesuperior_files()$fullname,
           ramp = raadfiles::ramp_files()$fullname,
           ## FIXME
           ga_srtm = grep("1sec-srtm/a05f7893-0050-7506-e044-00144fdd4fa6", allf$fullname, value = TRUE),
           rema_100m = raadfiles::rema_100m_files()$fullname[1L],
           rema_200m = raadfiles::rema_200m_files()$fullname[1L],
           rema_1km = raadfiles::rema_1km_files()$fullname[1L],
           rema_8m =   file.path(dirname(dirname(r8m_files$fullname[1])), "rema_mosaic_8m_dem.vrt"),
           srtm = local({
             files <- raadfiles::srtm_files()
             ## this will only work for nectar machines ... FIXME
             file.path(dirname(dirname(files$fullname[1])), "srtm_4.1.vrt")

           })


           )
  }
  if (length(topopath) < 1) stop(sprintf("cannot find %s", topo))
  if (!file.exists(topopath)) warning(sprintf("cannot file %s", topopath))
  topopath[1L]
}


#' @export
#' @name readtopo
read_rema_tiles <- function(...) {
  raster::shapefile(raadfiles::rema_tile_files(all = FALSE)$fullname[1])
}

