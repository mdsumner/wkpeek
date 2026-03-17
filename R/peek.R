#' peek - browse XYZ tiles in the RStudio Viewer via magick
#'
#' User provides an EPSG:3857 bounding box, peek picks an appropriate zoom,
#' fetches the tiles, composites them with magick, and displays the result.

# --- tile math (this is what grout does) ---

#' Web Mercator full extent
ORIGIN <- -20037508.342789244
TILE_SIZE <- 256L

#' Resolution (metres/pixel) at a given zoom level
tile_res <- function(zoom) {

  (-2 * ORIGIN) / (TILE_SIZE * 2^zoom)
}

#' Choose a zoom level that gives roughly the requested pixel width
#' for the given EPSG:3857 extent
pick_zoom <- function(xmin, xmax, target_width = 768, zmin = 19) {
  extent_m <- xmax - xmin
  # we want extent_m / res ~ target_width
  # res = full / (256 * 2^z)  =>  2^z = full * target_width / (256 * extent_m)
  full <- -2 * ORIGIN
  z <- log2(full * target_width / (TILE_SIZE * extent_m))
  # clamp to integer, and to sane range
  as.integer(min(max(round(z), 0), zmin))
}

#' Convert EPSG:3857 coordinate to tile index at given zoom
#' Note: y tile index is flipped (origin top-left in TMS-like XYZ)
xy_to_tile <- function(x, y, zoom) {
  n <- 2^zoom
  full <- -2 * ORIGIN
  res <- full / (TILE_SIZE * n)

  tx <- floor((x - ORIGIN) / (TILE_SIZE * res))
  # y axis: top of the world is ORIGIN * -1 (positive), tiles count down
  ty <- floor((-ORIGIN - y) / (TILE_SIZE * res))

  list(tx = as.integer(tx), ty = as.integer(ty))
}

#' Tile x/y back to EPSG:3857 origin (top-left corner of that tile)
tile_to_xy <- function(tx, ty, zoom) {
  n <- 2^zoom
  full <- -2 * ORIGIN
  res <- full / (TILE_SIZE * n)

  x <- ORIGIN + tx * TILE_SIZE * res
  y <- -ORIGIN - ty * TILE_SIZE * res

  list(x = x, y = y)
}

# --- tile fetching ---

#' Default tile URL template (Esri World Imagery)
ESRI_IMAGERY <- "https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"

#' OSM as an alternative
OSM_TILES <- "https://tile.openstreetmap.org/{z}/{y}/{x}.png"

#' Build a URL from template
tile_url <- function(tx, ty, zoom, template = ESRI_IMAGERY) {
  url <- gsub("{z}", zoom, template, fixed = TRUE)
  url <- gsub("{y}", ty, url, fixed = TRUE)
  url <- gsub("{x}", tx, url, fixed = TRUE)
  url
}

#' Fetch tiles to a cache directory, return file paths as a matrix
#' rows = y tiles (top to bottom), cols = x tiles (left to right)
fetch_tiles <- function(tx_range, ty_range, zoom,
                        template = ESRI_IMAGERY,
                        cache_dir = file.path(tempdir(), "peek_cache")) {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  txs <- seq(tx_range[1], tx_range[2])
  tys <- seq(ty_range[1], ty_range[2])

  urls <- character()
  paths <- character()

  for (ty in tys) {
    for (tx in txs) {
      url <- tile_url(tx, ty, zoom, template)
      # guess extension from template
      ext <- if (grepl("\\.png", template)) ".png" else ".jpg"
      path <- file.path(cache_dir, sprintf("z%d_%d_%d%s", zoom, tx, ty, ext))
      urls <- c(urls, url)
      paths <- c(paths, path)
    }
  }

  # only download tiles we don't already have
  need <- !file.exists(paths)
  if (any(need)) {
    message(sprintf("Fetching %d tiles ...", sum(need)))
    # simple sequential download; curl::multi_download would be better
    for (i in which(need)) {
      tryCatch(
        download.file(urls[i], paths[i], mode = "wb", quiet = TRUE),
        error = function(e) warning("Failed to fetch: ", urls[i])
      )
    }
  }

  # arrange as a matrix (rows = y, cols = x)
  matrix(paths, nrow = length(tys), ncol = length(txs), byrow = TRUE)
}

# --- compositing ---

#' Stitch a matrix of tile image paths into one magick image
composite_tiles <- function(path_matrix) {
  rows <- lapply(seq_len(nrow(path_matrix)), function(i) {
    imgs <- lapply(path_matrix[i, ], magick::image_read)
    magick::image_append(do.call(c, imgs), stack = FALSE)
  })
  magick::image_append(do.call(c, rows), stack = TRUE)
}

# --- main entry point ---

#' Browse a region of the world in the RStudio Viewer
#'
#' @param bbox numeric vector c(xmin, ymin, xmax, ymax) in EPSG:3857
#' @param width target pixel width of the composited image
#' @param template XYZ tile URL template
#' @return magick image (also displayed in Viewer)
#'
#' @examples
#' # Hobart area (roughly)
#' # In EPSG:3857:
#' hobart <- c(16348000, -5310000, 16360000, -5298000)
#' peek(hobart)
peek <- function(bbox, width = 768,
                 template = ESRI_IMAGERY, zmin = 19) {

  stopifnot(length(bbox) == 4)
  xmin <- bbox[1]; ymin <- bbox[2]; xmax <- bbox[3]; ymax <- bbox[4]

  zoom <- pick_zoom(xmin, xmax, target_width = width, zmin = zmin)
  message(sprintf("Using zoom level %d", zoom))

  # tile ranges
  tl <- xy_to_tile(xmin, ymax, zoom)  # top-left of bbox
  br <- xy_to_tile(xmax, ymin, zoom)  # bottom-right of bbox

  tx_range <- c(tl$tx, br$tx)
  ty_range <- c(tl$ty, br$ty)

  ntiles <- (diff(tx_range) + 1) * (diff(ty_range) + 1)
  message(sprintf("Tile grid: %d x %d = %d tiles",
                  diff(tx_range) + 1, diff(ty_range) + 1, ntiles))

  if (ntiles > 100) {
    stop("That's a lot of tiles (", ntiles, "). Narrow your bbox or reduce width.")
  }

  # fetch
  paths <- fetch_tiles(tx_range, ty_range, zoom, template)

  # composite
  img <- composite_tiles(paths)

  # crop to the actual bbox within the tile grid
  # the tile grid covers a slightly larger area than the requested bbox
  grid_tl <- tile_to_xy(tx_range[1], ty_range[1], zoom)
  res <- tile_res(zoom)

  # pixel offsets of the bbox within the composited image
  px_left   <- round((xmin - grid_tl$x) / res)
  px_top    <- round((grid_tl$y - ymax) / res)
  px_width  <- round((xmax - xmin) / res)
  px_height <- round((ymax - ymin) / res)

  # clamp to image dimensions
  info <- magick::image_info(img)
  px_left   <- max(0, px_left)
  px_top    <- max(0, px_top)
  px_width  <- min(px_width, info$width - px_left)
  px_height <- min(px_height, info$height - px_top)

  img <- magick::image_crop(img, sprintf("%dx%d+%d+%d",
                                         px_width, px_height,
                                         px_left, px_top))

  # display — print() on magick images triggers the Viewer in RStudio
  #print(img)

  invisible(img)
}

# --- simple navigation helpers ---

#' State: a mutable environment holding the current view
.peek_state <- new.env(parent = emptyenv())

#' Initialise the viewer with a bbox
peek_at <- function(bbox, width = 768, template = ESRI_IMAGERY) {
  .peek_state$bbox <- bbox
  .peek_state$width <- width
  .peek_state$template <- template
  peek(bbox, width, template)
}

#' Pan by a fraction of the current extent (e.g. 0.5 = half-screen)
pan <- function(dx = 0, dy = 0) {
  b <- .peek_state$bbox
  ex <- b[3] - b[1]
  ey <- b[4] - b[2]
  b[1] <- b[1] + dx * ex
  b[3] <- b[3] + dx * ex
  b[2] <- b[2] + dy * ey
  b[4] <- b[4] + dy * ey
  .peek_state$bbox <- b
  peek(b, .peek_state$width, .peek_state$template)
}

#' Zoom in/out by a factor (< 1 = zoom in, > 1 = zoom out)
zoom <- function(factor = 0.5) {
  b <- .peek_state$bbox
  cx <- (b[1] + b[3]) / 2
  cy <- (b[2] + b[4]) / 2
  hx <- (b[3] - b[1]) / 2 * factor
  hy <- (b[4] - b[2]) / 2 * factor
  b <- c(cx - hx, cy - hy, cx + hx, cy + hy)
  .peek_state$bbox <- b
  peek(b, .peek_state$width, .peek_state$template)
}

# convenience
left  <- function(amount = 0.5) pan(dx = -amount)
right <- function(amount = 0.5) pan(dx =  amount)
up    <- function(amount = 0.5) pan(dy =  amount)
down  <- function(amount = 0.5) pan(dy = -amount)
zoom_in  <- function(factor = 0.5) zoom(factor)
zoom_out <- function(factor = 2)   zoom(factor)

