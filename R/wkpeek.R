
#' Title
#'
#' @param x
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
wk_peek <- function(x, ...) {
  crs <- wk::wk_crs(x)

  if (!is.null(crs) && !is.na(crs)) {
    if (!requireNamespace("PROJ", quietly = TRUE)) {
      stop("PROJ is required to transform between known crs")
    }
    ## transform to the canvas CRS
    trans <- PROJ::proj_trans_create(crs, "EPSG:3857")
    x <- wk::wk_transform(x, trans)
  }
  bb <- as.numeric(wk::wk_bbox(x))
  xmin <- bb[1]; xmax <- bb[3]
  ymin <- bb[2]; ymax <- bb[4]

  imagecanvas <- peek(bb)

  ## we need the size of the canvas
  info <- magick::image_info(imagecanvas)

  ## transform to the canvas (0,ncol 0,nrow)
  trans <- wk::wk_affine_compose(
    wk::wk_affine_translate(dx = -xmin, dy = -ymax),
    wk::wk_affine_scale(
      scale_x = info$width / (xmax - xmin),
      scale_y = -info$height / (ymax - ymin)  # negative = y-flip
    )
  )


  geom_px <- wk::wk_transform(x, trans)
  dev <- magick::image_draw(imagecanvas)
  plot(geom_px, add = TRUE, ...)
  dev.off()
  dev
}
