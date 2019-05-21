dl_annotate <- function(geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL,
                        ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ...,
                        na.rm = FALSE) {
  args_list <- list(...)
  args_list["color"] <- PT$DL_BLUE
  args_list["geom"] <- geom
  args_list["x"] <- x
  args_list["y"] <- list(y)
  args_list["family"] <- PT$DL_FONT

  do.call(annotate, args = args_list)
}
