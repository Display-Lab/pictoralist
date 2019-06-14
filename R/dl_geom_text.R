dl_geom_text <-
  function(mapping = NULL, data = NULL, stat = "identity",  position = "identity",
           ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE,
           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  args_list <- list(...)
  args_list["color"] <- PT$DL_BLUE
  args_list["mapping"] <- mapping
  args_list["data"] <- data
  args_list["nudge_x"] <- nudge_x
  args_list["nudge_y"] <- nudge_y
  args_list["show.legend"] <- show.legend
  args_list["family"] <- PT$DL_FONT

  do.call(geom_text, args = args_list)
}
