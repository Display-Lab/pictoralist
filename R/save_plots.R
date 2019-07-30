#' @title Save Plots
#' @description Save the ggplots to given directory
#' @param figures List of ggplots
#' @param save_path Path to directory given to save the list of ggplots
#' @export
save_plots<- function(figures, save_path, performer_id) {
  # Save figures to current working directory if no path is provided
  if(is.null(save_path) || is.na(save_path)) {
    save_path = getwd()
  }
  # format: performer_id-template_name.pdf
  mapply(ggsave,
         filename=paste(performer_id,"-",names(figures),".png", sep=""),
         path=save_path,
         width=PT$DL_WIDTH,
         height=PT$DL_HEIGHT,
         plot=figures)
}
