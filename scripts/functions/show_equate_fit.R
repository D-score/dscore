show_equate_fit <- function(plot, equate_fit) {
  if (is.ggplot(plot)) return(annotate_item_fit(plot, itemfit = equate_fit))
  if (is.list(plot)) {
    for (i in 1:length(plot)) {
      the_name <- names(plot)[i]
      itemfit <- filter(equate_fit, equate == the_name)
      if (nrow(itemfit) == 1) plot[[i]] <- annotate_item_fit(plot[[i]], itemfit = itemfit)
    }
  }
  return(plot)
}
