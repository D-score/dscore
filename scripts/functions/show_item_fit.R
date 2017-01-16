show_item_fit <- function(plot, item_fit) {
  if (is.ggplot(plot)) return(annotate_item_fit(plot, itemfit = item_fit))
  if (is.list(plot)) {
    for (i in 1:length(plot)) {
      the_name <- names(plot)[i]
      itemfit <- item_fit[item_fit$item == the_name, , drop = FALSE]
      if (nrow(itemfit) == 1) plot[[i]] <- annotate_item_fit(plot[[i]], itemfit = itemfit)
    }
  }
  return(plot)
}
