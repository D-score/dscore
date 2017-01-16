plot_age_item <- function(pass, by_name = "item", 
                          model_name = "unspecified", ...) {
  # pre-allocate list of ggplots
  items <- gtools::mixedsort(unique(pass$item))
  plot_list <- vector("list", length(items))
  names(plot_list) <- items
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Item: ", as.character(i), items[i], "\n")
    plot_list[[i]] <- plot_age_one_item(pass, by_name = by_name, 
                                        by_value = items[i],
                                        i = i,
                                        model_name = model_name,
                                        ...)
  }
  
  return(plot_list)
}
