plot_by_grp <- function(pass, by_name = "equate", 
                        model_name = "unspecified", ...) {
  # pre-allocate list of ggplots
  by_grp <- gtools::mixedsort(unique(pass$equate))
  if (any(is.na(by_grp))) by_grp <- by_grp[-which(is.na(by_grp))]  # remove misisng 
  plot_list <- vector("list", length(by_grp))
  names(plot_list) <- by_grp
  
  # loop over plots
  for (i in 1:length(plot_list)) {
    cat("Group: ", as.character(i), by_grp[i], "\n")
    plot_list[[i]] <- plot_d_one_grp(pass, by_name = by_name, 
                                     by_value = by_grp[i],
                                     i = i, 
                                     model_name = model_name, ...)
  }
  return(plot_list)
}
