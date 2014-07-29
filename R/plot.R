# Plotting function:
plot.lvglasso <- function(
  object, # lvglasso object
  plot = c("S","L"), # "S" will plot the sparse network between items and "L" the latent loadings
  ask,
  ...
  ){
  if (missing(ask)) ask <- length(plot) > 1
  parOrig <- par()
  par(ask = ask)
  obs <- object$observed
  pcor <- object$pcor
  Res <- list()
  
  if ("S" %in% plot){
    Res$S <- qgraph(pcor[obs,obs], ..., title = "Sparse structure", shape = "square", layout = "spring")
  }
  
  if ("L" %in% plot){
    fCors <- as.matrix(pcor[!obs,!obs])
    load <- as.matrix(pcor[obs,!obs])
    
    rownames(load) <- colnames(object$wi)[obs]
    
    Res$L <- qgraph.loadings(load, factorCors = fCors, arrows = FALSE,..., title = "Low-rank structure", labels =  colnames(object$wi)[obs])
  }
  invisible(Res)
}
