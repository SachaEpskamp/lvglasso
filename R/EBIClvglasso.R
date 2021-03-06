
# Computes optimal glasso network based on EBIC:
EBIClvglasso <- function(
  S, # Sample cov
  n, # Sample size
  nLatents, # Number of latents
  gamma = 0.5, # EBIC parameter
  lambda, 
  ... # lvglasso arguments
){
  if (missing(lambda)) lambda <- NULL
  
  # If nLatents is vector, do this function for every latent:
  if (length(nLatents) > 1){
    Resses <- lapply(nLatents,function(nl)EBIClvglasso(S, n, nl, gamma, lambda, ...))
    opt <- which.max(sapply(Resses,'[[','ebic'))
    return(Resses[[opt]])
  }
  
  rho.max = max(max(S - diag(nrow(S))), -min(S - diag(nrow(S))))
  rho.min = rho.max/100
  rho = exp(seq(log(rho.min), log(rho.max), length = 100))
  
  lvglas_res <- lapply(rho, function(r)lvglasso(S, nLatents, r,lambda =  lambda, ...))
  
  # Likelihoods:
  EBICs <- sapply(lvglas_res,function(res){
    C <- solve(res$w[res$observed,res$observed])
    qgraph:::EBIC(S, C, n, gamma, E = sum(res$wi[lower.tri(res$wi, diag = TRUE)] != 0))
  })
  
  # Smalles EBIC:
  opt <- which.min(EBICs)
  
  Res <- lvglas_res[[opt]]
  Res$rho <- rho[opt]
  Res$ebic <- EBICs[opt]
  
  # Return 
  return(Res)
}


