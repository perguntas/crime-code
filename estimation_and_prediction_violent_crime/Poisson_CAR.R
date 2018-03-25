require(spdep)
require(matrixcalc)
require(MASS)

CAR_poisson = function(formula, data, listw, sigma_start=NULL, verbose=NULL){
  
  # setting up the clock
  timing = vector(length = 2)
  timing[1] = Sys.time()
  
  if (is.null(verbose)) verbose = F
  
  # get indices: data.table friendly subsetting
  ind = data[[1]]
  tind = data[[2]]
  rm(data)
  
  
  # match arguments
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf), 0)
  
  # get coefs + model defs
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  mf[[1]] = as.name("model.frame")
  mf = eval(mf, parent.frame())
  mt = attr(mf, "terms")
  

  # extract variables
  Y = model.extract(mf, "response")
  X = model.matrix(mt, mf)
  
  # reorder X and Y for random effects estimation
  # X = X[order(tind),]
  # Y = Y[order(tind)]
  
  if (any(is.na(Y))) stop("NAs in dependent variable")
  if (any(is.na(X))) stop("NAs in independent variable")
  n = nrow(X)
  
  # check for aliasing
  lm.base = lm(Y ~ X - 1)
  aliased = is.na(coefficients(lm.base))
  cn = names(aliased)
  names(aliased) = substr(cn, 2, nchar(cn))
  if (any(aliased)) {
    nacoef = which(aliased)
    X = X[,-nacoef]
    warning("there are aliased coefficients in the model")
  }
  
  
  # check panel dimensions
  if (n != length(listw$neighbours) * length(unique(tind)))
    stop("Input data and neighbourhood list have different dimensions")
  
  
  # Sanity checks
  if (!inherits(listw, "listw")) 
    stop("No neighbourhood list")
  if (listw$style != "B")
    stop("Binary spatial weights matrix required")
  
  # extract weights matrix
  Q = listw2mat(listw)
  
  # set neighbours from 1 to -1
  Q = -1 * Q
  # set diagonal equal to sum of neighbours
  diag(Q) = rowSums(apply(Q, 1, function(x) x < 0))

  # solve Q inverse
  q_inv = ginv_adapt(Q)

  Z = do.call(rbind, replicate(n = length(unique(tind)), Diagonal(length(unique(ind))), simplify = F))

  # set dimensions
  k = dim(X)[[2]]
  q = dim(Z)[[2]]
  
  # estimate beta, eta, sigma
  est = LL_Poisson(X, Y, Q, q_inv, Z, dim2 = length(unique(tind)), k, n, q, sigma_start = sigma_start)

  # extract results
  beta = est$coef[1:k]
  names(beta) = colnames(X)
  eta = est$coef[(k+1):(k+q)]
  sigma = est$sigma
  var_sigma = est$var_sigma

  # reorder X back
  # X = X[order(ind[order(tind)]),]
  # Y = Y[order(ind[order(tind)])]

  # calculate yhat
  signal_trend = drop(X %*% beta)
  signal_stochastic = rep(eta, times = length(unique(tind)))

  fitted = exp(signal_trend + signal_stochastic)

  # ensure numeric consistency
  #fitted = round(fitted, digits = 0)
  W = Diagonal(x = drop(fitted))

  resid = drop(Y - fitted)
  
  # calculate standard errors
  covmat = ginv_adapt((t(X) %*% W %*% X))
  se = sqrt(diag(covmat))

  # calculate standard errors
  SSE = sum(resid^2)
  df = (n - length(beta) - length(eta) - 1)

  tval = beta / se
  pval = 2 * pt(abs(tval), df, lower.tail = F)

  SE = cbind(beta, se, tval, pval)
  dimnames(SE) = list(names(beta),
                       c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

  timing[2] = Sys.time()

  cat(paste0("PCAR took ", round((timing[2] - timing[1])/60, digits = 0), " minutes"))

  return(list(coefficients=beta, fitted=fitted, residuals=resid, sigma2=sigma, signal_trend=signal_trend,
              signal_stochastic=signal_stochastic, SE=SE, var_sigma=var_sigma, eta=eta))
}


LL_Poisson = function(X, Y, Q, q_inv, Z, dim2, k, n, q, sigma_start=NULL){
  # based on initial params, calculate y*
  # solve for beta and eta
  # update sigma

  maxiter = 100

  # set inital parameters
  coefs = vector(length = (k+q))

  # set initial beta
  coefs[1:k] = (glm(Y ~ X - 1, family = "poisson"))$coefficients

  # set initial eta
  coefs[(k+1):(k+q)] = rep(0.5, q)

  # set initial sigma
  if (is.null(sigma_start)) sigma = 30 else
    sigma = sigma_start

  i = 0
  iter = 0

  repeat {
    # count full iterations
    i = i + 1

    cat("\nStarting iteration ", i, "....\n")

    if (i == 1) {
      infl = 3
    }

    # save old coefficients
    sigma_old = sigma
    coefs_old = coefs

    repeat {
      # count internal iterations
      iter = iter + 1
      cat("\nInternal Iteration ", iter, "\n")
      # calculate conditional mean lambda
      lambda = as.vector(exp(X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)))

      # calculate deviance
      dev_old = sum(abs(Y - lambda))

      # calculate working weights y*
      ystar = log(lambda) + (Y - lambda)/lambda

      # calculate iterative weights
      W = Diagonal(x = lambda)

      # assign block matrix H
      block = matrix(0, nrow = (k+q), ncol = (k+q))
      block = as(block, "dgeMatrix")

      block[1:k, 1:k] = crossprod(X, W %*% X)
      block[(k+1):(k+q), 1:k] = crossprod(Z, W %*% X)
      block[1:k, (k+1):(k+q)] = crossprod(X, W %*% Z)
      block[(k+1):(k+q), (k+1):(k+q)] = crossprod(Z, W %*% Z) + ((1/sigma^2) * Q)

      # invert H
      block_inv = ginv_adapt(block)

      # solve for coefficients beta and eta
      coefs = solve(block, rbind(crossprod(X, W %*% ystar), crossprod(Z, W %*% ystar)))

      # calculate derivate dVar / dsigma
      D = (2 * sigma * q_inv)

      # pre-calculate trace matrices (needed again later)
      trace_part1 = (block[(k+1):(k+q), (k+1):(k+q)] - ((1/sigma^2) * Q)) %*% D
      trace_part2 = crossprod(Z, W %*% cbind(X, Z) %*% block_inv %*% crossprod(cbind(X, Z), W %*% Z %*% D))

      # working residual vector
      resid = ystar - X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)

      # calculate sigma score
      score = -0.5 * sum(diag(trace_part1)) + 0.5 * sum(diag(trace_part2)) +
        0.5 * (t(resid) %*% W %*% Z %*% D %*% t(Z) %*% W %*% resid)

      # calculate fisher information
      fisher = 0.5 * sum(diag(trace_part1 %*% trace_part1)) - sum(diag(trace_part2 %*% trace_part1)) +
        0.5 * sum(diag(trace_part2 %*% trace_part2))

      # update sigma
      sigma = drop(sigma + infl * (1/fisher) * score)

      # calculate conditional mean lambda
      lambda = as.vector(exp(X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)))

      # calculate deviance
      dev = sum(abs(Y - lambda))
      
      # control convergence
      if (infl > 1 & iter < 10) infl = infl - 1
      
      if (iter > 10 & infl < 2 & infl > 0.03) infl = 0.5 * infl
      if (abs(sigma - sigma_old) < 0.5) infl = 0.5 * infl
      
      cat("\nStep size in iteration ", i, ": ", infl, "\n")

      # stop if step was successful
      if (dev <= dev_old) break
      
      # increase infl again if not successful
      if (infl > 1 & iter < 10) infl = infl + 1

      cat("\nStep size: ", infl, "\n")

      # end repeat
    }

    # print output
    cat("\n", i, "sigma2_old: ", sigma_old^2,
        "new sigma2: ", sigma^2, " improves deviance from ", dev_old, " to ", dev, "\n")
    cat("\nBeta Coefficients changed by ", sum(abs(coefs[2:k] - coefs_old[2:k])), "\n")

    # check for convergence
    if (all(abs(sigma - sigma_old) < 0.5 & abs(coefs[2:k] - coefs_old[2:k]) < 0.005)) break

    # stop at maximum iterations
    if (i > maxiter) break
  
  }

  cat(paste("\nConvergence took ", i, "iterations\n"))
  var_sigma = 1/fisher
  return(list(sigma = sigma^2, coefs = coefs, var_sigma = var_sigma))

}

ginv_adapt = function(x, tol = sqrt(.Machine$double.eps), sparse=F){
  Xsvd = svd(x)
  Positive = Xsvd$d > max(tol * Xsvd$d[1L], 0)
  if (all(Positive)) 
    k = Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
  else if (!any(Positive)) 
    k = array(0, dim(X)[2L:1L])
  else k = Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
                                             t(Xsvd$u[, Positive, drop = FALSE]))
  
  if (sparse) return(as(k, "dgCMatrix")) else return(as(k, "dgeMatrix"))
  
}


predict_Poisson = function(object, newdata){
  
  # check whether newdata has necessary columns
  if (grepl("Intercept", names(object$coefficients[1]))){
    stopifnot(names(object$coefficients[-1]) %in% names(newdata))
  } else {
    stopifnot(names(object$coefficients) %in% names(newdata))
  }

  vars = paste(names(object$coefficients)[-1], collapse = " + ")
  Terms = delete.response(terms(as.formula(paste("occ ~ ", paste0(vars)))))
  
  m = model.frame(Terms, newdata) 
  X = model.matrix(Terms, m)
  
  coef = object$coefficients
  random = object$eta
  
  predictor = exp(X %*% coef + rep(random, times = nrow(newdata)/length(random) ))
  predictor
}



# LL_Poisson = function(X, Y, Q, q_inv, Z, dim2, k, n, q, sigma_start=NULL){
#   # based on initial params, calculate y*
#   # solve for beta and eta
#   # update sigma
#   
#   
#   # set inital parameters
#   coefs = vector(length = (k+q))
#   
#   # set initial beta
#   coefs[1:k] = (glm(Y ~ X - 1, family = "poisson"))$coefficients
#   
#   # set initial eta
#   coefs[(k+1):(k+q)] = rep(5, q)
#   
#   # make grid search instead
#   sigmas = seq(from = 1, to = 3, by = 0.1)
#   sigmas_out = vector(length = length(sigmas))
#   devs = vector(length = length(sigmas))
#   
#   # start grid search
#   for (i in 1:length(sigmas)){
#     # calculate conditional mean lambda
#     lambda = as.vector(exp(X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)))
#     
#     # calculate working weights y*
#     ystar = log(lambda) + (Y - lambda)/lambda  
#     
#     # calculate iterative weights
#     W = Diagonal(x = lambda)
#     
#     # assign block matrix H
#     block = matrix(0, nrow = (k+q), ncol = (k+q))
#     block = as(block, "dgeMatrix")
#     
#     block[1:k, 1:k] = crossprod(X, W %*% X)
#     block[(k+1):(k+q), 1:k] = crossprod(Z, W %*% X)
#     block[1:k, (k+1):(k+q)] = crossprod(X, W %*% Z)
#     block[(k+1):(k+q), (k+1):(k+q)] = crossprod(Z, W %*% Z) + ((1/sigmas[i]^2) * Q)
#     
#     # invert H
#     block_inv = ginv_adapt(block)
#     
#     # solve for coefficients beta and eta
#     coefs = solve(block, rbind(crossprod(X, W %*% ystar), crossprod(Z, W %*% ystar)))
#     
#     # calculate derivate dVar / dsigma
#     D = (2 * sigmas[i] * q_inv)
#     
#     # pre-calculate trace matrices (needed again later)
#     trace_part1 = (block[(k+1):(k+q), (k+1):(k+q)] - ((1/sigmas[i]^2) * Q)) %*% D
#     trace_part2 = crossprod(Z, W %*% cbind(X, Z) %*% block_inv %*% crossprod(cbind(X, Z), W %*% Z %*% D))
#     
#     # working residual vector
#     resid = ystar - X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)                      
#     
#     # calculate sigma score
#     score = -0.5 * sum(diag(trace_part1)) + 0.5 * sum(diag(trace_part2)) + 
#       0.5 * (t(resid) %*% W %*% Z %*% D %*% t(Z) %*% W %*% resid)
#     
#     # calculate fisher information
#     fisher = 0.5 * sum(diag(trace_part1 %*% trace_part1)) - sum(diag(trace_part2 %*% trace_part1)) +
#       0.5 * sum(diag(trace_part2 %*% trace_part2))
#     
#     # update sigma
#     sigmas_out[i] = drop(sigmas[i] + (1/fisher) * score)
#     
#     # calculate conditional mean lambda
#     lambda = as.vector(exp(X %*% coefs[1:k] + rep(coefs[(k+1):(k+q)], dim2)))
#     
#     # calculate deviance
#     devs[i] = sum(abs(Y - lambda))
#   }
#   return(list(sigmas, sigmas_out, devs))  
#   
# }



  