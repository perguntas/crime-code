require(spdep)
require(matrixcalc)
require(MASS)

CARlm = function(formula, data, listw, family="CAR", method="eigen", verbose=NULL, 
                  tol.solve=.Machine$double.eps, llprof=NULL){

  if (is.null(verbose)) verbose = F
  
  # match arguments
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf), 0)
  
  # get coefs + model defs
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  mf[[1]] = as.name("model.frame")
  mf = eval(mf, parent.frame())
  mt = attr(mf, "terms")
  
  # data.frame and data.table friendly subsetting
  ind = unique(data[[1]])
  tind = data[[2]]
  rm(data)
  
  # extract variables
  Y = model.extract(mf, "response")
  X = model.matrix(mt, mf)
  rm(mf)

  if (any(is.na(Y))) stop("NAs in dependent variable")
  if (any(is.na(X))) stop("NAs in independent variable")
  n = nrow(X)
  
  # check panel dimensions
  if (n != length(listw$neighbours) * length(unique(tind)))
    stop("Input data and neighbourhood list have different dimensions")

  lm.base = lm(Y ~ X - 1)
  aliased = is.na(coefficients(lm.base))
  cn = names(aliased)
  names(aliased) = substr(cn, 2, nchar(cn))
  if (any(aliased)) {
    nacoef = which(aliased)
    X = X[,-nacoef]
    warning("there are aliased coefficients in the model")
  }
  rm(lm.base)
  
  
  # Sanity checks
  if (!inherits(listw, "listw")) 
    stop("No neighbourhood list")
  if (listw$style != "B")
    stop("Binary spatial weights matrix required")
  
  # extract weights matrix
  W = listw2mat(listw)
  if ( !isTRUE(all.equal(W, t(W), check.attributes=F)) )
    warning("Non-symmetric spatial weights in CAR model")
  if ( is.singular.matrix(as.matrix(W)) ) 
    stop("Spatial Weights matrix is singular")
  
  # create blockdiagonal weight matrix
  W_exp = bdiag(replicate(n = length(unique(tind)), W, simplify=F))
  
  
  # create identity matrix
  I = Diagonal(n)
  
  # put everything into one list
  data_list = list(vars=X, resp=Y, time_index=tind, Imat=I, #Wmat=W, 
                    n=n, Wmat_exp=W_exp)
  
  if (verbose) cat(paste("\nJacobian calculated using W eigenvalues\n"))

  eig = eigen(W, symmetric = T, only.value = T)$value
  eig_range = 1/range(eig)
  
  interval = eig_range
  rm(W)
  
  if (verbose) cat("\nLambda interval: ", interval, "\n")

  opt = optimize(LL_fit, interval=interval, maximum=T,
                  data_list=data_list, eigenv=eig, verbose=verbose)
  
  lambda = opt$maximum
  if (abs(lambda - interval[1]) < 0.01 | abs(lambda - interval[2]) < 0.01)
    warning("lambda on interval bounds")

  names(lambda) = "lambda"
  LL = opt$objective
  
  # get GLS coefficients
  fit = CAR_fit(lambda=lambda, data_list=data_list, out=TRUE)
  rm(data_list)
  
  coefficients = fit$coefficients
  SSE = fit$SSE
  
  # create residuals and fitted values (Cressie 1993, p. 564)
  signal_trend = drop(X %*% coefficients)

  signal_stochastic = lambda * W_exp %*% (Y - signal_trend)
  fitted.values = signal_trend + signal_stochastic
  residuals = drop(Y - fitted.values)
  
  # calculate standard errors
  SSE = SSE
  df = (n - length(coefficients) - 2)
  variance = SSE/df # SSE/degrees of freedom
  
  IlWX = (I - lambda * W_exp) %*% X
  covmat = t(X) %*% IlWX
  
  # (X'(I-C)^1x)^1
  covmat_inv = solve(covmat) 
  covmat_inv = nearPD(covmat_inv)
  
  # standard error
  se = sqrt(diag(covmat_inv$mat) * variance) 
  
  tval = coefficients/se # t-value
  pval = 2 * pt(abs(tval), df, lower.tail = F)
  
  SE = cbind(fit$coefficients, se, tval, pval)
  dimnames(SE) = list(names(coefficients), 
                      c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))

  GC = gc()
  res = list(coefficients=coefficients, SSE=SSE, SE=SE, lambda=lambda, LL=LL, call=match.call(),
              parameters=(ncol(X)+2), aliased=aliased, method=method, family=family,
              interval=interval, fitted.values=fitted.values, 
              signal_trend=signal_trend, signal_stochastic=signal_stochastic,
              residuals=residuals,
              lambda.se=NULL, X=X, Y=Y,terms=mt)

  res
}



# the func
CAR_fit = function(lambda, data_list, out=F){
  X = data_list$vars
  Y = data_list$resp
  tind = data_list$time_index
  I = data_list$Imat
  #W = data_list$Wmat
  n = data_list$n
  W_exp = data_list$Wmat_exp
  
  # get initial coefficients to optimize lambda over
  # beta is calculated as 
  # beta = ((1/lambda X'X - (X'Wy))*lambda)^-1 (1/lambda * X'y - (X'Wy))*lambda
  
  XX = crossprod(X)
  XWX = t(X) %*% W_exp %*% X
  Xy = crossprod(X, Y)
  XWY = t(X) %*% W_exp %*% Y

  coef = base::solve(a = (1/lambda * XX -  XWX) * lambda, 
                      b = as.vector((1/lambda * Xy - XWY) * lambda))

  residuals = Y - (X %*% coef)
  
  IlWE = (I - lambda * W_exp) %*% residuals
  
  SSE = drop(t(residuals) %*% IlWE)

  if (!out) return(SSE)
  
  # if not used within lambda optimization,
  # return regression object
  
  coef = c(coef)
  names(coef) = colnames(X)
  res = list(coefficients=coef, SSE=c(SSE), 
              N=length(residuals))
  res
}



# the LL
LL_fit = function(lambda, data_list, eigenv, verbose) {
  # fitting function called from optimize()
  SSE = CAR_fit(lambda=lambda, data_list=data_list, out=FALSE)
  
  # get sigma^2
  n = data_list$n
  s2 = SSE/n
  
  # calculate det of (I - lambda * W), reuse eigenvalue of W calculated above
  det = sum(log(1 - lambda * eigenv))
  
  # the log lik for CAR model
  #### is it a plus or a minus
  LL = 0.5 * det - ( (n/2) * log(2 * pi) ) - (n/2) * log(s2) + (SSE / (2 * s2))
  
  if (verbose) cat("lambda:", lambda, "function:", LL, "Jacobian", det, "SSE", SSE, "\n")
  
  LL
}


predict_CAR = function(object, newdata, listw){
  
  # check whether newdata has necessary columns
  if (grepl("Intercept", names(object$coefficients[1]))){
    stopifnot(names(object$coefficients[-1]) %in% names(newdata))
  } else {
    stopifnot(names(object$coefficients) %in% names(newdata))
  }
  
  # extract X vars
  Terms = delete.response(terms(object))
  m = model.frame(Terms, newdata) 
  X = model.matrix(Terms, m)
  
  # convert weights matrix
  weights = listw2mat(listw)
  
  # set indices
  spatial = dim(weights)[[1]]
  time = length(object$Y) / spatial
  new_time = nrow(newdata) / spatial
  
  # extract betas
  coef = object$coefficients
  
  # calculate new means
  expectation = X %*% coef
  # get time averaged mean of Y - mu
  muT = unlist(lapply((1:spatial)-1, 
                function(x) mean(object$Y[seq(1, spatial * time, spatial) + x] -
                                   object$fitted.values[seq(1, spatial * time, spatial) + x])))
  
  #sigma2 = object$SSE / (time * spatial)
  weights_exp = bdiag(replicate(n = new_time, object$lambda * weights, simplify = F))
  
  predictor = expectation + (weights_exp %*% rep(muT, new_time)) #+ sigma2/2
  drop(predictor)
  
}

