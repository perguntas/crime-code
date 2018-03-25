require(spdep)
require(matrixcalc)

SARlm = function(formula, data, listw, verbose=NULL,
                  tol.opt=.Machine$double.eps^0.5){
  
  if (is.null(verbose)) verbose = F

  # match arguments
  mf = match.call(expand.dots = FALSE)
  m = match(c("formula", "data"), names(mf), 0)
  
  # get coefs and model defs
  mf = mf[c(1, m)]
  mf$drop.unused.levels = TRUE
  mf[[1]] = as.name("model.frame")
  mf = eval(mf, parent.frame())
  mt = attr(mf, "terms")

  # extract variables
  Y = model.response(mf, "numeric")
  X = model.matrix(mt, mf)
  rm(mf)
  
  if (any(is.na(Y))) stop("NAs in dependent variable")
  if (any(is.na(X))) stop("NAs in independent variable")
  n = nrow(X)
  
  # data.frame and data.table friendly subsetting
  ind = unique(data[[1]])
  tind = data[[2]]
  rm(data)
  
  # check panel dimensions
  if (n != length(listw$neighbours) * length(unique(tind)))
    stop("Input data and neighbourhood list have different dimensions")
  
  if (!inherits(listw, "listw")) stop("No neighbourhood list")
  
  # check for aliased variables
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
  if (!(listw$style %in% c("W", "B"))) 
    stop("Invalid spatial weights style")
  
  # extract weights matrix
  W = listw2mat(listw)
  if ( is.singular.matrix(as.matrix(W)) ) 
    stop("Spatial Weights matrix is singular")
  
  # create blockdiagonal weight matrix
  W_exp = bdiag(replicate(n = length(unique(tind)), W, simplify=F))
  
  # create identity matrix
  I = Diagonal(n)
  
  # create lagged Wy vector
  Wy = W_exp %*% Y
  
  if (any(is.na(Wy)))
    stop("NAs in lagged dependent variable")
  
  # pre-calculate OLS residuals (necessary for lambda estimation)
  lm0 = lm(Y ~ X - 1)
  e0 = residuals(lm0)

  # put everything into one list
  data_list = list(vars=X, resp=Y, time_index=tind, Imat=I, 
                    Wmat_exp=W_exp, n=n,  Wy=Wy, e0=e0)
  
  if (verbose) cat(paste("\nJacobian calculated using spatial neighborhood matrix\n"))
  
  eig = eigen(W, symmetric = T, only.value = T)$value
  eig_range = 1/range(eig)
  
  interval = eig_range
  
  if (verbose) cat("\nLambda interval", interval, "\n")
  
  # optimize over lambda
  opt = optimize(SAR_fit, interval=interval, 
                  maximum=TRUE, data_list=data_list, eigenv=eig, verbose=verbose)
  lambda = opt$maximum
  
  # check lambda bounds
  if (isTRUE(all.equal(lambda, interval[1])) ||
      isTRUE(all.equal(lambda, interval[2]))) 
    warning("lambda on interval bound - results should not be used")
  
  names(lambda) = "lambda"
  
  LL = opt$objective

  # OLS step
  ols_y = Y - lambda * Wy
  lm.target = lm(as.vector(ols_y) ~ X - 1)
  
  # extract results
  coef.lambda = coefficients(lm.target)
  names(coef.lambda) = colnames(X)
  resid = lm.target$residuals
  SSE = sum(resid^2)
  fit = lm.target$fitted.values
  sigma = summary(lm.target)$sigma
  
  # calculate standard error by correcting lm.target errors
  p = lm.target$rank
  corr.SE = (summary(lm.target)$coefficients[,2]) * sqrt( (n - p)/n)
  
  df = n - ncol(X) - 2
  
  # t-value
  tval = coef.lambda/corr.SE
  pval = 2 * pt(abs(tval), df, lower.tail = F) # sure about p?
  
  SE = cbind(coef.lambda, corr.SE, tval, pval)
  dimnames(SE) = list(names(coef.lambda), 
                       c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
  
  call = match.call()

  ret = list(lambda=lambda,
              coefficients=coef.lambda, 
              SE=SE, 
              LL=LL, SSE=SSE, 
              parameters=(ncol(X)+2), 
              coef_lm.model=coef(lm0),
              y=Y, X=X,
              call=call, 
              residuals=resid, 
              opt=opt, fitted.values=fit, 
              terms=mt, time_index=tind, 
              sigma=sigma)
  GC = gc()
  ret
}

SAR_SSE = function(lambda, data_list) {
  # get data
  X = data_list$vars
  Y = data_list$resp
  tind = data_list$time_index
  I = data_list$Imat
  #W = data_list$Wmat
  n = data_list$n
  Wy = data_list$Wy
  W_exp = data_list$Wmat_exp

  # calculate SSE = (e0 - lambda * eL)^2
  # steps: 
  # 1) e0: calculate OLS residuals with beta = (X'X)^-1 X'y
  # 2) eL: calculate OLS residuals on Wy with beta = (X'X)^-1X'Wy

  # 1)
  e0 = data_list$e0
  
  # 2)
  # get (X'X)^-1X'Wy
  #XWY = lag_XWy(X, Y, W, tind)
  XWY = t(X) %*% W_exp %*% Y
  # solve for beta
  coefL = base::solve(a=crossprod(X), b=as.vector(XWY)) ###
  
  eL = as.vector(Wy - X %*% coefL) ###
  
  # calculate SSE
  SSE = sum((e0 - lambda * eL)^2)
  
  SSE
}

# function to optimize over
SAR_fit = function(lambda, data_list, eigenv, verbose) {
  SSE = SAR_SSE(lambda, data_list)
  
  # get sigma^2
  n = data_list$n
  s2 = SSE/n
  
  # calculate eigenv
  ldet = sum(log(1 - lambda * eigenv))
  
  # the log lik for the SAR
  LL = (ldet - ((n/2)*log(2*pi)) - (n/2)*log(s2) - (SSE/(2*(s2))))
  
  if (verbose) cat("lambda:", lambda, " function:", LL, " Jacobian:", ldet, " SSE:", SSE, "\n")

  LL
}


# lag_XWy = function(vars, resp, lag, time_index){
#   # create lagged X'Wy matrix
#   lag_Wy = list()
#   # lag Wy
#   for (j in 1:length(unique(time_index))){
#     lag_y = resp[time_index == unique(time_index)[j]]
#     lag_Wy[[j]] = lag %*% lag_y
#   }
#   Wy = do.call(rbind, lag_Wy)
#   # multiply X'Wy
#   XWy = crossprod(vars, Wy)
#   return(XWy)
# }
# 
# lag_Wy = function(resp, lag, time_index){
#   # lags y blockwise
#   lag_Wy = list()
#   
#   for (j in 1:length(unique(time_index))){
#     lag_y = resp[time_index == unique(time_index)[j] ]
#     lag_Wy[[j]] = lag %*% lag_y
#   }
#   Wy = do.call(rbind, lag_Wy)
#   Wy
# }


predict_SAR = function(object, listw, newdata){
  # predict yhat = (I - B)^-1 X * beta + (I - B)^-1 ehat_T
  
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

  # extract coefficients and residuals
  coef = object$coefficients
  resids = object$residuals
  
  # convert weights matrix
  W = listw2mat(listw)
  
  # set indices
  spatial = dim(W)[[1]]
  time = length(object$y) / spatial
  new_time = nrow(newdata) / spatial
  
  # calculate (I - lambda * W)^-1
  # identity matrix
  I_tmp = Diagonal(n = spatial)
  
  # inverse
  IlB_inv = solve(I_tmp - object$lambda * W)
  
  # expand blockdiagonal
  IlB_exp = bdiag(replicate(n = new_time, IlB_inv, simplify = F))
  
  # time averaged residuals
  ehat = 1/spatial * unlist(lapply((1:spatial)-1, 
                                    function(x) sum(resids[seq(1, spatial * time, spatial) + x])))
  
  # create IlB vars und IlB resid
  IlB_inv_Xb = IlB_exp %*% (X %*% coef)

  predictor = IlB_inv_Xb + IlB_exp %*% rep(ehat, new_time)
  drop(predictor)
}

# lag_WX = function(vars, lag, time_index){
#   # lags blockwise
#   lag_WX = list()
#   
#   for (j in 1:length(unique(time_index))){
#     lag_X = vars[time_index == unique(time_index)[j], ]
#     lag_WX[[j]] = lag %*% lag_X
#   }
#   WX = do.call(rbind, lag_WX)
#   WX
# }

# lag_We = function(resid, lag, time_index){
#   # lags blockwise
#   lag_We = list()
#   
#   for (j in 1:length(unique(time_index))){
#     lag_e = resid[time_index == unique(time_index)[j] ]
#     lag_We[[j]] = lag %*% lag_e
#   }
#   We = do.call(rbind, lag_We)
#   We
# }
# 
# ehat = function(resid, time_index){
#   # calculate 1/T * sum(ehat_t) 
#   
#   # add function to add list elements elementwise
#   add = function(x) Reduce("+", x)
#   
#   # extract residuals at unique timepoints
#   e_hat = lapply(1:length(unique(time_index)), 
#                   function(x) resid[time_index == unique(time_index)[x] ])
#   # sum residuals
#   e_vec = add(e_hat)
#   e_vec = 1/length(unique(time_index)) * e_vec
#   e_vec
# }

