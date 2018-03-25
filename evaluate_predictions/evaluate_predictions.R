# evaluate predictions

# create evaluation function with MSE and AMSE (optional)
evaluation = function(predictions, true, w = 1.3, cutoff = 0.5, AMSE=T){

  predictions[predictions < 0] = 0
  # round predictions
  if (cutoff == 0.5) {
    predictions = round(predictions, digits = 0)
  }
  else {
    predictions[predictions > 1] = round(predictions[predictions > 1], digits = 0)
    predictions[predictions >= cutoff & predictions < 1] = 1
    predictions[predictions <  cutoff] = 0
  }
  
  MSE = sum((predictions-true)^2) / length(predictions)
  
  if (AMSE){
    # set weights vector for AMSE
    weights = vector(length=length(predictions))
    weights = ifelse(predictions > true, 2-w, w)
    
    AMSE = sum(weights * ((predictions-true)^2)) / length(predictions)
    
    result = c(MSE, AMSE)
    names(result) = c("MSE", "AMSE")
  } else {
    result = MSE
    names(result) = "MSE"
  }

  return(result)
}


# this function extracts all model coefficients for the full model as well as their 
# standard errors
extraction = function(files, setting = 8){
  # creates the following list:
  # list = settings considered (1-8), $ all the models (1-5), $ parameters, residuals, and st. errors
  
  
  # define parameters to extract
  extracts = list(coefficients=list(), 
                   SE=list(), 
                   spatial=list(), 
                   residuals=list(),
                   fitted=list())
  # define number of models
  models = replicate(n = length(files), extracts, simplify = F)
  names(models) = substr(files, 8, 10)
  # define number of settings
  settings = replicate(n = length(setting), models, simplify = F)
  
  # load models
  for (i in 1:length(files)){
    # load model files
    model = get(load(files[i]))
    
    for (s in 1:length(settings)){
      # extract coefficients per setting
      settings[[s]][[i]]$coefficients = model[[s]]$coefficients
      
      # extract residuals
      settings[[s]][[i]]$residuals = model[[s]]$residuals
      
      # load parameters from custom models
      if (grepl("SAR|CAR|PAR", files[i])){
        settings[[s]][[i]]$SE = model[[s]]$SE
        
        # get lambda parameter from SAR and CAR
        if (grepl("SAR|CAR", files[i])){
          settings[[s]][[i]]$spatial = model[[s]]$lambda
          settings[[s]][[i]]$fitted = as.vector(model[[s]]$fitted.values)

        } else {
        # get sigma parameter from PCAR
          settings[[s]][[i]]$spatial = model[[s]]$sigma2
          settings[[s]][[i]]$random = model[[s]]$eta
          settings[[s]][[i]]$fitted = model[[s]]$fitted
        }
      } else {
      # load parameters from standard models
        settings[[s]][[i]]$SE = summary(model[[s]])$coefficients
        settings[[s]][[i]]$fitted = model[[s]]$fitted.values
      }
    }
  }
  if (length(settings) == 1){
    # simplify if only one setting is considered
    settings = settings[[1]]
  }
  return(settings)
}


# this function loads all the model predictions 
# and only the best results of the machine learning techniques
# because of that it needs an evaluation function and the true data
load_predictions = function(files, f, true, best_only=T){
  model_predictions = list()
  RGN_predictions = list()
  
  model_files = files[!grepl("RGN", files)]
  RGN_files = files[grepl("RGN", files)]
  
  if (length(RGN_files) > 0){
    for (i in 1:length(RGN_files)){
      pred = do.call(cbind, get(load(RGN_files[i])))
      
      if (best_only){
        pred = as.data.frame(pred)
        # create list
        splits = lapply(c("rf", "gbm", "nn"), function(x) pred[, grepl(x, colnames(pred))])
        
        
        # get the best prediction
        best = lapply(lapply(splits, function(predictions) 
          apply(predictions, 2, function(each_pred) f(each_pred, true))), 
          function(MSE) which.min(MSE[1,]))
        
        best_pred = lapply(1:3, function(ind) splits[[ind]][, best[[ind]]])
        
        names(best_pred) = paste0(unlist(lapply(best, function(ind) attr(ind, "names"))), "_", i)
        RGN_predictions[[i]] = do.call(cbind, best_pred)
      } else {
        RGN_predictions[[i]] = pred
      }
      
      
    }
  }

  
  if (length(model_files) > 0){
    # load predictions for models
    for (i in 1:length(model_files)){
      
      model = do.call(cbind, get(load(model_files[i])))
      colnames(model) = paste0(substr(model_files[i], 18, 20), 1:ncol(model))
      model_predictions[[i]] = model
    }
  }
  
  model_predictions = do.call(cbind, model_predictions)
  RGN_predictions = do.call(cbind, RGN_predictions)
  if (!is.null(RGN_predictions)){
    cols = 1:ncol(RGN_predictions)
    
    RGN_predictions = RGN_predictions[, c(cols[grepl("rf", colnames(RGN_predictions))],
                                           cols[grepl("gbm", colnames(RGN_predictions))],
                                           cols[grepl("nn", colnames(RGN_predictions))])]
  }
  return(cbind(model_predictions, RGN_predictions))
}



cutoff_search = function(predictions, true){
  # this function searches over a wide range of rounding cutoff values
  # parallely and return a frame with the chosen cutoff as the first row
  # and the remainder of the predictions
  
  # Calculate the number of cores
  no_cores = detectCores() - 1
  
  # Initiate cluster
  cl = makeCluster(no_cores)
  
  # cutoff sequence
  cutoff_set = seq(0.2, 0.8, 0.001)
  
  # error with default cutoff
  errors_d = apply(predictions, 2, function(x) evaluation(x, true))[1,]
  
  pred_set = foreach(i = 1:length(cutoff_set), .combine = "rbind") %dopar% {
    s = apply(predictions, 2, function(x) evaluation(x, true, cutoff = cutoff_set[i], AMSE=F))
    s = s - errors_d
  }
  stopCluster(cl)
  
  opt_cutoff = cutoff_set[apply(pred_set, 2, which.max)]
  
  final_pred = lapply(1:ncol(predictions), 
                       function(x) evaluation(predictions[,x], true, cutoff = opt_cutoff[x], AMSE=F))
  
  final_pred = do.call(cbind, final_pred)
  final_pred = rbind(opt_cutoff, final_pred)
  colnames(final_pred) = colnames(predictions)
  
  return(final_pred)
}


extract_fitted = function(files){
  fitted = list()
  
  # load models
  for (i in 1:length(files)){
    model = get(load(files[i]))
    
    # get fitted values
    fitted[[i]] = lapply(model, function(x) x$fitted.values)
  }
  names(fitted) = substr(files, 8, 10)
  assign("fitted", fitted, envir=.GlobalEnv)
}

# Singh test, cf. Singh Shukla (1983)
singh_test = function(resids, weight){
  N = length(resids)
  time = N/dim(weight)[[1]]
  
  # set W_nT
  H = bdiag(replicate(n = time, weight, simplify =F))
  
  top = resids %*% H %*% resids 
  bottom = resids %*% resids
  
  # calculate test statistic
  O = abs(drop(top/bottom))
  
  # get confidence intervals
  variance = (2 * (time * sum(diag(weight %*% weight)))) / (2 * N + N^2)
  
  stan = O / sqrt(variance)
  p = pnorm(stan)
  
  result = c(stan, p, p > 0.975)
  names(result) = c("Test Statistic", "p-value", "result")
  
  return(result)
}

# lag test, compare p. 442, Cressie (1993)
cressie_lag = function(resids, weight){
  J = resids %*% bdiag(replicate(n = length(resids)/dim(weight)[[1]], 
                                  weight, simplify = F)) %*% resids
  return(drop(J))
}

require(compiler)
SHC = cmpfun(function(X, Y, iter = 100L){
  
  # setting initial values
  N           = ncol(X)
  weights     = rep(0L, N)
  pred        = 0 * X
  sum.weights = 0L
  best        = sample(ncol(X), 1)
  conv        = vector(length = iter)
  
  # performing stochastic hill-climbing
  while(sum.weights < iter) {
    sum.weights   = sum.weights + 1L
    pred          = (pred + X) * (1L / sum.weights)
    errors        = colSums((pred - Y) ^ 2) / nrow(pred)
    # randomly select another neighbor
    neighbor      = sample(1:length(errors), 1)
    best          = ifelse(errors[neighbor] < errors[best], neighbor, which.min(errors))
    weights[best] = weights[best] + 1L
    pred          = pred[, best] * sum.weights
    conv[sum.weights] = errors[best]
    
  }
  
  # returning model weights
  return(list(best=(weights / sum.weights), conv=conv))
})


SHC_bag = cmpfun(function(X, Y, bags = 10L, p = 0.5, iter = 100L){
  
  # setting initial values
  i = 0L
  N = nrow(X)
  M = ncol(X)
  W = matrix(rbinom(bags * M, 1, p), ncol = M)
  conv = list()
  
  # performing bagging
  while(i < bags)  {
    
    # doing ES on a bagged sample
    i         = i + 1L
    ind       = which(W[i, ] == 1)
    alg       = SHC(X[, ind], Y, iter)
    W[i, ind] = W[i, ind] * alg$best
    conv[[i]] = alg$conv
  }
  
  # returning model weights
  return(list(best=(colSums(W) / bags), conv=conv))
})
