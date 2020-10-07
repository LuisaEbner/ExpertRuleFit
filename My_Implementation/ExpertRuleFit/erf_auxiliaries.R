################################################################################

#                                                                              #
#                   Expert RuleFit - Auxiliary Functions                       #
#                                                                              #                                                                              #

################################################################################

#' @name create_X_y_Xtest_ytest
#' @description takes a dataset including a binary target column and turns it into training and test data for X and y, separately. Converts y to make it applicable to glmnet. When train_frac = 1, no test data is created.
#' @param data: dataframe object including predictors and binary target
#' @param train_frac: value between 0 and 1 to specify the fraction of training samples to be sampled
#' @param pos_class: string or value of the positive class value of y, eg. 1,"pos","TRUE","yes" etc.
#' @return list object, including X, Xtest, y, ytest


create_X_y_Xtest_ytest <- function(data, train_frac, pos_class = 1, target_name = NULL, type_missing = NULL){
  
  # rename the target column
  if (is.null(target_name) == F){
    names(data)[names(data) == target_name] <- "y"
  } else{
    names(data)[ncol(data)] <- "y"
  }
  
  data$y <- factor(data$y)
  
  if(is.null(type_missing) == F){
    data[data == type_missing] <- NA
  }
  
  data <- data[complete.cases(data), ]
  
  # train-test-split
  set.seed(45)
  sample <- sample.int(n = nrow(data),
                       size = floor(train_frac*nrow(data)), replace = F)
  train <- data[sample, ]
  test  <- data[-sample, ]
  
  # convert dataframe to matrix format, remove target column
  X <- model.matrix(y ~., train)[,-1]
  Xtest <- model.matrix(y~.,test)[,-1]
  
  # Convert the target column of the training data to a 0-1-coded factor
  y <- factor(ifelse(train$y == pos_class, 1, 0))
  ytest <- factor(ifelse(test$y == pos_class, 1, 0))
  
  out = list(X, y, Xtest, ytest)
  
}


################################################################################

#'@name names_to_positions
#'@description replaces the name of a variable in an expert rule with its position to become readable for the function createX
#'@param X data frame of input variables
#'@param name_rules vector of strings, containing expert rules with variable names as in the original dataset
#'@return the same vector as name_rules, just with the original variable names replaced by their position in the dataset

names_to_positions <- function(X, name_rules){
  names <- colnames(X)
  positions <- c()
  for (i in 1:ncol(X)){
    positions[i] <- paste("X[,",i, "]", sep = "")
  }
  
  bool <- T
  iteration <- 0
  
  pos_rules <- c()
  for (j in 1:length(name_rules)){
    for (k in 1:length(names)){
      if(grepl(names[k],name_rules[j])){
        pos_rules[j] <- gsub(names[k], positions[k], name_rules[j])
      }
    }
  }
  
  
  while(bool){
    bool <- F
    for (j in 1:length(pos_rules)){
      for (k in 1:length(names)){
        if(grepl(names[k],pos_rules[j])){
          bool <- T
          pos_rules[j] <- gsub(names[k], positions[k], pos_rules[j])
        }
      }
    }
  }
  pos_rules
}

################################################################################

#'@name names_to_numbers
#'@description replaces the name of a variable in a number indicating its column position 
#'@param X data frame of input variables
#'@param name_rules vector of variable names as the names of relevant linear terms
#'@return a vector of numbers, as the variable's column positions in the dataset

names_to_numbers <- function(X, variable_names){
  names <- colnames(X)
  positions <- 1:ncol(X)
  
  num_variables <- c()
  for (j in 1:length(variable_names)){
    for (k in 1:length(names)){
      if(names[k] == variable_names[j]){
        num_variables[j] <- gsub(names[k], positions[k], variable_names[j])
      }
    }
  }
  as.numeric(num_variables)
}

################################################################################


#' @name regularized_regression
#' @description performs regularized regression as described in stage 2 of the ERF model
#' @param X: matrix of predictor variables (training set)
#' @param y: vector of target values as a binary, 0-1-encoded factor (training set)
#' @param type_measure "auc", "mse", "mae", "class"
#' @param Xtest optional matrix of predictor variables (test set)
#' @param ytest optional vector of target values, 0-1-encoded factor (test set)
#' @param nfolds: number of cross validation folds
#' @param s: string, either "lambda.min" or "lambda.1se", where "lambda.min" indicates the value of lambda that gives minimum mean cross-validated error, whereas  "lambda.1se": value of lambda which gives the most regularized model such that error is within one standard error of the minimum. 
#' @param confirmatory_cols: vector of integers indicating the columns of the input variables that should not be penalized (confirmatory model terms)
#' @param alpha: elastic net mixing parameter. Allowed values include: ???1???: for lasso regression, ???0???: for ridge regression, any value between 0 and 1 for elastic net regression.
#' @param print_output logical value, indicating whether a model output should be printed
#' @return if print_output == T: console output including: nfolds, s, lambda, number of terms in the final model, 
#' mean_cv_error, the final model as a dataframe of variable names and coefficients, Xtest, a confusion matrix, the AUC and the Classification error
#' else if print_output == F: list with the following elements: Results = dataframe of variable names and coefficient,  
#' n_terms = number of terms in the final model, lambda,  mean cv error, Confusion matrix, AUC and classification error.


regularized_regression <- function(X, y, Xtest = NULL, ytest = NULL,
                                   type_measure = "class",
                                   nfolds = 10,
                                   s = "lambda.min",
                                   confirmatory_cols, 
                                   alpha = 1, print_output = T){
  
  # find best lambda via cross validation
  set.seed(123) 
  cvfit <- cv.glmnet(X, y, family = "binomial", 
                              type.measure = type_measure, nfolds = nfolds)
  if (s == "lambda.min"){
    lambda <- cvfit$lambda.min
  } else{
    lambda <- cvfit$lambda.1se
  }
  
  # min of mean cv error
  mse.min <- min(cvfit$cvm)
  
  # define penalties according to confirmatory columns
  if(length(confirmatory_cols) == 0){
    p.fac = rep(1, ncol(X))
  }else{
    p.fac = rep(1, ncol(X))
    p.fac[confirmatory_cols] <- 0
  }
  
  # model fit
  fit <- cv.glmnet(X, y, family = "binomial", alpha = alpha, penalty.factor = p.fac)
  
  # variable coefficients
  coefs <- coef(fit, s=lambda);
  # non-zero coefficients
  coefs[which(coefs != 0 ) ] 
  # non-zero coefficient feature names
  coefs@Dimnames[[1]][which(coefs != 0 ) ]  
  # number of non-zero coefficients
  n_terms = length(coefs[which(coefs != 0 ) ])
  
  Results <- data.frame(
    features = coefs@Dimnames[[1]][ which(coefs != 0 ) ], #intercept included
    coefficients    = coefs       [ which(coefs != 0 ) ]
  )
  

  if (is.null(Xtest) == T) {
    if(print_output == T){
      # if no test data is given, return the model and its parameters
      # use function regr_output to create comprising model info in console
      output = regr_output(nfolds = nfolds, s=s, lambda = lambda, 
                        n_terms = n_terms, mean_cv_error = mse.min,
                        Results = Results, Xtest = Xtest)
      out <- list(output, Results = Results,  n_terms = n_terms, lambda =lambda, PenFac = p.fac)
    } else {
      # if no output is desired, store all important model parameters
      out <- list(Results = Results,  n_terms = n_terms, lambda =lambda, PenFac = p.fac)
    }
    
  } else{
    # predict class probabilities
    pred_prob <- predict(fit, newx = Xtest, s = lambda, type = "response")
    # predict binary classes
    pred_class <- predict(fit, newx = Xtest, s = lambda, type = "class")
    # confusion matrix
    conf_mat <- table(pred = pred_class,true = ytest)
    # AUC
    auc <-  auc(ytest, as.integer(pred_class))
    # Classification error
    ce <-   ce(ytest, as.integer(pred_class))
    
    if(print_output == T){
      # return model as console output + store important parameters, 
      # and predictive measures in list
      output <- regr_output(nfolds = nfolds, s=s, lambda = lambda, 
                         n_terms = n_terms, mean_cv_error = mse.min, 
                         Results = Results, Xtest = Xtest,
                         conf_mat = conf_mat, auc = auc, ce = ce)
      out <- list(output, Results = Results, n_terms = n_terms, lambda =lambda, 
                  mcve = mse.min, Conf_Mat = conf_mat, AUC = auc, CE = ce, PenFac = p.fac)
    } else {
      # store all relevant model information as list
      out <- list(Results = Results,  n_terms = n_terms, lambda =lambda, 
                  mcve = mse.min, Conf_Mat = conf_mat, AUC = auc, CE = ce, PenFac = p.fac)
    }
    
    
    
  }
  
  out
}

################################################################################

#' @title regr_output
#' @description  prints all relevant glmnet model information as received from the function 'regularized_regression'
#' @param see function description 'regularized_regression'
#' @return print statements


regr_output <- function(nfolds, s, lambda, n_terms, mean_cv_error, Results,
                        Xtest = NULL, conf_mat = NULL, auc = NULL, ce = NULL){
  cat(sprintf("Final ensemble with CV (k= %d) error with s = %s\n", nfolds, s))
  cat(sprintf("\n"))
  cat(sprintf("Lambda = %f \n", lambda))
  cat(sprintf("Number of terms = %d \n", n_terms))
  if (s == "lambda.min"){
    cat(sprintf("Mean cv error = %#.4f \n", mean_cv_error))
  }
  cat(sprintf("\n"))
  cat(sprintf("Regularized Logistic Regression Model: \n"))
  cat(sprintf("\n"))
  print(Results)
  cat(sprintf("\n"))
  if (is.null(Xtest) == F){
    cat(sprintf("Confusion matrix: \n"))
    cat(sprintf("\n"))
    print(conf_mat)
    cat(sprintf("\n"))
    cat(sprintf("AUC = %#.4f \n\n", auc))
    cat(sprintf("Classification error = %#.4f \n", ce))
    cat(sprintf("\n"))
  }
}

################################################################################


#' @title expert_occurences
#' @description calculates the proportion of expert rules that entered the final ERF model
#' @param expert_rules vector of rule strings, including all of the external expert knowledge in form of rules
#' @param model_rules vector of strings, including all rules and variables included in the final Expert-RuleFit model (after regularized regression)
#' @return value between 0 and 1, indicating the proportion of expert rules and expert linear terms that entered the final ERF model
#'                    

expert_occurences <- function(expert_rules, confirmatory_lins, model_features){
  ek <- c(expert_rules, confirmatory_lins)
  n_ek <- length(ek)
  # counter for expert rules in the final model
  ek_in = 0
  
  if(n_ek > 0){
    for (i in 1:n_ek){
      if(ek[i] %in% model_features){
        ek_in = ek_in + 1
      }
    }
    prop_ek <- ek_in/n_ek
  } else{
    prop_ek <- 0
  }
}

################################################################################


#' @title expert_output
#' @description  prints the expert rules and the proportion of expert knowledge that entered the final ERF model 
#' @param expert_rules vector of rule strings, including all of the external expert knowledge in form of rules
#' @param prop_ek proportion of expert rules and variables that entered the final ERF model
#' @return print statements

expert_output <- function(expert_rules, removed_expertrules, confirmatory_lins, prop_ek){
  cat(sprintf("All Expert Rules: \n"))
  cat(sprintf("\n"))
  print(expert_rules)
  cat(sprintf("\n"))
  cat(sprintf("Expert Rules removed due to low support or correlation withother rules in the model: \n"))
  cat(sprintf("\n"))
  print(removed_expertrules)
  cat(sprintf("\n"))
  cat(sprintf("Expert Linear terms: \n"))
  print(confirmatory_lins)
  cat(sprintf("\n"))
  cat(sprintf("Proportion of expert rules in the final model:  %#.4f \n", prop_ek))
  
} 


################################################################################

#'@name rf_vs_erf
#'@description compares ERF models with and without expert knowledge according to predictive accuracy and model complexity
#'@param see parameters of function 'ExpertRuleFit'
#'@return dataframe = table of results for AUC, CE and number of terms on different models

rf_vs_erf <- function(X=NULL, y=NULL, Xtest=NULL, ytest=NULL, name_rules = T,
                      expert_rules = NULL, confirmatory_rules = NULL,
                      linterms=NULL, confirmatory_lins = NULL,
                      ntree=250, ensemble= "RF", mix=0.5, L=4, S=6, minsup=.025, 
                      name_lins = T, intercept=F, corelim = 1, 
                      alpha = 1, nfolds = 10, type.measure = "class",
                      s = "lambda.min", print_output = T){
  
  rf <- ExpertRuleFit(X = X, y=y, Xtest=Xtest, ytest=ytest, name_rules=name_rules,
                      name_lins = name_lins, linterms = linterms, 
                      ntree = ntree, ensemble = ensemble, mix=mix, L=L, S=S, 
                      minsup=minsup, intercept=intercept, corelim=corelim, 
                      alpha =alpha, nfolds=nfolds, type.measure = type.measure, s=s, print_output = F)
  
  erf <- ExpertRuleFit(X = X, y=y, Xtest=Xtest, ytest=ytest, name_rules=name_rules,
                       expert_rules = expert_rules, confirmatory_rules = confirmatory_rules, 
                       name_lins = name_lins, linterms = linterms, confirmatory_lins = confirmatory_lins, 
                       ntree = ntree, ensemble = ensemble, mix=mix, L=L, S=S, 
                       minsup=minsup, intercept=intercept, corelim=corelim, 
                       alpha =alpha, nfolds=nfolds, type.measure = type.measure, s=s, print_output = F)
  
  
  erf_out <- c(erf$AUC, erf$ClassErr, erf$Nterms)
  rf_out <- c(rf$AUC, rf$ClassErr, rf$Nterms)
  
  out <- rbind(rf_out, erf_out)
  colnames(out) <- c("AUC", "Classification Error", "Number of terms")
  rownames(out) <- c("ERF w-o. EK", "ERF w. EK")
  out
  
  
}

# Example
# test <-rf_vs_erf(X, y, Xtest, ytest, name_rules = F, expert_rules = expert_rules, confirmatory_rules = expert_rules, 
#                 name_lins = F, linterms = lins, confirmatory_lins = confirmatory_lins) 
# test

################################################################################
