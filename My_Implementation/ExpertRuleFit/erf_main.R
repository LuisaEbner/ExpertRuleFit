################################################################################
################################################################################
###                                                                          ###
###                         EXPERT RULE FIT (ERF)                            ###
###                                                                          ###
################################################################################
################################################################################

# Packages 
.libPaths("C:/Users/ebner/Documents/R/win-library/4.0")

# Libraries
library(gbm)
library(inTrees)
library(randomForest)
library(pROC)
library(MASS)
library(bayesm)
library(glmnet)
library(coefplot)
library(purrr)
library(rlist)
library(tidyverse)
library(caret)
library(mlbench)
library(Metrics)

# External functions
setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/ERFSimulation")
source("simulation.R")

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/ExpertRuleFit")
source("erf_auxiliaries.R")

# functions implemented by Malte Nalenz
source("take1.R")
source("genrulesgbm.R")
source("genrulesrf.R")
source("createX.R")
source("create_test.R")

################################################################################

# Expert RuleFit

#' @title ExpertRuleFit
#' @description fits the Expert Rulefit model described in the MSc. Thesis "Complementing Prediction Rule Ensembles with Expert Knowledge" based on the work of Malte Nalenz for his model HorseRuleFit 
#' @param X A matrix containing the predictor variables to be used.
#' @param y A vector containing the binary response variable
#' @param Xtest optional matrix containing predictor variables of test set.
#' @param ytest optional vector containing the response values of the test set.
#' @param name_rules logical value. If set to TRUE, the original variable names are used in the expert_rules argument
#' @param name_lins logical value. If set to TRUE, the original variable names are used in the linterms argument
#' @param expert_rules optional vector containing rules extracted from subjective assessment, expert experience, from clinical guidelines, medical studies or textbooks.
#' @param confirmatory_rules vector specifying confirmatory rules as predictors for the final model.
#' @param ntree Number of trees in the ensemble step from which the rules are extracted.
#' @param ensemble Which ensemble method should be used to generate the rules? Options are "RF","GBM" or "both".
#' @param mix If ensemble = "both" mix*ntree are generated via random forest and (1-mix)*ntree trees via gradient boosting.
#' @param L Parameter controlling the complexity of the generated rules. Higher values lead to more complex rules.
#' @param S Parameter controlling the minimum number of observations in the tree growing process.
#' @param minsup Rules with support < minsup are removed. Can be used to prevent overfitting.
#' @param linterms vector of strings including column names of variables to be included as linear terms in the ERF model. Specified variables need to be numeric. Categorical variables have to be transformed (e.g. to dummies) before included as linear effects.
#' @param confirmatory_lins vector of strings including column names of confirmatory ERF variables (no penalization).
#' @param intercept If TRUE an intercept is included (for classification highly recommended)
#' @param corelim minimum value of correlation where correlated rules are cleaned
#' @param alpha The elasticnet mixing parameter, alpha = 1 corresponds to lasso, alpha = 0 to ridge, 0<alpha<1 to elastic net
#' @param nfolds number of CV folds to find the best lambda
#' @param type.measure measure according which to optimize lambda; can be either "auc", "class", "mse" or "mae"
#' @param s: string, either "lambda.min" or "lambda.1se", where "lambda.min" indicates the value of lambda that gives minimum mean cross-validated error, whereas  "lambda.1se": value of lambda which gives the most regularized model such that error is within one standard error of the minimum. 
#' @return An object of class ExpertRuleFit, which is a list of the following components:
#'   \item{Model}{regularized regression model including as dataframe of model features and its coefficients}
##'  \item{Features}{vector of strings including all features = intercept, rules, linear terms}
##'  \item{Coefficients}{vector of feature coefficients (numeric)}
##'  \item{Nterms}{number of terms in the final model}
##'  \item{ExpertRules}{vector of expert rule strings}
##'  \item{ConfTerms}{vector of expert rules specified as confirmatory predictors}
##'  \item{Removed_ExpertRules}{vector of expert rules that were removed due to too low support or too high correlation with other rules}
##'  \item{PropEK}{proportion of expert rules and relevant linear terms included in the final model}
##'  \item{LinearTerms}{vector of included linear terms}
##'  if Xtest is given, additional list elements are:
##'  \item{Conf_Mat}{Confusion matrix of predictions}
##'  \item{AUC}
##'  \item{ClassErr}{Classification Error}
##'  if print_output = T: all relevant list elements are additionally printed to the console
##'                  


ExpertRuleFit = function(X=NULL, y=NULL, Xtest=NULL, ytest=NULL,
                         name_rules = T, expert_rules = NULL, confirmatory_rules = NULL,
                         name_lins = T, linterms=NULL, confirmatory_lins = NULL,
                         ntree=250, ensemble= "RF", mix=0.5, L=4, S=6, minsup=.025, 
                         intercept=F, corelim = 1, 
                         alpha = 1, nfolds = 10, type.measure = "class",
                         s = "lambda.min", print_output = T) {
  
  
  # function input checks
  if((is.matrix(X)|is.data.frame(X))==F){
    stop("X must be a matrix or data frame.")
  }
  
  if((!is.factor(y))){
    stop("y is not a (binary) factor. Currently only (binary) classification is supported.")
  }
  
  if(!(is.null(Xtest))){
    if(dim(X)[2]!= dim(Xtest)[2]){
      stop("The dimensionality between X and Xtest differ.")
    }
  }
  
  if(is.null(ytest)==F){
    if(mode(y)!=mode(ytest)){
      stop("The mode of y and ytest differs.")
    }
  }
  
  if(!(is.null(expert_rules))){
    if(name_rules == T){
      expert_rules <- names_to_positions(X, expert_rules)
    }
  } else{
    expert_rules <- c()
  }
  
  
  if(!(is.null(confirmatory_rules))){
    if(name_rules == T){
      confirmatory_rules <- names_to_positions(X, confirmatory_rules)
    }
    if(!(all(confirmatory_rules %in% expert_rules))){
      stop("confirmatory_rules needs to be a subset of expert_rules.")
    }
  } else{
    confirmatory_rules <- c()
  }
  
  if(is.null(linterms)){
    linear = F
    linterms <- c()
  } else {
    linear = T
    if(name_lins == T){
      linterms <- names_to_numbers(X, linterms)
    }
    for(l in 1:length(linterms)){
      if(is.numeric(X[,linterms[l]])==F){
        stop(sprintf("Variable %i is not numeric and can not be included as
                     linear term. Please check the variable.",l))
      }
    }
  }
  
  
  if(!(is.null(confirmatory_lins))){
    if(name_lins == T){
      confirmatory_lins <- names_to_numbers(X, confirmatory_lins)
    } 
    if(!(all(confirmatory_lins %in% linterms))){
      stop("confirmatory_lins needs to be a subset of linterms.")
    }
  } else {
    confirmatory_lins <- c()
  }
  
  
  if(ntree<2){
    stop("Too few trees are chosen for ExpertRuleFit.")
  }
  
  
  if((mix<0)|(mix>=1)){
    stop("invalid choice for mix, please chose a value between 0 and 1.")
  }
  
  if(L<2){
    stop("Parameter L needs to be >=2.")
  }
  
  if(S<1){
    stop("Parameter S needs to be >=1.")
  }

  if((minsup<0)|(minsup>=1)){
    stop("invalid choice for minimum support, please chose a 
         value between 0 and 1.")
  }
  

  if(is.logical(intercept)==F){
    stop("Invalid intercept choice. Must be TRUE or FALSE.")
  }
  
  
  if((alpha<0)|(alpha>1)){
    stop("invalid choice for alpha, please chose a value between 0 and 1.")
  }
  
  if(nfolds < 1){
    stop("Invalid choice for nfolds. Number of cv folds needs to be
         an integer greater greater or equal to one.")
  }

  
  if(is.logical(print_output)==F){
    stop("Invalid choice regarding output print. Must be TRUE or FALSE.")
  }

  
  # tree ensemble -> rule ensemble generation
  N = length(y)
  if (ensemble == "RF") {
    capture.output(rulesf <- genrulesRF(X, y, nt=ntree, S=S, L=L))
  } else if (ensemble == "GBM") {
    capture.output(rulesf <- genrulesGBM(X, y, nt=ntree,S=S, L=L))
  } else if (ensemble == "both"){
    capture.output(rules1 <- genrulesRF(X, y, nt=round(ntree*mix),
                                        S=S, L=L))
    capture.output(rules2 <- genrulesGBM(X, y, nt=round(ntree*(1-mix)),
                                         S=S, L=L))
    rulesf = c(rules1, rules2)
  } else {
    print("invalid Tree ensemble choice")
  }
  
  # add expert rules to rule ensemble
  rulesf <- c(rulesf, expert_rules)
  

  # create data matrix with rules as columns
  dt = createX(X = X, rules = rulesf, t = minsup, corelim = corelim)
  Xr = dt[[1]]
  
  # initial set of rules
  rulesFin = dt[[2]]
  
  # get the expert rules, that were removed from createX due to low support or high correlation
  removed_expertrules <- c()
  
  if (length(expert_rules) > 0){
    for (i in 1:length(expert_rules)){
      if(!(expert_rules[i] %in% rulesFin)){
        removed_expertrules <- c(removed_expertrules, expert_rules[i])
      }
    }
    removed_expertrules
  }
  
  # standardize linear terms 
  sdl=0
  mul=0
  
  if(length(linterms)>1){
    mul = apply(X[,linterms], 2, mean)
    sdl = apply(X[,linterms], 2, sd)
    for(l in 1:length(linterms)){
      X[,linterms[l]] = (X[,linterms[l]]-mul[l])/sdl[l]
    }
  } else if(length(linterms)==1) {
    mul = mean(X[,linterms])
    sdl = sd(X[,linterms])
    X[,linterms] = (X[,linterms] - mul)/sdl
  }
  
  # add linear terms and intercept (optional) to rule matrix Xt
  if(linear==FALSE){
    if(intercept==TRUE){
      Xt = cbind(rep(1, times= dim(Xr)[1]),Xr)
    } else {
      Xt = Xr
    }
  } else {
    if(intercept==TRUE){
      Xt = cbind(rep(1, times=dim(X)[1]),X[,linterms], Xr)
    } else {
      Xt = cbind(X[,linterms], Xr)
    }
    
    # change column names: intercept = X0, linear terms = X1,...Xp, rules as specified conditions
    if(intercept == TRUE & length(linterms) > 0){
      colnames(Xt)[1] <- "X0"
      colnames(Xt)[2:(length(linterms)+1)] <- paste("X", linterms, sep = "")
      colnames(Xt)[(length(linterms)+2): ncol(Xt)] <- rulesFin
    } else if (intercept == TRUE & length(linterms) == 0){
      colnames(Xt)[1] <- "X0"
      colnames(Xt)[2: ncol(Xt)] <- rulesFin
    } else if (intercept == FALSE & length(linterms) > 0){
      colnames(Xt)[1:length(linterms)] <- paste("X", linterms, sep = "")
      colnames(Xt)[(length(linterms)+1): ncol(Xt)] <- rulesFin
    } else{      
      colnames(Xt) <- rulesFin
    }
    
    # define columns to be included in the final model without penalization
    if(length(confirmatory_lins)>0){
      confirmatory_lins <- paste("X", confirmatory_lins, sep = "")
    } else {
      confirmatory_lins <- c()
    }
    
    
    # get the column indices of the confirmatory terms
    if(length(confirmatory_rules) > 0|length(confirmatory_lins) > 0){
      confirmatory_terms <- c(confirmatory_rules, confirmatory_lins)
      conf_cols <- c()
      for(i in 1: length(confirmatory_terms)){
        if(confirmatory_terms[i] %in% colnames(Xt)){
          conf_cols <- c(conf_cols, which(colnames(Xt) == confirmatory_terms[i]))
        }
      }
      confirmatory_cols <- conf_cols
    } else{
      confirmatory_terms <- c()
      confirmatory_cols <- c()
    }
    
    
    if(is.null(Xtest) == T){
    regmodel = regularized_regression(X=Xt, y=y, Xtest = NULL, ytest =NULL,
                                      type_measure = type.measure,
                                      nfolds = nfolds,
                                      s = s,
                                      confirmatory_cols = confirmatory_cols,
                                      alpha = alpha, print_output = print_output)
    
    model_features <- regmodel$Results$features
    prop_ek <- expert_occurences(expert_rules, confirmatory_lins, model_features)
    
    
    if(print_output == T){
      exp_info <- expert_output(expert_rules, removed_expertrules, confirmatory_lins, prop_ek)
      output <- list(regmodel, exp_info)
      
      
      out = c(output, Train = Xt , Model = regmodel$Results, 
              Features = regmodel$Results$features, 
              Coefficients = regmodel$Results$coefficients, 
              Nterms = regmodel$n_terms, ExpertRules = expert_rules, 
              ConfTerms = confirmatory_terms,
              Removed_ExpertRules = removed_expertrules,  PropEK = prop_ek)
    } else{
      
      out = c(Train = Xt , Model = regmodel$Results, 
              Features = regmodel$Results$features, 
              Coefficients = regmodel$Results$coefficients, 
              Nterms = regmodel$n_terms, ExpertRules = expert_rules, 
              ConfTerms = confirmatory_terms,
              Removed_ExpertRules = removed_expertrules,  PropEK = prop_ek)
    }
      
    class(out) = "ExpertRulemodel"
    
    # else if test data is present:
    }else{
      
      #create rules.
      Xrt = createXtest(Xtest, rulesFin)
      
      ##preparing test data set. Standardize linear terms Xtest
      if(length(linterms > 0)){
        for(l in 1:length(linterms)){
          Xtest[,linterms[l]] = (Xtest[,linterms[l]]-mul[l])/sdl[l]
        }
      }
      
      #combine to data frame
      if(linear==FALSE){
        if(intercept==TRUE) {
          X_test = cbind(rep(1, times = dim(Xrt)[1]), Xrt)
        }else{X_test = Xrt}
      } else {
        if(intercept==TRUE) {
          X_test = cbind(rep(1, times = dim(Xrt)[1]), Xtest[,linterms], Xrt)
        }else{
          X_test = cbind(Xtest[,linterms], Xrt)
        }
      }
      
      # adapt column names
      if(intercept == TRUE & length(linterms) > 0){
        colnames(X_test)[1] <- "X0"
        colnames(X_test)[2:(length(linterms)+1)] <- paste("X", linterms, sep = "")
        colnames(X_test)[(length(linterms)+2): ncol(X_test)] <- rulesFin
      } else if (intercept == TRUE & length(linterms) == 0){
        colnames(X_test)[1] <- "X0"
        colnames(X_test)[2: ncol(X_test)] <- rulesFin
      } else if (intercept == FALSE & length(linterms) > 0){
        colnames(X_test)[1:length(linterms)] <- paste("X", linterms, sep = "")
        colnames(X_test)[(length(linterms)+1): ncol(X_test)] <- rulesFin
      } else{      
        colnames(X_test) <- rulesFin
      }
      
      # add prediction and error to model output
      regmodel = regularized_regression(X = Xt, y = y, Xtest = X_test,
                                        ytest = ytest, 
                                        type_measure = type.measure,
                                        nfolds = nfolds,
                                        s = s,
                                        confirmatory_cols = confirmatory_cols,
                                        alpha = alpha, print_output = print_output)
      
      model_features <- regmodel$Results$features
      prop_ek <- expert_occurences(expert_rules, confirmatory_lins, model_features)
      

      if(print_output == T){
        exp_info <- expert_output(expert_rules, removed_expertrules, confirmatory_lins, prop_ek)
        output <- list(regmodel, exp_info)
        
        out = list(output, Train = Xt, Test = X_test, Model = regmodel$Results, 
                   Features = regmodel$Results$features, 
                   Coefficients = regmodel$Results$coefficients, 
                   Nterms = regmodel$n_terms,
                   ConfusionMatrix = regmodel$Conf_Mat, AUC = regmodel$AUC, 
                   ClassErr = regmodel$CE, ExpertRules = expert_rules, 
                   ConfTerms = confirmatory_terms,
                   RemovedExpertRules = removed_expertrules,
                   PropEK = prop_ek)
      } else{
        
        out = list(Train = Xt, Test = X_test, Model = regmodel$Results, 
                   Features = regmodel$Results$features, 
                   Coefficients = regmodel$Results$coefficients, 
                   Nterms = regmodel$n_terms,
                   ConfusionMatrix = regmodel$Conf_Mat, AUC = regmodel$AUC, 
                   ClassErr = regmodel$CE, ExpertRules = expert_rules, 
                   ConfTerms = confirmatory_terms,
                   RemovedExpertRules = removed_expertrules,
                   PropEK = prop_ek)
        
      }
      
      class(out) = "ExpertRulemodel"
      
      }
    }
    out
}

    
#===============================================================================

# Simulation Example

# Example
# set.seed(179)
simulation <- create_simulation(n_vars = 20, n_obs = 1000,
                                mu = 0, sigma = 1, 
                                n_lin_preds = 3,
                                n_rule_vars = 10, 
                                n_rel_rules = 5, 
                                optional_lengths = c(1, 2),
                                weights = c(1/2, 1/2),
                                mu_beta = 0, sigma_beta = 5, 
                                mu_epsilon = 0, sigma_epsilon = 0.025, 
                                format = T)


# Dataset of relevant predictors + y 
# rel_predictor_data <- simulation[[1]]


# Data
data <- simulation[[2]]
X <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)[[1]]
y <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)[[2]]

Xtest <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)[[3]]
ytest <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)[[4]]

# Expert knowledge
expert_knowledge <- simulation[[3]]
expert_rules <- expert_knowledge[[1]]
lins <- as.numeric(expert_knowledge[[2]])
lins
confirmatory_lins <- c(1, 6)

# ExpertRuleFit Modell
erfmodel <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest, 
                          name_rules = F, expert_rules = expert_rules,
                          name_lins = F, linterms = lins, confirmatory_lins = confirmatory_lins)



#' TODO: in simulation linterms weglassen
#' cv.glmnet in einer funktion
#' gr????eres epsilon in der Simulation
#' relationale pfade, setwd
#' variable importance, wie viel Prozent der Varianz werden durch Expert-Rules erkl??rt
#' mit pre package vergleichen
#' 
#' Anruf ??rzte
#' Tortoise Git

