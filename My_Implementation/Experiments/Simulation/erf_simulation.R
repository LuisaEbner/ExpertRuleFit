################################################################################
################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Simulated Data                       ###
###                                                                          ###
################################################################################
################################################################################

#=============================== LIBRARIES =====================================

#.libPaths("C:/Users/ebner/Documents/R/win-library/4.0")

#library(pre)
#library(purrr)
#library(rlist)
#library(gbm)
#library(inTrees)
#library(randomForest)
#library(pROC)
#library(MASS)
#library(bayesm)
#library(glmnet)
#library(coefplot)
#library(purrr)
#library(rlist)
#library(tidyverse)
#library(caret)
#library(mlbench)
#library(Metrics)

# external functions
source("simulation.R")
source("erf.R")
#===============================================================================
#                               SIMULATION
#===============================================================================

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

#===============================================================================
#                                 DATA 
#===============================================================================

data <- simulation[[2]]

#===============================================================================
#                      SIMULATED EXPERT/DOMAIN KNOWLEDGE
#===============================================================================

expert_knowledge <- simulation[[3]]
expert_rules <- expert_knowledge[[1]]
expert_lins <- as.numeric(expert_knowledge[[2]])

#===============================================================================
#                          EXPERT RULEFIT MODEL
#===============================================================================

# Function Input

sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

# Model

erfmodel <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                           expert_rules = expert_rules,
                           linterms = expert_lins,
                           confirmatory_rules = expert_rules)


