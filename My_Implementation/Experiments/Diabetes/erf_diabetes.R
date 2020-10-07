################################################################################
################################################################################
###                                                                          ###
###                 EXPERT RULE FIT (ERF) for Diabetes                       ###
###                                                                          ###
################################################################################
################################################################################

#=============================== LIBRARIES =====================================

 .libPaths("C:/Users/ebner/Documents/R/win-library/4.0")

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
library(caret)
library(mlbench)
library(Metrics)
library(mice)
library(ggplot2)
library(glmnet)
library(readr)

# External functions

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/Experiments/Diabetes")
source("erf_diabetes_dataprep.R")

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/ExpertRuleFit")
source("erf_main.R")

#===============================================================================

# 1. Knowledge Sources
# 1.1. Training Data
# 1.2. Factual Domain Knowledge from Clinical Practice Guidelines

#===============================================================================
#                             TRAINING DATA
#===============================================================================

# DESCRIPTION

## dataset obtained from the University of California at Irvine (UCI) 
## Machine Learning Repository.
## http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes


## Patients: 768 females; >= 21 years old; of Pima Indian heritage.

## Number of Attributes: 8 (all numeric) + 1 binary target
# 1. Number of times pregnant
# 2. Plasma glucose concentration at 2 hours in an oral glucose tolerance test
# 3. Diastolic blood pressure (mm Hg)
# 4. Triceps skin fold thickness (mm)
# 5. 2-Hour serum insulin (Uh/ml)
# 6. Body mass index (Weight in kg / (Height in in))
# 7. Diabetes Pedigree Function:
#      provides a synthesis of the diabetes mellitus history in
#      relatives and the genetic relationship of those relatives to the
#      subject. The DPF uses information from parents, grandparents,
#      full and half siblings, full and half aunts and uncles, and first
#      cousins. It provides a measure of the expected genetic influence of
#      affected and unaffected relatives on the subject's eventual diabetes
#      risk.
# 8. Age (years)

# missing values present as 0s

# Class/Target Distribution: 500x0, 268x1 
#(class value 1 is interpreted as "tested positive for diabetes")

# see: https://rpubs.com/ikodesh/53189#:~:text=According%20to%20http%3A%2F%2F
#      www,family%20history%20to%20predict%20how

# Goal: Predict the presence of diabetes mellitus given a number of risk factors


# DATA 
setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/Experiments/Diabetes")
data <- read.csv(file = 'diabetes.csv', header = T)
data <- prepare_diabetes_data(data)

#===============================================================================
#                           DOMAIN KNOWLEDGE
#===============================================================================

# 1. Rules Extracted from: Nationale Versorgungs-Leitlinie
# Diabetes mellitus Typ 2, Kurzfassung 
# MAD = diastolischer Druck + 1/3 * (systolischer Druck/diastolischer Druck)
# MAD = 85 + 1/3 * (130/85)  = 100
# MAD = 90 + 1/3 * (140/90)  = 106.667

dk_rules1 <- c("Glucose>=80 & Glucose<=135", 
                "Glucose>=135 & Glucose<=160", "Glucose>160",
                "BloodPressure<100",
                "BloodPressure>=100 & BloodPressure<=107",
                "BloodPressure>107",
                "BMI>=19 & BMI<=24", "BMI>=24 & BMI<=26", "BMI>26" )

# 2. Rules extracted from American Diabetes Association, 
#    Standards of Medical Care in Diabetes -2018, 
#    Chapter 2: Classification and Diagnosis of Diabetes

dk_rules2 <- c("BMI>=25", "BloodPressure>=107", "Age>=45",
                   "Glucose>=144 & Glucose<=199")

#===============================================================================
#                         HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

ek_rules1 <- c()

#===============================================================================
#                          EXPERT RULEFIT MODEL
#===============================================================================

# Function Input
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]
linterms <- c("Age", "BMI", "Glucose", "Insulin")
expert_rules <- c(dk_rules1, dk_rules2)



# Model
erf_diabetes <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                              expert_rules = expert_rules,
                              confirmatory_rules = expert_rules, 
                              linterms = linterms)
