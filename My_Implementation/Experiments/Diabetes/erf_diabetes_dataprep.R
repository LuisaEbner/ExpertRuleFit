################################################################################
################################################################################
###                                                                          ###
###     EXPERT RULE FIT for Cervical Cancer - Training Data Preparation      ###
###                                                                          ###
################################################################################
################################################################################

# DATA PREPARATION FUNCTION

# library(mice)

#' @name prepare_diabetes_data
#' @description Preparation (Cleaning, Feature Engineering, Missing Value Imputation, Resampling) of the UCI dataset Pima Indians Diabetes.
#' @param data UCI dataset Pima Indians Diabetes
#' @param imp_method a single string, or a vector of strings, specifying the imputation method to be used for SkinThickness and Insulin. If specified as a single string, the same method will be used for all blocks. Common options: "pmm" = Predictive mean matching, "midastouch"	=	Weighted predictive mean matching, "sample" = 	Random sample from observed values, "cart"	=	Classification and regression trees and "rf" = 	Random forest imputations(default)
#' @return prepared UCI diabetes dataset

prepare_diabetes_data <- function(data, imp_method = "rf"){

  # 0s in Pregnancies and Outcome are no missing values
  # 0s in all other columns are missing values
  cols_missing <- colnames(data)[!colnames(data) %in% c("Pregnancies", "Outcome")]
  missing <- data[cols_missing] == 0
  data[cols_missing][missing] <- NA


  # Missing Data Imputation
  
  #' comparatively small number of observations -> to retain a maximum of
  #' information, no columns or rows shall be removed.
  #' Imputation Choices: (1) Replace missing data with sensible values
  #'                         (mean or median) given the distribution of the data.
  #'                     (2) Predict missing data.
  #' We use (1) if few values miss per column (Glucose, BloodPressure, BMI) and
  #'        (2) if many values are missing (SkinThickness, Insulin)
  
  # (1)
  data$Glucose[is.na(data$Glucose)] <- median(data$Glucose,na.rm = T)
  data$BloodPressure[is.na(data$BloodPressure)] <- median(data$BloodPressure,na.rm = T)
  data$BMI[is.na(data$BMI)] <- median(data$BMI,na.rm = T)
  
  
  # (2)
  mice_imp <- mice(data[, c("SkinThickness","Insulin")], method= imp_method, print = F) 
  mice_complete <- complete(mice_imp)
  data$SkinThickness <- mice_complete$SkinThickness
  data$Insulin <- mice_complete$Insulin

  return(data)
}


# EXAMPLE
setwd('C:/Users/ebner/Documents/MasterArbeit/Daten/Diabetes')
data <- read.csv(file = 'diabetes.csv', header = T)
data <- prepare_diabetes_data(data)
