################################################################################
################################################################################
###                                                                          ###
###           EXPERT RULE FIT (ERF) for Cervical Cancer                      ###
###                                                                          ###
################################################################################
################################################################################

#=============================== LIBRARIES =====================================

#.libPaths("C:/Users/ebner/Documents/R/win-library/4.0")


#install.packages('gbm')
#install.packages('inTrees')
#install.packages('randomForest')
#install.packages('pROC')
#install.packages('MASS')
#install.packages('pre')
#install.packages('mice')
#install.packages('DMwR')


library(gbm)
library(inTrees)
library(randomForest)
library(pROC)
library(MASS)
library(mice)
library(DMwR)

# external functions

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/Experiments/Cancer")
source("erf_cancer_dataprep.R")

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/ExpertRuleFit")
source("erf_main.R")

#===============================================================================

# 1. Knowledge Sources
# 1.1. Training Data
# 1.2. Factual Domain Knowledge from Clinical Practice Guidelines
# 1.3. Heuristic Expert Knowledge

#===============================================================================
#                             TRAINING DATA
#===============================================================================

# The dataset 'Cervical Cancer (Risk Factors)' was collected at the
# 'Hospital Universitario de Caracas' in Caracas, Venezuela and comprises
# demographic information, habits, and historic medical records of 858 women. 
# Several patients decided not to answer some of the questions because of
# privacy concerns (missing values).

setwd("C:/Users/ebner/Documents/MasterArbeit/My_Implementation/Experiments/Cancer")
data <- read.csv(file = 'risk_factors_cervical_cancer.csv', header = T)

data <- prepare_cervicalcancer_data(data = data, del_maj_missing = T,
                                    add_NA_features = F, impute_missing = T,
                                    target = "Risk1", balance = F)





#===============================================================================
#                           FACTUAL DOMAIN KNOWLEDGE
#===============================================================================

# 1.2.1. Rules extracted from Leitlinienprogramm Onkologie        

#' 1. Infection with human papilloma virus (mainly type 16, 18) as major
#'  influence factor, increasing OR by 150-400
# 2. Precancerous stages/dysplasias (Dx.Cancer)
# 3. Smoking (>15 cigarettes/day)
# 4. weakened immune system
# 5. Early start of sexual activity
# 6. Frequent change of sexual partners
# 7. low socio-economic status (no variable in the data)
# 8. poor sexual hygiene (no variable in the data)
# 9. Other infections e.g. genital herpes, chlamydia, gonococcus
# 10. Long-term use of oral contraceptives 
# 11. number of full-term pregnancies

dk_rules1 <- c("STDs.HPV==1",
                #"Dx.Cancer==1",
                "Smokes==1 & Smokes..packs.year.>=15",
                "First.sexual.intercourse<14",
                "Number.of.sexual.partners>4",
                "STDs.genital.herpes==1", 
                "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>=5 & Hormonal.Contraceptives..years.<=9",
                "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>=10",
                "Num.of.pregnancies>1")

#*******************************************************************************
# 1.2.2. Rules Extracted from American Cancer Society Guidelines

# 1. Sexual history
# 2. (long-term) Smoking
# 3. weakened immune system (HIV, Aids, Hepatitis)
# 4. Long-term use of oral contraceptives
# 5. Having multiple full-term pregnancies
# 6. Young age of first full-term pregnancy
# 7. Economic status (no variable in dataset)
# 8. Diet low in fruits and vegetables (no variables in dataset)
# 9. Intrauterine device use (risk lowering factor)

dk_rules2 <- c("First.sexual.intercourse<18",
                "Number.of.sexual.partners>2",
                "Smokes==1 & Smokes..years.>1",
                "Hormonal.Contraceptives==1 & Hormonal.Contraceptives..years.>5",
                "Num.of.pregnancies>=3",
                "Age<=20 & Num.of.pregnancies>=1",
                "IUD==0"
                )

#===============================================================================
#                         HEURISTIC EXPERT KNOWLEDGE
#===============================================================================

ek_rules1 <- c()

#===============================================================================
#                           EXPERT-RULEFIT MODEL
#===============================================================================

# Function Input
sets <- create_X_y_Xtest_ytest(data, 0.7, pos_class = 1)
X <- sets[[1]]
y <- sets[[2]]
Xtest <- sets[[3]]
ytest <- sets[[4]]

expert_rules <- c(dk_rules1, dk_rules2)


linterms <- c("Number.of.sexual.partners", "Age", "Num.of.pregnancies", 
         "First.sexual.intercourse", "Hormonal.Contraceptives..years.")

# Model
erf_cancer <- ExpertRuleFit(X=X, y=y, Xtest=Xtest, ytest=ytest,
                            expert_rules = expert_rules,
                            confirmatory_rules = expert_rules, 
                            linterms = linterms)


erf_cancer


