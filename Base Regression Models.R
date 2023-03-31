#Code for Base Regression Models

library(readxl)
library(writexl)
library(data.table)
library(MASS)
library(kableExtra)

invlogit <- function(x){1/(1 + exp(-x))}


#Main Files ####
AP_Youth_Survey <- read_excel("youth_survey_responses (12th Mar).xlsx")
AP_Household_Roster <- read_excel("Household Roster Youth Survey (12th Mar).xlsx")

#Codebooks ####
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- AP_Youth_Survey_Codebook$Variable_Name
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- c(AP_Household_Roster_Codebook$Variable_Name, "NAN")
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name


#Merging dataset and combining skilling variables####
AP_Youth_Survey_Merged <- merge(AP_Youth_Survey, AP_Household_Roster[AP_Household_Roster$H_1 == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))
attr(AP_Youth_Survey_Merged, "variable.labels") <- c(AP_Youth_Survey_Codebook$Column_Name, AP_Household_Roster_Codebook$Column_Name[AP_Household_Roster_Codebook$Column_Name != "_submission__uuid"])

AP_Youth_Survey_Merged <- AP_Youth_Survey_Merged[AP_Youth_Survey_Merged$`City Name` != "N",]

AP_Youth_Survey_Merged$YR_F_87 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_87,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_123, AP_Youth_Survey_Merged$Y_F_160))

AP_Youth_Survey_Merged$YR_F_92 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_92,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_128, AP_Youth_Survey_Merged$Y_F_165))

AP_Youth_Survey_Merged$YR_F_94 <- ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Student", AP_Youth_Survey_Merged$Y_F_94,
                                         ifelse(AP_Youth_Survey_Merged$Y_F_81 == "Employed", AP_Youth_Survey_Merged$Y_F_130, AP_Youth_Survey_Merged$Y_F_167))


#City Size Class needs to be updated for new data
#Vectors are in a different file, but this should suffice if one is using March 12 data
Small <- c("Kadiri", "Peddapuram", "Rayadurga", "Nidadavolu", "Kavali","Kondapalli") #Less than 1 Lakh
Medium <- c("Adoni", "Eluru", "Hindupur", "Kadapa", "Kakinada", "Narasaraopet", "Rajahmundry",
            "Tadipatri", "Tenali", "Tirupati")#1-4 Lakhs
Large <- c("Guntur", "Visakhapatnam", "Kurnool", "Nellore", "Vijayawada") #More than 4 Lakhs





AP_Youth_Survey_Merged$City_Size_Class <- ifelse(AP_Youth_Survey_Merged$`City Name`%in% Small, "Small",
                                                 ifelse(AP_Youth_Survey_Merged$`City Name` %in% Medium, "Medium",
                                                        ifelse(AP_Youth_Survey_Merged$`City Name` %in% Large, "Large",
                                                               "NAN")))

rm(Small, Medium, Large)

#Adding Asset Index
Asset_File <- read.csv("youthsurvey_assets_march12.csv")

for (i in 1:nrow(AP_Youth_Survey_Merged)) {
  
  AP_Youth_Survey_Merged$Asset_Index[i] <- Asset_File$assets[Asset_File$X_uuid == AP_Youth_Survey_Merged$`_uuid`[i]]
  
}

rm(Asset_File)

#Part 1 - Predictor Variables #####
#Demographic Predictors

age <- AP_Youth_Survey_Merged$Age

male <- ifelse(AP_Youth_Survey_Merged$Gender == "Male", 1, 0)
female <- ifelse(AP_Youth_Survey_Merged$Gender == "Female", 1, 0)

rel_hindu <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Hinduism", 1, 0)
rel_muslim <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Islam", 1, 0)
rel_christ <- ifelse(AP_Youth_Survey_Merged$Y_A_13 == "Christianity", 1, 0)
rel_others <- ifelse((AP_Youth_Survey_Merged$Y_A_13 %in% c("Christianity", "Hinduism", "Islam")) == F, 1, 0)


caste_gen <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "General (OC)", 1, 0)
caste_sc <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Scheduled Caste (SC)", 1, 0)
caste_st <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Scheduled Tribe (ST)", 1, 0)
caste_bc <- ifelse(AP_Youth_Survey_Merged$Y_A_15 == "Backward Caste (BC)", 1, 0)
caste_others <- ifelse((AP_Youth_Survey_Merged$Y_A_15 %in% c("Backward Caste (BC)", "Scheduled Tribe (ST)", "Scheduled Caste (SC)", "General (OC)")) == F, 1, 0)

prim_act <- AP_Youth_Survey_Merged$Y_F_81

city_size <- AP_Youth_Survey_Merged$City_Size_Class

ever_married <- ifelse((AP_Youth_Survey_Merged$H_4 %in% c("Never Married", "Don't know")) == F, 1, 0)


#Non-Demographic Infrastructure Related Predictors
motorable_road <- ifelse(AP_Youth_Survey_Merged$Y_5 %in% c("Kuccha motorable", "Pucca motorable"), 1, 0)
closed_drainage <- ifelse(AP_Youth_Survey_Merged$Y_6 == "Pucca Closed", 1, 0)
rented_house <- ifelse(AP_Youth_Survey_Merged$Y_D_31 == "Yes", 1, 0)

#Have not used labour pref, but should figure out how to include this, possibly as four separate dummies
labour_pref <- ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Happy with current work","Do not wish to work"),
                      "Status Quo", ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Work for a private company", "Work for a multinational corporation", "Work for a non-profit organization"),
                                           "Private Sector", ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Start your own business", "Work for family business"),
                                                                    "Self/Family Business", ifelse(AP_Youth_Survey_Merged$Y_F_170 == "Work for the government/public sector",
                                                                                                   "Public Sector", "No Response"))))

asset_index <- AP_Youth_Survey_Merged$Asset_Index


#Part 2 - Outcome Variables####
#Skilling Enrollment
y1 <- ifelse(AP_Youth_Survey_Merged$YR_F_87 == "No", 0, 1)

#Skilling Interest  
y2 <- ifelse(AP_Youth_Survey_Merged$YR_F_92 == "No", 0, 1)

#Satisfaction with AP Government
y3 <- ifelse(AP_Youth_Survey_Merged$Y_G_187 %in% c("Much Better", "Somewhat Better"), 1, 0)
  
#Willingness to move outside city and Andhra Pradesh
y4 <- ifelse(AP_Youth_Survey_Merged$Y_F_173 == "Yes" & AP_Youth_Survey_Merged$Y_F_176 == "Yes", 1, 0)

#Preference for Cash Schemes
y5 <- ifelse(AP_Youth_Survey_Merged$Y_G_188 == "Much More" | AP_Youth_Survey_Merged$Y_G_188 == "Somewhat More" , 1, 0)

#Outcomes for Receipt of Social Schemes
#Support for Housing
y6 <- ifelse(AP_Youth_Survey_Merged$Y_E_50 == "No", 0, 1)

#Support for Education
y7 <- ifelse(AP_Youth_Survey_Merged$Y_E_55 == "No", 0, 1)

#Support for Health
y8 <- ifelse(AP_Youth_Survey_Merged$Y_E_60 == "No", 0, 1)

#Cash Support
y9 <- ifelse(AP_Youth_Survey_Merged$Y_E_65 == "No", 0, 1)

#Business Support & Debt Support
y10 <- ifelse(AP_Youth_Survey_Merged$Y_E_70 == "No", 0, 1)

#Household Services & Infrastructure
y11 <- ifelse(AP_Youth_Survey_Merged$Y_E_75 == "No", 0, 1)


#Political Attribution
#Political Attribution for Chief Minister
y12 <- ifelse(AP_Youth_Survey_Merged$Y_E_80 == "Chief Minister", 1, 0)

#Political Attribution for Prime Minister
y13 <- ifelse(AP_Youth_Survey_Merged$Y_E_80 == "Prime Minister", 1, 0)

#Political Attribution for Ward Councillor
y14 <- ifelse(AP_Youth_Survey_Merged$Y_E_80 == "Ward Councillor", 1, 0)

#Political Attribution for Government Officials
y15 <- ifelse(AP_Youth_Survey_Merged$Y_E_80 == "Government Officials", 1, 0)



#Skilling Process Categories
#Not enrolled, not interested
y16 <- ifelse(AP_Youth_Survey_Merged$YR_F_87=="No"&AP_Youth_Survey_Merged$YR_F_92=="No",1,0)

#Not enrolled, but interested
y17 <- ifelse(AP_Youth_Survey_Merged$YR_F_87=="No"&AP_Youth_Survey_Merged$YR_F_92!="No",1,0)

#Enrolled and Interested
y18 <- ifelse(AP_Youth_Survey_Merged$YR_F_87!="No"&AP_Youth_Survey_Merged$YR_F_92!="No",1,0)



#Part 3 - The Models ####
#Model 1
logit.fit1 <- glm(y1 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit")) 
summary(logit.fit1)

#Model 2
logit.fit2 <- glm(y2 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit")) 
summary(logit.fit2)

#Model 3
logit.fit3 <- glm(y3 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit")) 
summary(logit.fit3)

#Model 4
logit.fit4 <- glm(y4 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit4)


#Model 5 - 
logit.fit5 <- glm(y5 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit5)


#Models 6 to 11
logit.fit6 <- glm(y6 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit6)


logit.fit7 <- glm(y7 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit7)


logit.fit8 <- glm(y8 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit8)


logit.fit9 <- glm(y9 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit9)


logit.fit10 <- glm(y10 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit10)


logit.fit11 <- glm(y11 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit11)


#Models 12 to 15
logit.fit12 <- glm(y12 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit12)


logit.fit13 <- glm(y13 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                    caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                    rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit13)


logit.fit14 <- glm(y14 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                     caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                     rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit14)


logit.fit15 <- glm(y15 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                     caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                     rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit15)


#Models 16 to 18
logit.fit16 <- glm(y16 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                     caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                     rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit16)


logit.fit17 <- glm(y17 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                     caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                     rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit17)


logit.fit18 <- glm(y18 ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                     caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + 
                     rented_house + asset_index, family=binomial(link = "logit"))
summary(logit.fit18)



#Part 4 - Simulations and Uncertainty Bounds####
#First setting the baseline
quantile(asset_index, seq(0.01,1,0.01))

#For baseline, fixing median value of the asset index

#Baseline Demographic - OC, Hindu, Employed, Big City, Male, Unmarried, 
#Baseline Non Demographics - Access to motorable road, closed drainage, owns house, close to mean asset index of 0

x <- c(1,25,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0)

#Checking regular baseline probabilities for all 4
invlogit(logit.fit1$coefficients %*% x)
invlogit(logit.fit2$coefficients %*% x)
invlogit(logit.fit3$coefficients %*% x)
invlogit(logit.fit4$coefficients %*% x)
invlogit(logit.fit5$coefficients %*% x)


#Now for simulating this
sim.coef1 <- mvrnorm(15000, logit.fit1$coefficients, vcov(logit.fit1))
sim.coef2 <- mvrnorm(15000, logit.fit2$coefficients, vcov(logit.fit2))
sim.coef3 <- mvrnorm(15000, logit.fit3$coefficients, vcov(logit.fit3))
sim.coef4 <- mvrnorm(15000, logit.fit4$coefficients, vcov(logit.fit4))
sim.coef5 <- mvrnorm(15000, logit.fit5$coefficients, vcov(logit.fit5))


bounds <- matrix(NA, nrow = 18, ncol = 10)
mean_prob <- matrix(NA, nrow = 18, ncol = 5)


var_names <- c('Baseline', 'Female', 'Student', 'Unemployed', 'Medium City', 'Small City', 
               'Other Religions', 'Muslims', 'Christians', 'Other Castes', 'SC', 'ST', 'Backward Castes', 'Married',
               'Non Motorable Road', 'Open/No Drain', 'Rented House', 'Lower Quartile on Asset Index')


for (i in 1:18) {
  
  x <- c(1,25,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0)
  
  if(i > 1 & i <= 14 | i == 17) {
    
    x[i+1] = 1
    
  } else if(i %in% c(15,16,18)) {
    
    x[i+1] = x[i+1] - 1
    
  } 
  
  
  
  predval.sim1 <- invlogit(sim.coef1 %*% x)
  predval.sim2 <- invlogit(sim.coef2 %*% x)
  predval.sim3 <- invlogit(sim.coef3 %*% x)
  predval.sim4 <- invlogit(sim.coef4 %*% x)
  predval.sim5 <- invlogit(sim.coef5 %*% x)
  
  
  mean_prob[i,1] <- mean(predval.sim1)
  mean_prob[i,2] <- mean(predval.sim2)
  mean_prob[i,3] <- mean(predval.sim3)
  mean_prob[i,4] <- mean(predval.sim4)
  mean_prob[i,5] <- mean(predval.sim5)
  
  
  bounds[i,1:2] <- quantile(predval.sim1, c(0.025,0.975))
  bounds[i,3:4] <- quantile(predval.sim2, c(0.025,0.975))
  bounds[i,5:6] <- quantile(predval.sim3, c(0.025,0.975))
  bounds[i,7:8] <- quantile(predval.sim4, c(0.025,0.975))
  bounds[i,9:10] <- quantile(predval.sim5, c(0.025,0.975))
  
  
  
  
}


Means <- cbind.data.frame(var_names, mean_prob); colnames(Means) <- c("Variable_Name", "y1", "y2", "y3", "y4", "y5")
Means[,c(2:6)] <-  round(Means[,c(2:6)], 2)


CI_Bounds <- cbind.data.frame(var_names, bounds); colnames(CI_Bounds) <- c("Variable_Name", "y1_lower","y1_upper", "y2_lower", "y2_upper", "y3_lower","y3_upper",
                                                                           "y4_lower", "y4_upper", "y5_lower", "y5_upper")
CI_Bounds[,c(2:11)] <-  round(CI_Bounds[,c(2:11)], 2)


rm(sim.coef1, sim.coef2, sim.coef3, sim.coef4, sim.coef5, mean_prob, bounds, 
   predval.sim1, predval.sim2, predval.sim3, predval.sim4, predval.sim5, var_names, i)
