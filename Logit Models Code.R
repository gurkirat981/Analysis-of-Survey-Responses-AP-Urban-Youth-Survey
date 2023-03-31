#Code for Logit Models
#Required - Merged files and added variables, plus invlogit function

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


Small <- c("Kadiri", "Peddapuram", "Rayadurga", "Nidadavolu", "Kavali","Kondapalli") #Less than 1 Lakh
Medium <- c("Adoni", "Eluru", "Hindupur", "Kadapa", "Kakinada", "Narasaraopet", "Rajahmundry",
            "Tadipatri", "Tenali", "Tirupati")#1-4 Lakhs
Large <- c("Guntur", "Visakhapatnam", "Kurnool", "Nellore", "Vijayawada") #More than 4 Lakhs





AP_Youth_Survey_Merged$City_Size_Class <- ifelse(AP_Youth_Survey_Merged$`City Name`%in% Small, "Small",
                                                 ifelse(AP_Youth_Survey_Merged$`City Name` %in% Medium, "Medium",
                                                        ifelse(AP_Youth_Survey_Merged$`City Name` %in% Large, "Large",
                                                               "NAN")))


#Creating Skilling Variables by Sector
AP_Youth_Survey_Merged$IT_F87 <- grepl(c("Information Technology"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$English_F87 <- grepl(c("English"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$CompExam_F87 <- grepl(c("Competitive Exam"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$SciEng_F87 <- grepl(c("Engineering"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$Repair_F87 <- grepl(c("Technical"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)
AP_Youth_Survey_Merged$Craft_F87 <- grepl(c("Craftwork"), AP_Youth_Survey_Merged$YR_F_87, ignore.case = T)

#Interest
AP_Youth_Survey_Merged$IT_F92 <- grepl(c("Information Technology"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Computer"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$English_F92 <- grepl(c("English"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Communication"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$CompExam_F92 <- grepl(c("Competitive Exam"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$SciEng_F92 <- grepl(c("Engineering"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Science"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$Repair_F92 <- grepl(c("Technical"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T) | grepl(c("Repair"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)
AP_Youth_Survey_Merged$Craft_F92 <- grepl(c("Craftwork"), AP_Youth_Survey_Merged$YR_F_92, ignore.case = T)


AP_Youth_Survey_Merged$Total_Enr_F87 <- rowSums(AP_Youth_Survey_Merged[,c(256:261)])
AP_Youth_Survey_Merged$Total_Int_F92 <- rowSums(AP_Youth_Survey_Merged[,c(262:267)])

rm(Large, Medium, Small)

#Optional - Can Drop Small City Size Class
#AP_Youth_Survey_Merged <- AP_Youth_Survey_Merged[AP_Youth_Survey_Merged$City_Size_Class != "Small", ]
#If you do include small city responses, make sure to set your baseline afresh


#Models for Skilling####
#Predictors to include

#Age
#Religion of household - Y_A_13 - Christianity, buddh, dont know, hindu, islam, none, other
#Caste - Y_A_15 (Gen, SC, ST, BC)
#City Size Class
#Primary Activity
#Gender
#Educational Background - H_5

#Baseline - Male, hindu, large city, employed (typical age 25)


#Organising Variables
#For Enrollment in Skilling
y <- ifelse(AP_Youth_Survey_Merged$YR_F_87 == "No", 0, 1)

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

#Running the model and getting a probability estimate
logit.fit <- glm(y ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + caste_sc + caste_st + caste_bc + ever_married, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0)  ## OC, Hindu, Employed, Big City, Male

invlogit(logit.fit$coefficients %*% x)


#Simulations to generate predicted values
x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0)

set.seed(1234)
sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

predval.sim <- invlogit(sim.coef %*% x)

quantile(predval.sim, seq(0,1,0.01))

#Determine 95% confidence intervals

#To be done - additional analysis beyond baseline for
#Schedule caste, ST, muslim, women, student, unemployed


#Loop to determine confidence intervals

sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

bounds <- matrix(NA, nrow = 14, ncol = 2)
mean_prob <- rep(NA, 14)

var_names <- c('Baseline', 'Female', 'Student', 'Unemployed', 'Medium City', 'Small City', 
               'Other Religions', 'Muslims', 'Christians', 'Other Castes', 'SC', 'ST', 'Backward Castes', 'Married')

for (i in 1:14) {
  
  x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0)

  if(i > 1) {
    
    x[i+1] = 1
    
  }
  
  predval.sim <- invlogit(sim.coef %*% x)
  
  mean_prob[i] <- mean(predval.sim)
  
  bounds[i,] <- quantile(predval.sim, c(0.025,0.975))
  
}


Conf_Int_Probs1 = cbind.data.frame(Variable = var_names, Lower_Bound = bounds[,1], Mean = mean_prob, Upper_Bound = bounds[,2])

Conf_Int_Probs1[,c(2:4)] <-  round(Conf_Int_Probs1[,c(2:4)], 2)



#Model 2 - For interest in skilling####
y <- ifelse(AP_Youth_Survey_Merged$YR_F_92 == "No", 0, 1)

logit.fit <- glm(y ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + caste_sc + caste_st + caste_bc + ever_married, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0)  ## OC, Hindu, Employed, Big City, Male

invlogit(logit.fit$coefficients %*% x)

#Loop to determine confidence intervals
set.seed(1234)
sim.coef <- mvrnorm(15000, logit.fit$coefficients, vcov(logit.fit))

bounds <- matrix(NA, nrow = 14, ncol = 2)
mean_prob <- rep(NA, 14)


var_names <- c('Baseline', 'Female', 'Student', 'Unemployed', 'Medium City', 'Small City', 
               'Other Religions', 'Muslims', 'Christians', 'Other Castes', 'SC', 'ST', 'Backward Castes', 'Married')

for (i in 1:14) {
  
  x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0)
  
  if(i > 1) {
    
    x[i+1] = 1
    
  }
  
  predval.sim <- invlogit(sim.coef %*% x)
  
  mean_prob[i] <- mean(predval.sim)
  
  bounds[i,] <- quantile(predval.sim, c(0.025,0.975))
  
}


Conf_Int_Probs2 = cbind.data.frame(Variable = var_names, Lower_Bound = bounds[,1], Mean = mean_prob, Upper_Bound = bounds[,2])

Conf_Int_Probs2[,c(2:4)] <-  round(Conf_Int_Probs2[,c(2:4)], 2)

rm(predval.sim, bounds,sim.coef,i, mean_prob, var_names)




#Outcome 3 - Mapping Labour Sector Interest to Skilling Course Interest ####
#YF_170
l1 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$IT_F87), 1),1))
l2 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$English_F87), 2),1))
l3 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$CompExam_F87), 2),1))
l4 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$SciEng_F87), 2),1))
l5 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Repair_F87), 2),1))
l6 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Craft_F87), 2),1))


l1 <- l1[l1$Var2 == T,]; l2 <- l2[l2$Var2 == T,]; l3 <- l3[l3$Var2 == T,]; l4 <- l4[l4$Var2 == T,]; l5 <- l5[l5$Var2 == T,]; l6 <- l6[l6$Var2 == T,]

Labour_Skill <- cbind.data.frame(Labour_Pref = l1$Var1, l1$Freq, l2$Freq, l3$Freq, l4$Freq, l5$Freq, l6$Freq )
colnames(Labour_Skill) <- c("Labour Preference","IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")


#Interest in Skilling
l1 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$IT_F92), 1),1))
l2 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$English_F92), 2),1))
l3 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$CompExam_F92), 2),1))
l4 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$SciEng_F92), 2),1))
l5 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Repair_F92), 2),1))
l6 <- as.data.frame(round(100*prop.table(table(AP_Youth_Survey_Merged$Y_F_170, AP_Youth_Survey_Merged$Craft_F92), 2),1))

l1 <- l1[l1$Var2 == T,]; l2 <- l2[l2$Var2 == T,]; l3 <- l3[l3$Var2 == T,]; l4 <- l4[l4$Var2 == T,]; l5 <- l5[l5$Var2 == T,]; l6 <- l6[l6$Var2 == T,]

Labour_Skill_Int <- cbind.data.frame(Labour_Pref = l1$Var1, l1$Freq, l2$Freq, l3$Freq, l4$Freq, l5$Freq, l6$Freq )
colnames(Labour_Skill_Int) <- c("Labour Preference","IT", "English", "Competitive Exam", "Engineering/Science", "Technical/Repair", "Craftwork")


rm(l1,l2,l3,l4,l5,l6)

View(Labour_Skill)
View(Labour_Skill_Int)


#Output 4 - Distribution of No. of Skilling Requirements ####
par(mfrow = c(1,2))
a <- prop.table(table(AP_Youth_Survey_Merged$Total_Enr_F87[AP_Youth_Survey_Merged$Total_Enr_F87 > 0 & AP_Youth_Survey_Merged$Total_Enr_F87 < 5]))
bp <-barplot(100*a, ylim = c(0,86), xlab = "No. of Courses Enrolled In", ylab = "Percentage of Total People Enrolled", main = "Enrollment Frequency in Skilling Courses",
        col = "dodgerblue3", border = NA)
text(bp, 100*a, round(100*a), pos = 3, col = "black")

a <- prop.table(table(AP_Youth_Survey_Merged$Total_Int_F92[AP_Youth_Survey_Merged$Total_Int_F92 > 0 & AP_Youth_Survey_Merged$Total_Int_F92 < 5]))
bp <-barplot(100*a, ylim = c(0,85), xlab = "No. of Courses Interested In", ylab = "Percentage of Total People Interested", main = "Interest Frequency in Skilling Courses",
             col = "dodgerblue3", border = NA)
text(bp, 100*a, round(100*a), pos = 3, col = "black")


AP_Youth_Survey_Merged$Which_Enr_F87 <- apply(AP_Youth_Survey_Merged[,c(256:261)], 1, function(x) paste(names(AP_Youth_Survey_Merged[,c(256:261)])[x > 0], collapse = "-"))
x <- as.data.frame(table(AP_Youth_Survey_Merged$Which_Enr_F87, AP_Youth_Survey_Merged$Total_Enr_F87)); x <- x[x$Freq >0 & x$Var2 != 0,]

#x <- table(AP_Youth_Survey_Merged$Which_Enr_F87, AP_Youth_Survey_Merged$Total_Enr_F87)

a <- order(-x$Freq[x$Var2 == 1])
x$Var1[a]

par(mfrow = c(1,2))

bp <- barplot(100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a]), names.arg = c("IT", "Comp. Exam", "Craft", "English", "Repair", "Engineering") , ylim = c(0,80), cex.names = 0.75,
        col = "dodgerblue3", border = NA, main = c("Distribution of Enrollment:", "Individuals with Single Course"), las = 1, ylab = "Percentage (Within Individuals with Single Course)", xlab = "Course Category")
text(bp, 100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a]), round(100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a])), pos = 3, col = "black")


#Manual entry
#Single Skill - IT, English, Comp Exam, 
#IT and English, IT and Others, Non IT Combinations
multi_vec <- c(97,44,15)
bp <- barplot(100*multi_vec/sum(multi_vec), names.arg = c("IT and English", "IT and Others", "Non IT"), cex.names = 0.75, ylim  = c(0,80),
        col = "coral3", border = NA, main = c("Distribution of Enrollment:", "Individuals with Multiple Courses"), las = 1, ylab = "Percentage (Within Individuals with Multiple Courses)", xlab = "Course Categories")

text(bp, 100*multi_vec/sum(multi_vec), round(100*multi_vec/sum(multi_vec)), pos = 3, col = "black")



#Interest in Enrolment
par(mfrow = c(1,2))
AP_Youth_Survey_Merged$Which_Int_F92 <- apply(AP_Youth_Survey_Merged[,c(262:267)], 1, function(x) paste(names(AP_Youth_Survey_Merged[,c(262:267)])[x > 0], collapse = "-"))
x <- as.data.frame(table(AP_Youth_Survey_Merged$Which_Int_F92, AP_Youth_Survey_Merged$Total_Int_F92)); x <- x[x$Freq >0 & x$Var2 != 0,]

a <- order(-x$Freq[x$Var2 == 1])
x$Var1[x$Var2 == 1][a]

bp <- barplot(100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a]), names.arg = c("IT", "Comp. Exam", "Craft","English", "Repair", "Engineering") , ylim = c(0,80), cex.names = 0.75,
              col = "dodgerblue3", border = NA, main = c("Distribution of Skilling Interest:", "Single Course"), las = 1, ylab = "Percentage (Within Those Interested in Single Course)", xlab = "Course Category")
text(bp, 100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a]), round(100*x$Freq[x$Var2 == 1][a]/sum(x$Freq[x$Var2 == 1][a])), pos = 3, col = "black")


#Manual entry
#Single Skill - IT, English, Comp Exam, 
#IT and Others, IT and English, Non IT Combinations
multi_vec <- c(151,94,26)
bp <- barplot(100*multi_vec/sum(multi_vec), names.arg = c("IT and English", "IT and Others", "Non IT"), cex.names = 0.75, ylim  = c(0,80),
              col = "coral3", border = NA, main = c("Distribution of Skilling Interest:", "Multiple Courses"), las = 1, ylab = "Percentage (Within Those Interested in Multiple Courses)", xlab = "Course Categories")

text(bp, 100*multi_vec/sum(multi_vec), round(100*multi_vec/sum(multi_vec)), pos = 3, col = "black")




#Adding variables from Roster
M <- as.data.table(AP_Household_Roster)
M$UID = M$`_submission__uuid`
M$Ma <- M$Gender == "Male" & M$H_1 == "Self"
M$Fa <- M$Gender == "Female" & M$H_1 == "Self"
M$UID = M$`_submission__uuid`

Mal <- M$`_submission__uuid`[M$Ma == T]
Fal <- M$`_submission__uuid`[M$Fa == T]

Male <- M[UID %in% Mal,.(Father_Ed = H_5[H_1 == "Father"], Self_Ed = H_5[H_1 == "Self"],
                         Father_Occup = Occup_Cat_Level[H_1 == "Father"], 
                         Self_Occup = Occup_Cat_Level[H_1 == "Self"]), by=c("UID")]

Female <- M[UID %in% Fal,.(Father_Ed = H_5[H_1 == "Father"], Self_Ed = H_5[H_1 == "Self"],
                           Father_Occup = Occup_Cat_Level[H_1 == "Father"], 
                           Self_Occup = Occup_Cat_Level[H_1 == "Self"]), by=c("UID")]

Educ_Occup <- rbind(Male, Female)



for (i in 1:nrow(AP_Youth_Survey_Merged)) {

  
  AP_Youth_Survey_Merged$Father_Educ[i] = ifelse((is.na(Educ_Occup$Father_Ed[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]])) == F, 
                                                 Educ_Occup$Father_Ed[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]], NA) 
  
  
  AP_Youth_Survey_Merged$Father_Occup[i] = ifelse((is.na(Educ_Occup$Father_Occup[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]])) == F, 
                                                  Educ_Occup$Father_Occup[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]], NA) 
  
  AP_Youth_Survey_Merged$Self_Occup_Cat[i] = ifelse((is.na(Educ_Occup$Self_Occup[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]])) == F, 
                                                    Educ_Occup$Self_Occup[Educ_Occup$UID == AP_Youth_Survey_Merged$`_uuid`[i]], NA) 
  
  
}



#Models with non-demographic predictors ####
motorable_road <- ifelse(AP_Youth_Survey_Merged$Y_5 %in% c("Kuccha motorable", "Pucca motorable"), 1, 0)
closed_drainage <- ifelse(AP_Youth_Survey_Merged$Y_6 == "Pucca Closed", 1, 0)
rented_house <- ifelse(AP_Youth_Survey_Merged$Y_D_31 == "Yes", 1, 0)

labour_pref <- ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Happy with current work","Do not wish to work"),
                      "Status Quo", ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Work for a private company", "Work for a multinational corporation", "Work for a non-profit organization"),
                                           "Private Sector", ifelse(AP_Youth_Survey_Merged$Y_F_170 %in% c("Start your own business", "Work for family business"),
                                                             "Self/Family Business", ifelse(AP_Youth_Survey_Merged$Y_F_170 == "Work for the government/public sector",
                                                                                            "Public Sector", "No Response"))))

#Status quo, government, private or business

#Min. income threshold for job
#Move out of AP
y_movement <- ifelse(AP_Youth_Survey$Y_F_173 == "Yes" & AP_Youth_Survey$Y_F_176 == "Yes", 1, 0)

#Economic fortunes vs parents
#Ecuation level: completed for all, current for students

logit.fit <- glm(y ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                   caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + rented_house, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,1,1,0)  ## OC, Hindu, Employed, Big City, Male, unmarried, motorable road, closed drainage, own house

invlogit(logit.fit$coefficients %*% x)


#New outcome variables - 
#AP_Satisfaction
#Positive vs. Non Positive
y_satisfaction <- ifelse(AP_Youth_Survey_Merged$Y_G_187 %in% c("Much Better", "Somewhat Better"), 1, 0)


logit.fit <- glm(y_satisfaction ~ age + female + factor(prim_act) + factor(city_size) + rel_others + rel_muslim + rel_christ + caste_others + 
                   caste_sc + caste_st + caste_bc + ever_married + motorable_road + closed_drainage + rented_house, family=binomial(link = "logit")) 
summary(logit.fit)

logit.fit$coefficients

x <- c(1,25, 0, 0, 0, 0, 0,0,0,0,0,0,0,0,0,1,1,0)  ## OC, Hindu, Employed, Big City, Male, unmarried, motorable road, closed drainage, own house

invlogit(logit.fit$coefficients %*% x)

