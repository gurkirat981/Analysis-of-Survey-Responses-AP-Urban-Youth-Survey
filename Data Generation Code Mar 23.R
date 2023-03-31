#Final file to merge all 4 surveys
library(readxl)
library(writexl)

#Main Survey Data Generation ####
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-02-01-15-25-49.xlsx")
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-37.xlsx")

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-49-19.xlsx")

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:213)

ys_responses_1 <- ys_responses_1[,-205]

youth_survey_responses <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(youth_survey_responses)

#Add Clone file Responses
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-10.xlsx")

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:213)

youth_survey_responses <- rbind(youth_survey_responses, ys_responses_5)


ys_responses_6 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Social_Audit_test_-_all_versions_-_English_en_-_2023-03-24-03-49-42.xlsx")

ys6 <- colnames(ys_responses_6)

match(ys4,ys6) %in% c(1:213)

ys_responses_6$`_version__001` <- ys_responses_6$`_version_` <- ys_responses_6$`__version__` <- NA

ys_responses_6 <- ys_responses_6[,c(1:201,211:213,202:210)]

ys6 <- colnames(ys_responses_6)

a <- as.data.frame(cbind(ys6,ys4, ys6 == ys4))

youth_survey_responses <- rbind(youth_survey_responses, ys_responses_6)
  
write_xlsx(youth_survey_responses, "youth_survey_responses (23rd Mar).xlsx")



#Roster Data Generation ####

#Household Roster
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-02-01-15-25-49.xlsx", sheet = 2)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-37.xlsx", sheet = 2)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-49-19.xlsx", sheet = 2)

ys_responses_1$`_submission___version__` = NA; ys_responses_1 <- ys_responses_1[,c(1:37,39,38)]

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:38)

a <- as.data.frame(cbind(ys1,ys2,ys3))


Household_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Household_Roster)


#Clone File Rosters
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-10.xlsx", sheet = 2)

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:38)

Household_Roster <- rbind(Household_Roster, ys_responses_5)


ys_responses_6 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Social_Audit_test_-_all_versions_-_English_en_-_2023-03-24-03-49-42.xlsx", sheet = 2)

ys6 <- colnames(ys_responses_6)

match(ys4,ys6) %in% c(1:38)

a <- as.data.frame(cbind(ys6,ys4, ys6 == ys4))

Household_Roster <- rbind(Household_Roster, ys_responses_6)


write_xlsx(Household_Roster, "Household Roster Youth Survey (23rd Mar).xlsx")



#Outmigration Roster ####
ys_responses_1 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Andhra_Pradesh_Translated_30thNov_-_all_versions_-_English_en_-_2023-02-01-15-25-49.xlsx", sheet = 3)
ys_responses_2 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-37.xlsx", sheet = 3)

ys_responses_3 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_AU_21stDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-49-19.xlsx", sheet = 3)

ys_responses_1$`_submission___version__` = NA; ys_responses_1 <- ys_responses_1[,c(1:17,19,18)]

ys1 <- colnames(ys_responses_1)
ys2 <- colnames(ys_responses_2)
ys3 <- colnames(ys_responses_3)

match(ys2,ys1) %in% c(1:19)

Outmigration_Roster <- rbind(ys_responses_1, ys_responses_2, ys_responses_3)

ys4 <- colnames(Outmigration_Roster)


#Clone File Rosters
ys_responses_5 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Clone_of_Youth_Survey_ANU_20thDec_Translated_-_all_versions_-_English_en_-_2023-03-24-03-48-10.xlsx", sheet = 3)

ys5 <- colnames(ys_responses_5)

match(ys5,ys4) %in% c(1:38)

Outmigration_Roster <- rbind(Outmigration_Roster, ys_responses_5)

ys_responses_6 <- read_excel("/Users/vaibhav/Documents/CPR\ -\ Work/AP\ Project/Youth_Survey_Social_Audit_test_-_all_versions_-_English_en_-_2023-03-24-03-49-42.xlsx", sheet = 3)

ys6 <- colnames(ys_responses_6)

a <- as.data.frame(cbind(ys6,ys4, ys6 == ys4))

Outmigration_Roster <- rbind(Outmigration_Roster, ys_responses_6)


write_xlsx(Outmigration_Roster, "Outmigration Roster Youth Survey (23rd Mar).xlsx")
