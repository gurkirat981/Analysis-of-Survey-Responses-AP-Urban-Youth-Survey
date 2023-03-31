#New City Name Fixer File - 23rd Jan
library(data.table)
library(readxl)
library(writexl)

#Main Files ####
AP_Youth_Survey <- read_excel("youth_survey_responses (23rd Mar).xlsx")
AP_Household_Roster <- read_excel("Household Roster Youth Survey (23rd Mar).xlsx")
AP_Outmigration_Roster <- read_excel("Outmigration Roster Youth Survey (23rd Mar).xlsx")


#Codebooks ####
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- AP_Youth_Survey_Codebook$Variable_Name
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- c(AP_Household_Roster_Codebook$Variable_Name, "NAN")
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name

AP_Outmigration_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 3)
colnames(AP_Outmigration_Roster) <- AP_Outmigration_Roster_Codebook$Variable_Name
attr(AP_Outmigration_Roster, "variable.labels") <- AP_Outmigration_Roster_Codebook$Column_Name


#Fixing City Name for City Tables ####
v <- c("VIJAYAWADA", "VIJAYAWADa", "Vijawada", "Vijayay")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v, 
                                      "Vijayawada", AP_Youth_Survey$`City Name`)


v <- c("GVMC(VISAKHAPATNAM)", "Gvmc visakhapatnam", "GVMC (VISAKHAPATNAM)", "Gvmc Visakhapatnam",
       "GVMC visakhapatnam", "GVMC VISAKHAPATNAM", "GVMC VISHAKAPATNAM", "Gvmc(Visakhapatnam)",
       "GVMC(VISAKHAPATNAM)", "VISAKHAPATNAM", "GVMC ( VISAKHAPATNAM )", "GVMC (VISAKHAPATNAM )",
       "Gvmc visakapatnam", "GVMC Visakhapatnam", "GVMC Vishakhapatnam", "GVMC VISKHAPATNAM", 
       "GVMC(VISAKHAPATNAM,)", "Viskhapatnam GVMC", "Viskhapatnam (GVMC)", "Gvmc ( visakhapatnam )",
       "GVMC ( VISAKHAPATNAM)", "GVMC (Visakhapatnam )", "GVMC Visakapatnam", "GVMC VISAKAPATNAM",
       "GvMC Visakhapatnam", "Gvmc vishakapatnam", "GVMC Vishakapatnam", "Visakhaptnam", "GVMC",
       "GVMC (Visakhapatnam", "Gvmc Visakhaption", "Visakhapatnam GVMC", "Viskhapatnam", "GVMC  Visakhapatnam",
       "GVMC ( Visakhapatnam )", "GVMC (VISAKHAPATNAM)\n", "GVMC(VISAKHAPATNAM )", "Gvms visakhapatnam", "Gvmc")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Visakhapatnam", AP_Youth_Survey$`City Name`)

v <- c("Tirupathi", "tirupati", "Tirupati", "TIRUPATI", "1012")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tirupati", AP_Youth_Survey$`City Name`)



v <- c("Tadipatri", "TADIPATRI", "TADIPARI", "Tadipari", "1007")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tadipatri", AP_Youth_Survey$`City Name`)


v <- c("KURNOOL", "Kurnool", "kurnool", "Kurnnol", "1016", "Kurnool\n")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kurnool", AP_Youth_Survey$`City Name`)

v <- c("Vaddilapeta", "Kakinada", "KAKINADA", "Kakinda")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kakinada", AP_Youth_Survey$`City Name`)


v <- c("NELLORE", "Nellore", "21031019", "NELLLORE", "1031", "Nellor")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Nellore", AP_Youth_Survey$`City Name`)


v <- c("Rayadurgh", "Rayadurg", "Rayadurgham")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rayadurga", AP_Youth_Survey$`City Name`)


v <- c("KADIRI", "Kadiri", "\nKadiri", "1005")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kadiri", AP_Youth_Survey$`City Name`)


v <- c("Hindupur", "Hindhupur", "Himdupur", "Hi", "Hinduput")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Hindupur", AP_Youth_Survey$`City Name`)


v <- c("GUNTUR", "Guntur", "Nalandanagar", "1024", "Gunturu", "Rajivgandhinagar-2", "Reddypalem-05",
       "GUITAR", "Gunter", "GUTUR")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Guntur", AP_Youth_Survey$`City Name`)

v <- c("ELURU", "Eluru")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Eluru", AP_Youth_Survey$`City Name`)

v <- c("Adoni", "ADONI", "ADINI", "1015")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Adoni", AP_Youth_Survey$`City Name`)

v <- c("Rajahmundry", "Rajamhundry", "Rajamundry", "RAJAHMUNDRY", "T.rakesh", "రాజముండ్రి")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Rajahmundry", AP_Youth_Survey$`City Name`)

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "KADAPA" | AP_Youth_Survey$`City Name` == "R NARASIMHA SAPTAGIRI" |
                                      AP_Youth_Survey$`City Name` ==  "Kadappa",
                                      "Kadapa", AP_Youth_Survey$`City Name`)


AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "Nidadavole",
                                      "Nidadavolu", AP_Youth_Survey$`City Name`)

v <- c("PEDDHAPURAM", "PEDDAPURAM", "Pedhapuram")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Peddapuram", AP_Youth_Survey$`City Name`)


v <- c("Kondapalli", "KONDAPALLI", "Kondepalli", "Kindapalli")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Kondapalli", AP_Youth_Survey$`City Name`)


v <- c("Tenali", "TENALI", "Tenli")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Tenali", AP_Youth_Survey$`City Name`)


v <- c("Narasaraopeta", "NARASARAOPETA", "Narasaropeta", "NARASARAOPET", "NARADARAOPETA")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Narasaraopet", AP_Youth_Survey$`City Name`)


v <- c("Mandapaeta", "Mandapet", "MANDAPET", "MANDAPETA", "Mandapeta")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Mandapeta", AP_Youth_Survey$`City Name`)


v <- c("SULLURPETA", "Sullurupeata", "Sullurupeta", "SULLURUPETA", "Sulurupeta")

AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` %in% v,
                                      "Sullurupeta", AP_Youth_Survey$`City Name`)


AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "TIRUVURU",
                                      "Tiruvuru", AP_Youth_Survey$`City Name`)


AP_Youth_Survey$`City Name` <- ifelse(AP_Youth_Survey$`City Name` == "KUPPAM",
                                      "Kuppam", AP_Youth_Survey$`City Name`)

AP_Youth_Survey <- AP_Youth_Survey[AP_Youth_Survey$`City Name` != "N",]

write_xlsx(AP_Youth_Survey, "youth_survey_responses (23rd Mar).xlsx")

City_Wise_Numbers <- as.data.frame(table(AP_Youth_Survey$`City Name`)); colnames(City_Wise_Numbers) <- c("City Name", "Responses")

write_xlsx(City_Wise_Numbers, "AP Citywise Survey Responses.xlsx")

#Adding City Size Class for Analysis
#According to our data - The cities should be classified as follows

Small <- c("Kadiri", "Peddapuram", "Rayadurga", "Nidadavolu", "Kavali","Kondapalli", 
           "Sullurupeta", "Kuppam", "Mandapeta", "Tiruvuru") #Less than 1 Lakh
Medium <- c("Adoni", "Eluru", "Hindupur", "Kadapa", "Kakinada", "Narasaraopet", "Rajahmundry",
            "Tadipatri", "Tenali", "Tirupati") #1-4 Lakhs
Large <- c("Guntur", "Visakhapatnam", "Kurnool", "Nellore", "Vijayawada") #More than 4 Lakhs



AP_Youth_Survey$City_Size_Class <- ifelse(AP_Youth_Survey$`City Name`%in% Small, "Small",
                                          ifelse(AP_Youth_Survey$`City Name` %in% Medium, "Medium",
                                                 ifelse(AP_Youth_Survey$`City Name` %in% Large, "Large",
                                                        "NAN")))


#Roster and Main Survey merge ####
#Merge only for those respondents who have a self
#Non Self data file
v <- AP_Youth_Survey$`_uuid`[(AP_Youth_Survey$`_uuid` %in% AP_Household_Roster$`_submission__uuid`[AP_Household_Roster$H_1 == "Self"]) == F]

AP_Household_Roster$H_1[AP_Household_Roster$`_submission__uuid` %in% v]

AP_Household_NonSelf <- AP_Household_Roster[AP_Household_Roster$`_submission__uuid` %in% v,]
AP_Household_NonSelf <- AP_Household_NonSelf[AP_Household_NonSelf$Age > 18,]


for(i in 1:nrow(AP_Household_NonSelf)) {
  
  AP_Household_NonSelf[i,c(39:50)] = AP_Youth_Survey[AP_Youth_Survey$`_uuid` == AP_Household_NonSelf$`_submission__uuid`[i], c(1:12)]
  
}


write_xlsx(AP_Household_NonSelf, "AP_Household_NonSelf_Responses.xlsx")


#Main Survey and Roster merged
AP_Youth_Survey_Merged <- merge(AP_Youth_Survey, AP_Household_Roster[AP_Household_Roster$H_1 == "Self",], by.x = c("_uuid"), by.y = c("_submission__uuid"))
attr(AP_Youth_Survey_Merged, "variable.labels") <- c(AP_Youth_Survey_Codebook$Column_Name, "City Size Class", AP_Household_Roster_Codebook$Column_Name[AP_Household_Roster_Codebook$Column_Name != "_submission__uuid"], "NAN")

write_xlsx(AP_Youth_Survey_Merged, "AP_YouthSurvey_Roster_Merged (Mar 23rd).xlsx")


#Tables to be created - A (Skilling Response Levels) ####

vec <- c(119,121, 122, 123, 125, 127, 129, 130, 132, 133, 137, 138, 139, 150,151,152,153, 154, 156,158, 160, 162, 163, 165, 166, 167, 168, 169, 170, 174, 175, 176)

a <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec])

No_Resp <- Tot_Resp <- rep(NA, nrow(a))




for (i in 1:nrow(a)) {
  
  t <- as.data.frame(table(AP_Youth_Survey[,a$Col_No[i]] == "No Response"))
  No_Resp[i] <- sum(t$Freq, na.rm = T) - t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
}


a <- cbind.data.frame(a, No_Resp, Tot_Resp)
a$Non_Resp_Rate = round(100*a$No_Resp/a$Tot_Resp, 2)

#Table B - Youth section non-responses - ####

vec <- c(182:201)

#Add welfare matters - who do they hold responsible

b <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec])

No_Resp <- Tot_Resp <- rep(NA, nrow(b))




for (i in 1:nrow(b)) {
  
  t <- as.data.frame(table(AP_Youth_Survey[,b$Col_No[i]] == "No Response"))
  No_Resp[i] <- sum(t$Freq, na.rm = T) - t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
}


b <- cbind.data.frame(b, No_Resp, Tot_Resp)
b$Non_Resp_Rate = round(100*b$No_Resp/b$Tot_Resp, 2)

b <- b[b$Tot_Resp >= 2395,]


#Table C - Scheme based responses ####
#Scheme-Wise Receipts
Neg_Resp <- c("No", "Don't Know", "Don't know")

vec <- c(62, 67, 72, 77, 82) + 1

c <- cbind.data.frame(Col_No = as.numeric(vec), Var_Codes = colnames(AP_Youth_Survey_Merged)[vec], Q_Name = AP_Youth_Survey_Codebook$Column_Name[vec - 1])

Yes_Resp <- Tot_Resp <- rep(NA, length(vec))

for (i in 1:nrow(c)) {
  
  t <- as.data.frame(table(AP_Youth_Survey_Merged[,c$Col_No[i]] %in% Neg_Resp))
  Yes_Resp[i] <- t$Freq[t$Var1 == F]
  Tot_Resp[i] <- sum(t$Freq, na.rm = T)
  
  
}

c <- cbind.data.frame(c, Yes_Resp, Tot_Resp)
c$Benefit_Rate = round(100*c$Yes_Resp/c$Tot_Resp, 2)


#Combining Skill Question Columns 
#In main survey
AP_Youth_Survey$YR_F_87 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_87,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_123, AP_Youth_Survey$Y_F_160))

AP_Youth_Survey$YR_F_92 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_92,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_128, AP_Youth_Survey$Y_F_165))

AP_Youth_Survey$YR_F_94 <- ifelse(AP_Youth_Survey$Y_F_81 == "Student", AP_Youth_Survey$Y_F_94,
                                  ifelse(AP_Youth_Survey$Y_F_81 == "Employed", AP_Youth_Survey$Y_F_130, AP_Youth_Survey$Y_F_167))


#Code for region wise coding
Coastal_North <- c("Kakinada", "Peddapuram", "Rajahmundry", "Eluru", "Visakhapatnam", "Nidadavolu", "Mandapeta")
Coastal_South <- c("Guntur", "Narasaraopet", "Tenali", "Kavali", "Nellore", "Vijayawada", "Kondapalli", "Tiruvuru")
Inland_South <- c("Hindupur", "Kadiri", "Rayadurga", "Tirupati","Adoni", "Kurnool", "Tadipatri", "Kadapa",
                  "Kuppam", "Sullurupeta")


AP_Youth_Survey$City_Geo_Class <- ifelse(AP_Youth_Survey$`City Name`%in% Coastal_North, "Coastal_North",
                                         ifelse(AP_Youth_Survey$`City Name` %in% Coastal_South, "Coastal_South",
                                                ifelse(AP_Youth_Survey$`City Name` %in% Inland_South, "Inland_South",
                                                       "NAN")))

