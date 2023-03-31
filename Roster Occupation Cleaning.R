library(data.table)
library(readxl)
library(writexl)
library(dplyr)
library(MASS)
library(kableExtra)
library(foreign)

AP_Household_Roster <- read_excel("Household Roster Youth Survey (12th Mar).xlsx")

AP_Household_Roster_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx", sheet = 2)
colnames(AP_Household_Roster) <- c(AP_Household_Roster_Codebook$Variable_Name,"NAN")
attr(AP_Household_Roster, "variable.labels") <- AP_Household_Roster_Codebook$Column_Name

table(AP_Household_Roster$H_7[AP_Household_Roster$H_6 == "Unpaid Family Worker"])


AP_Youth_Survey <- read_excel("youth_survey_responses (12th Mar).xlsx")
AP_Youth_Survey_Codebook <- read_excel("AP_Youth_Survey_Codebook.xlsx")
colnames(AP_Youth_Survey) <- c(AP_Youth_Survey_Codebook$Variable_Name, "NAN")
attr(AP_Youth_Survey, "variable.labels") <- AP_Youth_Survey_Codebook$Column_Name

AP_Household_Roster$HR_7 <- ifelse(is.na(AP_Household_Roster$H_7) == F, AP_Household_Roster$H_7, 0)

AP_Household_Roster$HR_7 <- tolower(AP_Household_Roster$HR_7)
AP_Household_Roster$HR_7 <- gsub(" ", "", AP_Household_Roster$HR_7)

#COMMON METHOD - We are only cleaning specific categories such as Others here and collapsing them into existing ones
#While there are known inconsistencies in the data, in the interest of time, we will have to work with just a cleaned up bunch of others

#H_6/HR_6 - Primary Activity####
#Cleaning Primary Activity H_6, Reclassifying Unpaid Family Workers on the basis of response to
#Description of Occupation H_11


AP_Household_Roster$HR_6 <- ifelse(is.na(AP_Household_Roster$H_6) == F, AP_Household_Roster$H_6, 0)


#Domestic, Home, House

Home_Vec <- c("Home|Domestic|House|Hiuse|Hiusewife|Wife|Maker|Hous")

for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_6[i] == "Unpaid Family Worker") {
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Home_Vec, AP_Household_Roster$H_11[i], ignore.case = T, fixed = F) == T, 
                                          "Attending Domestic Duties", AP_Household_Roster$HR_6[i])
    
    
  } 
  
}

A <- as.data.frame(table(AP_Household_Roster$H_11[AP_Household_Roster$HR_6 == "Unpaid Family Worker"]))


Child <- c("childern|childran|chilf|kg|kage|child|children|పిల్లలు|yearsolb|yearsold|ageisverylow|belowschoolage|chiled|ciled|chaild|baby|child|kid|bebi|boy|bybi|notjoinedinschool|childean")

Home_Vec2 <- c("howework|homemaker|hauswife|wife|housewife|homemaker|housewife|homeworker|houswife")
Unemployed <- c("nowork|noworking|unemployment|notworking|none|unemployees|unemploye|notwork|searching|seek|unemp|unemployed|athome|lookingforjob|nonemployed")
Reg_Sal <- c("wordboy|admin|government|municipal|sachivalayam|salesman|software|hospital|inmall|bank|salesman|incharge|volantry|nteer|asha|Workingaspaintingshop|paintshopworker|teachear|teacher|anm|clerk|contract work|railwaycontract|workinginshop|workinginphotographerinshop|reliance|securitygaurd|watchman|watchmen|satizationinvmc|sacurity|teacher|sachivalayam|billcollecter|pwd|previtecompany|volunteer|anganwadi|teacher|manager|contractwork|husbandary|generalstore|employee|ricemillworker|workfromhome|privatejob|workinginhospital|watchman|pgtteachear|municipalworker|contractclerk|volunteer|software|anmworker|railwaycontract|wardvolunteer|hrdepartment|sacurity|teacher|volantry|ricemillworker|accountant|security|securityguard|doctor|cookingfield|banking|officer|sweeping|valenter|volenteer|jeriwork|varun|systemoperator|train|sachivalayam|government|phdresearchinunitedkingdom|workinbank|preprimary|preschool|assistant|accountant|millworker|railway|jobholder|medical|nursing|police|maneger|cashier|housekeeping|hotelmanagement")
Self_Emp <- c("driving|agricultural|agriculture|farmer|farmmer|driver|cardrivaer|business|fishing|fisher|own|poojari|priest|teashop|trading|gold|electric|carpenter|freelancing|mechanic|construction|masonworker|builder|generalstore|rationshop|real-estate|stechingshop|shop|buildinghouse|technician|clothiaranig|bussnes|pastor|bikesremodel|cooking|weldingwork|workingasagriculture|coaching|ricebusiness|realestate|vendor|licagent|tailor|tailoring|tiffenshop|political|ownedtextiles|vender|catering|pooja|selfemployed|claywork|pujari|tailering|fisherman|press|iron|mecanic|dealer|services|transportation|welder|fancystore|artist")
Cas_Lab <- c("constructionworker|daily|labor|labour|painter|painting|labourer|kooli|kulli|farmingatothersland|mirchiyardworker|cooli")
Retired <- c("retarderd|retardant|health|helath|disabled|oldperson|aged|mental|oldage|oldage|oldwom|ldp|physical|handicapped|healthproblem|healthissue|patient|medicallyunfit|hadicaped|old women|pwd|mentally|piyically")
Student <- c("student|study")
Others <- c("oldwomen60years|wardboyinhospital|voulanterinngocolony02sachivalayam|notworkingsmallkid|accountantathometownfromhome|heisdoingnowork.athome|noworkbecauseofphysicallyhandicapped|noworkbecausephysicallyhandicapped|noworkduetoagefactor|noworkduetohelathissuses|sweepingathome|notworking(accident)|cardriving(driver)|medicallyunfittodowor|thisispwd(polio)candidate|unemployee|unemployees|chiefatcookingfield|sheisstayinginownwithoutanywork|ricevehicledriverundergovernment|workingaspaintingshop|childaged1year|marriagedeclaration|pwd")


for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_6[i] == "Others") {
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Child, AP_Household_Roster$HR_7[i]) == T, 
                                          "Child", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Home_Vec2, AP_Household_Roster$HR_7[i]) == T, 
                                          "Attending Domestic Duties", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Unemployed, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Unemployed but Seeking Work", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Reg_Sal, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Regular Salaried", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Self_Emp, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Self-Employed", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Cas_Lab, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Casual/Daily Labour", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Retired, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Retired", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Student, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Student", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Others, AP_Household_Roster$HR_7[i]) == T, 
                                          "Others", AP_Household_Roster$HR_6[i])
    
    
  }
  
  
  
  
}


#Final Round of Cleaning - If Needed####
Child <- c("notworkingsmallkid")

Home_Vec2 <- c("sweepingathome")
Unemployed <- c("unemployee|unemployees")
Reg_Sal <- c("wardboyinhospital|voulanterinngocolony02sachivalayam|accountantathometownfromhome|cardriving(driver)")
Self_Emp <- c(0)
Cas_Lab <- c(0)
Retired <- c("oldwomen60years|noworkduetoagefactor")
Student <- c(00)
Others <- c("heisdoingnowork.athome|noworkbecauseofphysicallyhandicapped|noworkbecausephysicallyhandicapped|noworkduetohelathissuses|notworking(accident)|medicallyunfittodowor|thisispwd(polio)candidate|chiefatcookingfield|sheisstayinginownwithoutanywork|ricevehicledriverundergovernment|workingaspaintingshop|childaged1year|marriagedeclaration|pwd")


for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_6[i] == "Others") {
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Child, AP_Household_Roster$HR_7[i]) == T, 
                                          "Child", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Home_Vec2, AP_Household_Roster$HR_7[i]) == T, 
                                          "Attending Domestic Duties", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Unemployed, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Unemployed but Seeking Work", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Reg_Sal, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Regular Salaried", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Self_Emp, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Self-Employed", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Cas_Lab, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Casual/Daily Labour", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Retired, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Retired", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Student, AP_Household_Roster$HR_7[i], ignore.case = T, fixed = F) == T, 
                                          "Student", AP_Household_Roster$HR_6[i])
    
    AP_Household_Roster$HR_6[i] <- ifelse(grepl(Others, AP_Household_Roster$HR_7[i]) == T, 
                                          "Others", AP_Household_Roster$HR_6[i])
    
    
  }
  
  
  
  
}

AP_Household_Roster$HR_6 = ifelse(AP_Household_Roster$HR_6 == 0, NA, AP_Household_Roster$HR_6)

table(AP_Household_Roster$H_6)
table(AP_Household_Roster$HR_6)

#H_8/HR_8 - Industry/Sector####
AP_Household_Roster$HR_9 <- ifelse(is.na(AP_Household_Roster$H_9) == F, AP_Household_Roster$H_9, 0)

AP_Household_Roster$HR_9 <- tolower(AP_Household_Roster$HR_9)
AP_Household_Roster$HR_9 <- gsub(" ", "", AP_Household_Roster$HR_9)


AP_Household_Roster$HR_8 <- ifelse(is.na(AP_Household_Roster$H_8) == F, AP_Household_Roster$H_8, 0)

Home_Makers <- c("housewife|wife|hiuswife|household|domestic")
Anganwadi <- c("angan|wadi|vadi|asha|aasha|aya")
Transport <- c("auto|bus|driv|riks|vehic|transport|apsrtc|rtc|rto|carr|lorry")
Services <- c("gov|gvmc|vol|gsws|supervisor|secret|sachiv|valent|teer|tear|police|railway|municipal|munisipal|panchayat|department|clark|clerk|train|security|watch|commun|priest|scientist|civil|staf|stuf|salo|beauty|clean|wash|charge|cat|depart|sanit|amazon|swiggy|zomato|office|law|artist|liter|parcel|public|fashion|cook|maid|garden|clean|recep|desig|barb|petr|social|aqua|resea|army|advoc|packer|ironing|milit|consul|gig|courier|sweep|bsnl|jour|cast|paperwork|temple|prie|flipkart|custom|guard|event|assis|beau|hair|elect|station|amej|arce")
Trade_Hotels <- c("sales|shop|kirana|business|agenc|store|mall|canteen|warehouse|gold|goald|silver|jewel|showroom|own|market|sell|agency|trad|supplier|xerox|photo|electric|laundry|parlour|pharma|hotel|photog|acces|sticker|sari|saree|distr|butor|studio|timber|tatto|export|show|tred|ration|baker|cloth|achar|comme|book|medicalrep")
Vendors <- c("fruit|tiffen|tiffin|food|panipuri|drink|tea|sweet|flower|pan|pickle|vend|hawk|bang|puri")
Factory <- c("steel|plant|automobile|motor|mill|scrap|manuf|factory|machin|brick|battery|well|plast|mak|smith|trici|rub|loom|garment|indus|metal")
Construction <- c("mas|mest|cons|build|civil|ement|tile|wir|marb|paint|crane|mansion")
Finance_Real_Est <- c("bank|fin|propert|insurance|tax|cashier|account|stock|flat|bharatpay|brok|ibm|tcs|infos|real|contr")
Crafts_Technical <- c("tail|taylor|textile|weav|furniture|stich|wood|weld|maker|carpen|corpen|glass|silk|scrap|flam|gann|plumber|electri|mec|techni|repair|technic|tronic|board|ply|tric")
IT <- c("it|info|software|multi|customerservice|analyst|data|programmer|engine|artifi|autom|robot|tech|syst|develop|hcl|concentrix|cognizant|compu|saft")
Educ_Health <- c("hela|teach|school|nurse|doctor|hosp|college|helth|health|professor|lecturer|ayurvedic|education|hostel|medic|master|facult")
Agri <- c("farm|agri|fish|dairy|cattle|animal")


Vec <- rbind(Home_Makers, Anganwadi, Transport, Services, Trade_Hotels, Vendors, Factory, Construction, 
             Finance_Real_Est, Crafts_Technical, IT, Educ_Health, Agri)

names <- c("Attending Domestic Duties", "Anganwadi", "Transport", "Services", "Trade and Hotels", "Vendors",
           "Factory and Manufacturing", "Construction", "Finance and Real Estate", "Crafts and Technical Work",
           "Information Technology", "Education and Health", "Agriculture")


for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$HR_8[i] == "Others") {
    
    for (j in 1:length(Vec)) {
    
    AP_Household_Roster$HR_8[i] <- ifelse(grepl(Vec[j], AP_Household_Roster$HR_9[i]) == T, 
                                           names[j], AP_Household_Roster$HR_8[i])
    
  }
  
  }
  
}


#Collapsing and consolidating categories
AP_Household_Roster$HR_8 <- ifelse(AP_Household_Roster$HR_8 %in% c("Anganwadi", "Services"), 
                                               "Other Service Workers", AP_Household_Roster$HR_8)

vec <- c("Agriculture", "Domestic", "Education", "Manufacturing", "Real Estate", "Health","Hotels",
         "Information Technology" ,"Service", "Trading", "Transport")
final_names <- c("Agriculture", "Attending Domestic Duties", "Education and Health", "Manufacturing","Finance and Real Estate", "Education and Health","Trade and Hotels",
                 "Information Technology", "Other Service Workers", "Vendors", "Transport")


for (i in 1:nrow(AP_Household_Roster)) {
  
  for (j in 1:length(vec)) {
    
    AP_Household_Roster$HR_8[i] <- ifelse(grepl(vec[j], AP_Household_Roster$HR_8[i], ignore.case = T, fixed = F) == T,
                                                   final_names[j], AP_Household_Roster$HR_8[i])
    
  }
  
}

AP_Household_Roster$HR_8 = ifelse(AP_Household_Roster$HR_8 == 0, NA, AP_Household_Roster$HR_8)

as.data.frame(table(AP_Household_Roster$H_8))
as.data.frame(table(AP_Household_Roster$HR_8))

#H_10/HR_10/Occup_Cat_Level####
#Cleaning H_10 others on the basis of H_11 description
AP_Household_Roster$HR_10 <- ifelse(is.na(AP_Household_Roster$H_10) == F, AP_Household_Roster$H_10, 0)


Doctor  <- c(0)
Engineer <- c("Software|Soft ware|Web developed")
Freelancer  <- c("Swiggy|Zomato")
Manager  <- c("manager")
Nurse  <- c("Ward boy|Working in hospital")
Officer  <- c("officer|GVMC")
Other_service_workers <- c("Security|Cleark|Sanitary|Sanitory|Sanation|Sanitation|Sweeper|Watchman|Watchmen|Sales|Sells|delivery|cook|coocking|food|buty|anganwadi|asha|shop worker|security|driver|sachivalayam|attender|assistant")
Owner_Home_based <- c("home|maker|house|Housr")
Owner_Non_home_based <- c("Tea shop|Tea.stall|Tea maker|Tailor|Tailer|Tailar|Taylor|Tylor|Weaver|Sweet|auto driving|auto driver|auto rickshaw|autodriver|autu|dry cleaner|barber|barbar|barbr|dhobhi|b dhobi|busines|bussiness|marchant|vendor|vender|Chiken shop|Fancy store|Fansy store|Owner|Flower shhop|Fruit Sales|Footwear shop|farming|farm|agriculture|dry cleaner|barber|barbar|barbr|Business|dhobhi|b dhobi|busines|laundry|Laundry|merchant|bussiness|marchant|vendor|vender")
Professionals <- c("account|acount|advocate|finance|scientist|book keeper|ca|constable|Lecturer|manager|journalist|Journalist")
Skilled_Construction_workers <- c(0)
Supervisors <- c("Supervisor|Admin|Valanteer|Valunter|Vaulanteer|Volunteer|Volenteer|Voulanter|Voulenter|valunteer|vollenters")
Teacher <- c("Teacher|Teaching")
Technician <- c("electri|data|computer|mechanic|contractor|carpenter|corpenter|clerk|clarke|technician|electri|data|craft|smith|Gold|factory|computer|mechanic|Mechanic|Mechanical|mechanical|contractor|carpenter|corpenter|repair|Repair|clerk|clarke|technician|operator")
Unskilled_labourer <- c("daily|labour|cooli")

Vec <- rbind(Doctor, Engineer, Freelancer, Manager, Nurse, Officer, Other_service_workers, Owner_Home_based, Owner_Non_home_based,
             Professionals, Skilled_Construction_workers, Supervisors, Teacher, Technician, Unskilled_labourer)

names <- c("Doctor" , "Engineer","Freelancer","Manager","Nurse","Officer","Other service workers (clerks, shop assistants etc.)",
           "Owner: Home based","Owner: Non-home based","Professionals","Skilled Construction workers", "Supervisors (service workers like ANMs)",
           "Teacher","Technician","Unskilled labourer")



for (i in 1:nrow(AP_Household_Roster)) {
    
  if(AP_Household_Roster$HR_10[i] == "Others") {
    
    for (j in 1:length(Vec)) {
      
      AP_Household_Roster$HR_10[i] <- ifelse(grepl(Vec[j], AP_Household_Roster$H_11[i], ignore.case = T, fixed = F) == T, 
                                             names[j], AP_Household_Roster$HR_10[i])
      
    }
    
  }
  
}

#Movement Check
x <- as.data.frame(table(AP_Household_Roster$H_10, AP_Household_Roster$HR_10)); x <- x[x$Var2 != 0,]
x$Var1 <- as.character(x$Var1); x$Var2 <- as.character(x$Var2)
x$check <- ifelse(x$Var1 == x$Var2, 0,1)




as.data.frame(table(AP_Household_Roster$H_10))
as.data.frame(table(AP_Household_Roster$HR_10))






#Replace 0s with NAs when done


#Final Collapsed Categorisation - 
#A - Doctor, Engineer, Teacher, Manager, Officer, Professional - Highly Skilled
#B - Technician, Skilled Const Workers, Nurses, Supervisors, Other service workers, Freelancers 
#C - Unskilled Labour
#O - Owner: HB + NHB
#E - Others

#General collapse based on HR_10
A <- c("Doctor", "Engineer", "Manager", "Officer", "Professionals", "Teacher")
B <- c("Freelancer", "Technician", "Skilled Construction workers", "Nurse", 
       "Supervisors (service workers like ANMs)","Other service workers (clerks, shop assistants etc.)")
O <- c("Owner: Home based", "Owner: Non-home based")

AP_Household_Roster$Occup_Cat_Level <- ifelse(AP_Household_Roster$HR_10 %in% A, "A",
                                              ifelse(AP_Household_Roster$HR_10 %in% B, "B",
                                                     ifelse(AP_Household_Roster$HR_10 == "Unskilled labourer", "C",
                                                            ifelse(AP_Household_Roster$HR_10 %in% O, "O",
                                                                   ifelse(AP_Household_Roster$HR_10 == "Others", "E", -1)))))

AP_Household_Roster$HR_10 = ifelse(AP_Household_Roster$HR_10 == 0, NA, AP_Household_Roster$HR_10)

#Final Cleanup for Occup_Cat_Level, using H_11
A <- c("Analyst|Ast Director|Engineer|Engineering|Enginner|Head of the own school|It sector|IT sector|Java|Marketing Field|Physiotheraphy|Robotic automation|System operater|it |developer|BPO |Works at Bharat pay|Consultant in social welfare|Junior lawyer|Insurance agent|Lynux system administrator|Marketing|Pharmacist|Software Administrator ,|System Admin|System Administrator|Exicutive|HR|Human Resource|Exicutive|HR|Human Resource|Assistant Engineer|Sales department in phone pe|Sales & Excutive|Sales co ordinator|Sales engineering|Sales Excutive|Sales executive\n|Sales executive in concentrix|Sales executive in private company|civil engineer|site engeneer|event manager|HR manager|account|accounts|dentist|data analyst")

B <- c("Aaya in Hospital management|Being resturant to saw the conditions in restaurant|Body Guard|Corpentoor work|Corpentor|Corpentry|Darmal power plant healper|Delevery boy|Election|Junior assistant in agriculture department|Lab techneshion|Laundry worker in hospital|Machnicik|Mechanik|Miday meels|Municipal|Paint|Painter|Painting|Panchayat secretary|Plumbing|Private employee|Railway Employees|Recovery executive at tadipatri|Rice mill worker|Servant in a hotel|Shipping in merchant Navy|Soldier|Tv machanic|TVS Showroom work|volantri|Worker |workers|Working as a construction on buildings|Working as a document writer|Working as employee in private bank|Working at construction site|Working in a private hotel|Working in construction|Working in hotel|Working in coart|Works as jr assistance in municipal office\n|Works as petrol bunk woker|Yoga trainer|Works as master in hotel|Photographer|Photography|Aganvadi|Anganavadi|Anganvadi|Anganwadi|Gim trainer|Construction field|Constructs a building|Builder|volantr|Machin|Machinar|Mekanik|Shop|Kirana marketing|Mi store|Officer to mall|Product seller|Voluntary|anganwadi|aganwadi|auditor|Gig work|services|corpentar|corpentor|corpentoor|electrician|electreesian|bank|worker|driver|auto|bags carries|BPO|cab driver|driving|cashier|constable|corporator|call centre|customer care")

C <- c("Steel plant lobour|Steel plant lobon| Steel plant\n|Cooly|Helping to his mother in the vegetable shop|Cleans the roads|Kirana shop worker|Clothe store work|Clothes shop workr|Helper in a bakery|Helper in D Mart|Helper in Petrol Bunk|Helper in Footwear Manufacturing|Hospital sweeper|Hostel worker|Hotel servant|Petty Shop|Road side worker|Sweeper|watchman|watch man| worker| working|mali|Amali|attender|sweeping|cleaners|cleaning|helper|cleaning|keeping|bike mecanic|book keeper|macanic|canteen worker|labor|field|construction|cycle shop|fitter|helper|gig work|site|side")

O <- c("General store |Hair saloon |Real Estate|Kirana|Machanicle shop|General store|Cloth shop|Ration dealer|Shop keeper|Shop Keeper|farmers|cattle|cart|sugarcane business|business|shop|own|owner")

E <- c("Church priest|Priest|domestic work|physically challenged|physicallly handicapped|physically")

Vec <- rbind(A,B,C,O,E); names <- rownames(Vec)




for (i in 1:nrow(AP_Household_Roster)) {
  
  if(AP_Household_Roster$Occup_Cat_Level[i] == "E")
  
  for (j in 1:length(Vec)) {
    
    AP_Household_Roster$Occup_Cat_Level[i] <- ifelse(grepl(Vec[j], AP_Household_Roster$H_11[i], ignore.case = T, fixed = F) == T, 
                                           names[j], AP_Household_Roster$Occup_Cat_Level[i])
    
  }
  
  
  
}

AP_Household_Roster$Occup_Cat_Level = ifelse(AP_Household_Roster$Occup_Cat_Level == -1, NA, AP_Household_Roster$Occup_Cat_Level)



write_xlsx(AP_Household_Roster, "HH_Roster_Cleaning.xlsx")
