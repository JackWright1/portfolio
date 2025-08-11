salesforce_case <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\SalesForce_Case.csv")
install.packages("tidyverse")
library(tidyverse)

## preliminary look at the data
glimpse(salesforce_case)
summary(salesforce_case)

## since variables with too many blanks were removed we can begin to clean categorical variables
salesforce_case %>%
  count(Status)
## clean (mostly reclassify) status variable
salesforce_case <- salesforce_case %>% 
  mutate(Status = ifelse(Status %in% c("Client Unresponsive", "Missed", "Missed-Unresponsive To Outreach"),
                         "Client Unresponsive",
                         Status))
salesforce_case <- salesforce_case %>% 
  mutate(Status = ifelse(Status %in% c("Pending", "Pending (Responsive)", "Pending-Waiting On Client",
                                       "Pending-On HHELP", "On Hold"),
                         "Pending",
                         Status))

## clean (mostly reclassify) reason variable now
salesforce_case %>%
  count(Reason)

salesforce_case <- salesforce_case %>% 
  mutate(Reason = ifelse(Reason %in% c("Benefits Navigation", "Corporate Mentorship", "Entrepreneurship",
                                       "Housing & Shelter", "Income Support", "Individual & Family Support",
                                       "Legal Services", "Mental/Behavioral Health", "Other", "Professional Development",
                                       "Transportation", "Virtual Services Support"),
                         "Other",
                         Reason))

## clean (reclassify blanks to none) and other fixings requested support now
salesforce_case %>%
  count(Requested_Support__c)

table(is.na(salesforce_case$Requested_Support__c))
sum(salesforce_case$Requested_Support__c == "", na.rm = TRUE)
salesforce_case$Requested_Support__c <-  trimws(salesforce_case$Requested_Support__c)

salesforce_case$Requested_Support__c[salesforce_case$Requested_Support__c == ""] <- "none"

salesforce_case <- salesforce_case %>% 
  mutate(Requested_Support__c = ifelse(Requested_Support__c %in% c("CV / Portfolio Review", "Resume Feedback"),
                         "Resume Feedback",
                         Requested_Support__c))

salesforce_case %>%
  count(Reason, Requested_Support__c)


####### case file cleaning is done, moving on to contact
####### contact csv file cleaning
salesforce_contact <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\SalesForce_Contact.csv")
glimpse(salesforce_contact)
summary(salesforce_contact)

salesforce_contact %>% 
  count(MailingState)

## alaska AK
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("AK", "Ak", "Alaska"),
                         "AK",
                         MailingState))
## alamaba AL
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("AL", "Al", "Alabama", "al"),
                               "AL",
                               MailingState))
## arkansa AR
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("AR", "Ar", "Arkansas", "ar"),
                               "AR",
                               MailingState))
## arizona AZ
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("AZ", "Az", "Arizona", "az", "arizona", "Phoenix, AZ", "ARIZONA"),
                               "AZ",
                               MailingState))
## California CA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("CA", "Ca", "California", "ca", "cA", "Ca.", "Californio", "CALIFORNIA"),
                               "CA",
                               MailingState))
## Colorado CO
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("CO", "Co", "Colorado", "co", "colorado", "COLORADO"),
                               "CO",
                               MailingState))
## Connecticut CT
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("CT", "Ct", "Connecticut", "ct"),
                               "CT",
                               MailingState))
## Delaware DE
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("DE", "De", "Delaware"),
                               "DE",
                               MailingState))
## Dc DC
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("DC", "Dc", "dc", "Washington D.C.", "Washington DC", "Washington, DC", "District of Columbia", "D.C", "D.C."),
                               "DC",
                               MailingState))
## florida FL
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("FL", "Tampa, FL", "Fl", "Florida", "florida", "FLORIDA", "FLorida", "fl"),
                               "FL",
                               MailingState))
## Georgia GA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("GA", "Ga", "Georgia", "ga", "georgia", "GEORGIA", "GA - Georgia", "atlanta"),
                               "GA",
                               MailingState))
## Hawaii HI
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("HI", "hi", "Hawaii", "Hi"),
                               "HI",
                               MailingState))
## Idaho ID
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("ID", "Idaho"),
                               "ID",
                               MailingState))
## Illinois IL
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("IL", "Illinois", "illinois", "il", "Il"),
                               "IL",
                               MailingState))
## Indiana IN
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("IN", "Indiana", "In"),
                               "IN",
                               MailingState))
## Iowa IA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("IA", "Ia", "Iowa"),
                               "IA",
                               MailingState))
## Kansas KS
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("KS", "Ks", "Kansas"),
                               "KS",
                               MailingState))
## Kentucky KY
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("KY", "ky", "Ky", "kentucky", "Kentucky"),
                               "KY",
                               MailingState))
## LOuisiana LA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("LA", "Louisiana", "La", "LA 70072"),
                               "LA",
                               MailingState))
## Maine ME
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("ME", "Me", "Maine"),
                               "ME",
                               MailingState))
## Maryland MD
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MD", "Md", "Maryland", "md","Md.", "MARYLAND"),
                               "MD",
                               MailingState))
## Mass MA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MA", "Ma", "Massachusetts", "ma"),
                               "MA",
                               MailingState))
## Michigan MI
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MI", "Mi", "Michigan", "mi"),
                               "MI",
                               MailingState))
## Minesota MN
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MN", "Mn", "Minnesota"),
                               "MN",
                               MailingState))
## mississippi Ms
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MS", "Ms", "Mississippi", "ms", "MISSISSIPPI"),
                               "MS",
                               MailingState))
## Missouri MO
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MO", "Mo", "Missouri"),
                               "MO",
                               MailingState))
## Montana MT
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("MT", "Mt", "Montana"),
                               "MT",
                               MailingState))
## Nebraska NE
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NE", "Ne", "Nebraska", "NEBRASKA"),
                               "NE",
                               MailingState))
## Nevada NV
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NV", "Nv", "Nevada", "nv"),
                               "NV",
                               MailingState))
## New Hampshire NH
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NH", "Nh", "New Hampshire"),
                               "NH",
                               MailingState))
## New Jersey NJ
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NJ", "Nj", "New Jersey", "nj"),
                               "NJ",
                               MailingState))
## New Mexico NM
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NM", "Nm", "New Mexico"),
                               "NM",
                               MailingState))
## New York NY
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NY", "Ny", "New York", "ny", "new york", "NEW YORK"),
                               "NY",
                               MailingState))
## North carolina NC
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("NC", "Nc", "North Carolina", "north Carolina", "north carolina", "nc", "North carolina", "NORTH CAROLINA"),
                               "NC",
                               MailingState))
## North Dakota ND
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("ND", "Nd", "North Dakota"),
                               "ND",
                               MailingState))
## Ohio OH
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("OH", "Oh", "Ohio", "oh", "Oho"),
                               "OH",
                               MailingState))
## Oklahoma OK
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("OK", "Ok", "Oklahoma"),
                               "OK",
                               MailingState))
## Oregon OR
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("OR", "Or", "Oregon"),
                               "OR",
                               MailingState))
## Pennsylvania PA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("PA", "Pa", "Pennsylvania", "pa", "PENNSYLVANIA"),
                               "PA",
                               MailingState))
## rhode island RI
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("RI", "Ri", "Rhode Island"),
                               "RI",
                               MailingState))
## South carolina SC
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("SC", "Sc", "South Carolina", "sc", "south carolina", ""),
                               "SC",
                               MailingState))
## south dakota SD
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("SD", "Sd", "South Dakota"),
                               "SD",
                               MailingState))
## Tennesse TN
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("TN", "Tn", "Tennessee", "tn"),
                               "TN",
                               MailingState))
## Texas TX
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("TX", "Tx", "Texas", "texas", "tx", "TEXAS"),
                               "TX",
                               MailingState))
## utah UT
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("UT", "Ut", "Utah", "utah", "UTAH"),
                               "UT",
                               MailingState))
## vermont VT
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("VT", "Vermont", "vermont"),
                               "VT",
                               MailingState))
## Virginia VA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("VA", "Va", "Virginia", "va", "virginia", "VA`", "VIRGINIA", "Virginia (VA)"),
                               "VA",
                               MailingState))
## washington WA
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("WA", "Wa", "Washington", "wa", "washington", "WASHINGTON", ""),
                               "WA",
                               MailingState))
## WESt virginia WV
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("WV", "Wv", "West Virginia", ""),
                               "WV",
                               MailingState))
## Wisconsin WI
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("WI", "Wi", "Wisconsin"),
                               "Wi",
                               MailingState))
## Wyoming WY
salesforce_contact <- salesforce_contact %>% 
  mutate(MailingState = ifelse(MailingState %in% c("WY", "Wy", "Wyoming", "wy"),
                               "WY",
                               MailingState))

## removes na, blanks, and non state/dc codes
valid_states <- c(state.abb, "DC")
salesforce_contact <- salesforce_contact[!is.na(salesforce_contact$MailingState) & 
                 salesforce_contact$MailingState != "" & 
                 salesforce_contact$MailingState %in% valid_states, ]
salesforce_contact %>% 
  count(MailingState)
## mailing state is clean

## last rank or service rank
salesforce_contact %>% 
  count(Last_Rank__c)

salesforce_contact %>% 
  count(Service_Rank__c)
## will clean service rank and omit last rank**

salesforce_contact <- salesforce_contact[!is.na(salesforce_contact$Service_Rank__c) & 
                    salesforce_contact$Service_Rank__c != "" & 
                   !(salesforce_contact$Service_Rank__c %in% c("Prefer not to answer", "Dependent Spouse")), ]

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("E5", "GS-5"),
                                  "E-5",
                                  Service_Rank__c))

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("CW"),
                                  "W-1",
                                  Service_Rank__c))

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("CW-2"),
                                  "W-2",
                                  Service_Rank__c))

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("CW-3"),
                                  "W-3",
                                  Service_Rank__c))

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("CW-4"),
                                  "W-4",
                                  Service_Rank__c))

#### educations variables
salesforce_contact %>% 
  count(Education_Summary__c)

salesforce_contact %>% 
  count(Highest_Level_of_Education_Completed__c)

summary(salesforce_contact$Highest_Level_of_Education_Completed__c)

### education reassignment
remove_values <- c(
  "tdeutch@broadandcassel.com lawfirm",
  "ed_jopeck@sra.com",
  "use week of 12/10",
  "n/a"
)
salesforce_contact <- salesforce_contact %>%
  filter(!tolower(trimws(Highest_Level_of_Education_Completed__c)) %in% tolower(remove_values))

categorize_education <- function(desc) {
  if (str_trim(desc) == "") return(NA)
  desc_lower <- tolower(desc)
  
  # Define new education levels
  levels <- c(
    "No formal educational credential",
    "High school diploma or equivalent",
    "Some college, no degree",
    "Associate's degree",
    "Bachelor's degree",
    "Master's degree",
    "Doctoral or professional degree"
  )
  
  in_progress_keywords <- "enrolled|expected|in progress|working towards|pursuing|currently attending|anticipated|candidate|graduating|attending|working on|studying|majoring|ongoing|no degree"
  completed_levels <- c()
  
  if (str_detect(desc_lower, "phd|doctor|jd|md|dvm") &&
      !str_detect(desc_lower, paste0(in_progress_keywords, ".*(phd|doctor|jd|md|dvm)"))) {
    completed_levels <- c(completed_levels, "Doctoral or professional degree")
  }
  if (str_detect(desc_lower, "master|mba|m\\.s|msc") &&
      !str_detect(desc_lower, paste0(in_progress_keywords, ".*(master|mba|m\\.s|msc)"))) {
    completed_levels <- c(completed_levels, "Master's degree")
  }
  if (
    str_detect(desc_lower, "bachelor|b\\.a|b\\.s|ba in|bs in|4 year degree|ba\\b|bs\\b|college|technical college") &&
    !str_detect(desc_lower, paste0(in_progress_keywords, ".*(bachelor|b\\.a|b\\.s|ba|bs|college|technical college)"))
  ) {
    completed_levels <- c(completed_levels, "Bachelor's degree")
  }
  if (
    str_detect(desc_lower, "associate|2 year degree|aa\\b|as\\b|a\\.a|a\\.s|community college") &&
    !str_detect(desc_lower, paste0(in_progress_keywords, ".*(associate|aa|as|community college)"))
  ) {
    completed_levels <- c(completed_levels, "Associate's degree")
  }
  if (str_detect(desc_lower, "some college") | str_detect(desc_lower, in_progress_keywords)) {
    completed_levels <- c(completed_levels, "Some college, no degree")
  }
  if (str_detect(desc_lower, "high school|ged|secondary school|diploma|hs\\b|h\\.s\\.|technical school|highschool")) {
    completed_levels <- c(completed_levels, "High school diploma or equivalent")
  }
  if (length(completed_levels) > 0) {
    return(levels[max(match(completed_levels, levels, nomatch = 0))])
  } else {
    return("No formal educational credential")
  }
}

salesforce_contact <- salesforce_contact %>%
  filter(str_trim(Highest_Level_of_Education_Completed__c) != "") %>%
  mutate(education_category = sapply(Highest_Level_of_Education_Completed__c, categorize_education))
salesforce_contact %>% 
  count(education_category)

salesforce_contact <- salesforce_contact %>% 
  mutate(years_of_education = case_when(
    education_category == "No formal educational credential" ~ 0,
    education_category == "High school diploma or equivalent" ~ 0,
    education_category == "Some college, no degree" ~ 1.5,
    education_category == "Associate's degree" ~ 2,
    education_category == "Bachelor's degree" ~ 4,
    education_category == "Master's degree" ~ 6,
    education_category == "Doctoral or professional degree" ~ 9
  ))

salesforce_contact %>% 
  count(years_of_education)

### service branch variables
salesforce_contact %>% 
  count(Service_Branch__c)

salesforce_contact <- salesforce_contact %>% 
  mutate(Service_Branch__c = ifelse(Service_Branch__c %in% c("Merchant Marine"),
                                    "Marines",
                                    Service_Branch__c))

summary(salesforce_contact)

### ivmf variable now
salesforce_contact %>% 
  count(IVMF_Service_Branch__c)

reclassify_service <- function(service_string) {
  branches <- unlist(strsplit(service_string, ";"))
  branches <- trimws(branches)
  
  for (b in branches) {
    if (grepl("Army", b, ignore.case = TRUE)) {
      return("Army")
    } else if (grepl("Navy", b, ignore.case = TRUE)) {
      return("Navy")
    } else if (grepl("Marine", b, ignore.case = TRUE)) {
      return("Marines")
    } else if (grepl("Coast Guard", b, ignore.case = TRUE)) {
      return("Coast Guard")
    }
  }
  return(NA)  
}

salesforce_contact$IVMF_Service_Branch__c <- sapply(salesforce_contact$IVMF_Service_Branch__c, reclassify_service)

### combine the 2 service branch variables now
salesforce_contact <- salesforce_contact %>% 
  mutate(
    combined_service_branch = coalesce(Service_Branch__c, IVMF_Service_Branch__c))

salesforce_contact %>% 
  count(combined_service_branch)

salesforce_contact <- salesforce_contact %>%
  filter(!combined_service_branch %in% c("", "Air Force"))

salesforce_contact %>% 
  count(combined_service_branch)

### time in service
head(salesforce_contact$Date_of_Service_Entry__c)
summary(salesforce_contact$Date_of_Service_Entry__c)
salesforce_contact %>% 
  count(Date_of_Service_Entry__c)
str(salesforce_contact$Date_of_Service_Entry__c)

head(salesforce_contact$Date_of_Separation__c)
summary(salesforce_contact$Date_of_Separation__c)
salesforce_contact %>% 
  count(Date_of_Separation__c)
str(salesforce_contact$Date_of_Service_Entry__c)

## start/entry date
extract_first_date <- function(text) {
  patterns <- c(
    "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b",  
    "\\b\\d{1,2}-[a-z]{3}-\\d{2,4}\\b", 
    "\\b\\d{1,2}/\\d{4}\\b",    
    "\\b\\d{2}/\\d{2}\\b",   
    "\\b\\d{1,2}[a-z]{3}\\d{2,4}\\b", 
    "\\b\\d{1,2}/\\d{1,2}/\\d{2}\\b", 
    "\\b\\d{4}\\b",  
    "\\b\\d{1,2}/\\d{1,2}\\b" 
  )
  
  for (pat in patterns) {
    match <- str_extract(text, pat)
    if (!is.na(match)) return(match)
  }
  return(NA)
}

salesforce_contact <- salesforce_contact %>%
  mutate(
    Date_of_Service_Entry__c = str_to_lower(Date_of_Service_Entry__c),
    Date_of_Service_Entry__c = str_replace_all(Date_of_Service_Entry__c, "to|through|and|=", "-"),
    Date_of_Service_Entry__c = str_replace_all(Date_of_Service_Entry__c, "[^0-9a-zA-Z/\\- ]+", ""),
    Date_of_Service_Entry__c = str_replace_all(Date_of_Service_Entry__c, "\\s+", " "),
    Date_of_Service_Entry__c = sapply(Date_of_Service_Entry__c, extract_first_date),
    Date_of_Service_Entry__c = parse_date_time(Date_of_Service_Entry__c, 
                                               orders = c("mdy", "my", "Ym", "dmy", "dby", "bdY", "bdy", "dbY"),
                                               exact = FALSE)
  )

## end/separation date
extract_first_date <- function(text) {
  patterns <- c(
    "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\b",  
    "\\b\\d{1,2}-[a-z]{3}-\\d{2,4}\\b", 
    "\\b\\d{1,2}/\\d{4}\\b",    
    "\\b\\d{2}/\\d{2}\\b",   
    "\\b\\d{1,2}[a-z]{3}\\d{2,4}\\b", 
    "\\b\\d{1,2}/\\d{1,2}/\\d{2}\\b", 
    "\\b\\d{4}\\b",  
    "\\b\\d{1,2}/\\d{1,2}\\b" 
  )
  
  for (pat in patterns) {
    match <- str_extract(text, pat)
    if (!is.na(match)) return(match)
  }
  return(NA)
}

salesforce_contact <- salesforce_contact %>%
  mutate(
    Date_of_Separation__c = str_to_lower(Date_of_Separation__c),
    Date_of_Separation__c = str_replace_all(Date_of_Separation__c, "to|through|and|=", "-"),
    Date_of_Separation__c = str_replace_all(Date_of_Separation__c, "[^0-9a-zA-Z/\\- ]+", ""),
    Date_of_Separation__c = str_replace_all(Date_of_Separation__c, "\\s+", " "),
    Date_of_Separation__c = sapply(Date_of_Separation__c, extract_first_date),
    Date_of_Separation__c = parse_date_time(Date_of_Separation__c, 
                                               orders = c("mdy", "my", "Ym", "dmy", "dby", "bdY", "bdy", "dbY"),
                                               exact = FALSE)
  )

### combine/ find years of service
salesforce_contact <- salesforce_contact %>%
  mutate(
    entry_days = as.numeric(difftime(Date_of_Service_Entry__c, as.Date("1900-01-01"), units = "days")),
    separation_days = as.numeric(difftime(Date_of_Separation__c, as.Date("1900-01-01"), units = "days")),
    
    service_duration_days = separation_days - entry_days,
    service_duration_years = round(service_duration_days / 365.25, 2)
  ) %>% 
  filter(service_duration_years >= 0)

str(salesforce_contact$service_duration_years)
salesforce_contact %>% 
  count(service_duration_years)


###### cleaning for contact is done
###### cleaning hire file now
salesforce_hire <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\SalesForce_Hire_Information.csv")

## using countblank in excel i found the 3 variables with huge missing data
summary(salesforce_hire)

### remove those variables
salesforce_hire <- salesforce_hire %>% 
  select(-LastActivityDate, -Months_Unemployed__c)

salesforce_hire <- salesforce_hire %>% 
  select(-Hiring_Account__c)

## clean employment type
salesforce_hire %>% 
  count(Employment_Type__c)

salesforce_hire <- salesforce_hire %>% 
  mutate(Full_Time = case_when(Employment_Type__c == "Full-Time" ~ 1,
                               TRUE ~ 0))

salesforce_hire %>% 
  count(Full_Time)

######## hire file is all clean :)
######## time to export all 3 files for clustering an predictive analysis
getwd()
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\data")

salesforce_case_clean <- salesforce_case
write.csv(salesforce_case_clean, "salesforce_case_clean.csv", row.names = FALSE)

salesforce_contact_clean <- salesforce_contact
write.csv(salesforce_contact_clean, "salesforce_contact_clean.csv", row.names = FALSE)

salesforce_hire_clean <- salesforce_hire
write.csv(salesforce_hire_clean, "salesforce_hire_clean.csv", row.names = FALSE)



