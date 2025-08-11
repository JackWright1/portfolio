install.packages("caret")
install.packages("pROC")
install.packages("ROCR")
install.packages("xgboost")
installed.packages("tidyverse")
library(tidyverse)
library(caret)
library(pROC)
library(ROCR)
library(xgboost)

salesforce_contact_clean <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\salesforce_contact_clean.csv")

## prep cleaning
salesforce_contact_clean %>% 
  count(Service_Rank__c)

salesforce_contact_clean %>% 
  count(Gender__c)

salesforce_contact_clean <- salesforce_contact_clean %>% 
  filter(Gender__c != "")

salesforce_contact_clean %>% 
  count(education_category)

salesforce_contact_clean %>% 
  count(Service_Branch__c)

range(salesforce_contact_clean$service_duration_years)
salesforce_contact_clean <- salesforce_contact_clean %>% 
  filter(service_duration_years <= 56)
salesforce_contact_clean <- salesforce_contact_clean %>% 
  filter(service_duration_years > 0)

salesforce_contact_clean <- salesforce_contact_clean %>% 
  mutate(service_duration_years_sqrt = sqrt(service_duration_years))

## joins/merges
salesforce_case_clean <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\salesforce_case_clean.csv")
salesforce_hire_clean <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\salesforce_hire_clean.csv")

case_hire <- left_join(salesforce_case_clean, salesforce_hire_clean, by = c("ContactId" = "Client_Name__c"))
head(case_hire)

case_hire <- case_hire %>% 
  distinct(ContactId, .keep_all = TRUE)

summary(salesforce_contact_clean)

trimws(salesforce_contact_clean)
trimws(case_hire)

merged <- inner_join(salesforce_contact_clean, case_hire, by = c("Id" = "ContactId"))
head(merged)

merged %>% 
  count(Full_Time)

merged <- merged %>% 
  filter(!is.na(Full_Time))

## more fine tuning before regression

merged <- merged %>% 
  mutate(MailingState = ifelse(MailingState %in% c("DC", "VA", "MD"), 1, 0))

merged %>% 
  count(MailingState)

merged %>% 
  count(Service_Rank__c)

merged <- merged %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("O-1", "O-2", "O-3", "O-4", "O-5"), "O1-O10", Service_Rank__c))

merged <- merged %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("W-2", "W-3", "W-4"), "W1-W5", Service_Rank__c))

merged <- merged %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("E-2", "E-3", "E-4"), "E1-E4", Service_Rank__c))

merged <- merged %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("E-5", "E-6", "E-7"), "E5-E7", Service_Rank__c))

merged <- merged %>% 
  mutate(Service_Rank__c = ifelse(Service_Rank__c %in% c("E-8", "E-9"), "E8-E9", Service_Rank__c))

merged %>% 
  count(Status)

merged <- merged %>% 
  mutate(Status = ifelse(Status %in% c("Client Unresponsive", "Pending",
                                       "Request Withdrawn", "Unfulfilled"), "Incomplete", Status))

merged %>% 
  count(Requested_Support__c)

merged <- merged %>% 
  mutate(Requested_Support__c = ifelse(Requested_Support__c %in% c("Job Search Best Practices", "LinkedIn Review",
                                       "Search Focus", "none"), "Other", 
                                       Requested_Support__c))

merged %>% 
  count(education_category)

### hmmm let me think about that gradient boost
merged$Service_Rank__c <- as.integer(factor(merged$Service_Rank__c, 
                                            levels = c("E1-E4", "E5-E7", "E8-E9",
                                                       "W1-W5",
                                                       "O1-O10"))) 

merged$education_category <- as.integer(factor(merged$education_category, 
                                               levels = c("High school diploma or equivalent", "Associate's degree", "Bachelor's degree", "Doctoral or professional"))) 

cat_vars <- c("Status", "Requested_Support__c", "Gender__c", "combined_service_branch")

dummies <- dummyVars(" ~ .", data = merged[, cat_vars])
encoded_cats <- predict(dummies, newdata = merged)

x <- cbind(
  encoded_cats,
  MailingState = merged$MailingState,                         
  service_duration_years_sqrt = merged$service_duration_years_sqrt,
  Service_Rank__c = merged$Service_Rank__c,
  education_category = merged$education_category
)

# Target variable
y <- as.numeric(as.character(merged$Full_Time))

set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
x_train <- x[train_index, ]
x_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

dtrain <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)

xgb_model <- xgboost(data = dtrain,
                     objective = "binary:logistic",
                     eval_metric = "logloss",
                     nrounds = 100,
                     verbose = 0)

pred_probs <- predict(xgb_model, newdata = dtest)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

accuracy <- mean(pred_labels == y_test)
print(paste("Testing Accuracy:", round(accuracy, 4)))

roc_obj <- roc(response = y_test, predictor = pred_probs)
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 4)))

# Plot ROC curve
plot(roc_obj, col = "#2C3E50", lwd = 2, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "gray")


