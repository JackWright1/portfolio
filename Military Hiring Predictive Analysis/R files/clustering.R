install.packages("tidyverse")
library(tidyverse)
install.packages("cluster")
library(cluster)
salesforce_contact_clean <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data\\salesforce_contact_clean.csv")

salesforce_contact_clean %>% 
  count(years_of_education)

## remove blanks from gender variable 
salesforce_contact_clean %>% 
  count(Gender__c)

salesforce_contact_clean <- salesforce_contact_clean %>%
  filter(Gender__c != "") 

## clusters
salesforce_contact_clean %>% 
  count(Service_Rank__c)

salesforce_contact_clean %>% 
  count(education_category)

salesforce_contact_clean %>% 
  count(combined_service_branch)

range(salesforce_contact_clean$service_duration_years)
salesforce_contact_clean <- salesforce_contact_clean %>% 
  filter(service_duration_years <= 56)
salesforce_contact_clean <- salesforce_contact_clean %>% 
  filter(service_duration_years > 0)

hist(salesforce_contact_clean$service_duration_years, main = "histogram", xlab = "service duration")

salesforce_contact_clean <- salesforce_contact_clean %>% 
  mutate(service_duration_years_sqrt = sqrt(service_duration_years))

hist(salesforce_contact_clean$service_duration_years_sqrt, main = "histogram sqrt", xlab = "sqrt of service duration")

salesforce_contact_clean <- salesforce_contact_clean %>%
  mutate(
    Service_Branch__c = as.factor(Service_Branch__c),
    education_category = factor(education_category, levels = c("No formal educational credential", "High school diploma or equivalent",
                                                               "Associate's degree", "Bachelor's degree", "Doctoral or professional degree"), ordered = TRUE),
    Service_Rank__c = factor(Service_Rank__c, levels = c("E-1", "E-2", "E-3", "E-4", "E-5",
                                                         "E-6", "E-7", "E-8", "E-9", "W-1",
                                                         "W-2", "W-3", "W-4", "W-5", "O-1",
                                                         "O-2", "O-3", "O-4", "O-5", "O-6", "O-8"), ordered = TRUE),
    service_duration_years_sqrt = as.numeric(service_duration_years_sqrt)
  )

### correlation 
salesforce_contact_clean$years_of_education <- as.numeric(salesforce_contact_clean$years_of_education)
salesforce_contact_clean$Service_Rank__c <- as.numeric(salesforce_contact_clean$Service_Rank__c)

spearman_data <- na.omit(salesforce_contact_clean[, c("years_of_education", "service_duration_years_sqrt", "Service_Rank__c")])

install.packages("Hmisc")
library(Hmisc)
spearman_data <- salesforce_contact_clean[, c("years_of_education", "service_duration_years_sqrt", "Service_Rank__c")]
spearman_data <- na.omit(salesforce_contact_clean[, c("years_of_education", "service_duration_years_sqrt", "Service_Rank__c")])
spearman_result <- rcorr(as.matrix(spearman_data), type = "spearman")
spearman_result$r  # correlation coefficients

### cluster
salesforce_contact_clean$Gender__c <- as.factor(salesforce_contact_clean$Gender__c)

salesforce_contact_clean$Race__c <- as.factor(salesforce_contact_clean$Race__c)

salesforce_contact_clean$combined_service_branch <- as.factor(salesforce_contact_clean$combined_service_branch)

salesforce_contact_clean$Service_Rank__c <- as.factor(salesforce_contact_clean$Service_Rank__c)

cluster <- salesforce_contact_clean %>% 
  select(years_of_education, Gender__c, Race__c) ###### not real just test demographic cluster

cluster <- salesforce_contact_clean %>%
  select(service_duration_years_sqrt, Service_Rank__c, combined_service_branch) #### real military info demographic

gower_dist <- daisy(cluster, metric = "gower")
hc <- hclust(gower_dist, method = "ward.D2")
plot(hc, labels = FALSE, main = "Dendrogram")

as.matrix(gower_dist)[1:5, 1:5]

sil_scores <- c()

for (k in 2:10) {
  clust <- cutree(hc, k = k)
  sil <- silhouette(clust, gower_dist)
  sil_scores[k] <- mean(sil[, 3])
}

plot(2:10, sil_scores[2:10], type = "b",
     xlab = "Number of clusters", ylab = "Average silhouette width",
     main = "Silhouette Scores for k = 2 to 10")

optimal_k <- which.max(sil_scores)
salesforce_contact_clean$cluster <- cutree(hc, k = optimal_k)

table(cutree(hc, k = 3))

cluster$cluster_number <- cutree(hc, k = 3)

View(cluster)

View(salesforce_contact_clean)

salesforce_contact_clean %>% 
  count(cluster)

### n=3 
### save cluster file

getwd()
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Individual Submission Project\\Data")
write.csv(cluster, "cluster.csv", row.names = FALSE)

write.csv(salesforce_contact_clean, "salesforce_contact_large_cluster_tableau.csv", row.names = FALSE)













