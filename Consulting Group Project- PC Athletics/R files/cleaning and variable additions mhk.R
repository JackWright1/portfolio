install.packages("tidyverse")
library(tidyverse)

seat_manifest_mhk <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\2024-25 Seat Manifest - HKS.csv")
View(seat_manifest_mhk)

seat_manifest_mhk %>% 
  count(Event.Code)
seat_manifest_mhk %>% 
  count(Event.Full.Name)
seat_manifest_mhk %>% 
  count(Scanned.)

seat_manifest_mhk %>% 
  count(Item.Code)
seat_manifest_mhk %>% 
  count(Price.Type.Code)
seat_manifest_mhk %>% 
  count(Price.Level.Code)
seat_manifest_mhk %>% 
  count(Disposition.Code)


### ticket purchased variable
seat_manifest_mhk$ticket_purchased <- ifelse(
  seat_manifest_mhk$Scanned. %in% c("Yes", "No"),
  "Yes",
  "No"
)
table(seat_manifest_mhk$ticket_purchased)

### event date time conversion and weekday variable

seat_manifest_mhk$Event.Date.Time <- trimws(seat_manifest_mhk$Event.Date.Time)
head(seat_manifest_mhk$Event.Date.Time)
as.POSIXct("10/26/2025 16:00", format = "%m/%d/%Y %H:%M")
seat_manifest_mhk$Event.Date.Time <- as.POSIXct(seat_manifest_mhk$Event.Date.Time, format = "%m/%d/%Y %H:%M")
seat_manifest_mhk$day_of_week <- weekdays(seat_manifest_mhk$Event.Date.Time)

seat_manifest_mhk$weekday <- ifelse(seat_manifest_mhk$day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                    "Yes",
                                    "No")

### previous game won variable- figure out how to place 2nd AZ state game, first maine game
seat_manifest_mhk %>% 
  count(Event.Full.Name, Event.Date.Time)

seat_manifest_mhk <- seat_manifest_mhk %>%
  mutate(
    Previous_game_won = case_when(
      Event.Full.Name == "Friars Men's Hockey vs Northeastern" & Event.Date.Time == as.POSIXct("11/8/2024 19:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs UMass" & Event.Date.Time == as.POSIXct("11/16/2024 18:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Boston University" & Event.Date.Time == as.POSIXct("2/14/2025 19:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Long Island" & Event.Date.Time == as.POSIXct("11/23/2024 18:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Colorado College" & Event.Date.Time == as.POSIXct("12/6/2024 19:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Colorado College" & Event.Date.Time == as.POSIXct("12/7/2024 18:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Merrimack" & Event.Date.Time == as.POSIXct("2/28/2025 19:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Arizona State" & Event.Date.Time == as.POSIXct("10/19/2024 16:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      Event.Full.Name == "Friars Men's Hockey vs Maine" & Event.Date.Time == as.POSIXct("12/7/2025 19:00", format = "%m/%d/%Y %H:%M") ~ "Yes",
      TRUE ~ "No"
    )
  )
table(seat_manifest_mhk$Previous_game_won)

##### get rid of non ticket buyers cleaning
sum(is.na(seat_manifest_mhk$Scanned.))
seat_manifest_mhk <- seat_manifest_mhk %>% 
  mutate(Scanned. = trimws(as.character(Scanned.)))
seat_manifest_mhk <- seat_manifest_mhk %>% 
  filter(!is.na(Scanned.) & Scanned. != "")

#### grmid sorting

seat_manifest_mhk <- seat_manifest_mhk %>%
  group_by(GRMID) %>%
  mutate(
    purchase_count = n(),
    attendance_count = sum(`Scanned.` == "Yes", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    purchase_rank = 11 - ntile(purchase_count, 10)  
  )

seat_manifest_mhk %>% 
  count(purchase_rank)

##### customer summary new dataset
customer_summary <- seat_manifest_mhk %>%
  filter(!is.na(GRMID) & GRMID != "") %>%
  group_by(GRMID) %>%
  summarise(
    purchase_count = n(),  
    attendance_count = sum(`Scanned.` == "Yes", na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  mutate(
    purchase_group = 11 - ntile(purchase_count, 10) 
  )
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\cleaned sports csvs")
write.csv(customer_summary, "mhk_customer_summary.csv", row.names = FALSE)
View(customer_summary)



##### save mhk cleaned csv

getwd()
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\cleaned sports csvs")

write.csv(seat_manifest_mhk, "mhk_manifest_clean.csv", row.names = FALSE)
View(seat_manifest_mhk)










