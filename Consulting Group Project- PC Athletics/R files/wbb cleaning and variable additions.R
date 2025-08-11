install.packages("tidyverse")
library(tidyverse)

seat_manifest_wbb <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\2024-25 Seat Manifest - WBS .csv")
View(seat_manifest_wbb)

##### insert ticket purchased variable
seat_manifest_wbb$ticket_purchased <- ifelse(
  seat_manifest_wbb$Scanned. %in% c("Yes", "No"),
  "Yes",
  "No"
)
table(seat_manifest_wbb$ticket_purchased)

##### insert last game won? variable
seat_manifest_wbb %>% 
  count(Event.Full.Name)

seat_manifest_wbb$Previous_game_win <- ifelse(
  seat_manifest_wbb$Event.Full.Name %in% c("Friars Women's Basketball vs. Mercy University", "Friars Women's Basketball vs. Columbia",
                                           "Friars Women's Basketball vs. Brown", "Friars Women's Basketball vs. Penn State",
                                           "Friars Women's Basketball vs. Creighton University", "Friars Women's Basketball vs. Georgetown University",
                                           "Friars Women's Basketball vs. UConn", "Friars Women's Basketball vs. Seton Hall University"),
  "Yes",
  "No"
)
table(seat_manifest_wbb$Previous_game_win)

#### event date time variable cleaning mercy uni and southern conn state uni
seat_manifest_wbb$Event.Date.Time <- trimws(seat_manifest_wbb$Event.Date.Time)

seat_manifest_wbb$Event.Date.Time <- as.character(seat_manifest_wbb$Event.Date.Time)

seat_manifest_wbb <- seat_manifest_wbb %>% 
  mutate(Event.Date.Time = case_when(
    Event.Full.Name == "Friars Women's Basketball vs. Columbia" ~ "11/08/2024 19:00",
    Event.Full.Name == "Friars Women's Basketball vs. Mercy University" ~ "11/04/2024 19:00",
    Event.Full.Name == "Friars Women's Basketball vs. Southern Connecticut State University" ~ "10/29/2024 19:00",
    TRUE ~ Event.Date.Time  
  ))

##### datetime conversion to create weekday/weekend variable
head(seat_manifest_wbb$Event.Date.Time)
as.POSIXct("10/26/2025 16:00", format = "%m/%d/%Y %H:%M")
seat_manifest_wbb$Event.Date.Time <- as.POSIXct(seat_manifest_wbb$Event.Date.Time, format = "%m/%d/%Y %H:%M")
seat_manifest_wbb$day_of_week <- weekdays(seat_manifest_wbb$Event.Date.Time)

seat_manifest_wbb$weekday <- ifelse(seat_manifest_wbb$day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                    "Yes",
                                    "No")


##### scanned cleaning
sum(is.na(seat_manifest_wbb$Scanned.))
seat_manifest_wbb <- seat_manifest_wbb %>% 
  mutate(Scanned. = trimws(as.character(Scanned.)))
seat_manifest_wbb <- seat_manifest_wbb %>% 
  filter(!is.na(Scanned.) & Scanned. != "")

#### grmid sorting
seat_manifest_wbb <- seat_manifest_wbb %>%
  group_by(GRMID) %>%
  mutate(
    purchase_count = n(),
    attendance_count = sum(`Scanned.` == "Yes", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    purchase_rank = 11 - ntile(purchase_count, 10)  
  )

seat_manifest_wbb %>% 
  count(purchase_rank)

#### create newtable with unique grmid, purchase counts, 
customer_summary <- seat_manifest_wbb %>%
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
write.csv(customer_summary, "wbb_customer_summary.csv", row.names = FALSE)
View(customer_summary)



seat_manifest_wbb %>% 
  count(Event.Code)

seat_manifest_wbb %>% 
  count(Scanned.)


##### save cleaned csv

getwd()
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\cleaned sports csvs")

write.csv(seat_manifest_wbb, "wbb_manifest_clean.csv", row.names = FALSE)
View(seat_manifest_wbb)

seat_manifest_wbb %>% 
  count(purchase_rank)
seat_manifest_wbb %>% 
  count(Event.Full.Name, Event.Date.Time)














