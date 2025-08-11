install.packages("tidyverse")
library(tidyverse)

seat_manifest_mbs <- read.csv("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\2024-25 Seat Manifest - MBS.csv")
View(seat_manifest_mbs)

##### insert ticket purchased variable
seat_manifest_mbs$ticket_purchased <- ifelse(
  seat_manifest_mbs$Scanned. %in% c("Yes", "No"),
  "Yes",
  "No"
)
table(seat_manifest_mbs$ticket_purchased)

##### insert last game won? variable
seat_manifest_mbs$Previous_game_win <- ifelse(
  seat_manifest_mbs$Event.Full.Name %in% c("Friars Men's Basketball vs. Stonehill", "Friars Men's Basketball vs. Hampton",
                             "Friars Men's Basketball vs. Univ. of Wisconsin-Green Bay", "Friars Men's Basketball vs. Delaware State",
                             "Friars Men's Basketball vs. Seton Hall", "Friars Men's Basketball vs. Central Connecticut St."),
  "Yes",
  "No"
)
table(seat_manifest_mbs$Previous_game_win)

##### datetime conversion to create weekday/weekend variable
seat_manifest_mbs$Event.Date.Time <- trimws(seat_manifest_mbs$Event.Date.Time)
head(seat_manifest_mbs$Event.Date.Time)
as.POSIXct("10/26/2025 16:00", format = "%m/%d/%Y %H:%M")
seat_manifest_mbs$Event.Date.Time <- as.POSIXct(seat_manifest_mbs$Event.Date.Time, format = "%m/%d/%Y %H:%M")
seat_manifest_mbs$day_of_week <- weekdays(seat_manifest_mbs$Event.Date.Time)

seat_manifest_mbs$weekday <- ifelse(seat_manifest_mbs$day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
         "Yes",
         "No")

##### grmid clean

sum(is.na(seat_manifest_mbs$Scanned.))
seat_manifest_mbs <- seat_manifest_mbs %>% 
  mutate(Scanned. = trimws(as.character(Scanned.)))
seat_manifest_mbs <- seat_manifest_mbs %>% 
  filter(!is.na(Scanned.) & Scanned. != "")

#### grmid sorting
seat_manifest_mbs <- seat_manifest_mbs %>%
  group_by(GRMID) %>%
  mutate(
    purchase_count = n(),
    attendance_count = sum(`Scanned.` == "Yes", na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    purchase_rank = 11 - ntile(purchase_count, 10)  
  )

seat_manifest_mbs %>% 
  count(purchase_rank)

##### customer unique summary
customer_summary_mbs <- seat_manifest_mbs %>%
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
write.csv(customer_summary_mbs, "mbs_customer_summary.csv", row.names = FALSE)
View(customer_summary_mbs)


seat_manifest_mbs %>% 
  count(Event.Code)

seat_manifest_mbs %>% 
  count(Scanned.)


##### save cleaned csv

getwd()
setwd("C:\\Users\\jwright4\\Desktop\\Summer Consulting Project\\Group Project stuff\\data\\cleaned sports csvs")

write.csv(seat_manifest_mbs, "mbs_manifest_clean.csv", row.names = FALSE)
View(seat_manifest_mbs)





