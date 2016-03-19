library(tidyr)
library(dplyr)

#Load the data in RStudio
titanic3 <- read.csv("C:/Users/USTOJOS/titanic_original.csv")

View(titanic3)

titanic4 <- titanic3 %>%
  #Port of embarkation
    mutate(embarked = ifelse(embarked == "", "S", as.character(embarked))) %>%
  #Age -- Replace missing age with mean Age
    replace_na(titanic3, replace = list(age = 0)) %>%
    mutate(age = ifelse(age == 0, mean(age), age)) %>%
  #Boat -- Replace missing values with NA
    mutate(boat = ifelse(boat == "", NA, as.character(boat))) %>%
  #Cabin -- Replace missing values with NA and create a new column to indicate cabin indicator
    mutate(cabin = ifelse(cabin == "", NA, as.character(cabin))) %>%
    mutate(has_cabin_number = ifelse(is.na(cabin), 0, 1))

View(titanic4)

#Export clean data into new file
write.table(titanic4,
            file = "C:/Users/USTOJOS/titanic_clean.csv",
            sep = ",",
            row.name = FALSE)
