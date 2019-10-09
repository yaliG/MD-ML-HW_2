#####################################################
#### MDML Assignment 2: Part A -- Preprocess  #######
#### Yali Gao, Jenny Liu, Zhen Zhang          #######
#### October 3rd, 2019                        #######
#####################################################

# Question 1
library(tidyverse)
library(lubridate)
library(stringr)
tweets <- readr::read_tsv("../data_hw2/trump_data.tsv",
                          col_names = c("author", "date_and_time", "text"))

# Question 2 
tweets <- tweets %>% mutate(text = base::tolower(text),
                            x_1 = lubridate::hour(date_and_time),
                            x_2 = stringr::str_count(text,"a"),
                            x_3 = stringr::str_length(text))

data_question_two <- tweets %>% select(-date_and_time)

# Write data 
readr::write_csv(data_question_two, "data/question_two.csv")




