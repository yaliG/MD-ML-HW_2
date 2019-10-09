#####################################################
#### MDML Assignment 2: Part A -- Models      #######
#### Yali Gao, Jenny Liu, Zhen Zhang          #######
#### October 3rd, 2019                        #######
#####################################################

library(tidyverse)
library(lubridate)
library(tidytext)
require(class)

# Question 3 -------------------------------------------------------
set.seed(2048)
tweets <- readr::read_csv("data/question_two.csv")

tweets <- tweets %>% 
  mutate(label = as.factor(author)) %>% 
  select(-author)# %>% 
#slice(sample(1:n()))

# Determine index for 80% for the training
split_size = floor(nrow(tweets)*.8)

# Split data into training and creating labels
training_tweets <- tweets %>% slice(1:split_size)
training_tweets_labels <- training_tweets$label
training_tweets <- training_tweets %>% select(-label)

# Split data into training and creating labels
testing_tweets <- tweets %>% slice(split_size+1:n())
testing_tweets_labels <- testing_tweets$label
testing_tweets <- testing_tweets %>% select(-label)

# Question 4 -------------------------------------------------------
knn_predict_train <- knn(training_tweets[-1], 
                         training_tweets[-1], 
                         training_tweets_labels, k = 5)

1-sum(knn_predict_train==training_tweets_labels)/length(knn_predict_train)
# 0.2006048 ERROR 
# 0.7993952 ACCURACY 


knn_predict_test <- knn(training_tweets[-1], 
                        testing_tweets[-1], 
                        training_tweets_labels, k = 5)

1-sum(knn_predict_test==testing_tweets_labels)/length(knn_predict_test)
# 0.3387097 ERROR 
# 0.6612903 ACCURACY 

# Question 5 -------------------------------------------------------
training_tweets$author <- training_tweets_labels
word_author_table <- training_tweets %>%
  dplyr::select(author, text) %>%
  tidytext::unnest_tokens(word, text)


training_word_author_counts <- word_author_table %>% group_by(word) %>% 
  summarize(count_trump = sum(author=="Trump"),
            count_staff = sum(author=="Staff"),
            total = n(),
            pct_trump = count_trump/total,
            pct_staff = count_staff/total)

readr::write_csv(training_word_author_counts, "data/question_five.csv")

# Question 6 ------------------------------------------------------- 
# Question A6.1
ques6 <- training_word_author_counts %>% 
  filter(count_staff >= 10) %>% 
  arrange(desc(pct_staff)) %>%
  select(word, pct_staff)

ques6 <- ques6[1:20,]
readr::write_csv(ques6, "data/question_six.csv")

# Question A6.2
# Training 
training_tweets_aug <- training_tweets
training_tweets_aug <- training_tweets_aug %>% select(-author)

# Testing
testing_tweets_aug <- testing_tweets

# Add empty columns for both training and testing 
words <- as.vector(ques6$word)
for(i in words) {
  training_tweets_aug[,i] <- NA
  testing_tweets_aug[,i] <- NA
}

# Find if word is in text for both training and testing
for (i in 5:24){
  training_tweets_aug[i] <- grepl(ques6[i-4,1], training_tweets_aug$text)
  testing_tweets_aug[i] <- grepl(ques6[i-4,1], testing_tweets_aug$text)
}


# Question A6.3
# Train
# KNN 
knn_predict_train_aug <- knn(training_tweets_aug[-1], 
                             training_tweets_aug[-1], 
                             training_tweets_labels, k = 5)

1-sum(knn_predict_train_aug==training_tweets_labels)/length(knn_predict_train_aug)
# 0.1814516 ERROR 
# 0.8185484 ACCURACY 


knn_predict_test_aug <- knn(training_tweets_aug[-1], 
                            testing_tweets_aug[-1], 
                            training_tweets_labels, k = 5)

1-sum(knn_predict_test_aug==testing_tweets_labels)/length(knn_predict_test_aug)
# 0.3145161 ERROR 
# 0.6854839 ACCURACY 

# Question A6.5
training_tweets_aug <- training_tweets_aug %>% 
  mutate(x_1 = (x_1 - mean(x_1))/sd(x_1),
         x_2 = (x_2 - mean(x_2))/sd(x_2),
         x_3 = (x_3 - mean(x_3))/sd(x_3))


testing_tweets_aug <- testing_tweets_aug %>% 
  mutate(x_1 = (x_1 - mean(x_1))/sd(x_1),
         x_2 = (x_2 - mean(x_2))/sd(x_2),
         x_3 = (x_3 - mean(x_3))/sd(x_3))

# Train
# KNN 
knn_predict_train_aug_stand <- knn(training_tweets_aug[-1], 
                                   training_tweets_aug[-1], 
                                   training_tweets_labels, k = 5)

1-sum(knn_predict_train_aug_stand==training_tweets_labels)/length(knn_predict_train_aug_stand)
# 0.125 ERROR 
# 0.875 ACCURACY 


knn_predict_test_aug_stand <- knn(training_tweets_aug[-1], 
                                  testing_tweets_aug[-1], 
                                  training_tweets_labels, k = 5)

1-sum(knn_predict_test_aug_stand==testing_tweets_labels)/length(knn_predict_test_aug_stand)
# 0.2741935 ERROR 
# 0.7258065 ACCURACY 

