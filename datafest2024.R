library(data.table)
library(readr)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggbeeswarm)
library(sqldf)
library(dplyr)

#load data sets
media_views <- fread("/Users/jessicali/Downloads/datafest 2024/2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/media_views.csv")
checkpoints_eoc <- fread("/Users/jessicali/Downloads/datafest 2024/2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/checkpoints_eoc.csv")
checkpoints_pulse <- fread("/Users/jessicali/Downloads/datafest 2024/2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/checkpoints_pulse.csv")
page_views <- fread("/Users/jessicali/Downloads/datafest 2024/2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/page_views.csv")

#view scores and video watch length for chapters 4 and 5 bc they contain videos to supplement learning
checkpoints_eoc_4 <- filter(checkpoints_eoc, chapter_number == "4")
checkpoints_eoc_5 <- filter(checkpoints_eoc, chapter_number == "5")
media_views_4 <- filter(media_views, chapter_number == "4")
media_views_5 <- filter(media_views, chapter_number == "5")

#combine scores and views for each chapter
eoc_vs_views_4 <- left_join(checkpoints_eoc_4, media_views_4, by = "student_id")
eoc_vs_views_5 <- left_join(checkpoints_eoc_5, media_views_5, by = "student_id")

#replace all missing values 
media_views[is.na(media_views)] <- 0
media_views

#trim data
eoc_vs_views_4_trimmed <- data.table(student_id = eoc_vs_views_4$student_id, 
                                     eoc_score = (eoc_vs_views_4$n_correct/eoc_vs_views_4$n_possible), 
                                     video_proportion = eoc_vs_views_4$proportion_video)
eoc_vs_views_5_trimmed <- data.table(student_id = eoc_vs_views_5$student_id, 
                                     eoc_score = (eoc_vs_views_5$n_correct/eoc_vs_views_5$n_possible), 
                                     video_proportion = eoc_vs_views_5$proportion_video)


#scatter plots of video watched vs. score for each chapter
ggplot(eoc_vs_views_4_trimmed, aes(x = video_proportion, y = eoc_score)) + geom_point() +
  xlab("Proportion of Video Watched") + ylab("EOC Score") + ggtitle("Videos Watched vs Score on EOC (Ch #4)")
ggplot(eoc_vs_views_5_trimmed, aes(x = video_proportion, y = eoc_score)) + geom_point() +
  xlab("Proportion of Video Watched") + ylab("EOC Score") + ggtitle("Videos Watched vs Score on EOC (Ch #5)")


#store each chapter mean in checkpoints_score_means
checkpoints_score_means <- c(1:16)


#filter data by scores for each chapter, then store mean scores for each
for(i in 1:16){
  checkpoint <- filter(checkpoints_eoc, chapter_number == i)
  score <- c(checkpoint$n_correct / checkpoint[i]$n_possible)
  checkpoints_score_means[i] <- mean(score)
}

#check summary values
summary(checkpoints_score_means)
sd(checkpoints_score_means)
x_values <- 1:16

checkpoints_score_meansNo1 <- checkpoints_score_means[-c(1)]

#plot avg EOC scores
plot(x_values, checkpoints_score_means, type = "p", xaxt = 'n', 
     xlab = "Chapter", ylab = "Mean EOC Score", main = "Average EOC Scores")
axis(1, at = c(1:16))
model <- lm(checkpoints_score_meansNo1 ~ x_valuesNo1)
summary(model)


for (i in 1:16) {
  print(summary(filter(checkpoints_eoc, chapter_number == i)))
}

# TO DO: investigating downward trend
mean_values <- page_views
  group_by(student_id) %>%
  summarize(mean_values = mean(idle_brief, na.rm = TRUE))





