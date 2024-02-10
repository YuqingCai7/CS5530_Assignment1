library(tidyverse)
library(ggplot2)

raw_StudentsPerformance <- read.csv("C:/Users/yuqin/Desktop/StudentsPerformance_Project/data_raw/raw_StudentsPerformance.csv")

summary_data_math <- raw_StudentsPerformance %>%
  filter(`math.score` >= 80) %>%
  group_by(`race.ethnicity`) %>%
  summarize(math.count = n())

summary_data_writing <- raw_StudentsPerformance %>%
  filter(`writing.score` >= 80) %>%
  group_by(`race.ethnicity`) %>%
  summarize(writing.count = n())

summary_data_reading <- raw_StudentsPerformance %>%
  filter(`reading.score` >= 80) %>%
  group_by(`race.ethnicity`) %>%
  summarize(reading.count = n())

summary_data <- summary_data_math %>%
  left_join(summary_data_writing, by = "race.ethnicity") %>%
  left_join(summary_data_reading, by = "race.ethnicity")

write_csv(summary_data, "C:/Users/yuqin/Desktop/StudentsPerformance_Project/data_clean/clean_StudentsPerformance.csv")

bar_width <- 0.20

ggplot(summary_data, aes(x = `race.ethnicity`)) +
  labs(x = "Race/Ethnicity", y = "Number of Individuals",
       title = "Number of Individuals with Course Score >= 80 by Race/Ethnicity") +
  scale_fill_manual(values = c("Math Score >= 80" = "blue", "Writing Score >= 80" = "red", "Reading Score >= 80" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(title = "Category")) +
  geom_bar(aes(y = math_count - bar_width/2, fill = "Math Score >= 80"), stat = "identity", width = bar_width, position = position_nudge(x = -bar_width)) +
  geom_bar(aes(y = writing_count - bar_width/2, fill = "Writing Score >= 80"), stat = "identity", width = bar_width) +
  geom_bar(aes(y = reading_count + bar_width/2, fill = "Reading Score >= 80"), stat = "identity", width = bar_width, position = position_nudge(x = bar_width))

high_score_students <- raw_StudentsPerformance %>%
  filter(`math.score` >= 80 | `reading.score` >= 80 | `writing.score` >= 80)

summary_data_prep_course <- high_score_students %>%
  group_by(`test.preparation.course`) %>%
  summarize(num_students = n())

write_csv(summary_data_prep_course, "C:/Users/yuqin/Desktop/StudentsPerformance_Project/data_clean/clean_StudentsPerformance_prep_course.csv")

bar_width <- 0.30 
ggplot(summary_data_prep_course, aes(x = `test.preparation.course`, y = num_students)) +
  geom_bar(stat = "identity", fill = "blue", width = bar_width) +  # Adjust the width here
  labs(x = "Test Preparation Course", y = "Number of Students",
       title = "Number of Students with Score >= 80 by Test Preparation Course Completion") +
  theme_minimal()

summary_data_lunch <- high_score_students %>%
  group_by(`lunch`) %>%
  summarize(num_students = n())

write_csv(summary_data_lunch, "C:/Users/yuqin/Desktop/StudentsPerformance_Project/data_clean/clean_StudentsPerformance_lunch.csv")

bar_width <- 0.30 

ggplot(summary_data_lunch, aes(x = `lunch`, y = num_students)) +
  geom_bar(stat = "identity", fill = "red", width = bar_width) +  # Adjust the width here
  labs(x = "Lunch", y = "Number of Students",
       title = "Number of Students with Score >= 80 by Lunch Type") +
  theme_minimal()


