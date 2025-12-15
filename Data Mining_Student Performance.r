#Section 2 - Data Cleaning
df<-read.csv("/Users/bruklinanoka/Desktop/StudentPerformanceFactors.csv")
str(df)
dim(df)
summary(df)
View(df) # ORIGINAL DATAFRAME, UNCLEANED

#Checking for missing or null values 
missing_counts <- colSums(is.na(df))
missing_counts 

#Converting character variables into factor
library(tidyverse)
df <- df %>% mutate(across(where(is.character), as.factor))
str(df)

#Renaming columns for clearer readability
names(df)
df <- df %>% rename(
  Test_Score            = Exam_Score,
  Parent_Involvement    = Parental_Involvement,
  Hours_Slept           = Sleep_Hours,
  Previous_Test_Scores  = Previous_Scores
)

View(df) # WHEN YOU RUN THE CODE, IT SHOWS THE CLEANED DATAGFRAME


#Correction of spelling of variables
library(dplyr)
df$Gender <- dplyr::recode(df$Gender,
                           "female" = "Female",
                           "male"   = "Male")

#Section 3 - Visualizations

#Visualization 1
library(ggplot2)
ggplot(df, aes(x = Test_Score)) +
  geom_histogram(bins = 10, fill = "blue", color ="white") +
  labs(title = "Distribution of Test Scores",
       x = "Test Score",
       y = "Frequency")


# Visualization 2 
library(ggplot2)
ggplot(df, aes(x = Hours_Studied, y = Test_Score)) +
  geom_point(alpha = 0.6, color = "green") +
  geom_smooth(color = "black") +
  labs(title = "Relationship Between Study Hours and Test Score",
       x = "Weekly Study Hours",
       y = "Final Exam Score")

#Visualization 3
library(ggplot2)
ggplot(df, aes(x = Gender, y = Test_Score, fill = Gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(title = "Test Scores by Gender",
       x = "Gender",
       y = "Final Exam Score")

#Query 1 
library(sqldf)
sqldf("
  SELECT Gender,
         min(Test_Score) AS min_exam_score,
         max(Test_Score) AS max_exam_score,
         avg(Test_Score) AS avg_exam_score
  FROM df
  GROUP BY Gender
")

#Queries 2&3
library(sqldf)
attendance_summary <- sqldf("
  SELECT
    Attendance,
    AVG(Test_Score) AS avg_test_score,
    COUNT(*) AS n_students
  FROM df
  GROUP BY Attendance
  ORDER BY Attendance
")
attendance_summary

library(sqldf)
top_5_highest <- sqldf("
  SELECT
    Attendance,
    AVG(Test_Score) AS avg_test_score,
    COUNT(*) AS n_students
  FROM df
  GROUP BY Attendance 
  ORDER BY avg_test_score DESC
  LIMIT 5
")
top_5_highest 

top_5_lowest <- sqldf("
  SELECT
    Attendance,
    AVG(Test_Score) AS avg_test_score,
    COUNT(*) AS n_students
  FROM df
  GROUP BY Attendance
  ORDER BY avg_test_score ASC
  LIMIT 5
")
top_5_lowest


#Visualization 4
library(ggplot2)
ggplot(attendance_summary,
       aes(x = Attendance, y = avg_test_score)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Test Score by Attendance",
    x = "Attendance (%)",
    y = "Average Test Score"
  ) +
  theme_minimal()

#Query 4
library(sqldf)
learning_dis_summary <- sqldf("
  SELECT
    Learning_Disabilities,
    COUNT(*) AS n_students,
    AVG(Test_Score) AS avg_test_score,
    MIN(Test_Score) AS min_test_score,
    MAX(Test_Score) AS max_test_score
  FROM df
  GROUP BY Learning_Disabilities
  ORDER BY Learning_Disabilities
")

learning_dis_summary

##Visualization 5
library(ggplot2)
ggplot(learning_dis_summary,
       aes(x = Learning_Disabilities,
           y = avg_test_score,
           fill = Learning_Disabilities)) +
  geom_col() +
  labs(
    title = "Average Exam Score by Learning Disability Status",
    x = "Learning Disabilities (Yes / No)",
    y = "Average Exam Score"
  ) +
  theme_minimal()

#Query 5
library(sqldf)
sleep_summary_sql <- sqldf("
  SELECT
    Hours_Slept,
    AVG(Test_Score) AS avg_test_score,
    COUNT(*) AS n_students
  FROM df
  GROUP BY Hours_Slept
  ORDER BY Hours_Slept
")
sleep_summary_sql

#Visualization 6
library(dplyr)
sleep_summary <- df %>%
  group_by(Hours_Slept) %>%
  summarise(avg_test = mean(Test_Score))

ggplot(sleep_summary, aes(x = Hours_Slept, y = avg_test)) +
  geom_line(color = "black") +
  geom_point(size = 2) +
  labs(
    title = "Average Test Score by Hours Slept",
    x = "Hours Slept",
    y = "Average Test Score"
  ) +
  theme_minimal()

#Visualization 7
library(ggplot2)
ggplot(df, aes(
  x = Peer_Influence,
  y = Test_Score,
  fill = Motivation_Level
)) +
  geom_violin(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  labs(title = "Test Score Distribution by Peer Influence & Motivation") +
  theme_minimal()


#Visualization 8
library(ggplot2)
ggplot(df, aes(x = Tutoring_Sessions, y = Test_Score)) +
  geom_jitter(width = 0.15, alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "yellow") +
  labs(title = "Tutoring Sessions vs Exam Score",
       x = "Tutoring Sessions",
       y = "Final Exam Score")

#Query 6
library(sqldf)
sqldf("
  SELECT
    CASE
      WHEN Tutoring_Sessions = 0 THEN 'No Tutoring'
      WHEN Tutoring_Sessions BETWEEN 1 AND 3 THEN '1-3 Sessions'
      ELSE '4+ Sessions'
    END AS tutoring_group,
    COUNT(*)        AS n_students,
    AVG(Test_Score) AS avg_test_score
  FROM df
  GROUP BY tutoring_group
  ORDER BY avg_test_score DESC
")

#Visualization 9
library(dplyr)
df %>%
  group_by(School_Type) %>%
  summarize(avg_score = mean(Test_Score, na.rm = TRUE)) %>%
  ggplot(aes(x = School_Type, y = avg_score, fill = School_Type)) +
  geom_col(alpha = 0.7) +
  labs(
    title = "Average Exam Score by School Type",
    x = "School Type",
    y = "Average Exam Score"
  ) 

#Query 7
library(sqldf)
sqldf("
  SELECT
    School_Type,
    COUNT(*) AS n_students,
    AVG(Test_Score) AS avg_exam_score
  FROM df
  GROUP BY School_Type
  ORDER BY avg_exam_score DESC
")

#Visualization 10
library(ggplot2)
ggplot(df, aes(x = Internet_Access, y = Test_Score, 
               fill = Internet_Access)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Exam Score Distribution by Internet Access",
    x = "Internet Access",
    y = "Exam Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Visualization 11
library(dplyr)
library(ggplot2)
avg_scores <- df %>%
  group_by(Previous_Test_Scores) %>%
  summarize(
    avg_test_score = mean(Test_Score),
    n_students = n()
  )

ggplot(avg_scores, aes(x = Previous_Test_Scores, y = avg_test_score)) +
  geom_point(color = "darkblue") +
  geom_line(color = "darkblue") +
  labs(title = "Average Previous Test Score vs Average Final Test Score",
       x = "Average Previous Test Score",
       y = "Average Final Test Score")

#Visualization 12
library(ggplot2)
ggplot(df, aes(x = Hours_Studied, y = Test_Score,
                      color = Motivation_Level)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Test Score vs Hours Studied by Motivation Level",
    x = "Hours Studied",
    y = "Test Score"
  )

#Query 8
library(sqldf)
sqldf("
SELECT Parent_Involvement,
       COUNT(*) AS n_students,
       ROUND(AVG(Test_Score), 2) AS avg_score
FROM df
GROUP BY Parent_Involvement
ORDER BY avg_score DESC
")

#Query 9
library(sqldf)
sqldf("
  SELECT
    Teacher_Quality,
    COUNT(*)        AS n_students,
    AVG(Test_Score) AS avg_test_score
  FROM df
  GROUP BY Teacher_Quality
  ORDER BY avg_test_score DESC
")

#Queary 10
library(sqldf)
sqldf("
SELECT Family_Income,
       Access_to_Resources,
       COUNT(*) AS n_students,
       ROUND(AVG(Test_Score), 2) AS avg_score
FROM df
GROUP BY Family_Income, Access_to_Resources
ORDER BY avg_score DESC
")

#Query 11 - Primary Key & Foreign Key
library(sqldf)
df$Student_ID <- 1:nrow(df)
StudentInfo <- df %>%
  select(Student_ID,
         Gender,
         Parental_Education_Level,
         Family_Income,
         School_Type,
         Peer_Influence)

library(sqldf)
StudentPerformance <- df %>%
  select(Student_ID,        
         Test_Score,
         Previous_Test_Scores,
         Hours_Studied,
         Hours_Slept,
         Motivation_Level,
         Parent_Involvement,
         Access_to_Resources,
         Internet_Access,
         Tutoring_Sessions,
         Physical_Activity,
         Attendance)

library(sqldf)
joined_df <- sqldf("
  SELECT si.Student_ID,
         si.Gender,
         si.Parental_Education_Level,
         sp.Test_Score,
         sp.Hours_Studied,
         sp.Motivation_Level
  FROM StudentInfo AS si
  JOIN StudentPerformance AS sp
    ON si.Student_ID = sp.Student_ID
")

head(joined_df)

#Query 12
library(sqldf)
sqldf("
  SELECT
    Student_ID,
    Gender,
    Parental_Education_Level,
    Hours_Studied,
    Motivation_Level,
    Test_Score
  FROM joined_df
  ORDER BY Test_Score DESC
  LIMIT 10
")


