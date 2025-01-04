#Hypothesis: For males and females between the ages of 18 and 40, VR based therapy is more effective than traditional therapy in alleviating symptoms of depression.

install.packages("ggplot2")
class(data)


# Load Packages
library(ggplot2)
library(tidyverse)


#Access Data
depression_data <- read.csv("C:/Users/Ruth/Desktop/Ca1_RuthC/RC_Data_CA1/Data/participants_data_final.csv")



head(depression_data)          # View first few rows
dim(depression_data)           # Check dimensions (rows and columns)
names(depression_data)         # Check column names

#Pre Treatment Histogram
ggplot(depression_data, aes(x = Pre_Treatment_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Distribution of Pre-Treatment Scores", x = "Pre-Treatment Score", y = "Frequency") +
  theme_minimal()

#Post Treatment Histogram
ggplot(depression_data, aes(x = Post_Treatment_Score)) +
  geom_histogram(binwidth = 5, fill = "red", color = "white") +
  labs(title = "Distribution of Post-Treatment Scores", x = "Post-Treatment Score", y = "Frequency") +
  theme_minimal()

# Basic boxplot
ggplot(depression_data, aes(x = Group, y = Change_in_Score, fill = Group)) +
  geom_boxplot() +
  labs(
    title = "Change in Score by Group",
    x = "Group",
    y = "Change in Score"
  ) +
  theme_minimal()

# Pre treatment Comparison Histogram
ggplot(depression_data, aes(x = Group, y = Pre_Treatment_Score, fill = Group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(
    title = "Pre-Treatment Scores by Group",
    x = "Group",
    y = "Pre-Treatment Score"
  ) +
  theme_minimal()
