#Hypothesis: For males and females between the ages of 18 and 40, VR based therapy is more effective than traditional therapy in alleviating symptoms of depression.


##Packages used
#install.packages("ggplot2")
#install.packages("devtools")
#install.packages("dplyr")
#devtools::install_github("r-lib/conflicted")


# Load Packages
library(ggplot2)
library(tidyverse)
library(conflicted)
library(dplyr)
#Preference dplyr for conflicts
conflicts_prefer(dplyr::filter)


#Define Dataset
depression_data <- read.csv("C:/Users/Ruth/Desktop/Ca1_RuthC/RC_Data_CA1/Data/participants_data_final.csv")




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

# Boxplot of change in score by group
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

# Load the CSV data
data <- read.csv("C:/Users/Nolly/RStudioCA1/RuthRepo/RC_Data_CA1/Data/participants_data_final.csv")

head(data)

# Inspect the structure of the Data
str(data)

# Summarise statistics of CSV data
summary(data)

# Check data for missing or null values
sum(is.na(data)) # Total of missing values
colSums(is.na(data)) # Missing values in each column

# Check structure of data once more
str(data)

# Check for duplicate rows
sum(duplicated(data))

# Check each column has the correct data type
sapply(data, class)

# convert 'group' and 'gender' from character to factors for easier analysis
data$Group <- as.factor(data$Group)
data$Gender <- as.factor(data$Gender)

#Check data once more
sapply(data, class)

# Get summary statistics to show no character strings
summary(data)

# T-test for pre-treatment scores
t_test_pre <- t.test(Pre_Treatment_Score ~ Group, data = data)

# Show results of the t-test
summary(t_test_pre)

# T-test for post-treatment scores
t_test_post <- t.test(Post_Treatment_Score ~ Group, data = data)

# show the results of the t-test
summary(t_test_post)

# Create variable for (Post - Pre) called Change_in_Score
data$Change_in_score <- data$Post_Treatment_Score - data$Pre_Treatment_Score

# Paired t_test for control group (CBT)
paired_t_test_control <- t.test(data$Pre_Treatment_Score[data$Group == "Control (CBT)"])
data$Post_Treatment_Score[data$Group == "Control (CBT)"]
paired = TRUE
                                
# Show the results for the control group
summary(paired_t_test_control)

#Paired t-test for experimental group (AR)
paired_t_test_experimental <- t.test(data$Pre_Treatment_Score[data$Group == "Experimental (VR)"], 
data$Post_Treatment_Score[data$Group == "Experimental (VR)"], 
paired = TRUE)

# Show the results of Experimental Group
summary(paired_t_test_experimental)








