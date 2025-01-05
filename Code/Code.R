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
library(dplyr)

# Perform the t-tests
# Independent t-test: Pre-treatment Scores
t1 <- t.test(Score ~ Group, data = filter(data, Time == "Pre-Treatment"))

# Independent t-test: Post-treatment Scores
t2 <- t.test(Score ~ Group, data = filter(data, Time == "Post-Treatment"))

# Paired t-test: Change in Scores
# Create a column for the change in scores (Post - Pre)
data <- data %>%
  group_by(ID) %>%  # Ensure unique participant IDs
  mutate(Change = Score[Time == "Post-Treatment"] - Score[Time == "Pre-Treatment"]) %>%
  ungroup()

t3 <- t.test(filter(data, Group == "Experimental")$Change, 
             filter(data, Group == "Control")$Change)

# Create a summary table
results_table <- tibble(
  `Test Type` = c("Independent t-test", "Independent t-test", "Paired t-test"),
  `Group Comparison` = c("Pre-treatment Scores", "Post-treatment Scores", "Change in Scores"),
  `T-value` = c(t1$statistic, t2$statistic, t3$statistic),
  `P-value` = c(t1$p.value, t2$p.value, t3$p.value),
  `Conclusion` = ifelse(c(t1$p.value, t2$p.value, t3$p.value) < 0.05, "Significant", "Not Significant")
)

# View the results table
print(results_table)

# Save the table in a pretty format (optional)
library(knitr)
kable(results_table, digits = 3, caption = "Statistical Test Results")
