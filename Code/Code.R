#Hypothesis: For males and females between the ages of 18 and 40, VR based therapy is more effective than traditional therapy in alleviating symptoms of depression.

install.packages("ggplot2")
class(data)

depression_data <- read.csv("C:/Users/Ruth/Desktop/Ca1_RuthC/RC_Data_CA1/Data/participants_data_final.csv")


# Test Plot
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()


# Load Packages
library(ggplot2)



head(depression_data)          # View first few rows
dim(depression_data)           # Check dimensions (rows and columns)
names(depression_data)         # Check column names

#Pre Treatment Histogram
ggplot(depression_data, aes(x = Pre_Treatment_Score)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(title = "Distribution of Pre-Treatment Scores", x = "Pre-Treatment Score", y = "Frequency") +
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

