#Install the packages needed for a dimensionality Reduction 
install.packages(c("ggplot2", "tidyverse", "factoextra", "caret"))
#Import the needed libraries
library(ggplot2) #Graphs, plots, charts
library(tidyverse) #
library(factoextra) #
library(caret) #

library(readr) #Import the csv and be able to read it 
OG_baseball_data <- read_csv("R-Studio (Data Science)/mlb-batter-exit-velocity.csv")
View(OG_baseball_data)

#display a summary of the data
str(OG_baseball_data)
summary(OG_baseball_data)
#All statistical categories (min, median, max, quartiles) of the data are shown for each column

#Remove the irrelevant data
baseball_data <- OG_baseball_data %>% select(-rank, -player)

#Drop the NA's using the omit statement
baseball_data <- na.omit(baseball_data)

#Normalize the data for equal numerical data
baseball_scale <- scale(baseball_data)

#Get a correlation matrix of the now normalized data
baseball_corr_matrix <- cor(baseball_scale)

#Display a correlation matrix
library(corrplot)
corrplot::corrplot(baseball_corr_matrix, method = "color", type = "upper")
View(baseball_corr_matrix)

#Hard Hit 95mph+ and batted ball events with a correlation score of 0.823
#Launch angle and Average Distance with score of 0.832
#Batted ball events with exit velocity and hard hit percentage define the power of a swing 
#Test a unit of power for last example

baseball_data <- baseball_data %>%
  mutate(Power = ((average_ev * hard_hit_percentage) / batted_ball_events) * 1.73448503) %>% 
  select(-average_ev, -hard_hit_percentage, -batted_ball_events)
View(baseball_data)

#Max without 1.734 was 57.654
#To get a Max of 100 (be on a scale of 100), multiply the end result by 1.73448503


#reprint summary of data without the categorical Data, and addition of Power
summary(baseball_data)

#Perform a Principle Component Analysis(PCA) to reduce the dimensions
pca_baseball <- prcomp(baseball_data, center = TRUE, scale. = TRUE)

#Summarize the PCA Results
summary(pca_results)

#Plot the variance of the data 
plot(pca_baseball, type = 'lines', main = 'screen plot',) +
labs(x = 'Principal Components', y = 'Variance' )
#Screen Plot = High Variance from PC1-PC3, elbow officially flattens at 9

#Produce a graph of power and total barrels
ggplot(baseball_data) + geom_point(aes(x = Power, y = total_barrels), color = 'skyblue') +
  labs(x = "Power", y = "total_barrels")

#Correlation Score for power and total barrels
corr_power_barrels <- cor(baseball_data$Power, baseball_data$total_barrels)
print(corr_power_barrels)

linear_model <- lm(baseball_data$Power ~ baseball_data$total_barrels)
print(linear_model)

coefficients <- coef(linear_model)
print(coefficients)
lm_y_intercept <- coefficients[1]
lm_slope <- coefficients[2]

cat("Y intercept: ", lm_y_intercept, "Slope: ", lm_slope) #print multiple things in one line, can't use print

ggplot(baseball_data) + geom_point(aes(x = Power, y = total_barrels)) + 
  geom_smooth(aes(x = Power, y = total_barrels), method = lm, se = F)

power_barrels_cont_table <- table(baseball_data$Power, baseball_data$total_barrels)
print(power_barrels_cont_table)

chisq.test(power_barrels_cont_table)
