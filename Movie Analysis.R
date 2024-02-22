# Library Import
library(tidyverse)  # For data manipulation and visualization
library(broom)     # For tidying regression results
library(GGally)     # For correlation plot
library(readxl)  # For reading the Excel file
library(car)
library(lmtest)
library(ggplot2)  # For plotting
library(sandwich) # For heteroscedasticity-robust covariance matrices

# Dataset Import
excel_file_path <- "box_office_final.xlsx"
movies <- read_excel(excel_file_path)

# Display all variables and values
str(movies)


#_____________________
# Variable Conversions
# Adjusting date variable to consider only months
movies$release_month <- as.factor(format(movies$release_Date, "%m"))
# Convert 'release_month' to numeric
movies$release_month <- as.numeric(as.character(movies$release_month))

# Convert categorical non-dummy features to factors
movies$country <- as.factor(movies$country)
movies$age_rating <- as.factor(movies$age_rating)


#____________
# Exploration (Basic Statistic Analysis)
summary(movies)
#For the purpose of multiple linear regression analysis, we won't use director, writer, actor and name
#variables. This is why they weren't converted to factors.

# Boxplot gross income vs months
ggplot(movies, aes(x = factor(release_month), y = gross)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Month", y = "Gross Income") +
  ggtitle("Distribution of Gross Income by Month") +
  scale_y_continuous(
    limits = c(0, 1.0e+09)
  )

# Assess statistical significance between gross income and release month
# ANOVA example
anova_model <- aov(gross ~ release_month, data = movies)
summary(anova_model)
# Let's apply the logarithmic transformation to gross income
anova_model2 <- aov(log(gross) ~ release_month, data = movies)
summary(anova_model2)

#Since we fail to reject h0 we cannot assume that there is a significant difference in gross income between
#months. Nevertheless, the lowest values in terms of gross income coincide with the dump months (January, 
#February, August and September) and the higher income values coincide with the months usually associated to
#be more profitable (July and November). 

# Correlation identification
#Even though "release_month" is numerical, it represents months. As such, it is an ordinal variable, so we
#won't include it in the correlation matrix.
cor_matrix <- cor(movies[c('metascore', 'imdb_score', 'votes_imdb', 'runtime', 'budget')])
# Print the correlation matrix
print(cor_matrix)

#We have "metascore" and "imdb_score" with 0.67 and "votes_imdb" and "budget" with 0.65.
#As such "votes_imdb" and "imdb_score" will be dropped.
#We will preserve "metascore" over "imdb_score" since it encompasses a bigger scale of movie rating and,
#therefore, we will assume it as being more precise. Also, we will preserve "budget" over "votes_imdb" since
#the last doesnt provide that much valuable information.


#_____
# MLRM (Mulitple Linear Regression Model)
movies_model <- lm(log(gross) ~ age_rating + country + metascore + director_female + writer_female + star_female +
              budget + company_major + runtime + Action + Adventure + Animation + Biography +
              Comedy + Crime + Drama + Family + Fantasy + Horror + Mystery + Thriller + War +
              Romance + Musical + SciFi + Western + Sport+release_month,
              data = movies)

# Print the summary of the regression model
summary(movies_model)

significant_variable_names <- names(coef(movies_model)[summary(movies_model)$coefficients[, "Pr(>|t|)"] < 0.05])
significant_variable_names


#__________________
# Multicollinearity
# Calculate VIF for all variables in the model
vif_values <- vif(movies_model)
vif_values

#We will drop the variables age_rating and country and recompute the model.
movies_model2 <- lm(log(gross) ~ metascore + director_female + writer_female + star_female +
                     budget + company_major + runtime + Action + Adventure + Animation + Biography +
                     Comedy + Crime + Drama + Family + Fantasy + Horror + Mystery + Thriller + War +
                     Romance + Musical + SciFi + Western + Sport+release_month,
                    data = movies)

summary(movies_model2)


#__________________
##Joint significance
# Define the null hypothesis (at least one coefficient is non-zero)
h0 <- c("metascore = 0", "director_female = 0",
                "writer_female = 0", "star_female = 0", "budget = 0", "company_major = 0",
                "runtime = 0", "Action = 0", "Adventure = 0", "Animation = 0", 
                "Biography = 0", "Comedy = 0", "Crime = 0", "Drama = 0", "Family = 0", 
                "Fantasy = 0", "Horror = 0", "Mystery = 0", "Thriller = 0", "War = 0",
                "Romance = 0", "Musical = 0", "SciFi = 0", "Western = 0", "Sport = 0",
                "release_month = 0")

# Joint significance test
#joint_test <- linearHypothesis(movies_model2, h0)

#Since we got a warning of 'system is computationally singular', this could imply that we have variables that
#have highly correlated predictors, or maybe that one of the variables contains extremely large numbers in
#comparison with other variables. As such, this could be induced by the variable "budget", so we will try to
#use another model with log (budget)

movies_model3 <- lm(log(gross) ~ metascore + director_female + writer_female + star_female +
                      log(budget) + company_major + runtime + Action + Adventure + Animation + Biography +
                      Comedy + Crime + Drama + Family + Fantasy + Horror + Mystery + Thriller + War +
                      Romance + Musical + SciFi + Western + Sport+release_month,
                    data = movies)

summary(movies_model3)

# Introducing log increased both R2 and Adjusted R2 automatically, as well as the number of coefficients
# statistically significant.

# Define the null hypothesis (at least one coefficient is non-zero)
h0_2 <- c("metascore = 0", "director_female = 0",
        "writer_female = 0", "star_female = 0", "log(budget) = 0", "company_major = 0",
        "runtime = 0", "Action = 0", "Adventure = 0", "Animation = 0", 
        "Biography = 0", "Comedy = 0", "Crime = 0", "Drama = 0", "Family = 0", 
        "Fantasy = 0", "Horror = 0", "Mystery = 0", "Thriller = 0", "War = 0",
        "Romance = 0", "Musical = 0", "SciFi = 0", "Western = 0", "Sport = 0",
        "release_month = 0")

# Joint significance test
joint_test2 <- linearHypothesis(movies_model3, h0_2)
joint_test2

#We reject H0, so the group of variables included in the regression are jointly significant and contribute to
#explain the variance of the gross income. From this point moving forward we will disconsider movies_model and
#movies_model2, using only movies_model3 as our MLRM.

#Another important note is that, instead of computing the full test for global joint significance, we could
#have just observed the F-test value and p-value retrieved from the summary of the model.


#_________________
# Homoskedasticity
#We will test homoskedasticity with Breusch-Pagan test and with White special test.

# Breusch-Pagan Test
bptest(movies_model3)

# White Special Test
bptest(movies_model3, ~ fitted(movies_model3) + I(fitted(movies_model3)^2) )

#Since this last test had p-value < 0.05, we will reject H0, meaning that there is evidence that we have 
#heteroscedasticity.


#_______________________________
# Account for Heteroscedasticity
#We will compute heteroscedasticity-robust standard errors and use them for coefficient inference
coef_HC1<-coeftest(movies_model3, vcov = vcovHC(movies_model3, type = "HC1"))
#HC1: MacKinnon and White's heteroscedasticity-robust covariance matrix
coef_HC1

#Maybe consider "release_month", "thriller" and "horror" marginally significant


#____________
# RAMSEY TEST for misspecification assessment
reset_movies<-resettest(movies_model3)
reset_movies 

#Brief explanation: When we call resettest(movies_model3), it automatically adds squared and cubed terms of
#the fitted values to the model, and then tests whether these additional terms are jointly significant.
#In this situation, since p-value > 0.05, there is not enough evidence to #conclude that your model is
#misspecified.
#In practical terms, this means that the original linear model appears to be adequately specified, and there
#is no strong evidence suggesting the need for additional non-linear terms. 


#_________________________________
# Zero Conditional Mean Assessment
#We will implement t-test on the mean of residuals to test if it is significantly different from zero, by
#first storing all residuals in a variable named "residuals".
residuals <- residuals(movies_model3, type = "response", robust = "HC1")
residuals

residual_test <- t.test(residuals)
residual_test

#Since p-value > 0.05, there is no significant evidence to reject the null hypothesis that the true mean of
#the residuals is zero. Therefore, we have indication of the validity of the assumption of a zero conditional
#mean, indicating that, on average, the residuals are centered around zero. 

summary(movies_model3, robust = "HC1")


#_________________________
# Partial Regression Plots (statistically significant explanatory variables)
#Produce added variable plots
avPlots(movies_model3)
summary(movies_model3)

#Here, we see that the coefficient estimates are equal on both cases, which may indicate that
#heteroscedasticity may not have a substantial impact on our parameter estimates.


#____________________________
# Interpretation of Coefficients
#metascore (log-level relationship)
coef(movies_model3)['metascore'] * 100
#If the movie's metascore increases by one point, it is expected that the gross income will increase by around
#1.81%.

#log(budget) (elasticity relationship)
coef(movies_model3)['log(budget)']
#If the movie's budget increases one percent, it is expected that the gross income will increase by around
#0.95%.

#crime (log-level relationship)
(exp(coef(movies_model3)['Crime']) - 1) * 100
#If a movie is falls in the "crime" category, it is expected that the gross income will decrease by around
#54.91%.

#war (log-level relationship)
(exp(coef(movies_model3)['War']) - 1) * 100
#If a movie is falls in the "war" category, it is expected that the gross income will decrease by around
#93.67%.