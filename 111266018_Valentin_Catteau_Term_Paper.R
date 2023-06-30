# packages used
library(dplyr)
library(ggplot2)
library(plm)
library(readxl)
library(tidyverse)



# Causal Inference and Data Science in Economics


  # Final paper

# Topic of research: Effect of a minimum wage increase on unemployment rate: a Difference-in-Differences approach.

#Chinese Name: 萬瓏棠
#English Name: Valentin CATTEAU
#UID: 111266018
#E-mail: catteauv.pro@gmail.com



    # DATA MANIPULATION & CLEANING 



# Import 2 data sets

setwd("/Users/valentincatteau/Desktop/Education/3. NCCU/2. S2 - Spring 2023/2. Causal Inference and Data Science in Economics/Assignments/Final")

wage <- read.csv("Minimum_Wage_Data.csv")
unemployment <- read_excel("unemployment_data.xls", 1)

# examine data

summary(wage)
summary(unemployment)

# select variables and observations of interest

# Filter observations of interest (drop Guam, US Virgin Islands, Puerto Rico, select 1980-2018)
# Arrange observations by State and year, then add an ID variable to help identify each observation

wage <- wage %>%
  filter(State != "U.S. Virgin Islands" & State != "Guam" & State != "Puerto Rico" & Year > 1979 & Year < 2019) %>%
  arrange(State, Year) %>%
  mutate(ID = row_number())

# select only variables of interests while rearranging the order

wage <- wage %>%
  select(ID, State, Year, Federal.Minimum.Wage, Federal.Minimum.Wage.2020.Dollars, State.Minimum.Wage, State.Minimum.Wage.2020.Dollars, Effective.Minimum.Wage, Effective.Minimum.Wage.2020.Dollars)

# Do the same thing for the unemployment data set

unemployment <- unemployment %>%
  arrange(State, Year) %>%
  mutate(ID = row_number())

unemployment <- unemployment %>% select(ID, State, Year, Unemployment_rate)

# At this stage, I have 2 data sets for my empirical analysis that have now been cleaned.
# I can merge the variable Unemployment_rate from the unemployment data set to the wage data set in new data set called full_data
# This new data set will be the final data set with all variables and observations of interest from both previous data sets

full_data <- wage %>%
  mutate(Unemployment_Rate = unemployment$Unemployment_rate)

# create additional variables for further use in the empirical analysis (state ID, change in unemployment rate, change in minimum wage, both in absolute value and in %)

# state_ID

state_ID_df <- full_data %>%
  distinct(State, .keep_all = FALSE) %>%
  mutate(State_ID = seq_along(State))

full_data <- left_join(state_ID_df, full_data, by = "State")

View(full_data)

# change in effective minimum wage (absolute value and %)
# change in effective minimum wage in 2020 US$ (absolute value and in %)
# change in unemployment rate (absolute value and in %)

# I assign NA values for Year 1980 because it is the first observation, the "base year" for each state

full_data$`Diff_EMW` <- c(NA, diff(full_data$Effective.Minimum.Wage))
full_data$`Diff_EMW`[full_data$Year == 1980] <- NA

full_data$`Diff_EMW_%` <- full_data$`Diff_EMW` / lag(full_data$Effective.Minimum.Wage, default = NA) * 100
full_data$`Diff_EMW_%` <- round(full_data$`Diff_EMW_%`, 2)

full_data$`Diff_EMW_2020_USD` <- c(NA, diff(full_data$Effective.Minimum.Wage.2020.Dollars))
full_data$`Diff_EMW_2020_USD`[full_data$Year == 1980] <- NA

full_data$`Diff_EMW_2020_USD_%` <- full_data$`Diff_EMW_2020_USD` / lag(full_data$Effective.Minimum.Wage.2020.Dollars, default = NA) * 100
full_data$`Diff_EMW_2020_USD_%` <- round(full_data$`Diff_EMW_2020_USD_%`, 2)

full_data$`Diff_Unemployment_Rate` <- c(NA, diff(full_data$Unemployment_Rate))
full_data$`Diff_Unemployment_Rate`[full_data$Year == 1980] <- NA

full_data$`Diff_Unemployment_Rate_%` <- full_data$`Diff_Unemployment_Rate` / lag(full_data$Unemployment_Rate, default = NA) * 100
full_data$`Diff_Unemployment_Rate_%` <- round(full_data$`Diff_Unemployment_Rate_%`, 2)

# reorder variables

full_data <- full_data %>%
  select(ID, State_ID, State, Year, Effective.Minimum.Wage, Diff_EMW, `Diff_EMW_%`, Effective.Minimum.Wage.2020.Dollars, Diff_EMW_2020_USD, `Diff_EMW_2020_USD_%`, Unemployment_Rate, Diff_Unemployment_Rate, `Diff_Unemployment_Rate_%`)



    # DATA VISUALIZATION



# summary statistics of data set and variables of interest (unemployment rate over time, minimum wage overtime, change in effective minimum wage)




# plot basic distribution of data set (bar plot, line plot)

#average of unemployment and Effective minimum wage, interquartile, outliers?

# line plot of all 50 states' (+ D.C.) unemployment rate

ggplot(full_data, aes(x = Year, y = Unemployment_Rate, group = State, color = State)) +
  geom_line() +
  labs(title = "Yearly Unemployment Rate",
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme_minimal()

# bar plot of all 50 states' (+D.C.) change in effective minimum wage

ggplot(full_data, aes(x = Year, y = Diff_EMW, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Yearly Change in Effective Minimum Wage",
       x = "Year",
       y = "Change in Effective Minimum Wage") +
  theme_minimal()

# line plot of all 50 states' (+ D.C.) Effective Minimum Wage (nominal)

ggplot(full_data, aes(x = Year, y = Effective.Minimum.Wage, group = State, color = State)) +
  geom_line() +
  labs(title = "Effective Minimum Wage (nominal)",
       x = "Year",
       y = "Effective Minimum Wage (nominal)") +
  theme_minimal()

# line plot of all 50 states' (+ D.C.) Effective Minimum Wage in 2020 USD

ggplot(full_data, aes(x = Year, y = Effective.Minimum.Wage.2020.Dollars, group = State, color = State)) +
  geom_line() +
  labs(title = "Effective Minimum Wage in 2020 USD",
       x = "Year",
       y = "Effective Minimum Wage (2020 USD)") +
  theme_minimal()


    # LINEAR REGRESSION

# relationship between difference in effective minimum wage and unemployment rate

regression <- lm(Unemployment_Rate~Diff_EMW, data = full_data)
regression

summary(regression)

# relationship between change in effective minimum wage and demographics-specific unemployment rate





    # DiD DESIGN

# create a dummy variable with rules "increase in minimum wage (in %) < 10 = 0" and "increase in minimum wage (in %) >= 10 = 1"

full_data$dummy_increase <- ifelse(full_data$'Diff_EMW_%' < 10, 0, 1)

# bar plot of all 50 states' (+D.C.) change in effective minimum wage above 10%

ggplot(full_data, aes(x = Year, y = dummy_increase, fill = State)) +
  geom_bar(stat = "identity") +
  labs(title = "Yearly Change in Effective Minimum Wage (yearly increases above 10%)",
       x = "Year",
       y = "Change in Effective Minimum Wage (over 10%)") +
  theme_minimal()

# create new data sets for states-pairing

GA_SC <- filter(full_data, State %in% c("Georgia", "South Carolina"))
AL_MS <- filter(full_data, State %in% c("Alabama", "Mississippi"))
KY_TN <- filter(full_data, State %in% c("Kentucky", "Tennessee"))
OR_WA <- filter(full_data, State %in% c("Oregon", "Washington"))
MA_RI <- filter(full_data, State %in% c("Massachusetts", "Rhode Island"))
NC_SC <- filter(full_data, State %in% c("North Carolina", "South Carolina"))
MN_WI <- filter(full_data, State %in% c("Minnesota", "Wisconsin"))
IN_OH <- filter(full_data, State %in% c("Indiana", "Ohio"))
AL_TN <- filter(full_data, State %in% c("Alabama", "Tennessee"))


# use dummy to 



# import demographics-specific unemployment data & merge to existing data set


# run regression for each pairing for overall unemployment rate + different demographics groups






model <- lm(Unemployment_Rate, data = full_data)
tidy(model)

