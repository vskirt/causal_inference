# packages used
library(broom)
library(dplyr)
library(ggplot2)
library(lmtest)
library(plm)
library(readxl)
library(sandwich)
library(tidyverse)



# Causal Inference and Data Science in Economics


  # Final paper

# Topic of research: Effect of Minimum Wage Increase on Unemployment Rate: Evidence from the USA

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

# create additional variables for further use in the empirical analysis (state ID, change in unemployment rate, change in minimum wage (nominal and real), both in absolute value and in %)

# state_ID

state_ID_df <- full_data %>%
  distinct(State, .keep_all = FALSE) %>%
  mutate(State_ID = seq_along(State))

full_data <- left_join(state_ID_df, full_data, by = "State")

View(full_data)

# I assign NA values for Year 1980 because it is the first observation, the "base year" for each state

# change in effective minimum wage (absolute value and %)

full_data$`Diff_EMW` <- c(NA, diff(full_data$Effective.Minimum.Wage))
full_data$`Diff_EMW`[full_data$Year == 1980] <- NA

full_data$`Diff_EMW_%` <- full_data$`Diff_EMW` / lag(full_data$Effective.Minimum.Wage, default = NA) * 100
full_data$`Diff_EMW_%` <- round(full_data$`Diff_EMW_%`, 2)

# change in effective minimum wage in 2020 US$ (absolute value and in %)

full_data$`Diff_EMW_2020_USD` <- c(NA, diff(full_data$Effective.Minimum.Wage.2020.Dollars))
full_data$`Diff_EMW_2020_USD`[full_data$Year == 1980] <- NA

full_data$`Diff_EMW_2020_USD_%` <- full_data$`Diff_EMW_2020_USD` / lag(full_data$Effective.Minimum.Wage.2020.Dollars, default = NA) * 100
full_data$`Diff_EMW_2020_USD_%` <- round(full_data$`Diff_EMW_2020_USD_%`, 2)

# change in unemployment rate (absolute value and in %)

full_data$`Diff_Unemployment_Rate` <- c(NA, diff(full_data$Unemployment_Rate))
full_data$`Diff_Unemployment_Rate`[full_data$Year == 1980] <- NA

full_data$`Diff_Unemployment_Rate_%` <- full_data$`Diff_Unemployment_Rate` / lag(full_data$Unemployment_Rate, default = NA) * 100
full_data$`Diff_Unemployment_Rate_%` <- round(full_data$`Diff_Unemployment_Rate_%`, 2)

# reorder variables

full_data <- full_data %>%
  select(ID, State_ID, State, Year, Effective.Minimum.Wage, Diff_EMW, `Diff_EMW_%`, Effective.Minimum.Wage.2020.Dollars, Diff_EMW_2020_USD, `Diff_EMW_2020_USD_%`, Unemployment_Rate, Diff_Unemployment_Rate, `Diff_Unemployment_Rate_%`)



    # DATA VISUALIZATION



# summary statistics of data set and variables of interest (unemployment rate over time, minimum wage overtime, change in effective minimum wage)

summary(full_data)
str(full_data)

# average & interquartile of Unemployment Rate and Effective minimum wage

summary(full_data$Effective.Minimum.Wage)
sd(full_data$Effective.Minimum.Wage)
summary(full_data$Unemployment_Rate)
sd(full_data$Unemployment_Rate)

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

lm <- lm(Unemployment_Rate~Effective.Minimum.Wage, data = full_data)
lm
tidy(lm)
summary(lm) # slight negative correlation, as shown by the following scatter plot. This seems inconsistent with empirical expectations

scatter <- ggplot(data = full_data, aes(Effective.Minimum.Wage, Unemployment_Rate)) +
  geom_point(color = "orange") + 
  ggtitle("Unemployment Rate & Effective Minimum Wage of all 50 US States (+ D.C.) from 1980-2018") +
  ylab("Unemployement Rate") +
  geom_smooth(method='lm', formula= y~x, color = "purple") +
  xlab("Effective Minimum Wage (in USD)")
scatter

lm_Diff_EMW <- lm(Unemployment_Rate~`Diff_EMW_%`, data = full_data)
lm_Diff_EMW
tidy(lm_Diff_EMW)
summary(lm_Diff_EMW)

lm_Diff_Un <- lm(`Diff_Unemployment_Rate_%`~`Diff_EMW_%`, data = full_data)
lm_Diff_Un
tidy(lm_Diff_Un)
summary(lm_Diff_Un)

lm_Diff_adjusted <- lm(Unemployment_Rate~`Diff_EMW_2020_USD_%`, data = full_data)
lm_Diff_adjusted
tidy(lm_Diff_adjusted)
summary(lm_Diff_adjusted)


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
ggplot(GA_SC, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

AL_MS <- filter(full_data, State %in% c("Alabama", "Mississippi"))
ggplot(AL_MS, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

KY_TN <- filter(full_data, State %in% c("Kentucky", "Tennessee"))
ggplot(KY_TN, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

OR_WA <- filter(full_data, State %in% c("Oregon", "Washington"))
ggplot(OR_WA, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

MA_RI <- filter(full_data, State %in% c("Massachusetts", "Rhode Island"))
ggplot(MA_RI, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

NC_SC <- filter(full_data, State %in% c("North Carolina", "South Carolina"))
ggplot(NC_SC, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

MN_WI <- filter(full_data, State %in% c("Minnesota", "Wisconsin"))
ggplot(MN_WI, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

IN_OH <- filter(full_data, State %in% c("Indiana", "Ohio"))
ggplot(IN_OH, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

AL_TN <- filter(full_data, State %in% c("Alabama", "Tennessee"))
ggplot(AL_TN, aes(x = Year)) +
  geom_line(aes(y = Unemployment_Rate, color = State), linetype = "dashed") +
  geom_line(aes(y = Effective.Minimum.Wage, color = State), linetype = "solid") +
  labs(x = "Year", y = "Unemployment Rate", color = "Effective Minimum Wage") +
  ggtitle("Unemployment Rate and Minimum Wage Over Time") +
  theme_minimal()

# import demographics-specific unemployment data for states of interest & merge to existing state-paired data sets

# demographic data for specific states was not available before 2010, therefore making the analysis impossible
# I have found records in the US Bureau of Labor Statistics in TXT file, but as 1 separate file for each year and for each state, making the data cleaning process too time-consuming long for this paper.
# The DiD design is unfortunately limited to simple linear regression between raise in minimum wage and overall unemployment rate, while controlling for all the different factors taken into account in the state-pairing selection.

# run regression for OR-WA state-pairing for increase in effective minimum wage on overall unemployment rate 

# OR-WA
model <- lm(Unemployment_Rate ~ Diff_EMW, data = OR_WA)
tidy(model)
summary(model)

# MA-RI from 2003-2013 (common trend from 2004 - 2008, then increase in MA but not RI)

MA_RI_2 <- MA_RI %>% filter(Year > "2003") %>% filter(Year < "2013")
MA_RI_2$treatment <- ifelse(MA_RI_2$State %in% c("Massachusetts"), 1, 0) # Treatment term
MA_RI_2$post_treatment <- ifelse(MA_RI_2$Year >= 2008, 1, 0)  # Post-treatment indicator
MA_RI_2$treatment_post <- MA_RI_2$treatment * MA_RI_2$post_treatment  # Interaction term

#Estimate the DiD Model
model <- lm(Unemployment_Rate ~ treatment + post_treatment + treatment_post, data = MA_RI_2)
summary(model) # treatment_post = did_estimate
robust_se <- coeftest(model, vcov = sandwich)
summary(robust_se)

