# causal_inference<br>
NCCU / Causal Inference and Data Science in Economics research project<br>

Chinese Name: 萬瓏棠<br>
English Name: Valentin CATTEAU<br>
UID: 111266018<br>
E-mail: catteauv.pro@gmail.com<br>

**README**<br>
This repository contains code for analyzing the effect of a minimum wage increase on the unemployment rate using regression analysis, data visualization and a Difference-in-Differences (DiD) approach. The code is written in R and utilizes various packages for data manipulation, cleaning, visualization, and regression analysis.

**Packages Used**<br>
The following R packages are used in this code:<br>

- broom
- dplyr
- ggplot2
- lmtest
- plm
- readxl
- sandwich
- tidyverse

**Data**<br>
Two datasets are imported and used for the analysis: "Minimum_Wage_Data.csv" and "unemployment_data.xls". Make sure to update the directory path accordingly or place the files in the correct location.

**Data Manipulation & Cleaning**<br>
The code first imports the datasets and then performs various data manipulation and cleaning steps. The steps include filtering out irrelevant observations, selecting variables of interest, creating additional variables, and merging the datasets. The resulting cleaned dataset is stored in the "full_data" object.

**Data Visualization**<br>
After cleaning the data, the code provides various data visualizations to explore the variables of interest. These visualizations include line plots of the unemployment rate, bar plots of the change in effective minimum wage, and line plots of the effective minimum wage over time.

**Linear Regression**<br>
The code performs linear regression analysis to examine the relationship between the unemployment rate and the effective minimum wage. The results of the regression models are displayed, and scatter plots are generated to visualize the relationships.

**Difference-in-Differences (DiD) Design**<br>
The code implements a DiD design by creating a dummy variable based on the change in the effective minimum wage. The code then generates bar plots and line plots to compare the unemployment rate and minimum wage over time for specific state pairs of interest.
