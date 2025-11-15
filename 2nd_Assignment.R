#2nd_Assignment 
#Abel Juan Agut Rabadán
#Date of last revision:
#Descriptive statistics and linear regressions to predict GPI.

#This is the link where our data is stored 
open_project_folder <- function() {
  link <- "https://drive.google.com/drive/u/0/folders/1YNj60eZEQTmyZ7nyGbrlCxwOHrEaWEsM"
  browseURL(link)
}
open_project_folder()

#This is the selection of packages that are needed and we must charge all of them
packages <- c("dplyr", "ggplot2", "readr", "tidyr", "tidyverse", "ggrepel", "writexl")

package.check <- lapply(packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
})

library(tidyverse)
library(ggplot2)
library (dplyr)
library (readr)
library (ggrepel) 
library(tidyr)
library (writexl)

#We check the compatibility of the packages with the R version
R.version
packageDescription("tidyverse")
packageDescription("ggplot2")
packageDescription("dplyr")
packageDescription("readr")
packageDescription("ggrepel")
packageDescription("tidyr")
packageDescription("writexl")

#We create the needed folders
base_dir <- getwd()  
dir.create(file.path(base_dir, "raw-data"), showWarnings = FALSE)
dir.create(file.path(base_dir, "processed-data"), showWarnings = FALSE)
dir.create(file.path(base_dir, "figures"), showWarnings = FALSE)

#We reed ourr original dataset
data_100474258 <- read_delim("raw-data/data_100474258.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)


#Create a copy of the dataset 
Data_2ndAssignment <- data_100474258

#####################################################

#1st Exercise 
#We convert the OECD variable into factor 
Data_2ndAssignment$OECD <- as.factor(Data_2ndAssignment$OECD)
#We first calculate the main statistical indicators of GPI for countries that are part of the OECD. 
Statistical_summary_OECD <- Data_2ndAssignment %>%
  filter(OECD == 1) %>%
  summarise(
    mean_GPI = mean(GPI, na.rm = TRUE),
    median_GPI = median(GPI, na.rm = TRUE),
    min_GPI = min(GPI, na.rm = TRUE),
    max_GPI = max(GPI, na.rm = TRUE),
    sd_GPI = sd(GPI, na.rm = TRUE)
  )
if (file.exists("Statistical_summary_OECD.xlsx")) file.remove("Statistical_summary_OECD.xlsx")
write_xlsx(Statistical_summary_OECD, path = "processed-data/Statistical_summary_OECD.xlsx")
#We do the same for countries that are not part of the OECD. 
Statistical_summary_non_OECD <- Data_2ndAssignment %>%
  filter(OECD == 0) %>%
  summarise(
    mean_GPI = mean(GPI, na.rm = TRUE),
    median_GPI = median(GPI, na.rm = TRUE),
    min_GPI = min(GPI, na.rm = TRUE),
    max_GPI = max(GPI, na.rm = TRUE),
    sd_GPI = sd(GPI, na.rm = TRUE)
  )
if (file.exists("Statistical_summary_non_OECD.xlsx")) file.remove("Statistical_summary_non_OECD.xlsx")
write_xlsx(Statistical_summary_non_OECD, path = "processed-data/Statistical_summary_non_OECD.xlsx")
#We plot a bar chart with the GPI means for countries in the OECD and countries not in OECD
ggplot(Data_2ndAssignment, aes(x = factor(OECD, levels = c(1, 0)), y = GPI)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black", width = 0.5) +
  labs(
    title = "Mean Global Peace Index (GPI) by OECD Membership",
    x = "OECD Membership",
    y = "Mean GPI (degree of violence)"
  ) +
  scale_x_discrete(labels = c("1" = "OECD", "0" = "Non-OECD")) +
  theme_minimal()
#We now draw a graph with two boxplots, one for OECD and another one for Non-OECD countries considering the 
#distribution of the GPI for each of them. 
ggplot(Data_2ndAssignment, aes(x = factor(OECD, levels = c(1, 0)), y = GPI)) +
  geom_boxplot(fill = "skyblue", color = "black", width = 0.5) +
  labs(
    title = "Distribution of Global Peace Index (GPI) by OECD Membership",
    x = "OECD Membership",
    y = "GPI (degree of violence)"
  ) +
  scale_x_discrete(labels = c("1" = "OECD", "0" = "Non-OECD")) +
  theme_minimal() + theme(
    axis.title.y = element_text(margin = margin(r = 15))
  )

################################################################

#2nd exercise 
#Here again we calculate the main statistical summaries for GPI, but in this case dividing it by Continents of the "Division" variable 
statistical_summary_division <- Data_2ndAssignment %>%
  group_by(Division) %>%
  summarise(
    mean_GPI = mean(GPI, na.rm = TRUE),
    median_GPI = median(GPI, na.rm = TRUE),
    min_GPI = min(GPI, na.rm = TRUE),
    max_GPI = max(GPI, na.rm = TRUE),
    sd_GPI = ifelse(n() > 1, sd(GPI, na.rm = TRUE), 0)
  )
if (file.exists("statistical_summary_division.xlsx")) file.remove("statistical_summary_division.xlsx")
write_xlsx(statistical_summary_division, path = "processed-data/statistical_summary_division.xlsx")
#We plot a bar chart with the GPI means for each of the continents 
ggplot(Data_2ndAssignment, aes(x = reorder(Division, -GPI, FUN = mean), y = GPI)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black", width = 0.6) +
  labs(
    title = "Mean Global Peace Index (GPI) by Geographical division",
    x = "Geographical division",
    y = "Mean GPI (degree of violence)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#We now draw two graphs, one for Africa, North America and Oceania and one for Asia, South America and Europe 
#with boxplots of the distribution of the Global Peace Index for all the continents 
Data_2ndAssignment %>%
  filter(Division %in% c("Africa", "North America", "Oceania")) %>%
  ggplot(aes(x = Division, y = GPI)) +
  geom_boxplot(fill = "skyblue", color = "black", width = 0.6) +
  labs(
    title = "Distribution of Global Peace Index (GPI)",
    x = "Geographic division", 
    y = "GPI (degree of violence)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15))
  )
Data_2ndAssignment %>%
  filter(Division %in% c("Asia", "South America", "Europe")) %>%
  ggplot(aes(x = Division, y = GPI)) +
  geom_boxplot(fill = "skyblue", color = "black", width = 0.6) +
  labs(
    title = "Distribution of Global Peace Index (GPI)",
    x = "Geographic division",   
    y = "GPI (degree of violence)"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(margin = margin(r = 15))  
  )

##############################################################################3

#3rd exercise 
#We want to see which variables are quantitative.
str(Data_2ndAssignment)
#We make a scatterplot that relates GPI with military expenditure. We also add a regression line
ggplot(Data_2ndAssignment, aes(x = MilitaryExpenditure, y = GPI)) +
  geom_point(
    data = subset(Data_2ndAssignment, Country != "Ukraine"),
    color = "green", shape = 16, size = 3
  ) +
  geom_point(
    data = subset(Data_2ndAssignment, Country == "Ukraine"),
    color = "red", shape = 16, size = 3
  ) +
  geom_text_repel(
    data = subset(Data_2ndAssignment, Country == "Ukraine"),
    aes(label = Country),
    color = "red",
    size = 3.5,
    nudge_y = 0.1
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship between Military Expenditure and Global Peace Index (GPI)",
    x = "Military Expenditure (% of GDP)",
    y = "GPI (degree of violence)"
  ) +
  theme_minimal()
#Now we make the same scatterplot, but excluding Ukraine, a clear outlier 
Data_2ndAssignment %>%
  filter(Country != "Ukraine") %>%   
  ggplot(aes(x = MilitaryExpenditure, y = GPI)) +
  geom_point(color = "green", shape = 16, size = 3) +  # green dots
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship between Military Expenditure and Global Peace Index (GPI)\n(excluding Ukraine)",
    x = "Military Expenditure (% of GDP)",
    y = "GPI (degree of violence)"
  ) +
  theme_minimal()
#We make a scatterplot that relates GPI with inflation. We also add the regression line
ggplot(Data_2ndAssignment, aes(x = Inflation, y = GPI)) +
  geom_point(color = "blue", shape = 16, size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship between Inflation and Global Peace Index (GPI)",
    x = "Inflation rate (%)",
    y = "GPI (degree of violence)"
  ) +
  theme_minimal()
#We make a scatterplot that related GPI and GDPpc. We also add the regression line. 
ggplot(Data_2ndAssignment, aes(x = GDPpc, y = GPI)) +
  geom_point(color = "brown", shape = 16, size = 3) +   
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(
    title = "Relationship between GDP per Capita and Global Peace Index (GPI)",
    x = "GDP per capita (US dollars)",
    y = "GPI (degree of violence)"
  ) +
  theme_minimal()

################################################################################

#Exercise 4 
#We trace the correlation between Military Expenditure and GPI
cor(Data_2ndAssignment$MilitaryExpenditure, Data_2ndAssignment$GPI)
#We make a regression between Military Expenditure and GPI 
GPI_MilitaryExpenditure_Regression <- lm(GPI ~ MilitaryExpenditure, data = Data_2ndAssignment)
summary(GPI_MilitaryExpenditure_Regression)
GPI_MilitaryExpenditure_summary <- summary(GPI_MilitaryExpenditure_Regression)
#We make a table from the regression data we are interested in 
GPI_MilitaryExpenditure_table <- data.frame(
  Variable = rownames(GPI_MilitaryExpenditure_summary$coefficients),
  Estimate = GPI_MilitaryExpenditure_summary$coefficients[, "Estimate"],
  p_value = GPI_MilitaryExpenditure_summary$coefficients[, "Pr(>|t|)"]
)
GPI_MilitaryExpenditure_table$Multiple_R_squared <- GPI_MilitaryExpenditure_summary$r.squared
if (file.exists("GPI_MilitaryExpenditure_table.xlsx")) file.remove("GPI_MilitaryExpenditure_table.xlsx")
write_xlsx(GPI_MilitaryExpenditure_table, path = "processed-data/GPI_MilitaryExpenditure_table.xlsx")
#We run again the regression but this time excluding Ukraine 
GPI_MilitaryExpenditure_Regression_noUkraine <- lm(
  GPI ~ MilitaryExpenditure, 
  data = subset(Data_2ndAssignment, Country != "Ukraine")
)
summary(GPI_MilitaryExpenditure_Regression_noUkraine)
#We calculate the predicted values for GPI according to the model established with the Military Expenditure regression and the residuals excluding Ukraine 
fitted_vals_GPI_MilitaryExpenditure <- fitted(GPI_MilitaryExpenditure_Regression)
residuals_vals_GPI_MilitaryExpenditure <- residuals(GPI_MilitaryExpenditure_Regression)
#We plot the residuals to see if there is any pattern 
plot(fitted_vals_GPI_MilitaryExpenditure, residuals_vals_GPI_MilitaryExpenditure,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "GPI-Military Expenditure Residuals vs Fitted Values",
     pch = 16, col = "green")
abline(h = 0, col = "black", lwd = 2)
#We plot a histogram to see if there is a normal distribution 
hist(residuals_vals_GPI_MilitaryExpenditure,
     breaks = 10,             
     col = "green",        
     border = "black",          
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")


#We trace trace the correlation between Inflation and GPI
cor(Data_2ndAssignment$Inflation, Data_2ndAssignment$GPI)
#We make a regression between Inflation and GPI 
GPI_Inflation_Regression <- lm(GPI ~ Inflation, data = Data_2ndAssignment)
summary(GPI_Inflation_Regression)
GPI_Inflation_summary <- summary(GPI_Inflation_Regression)
#We make a table from the regression data we are interested in 
GPI_Inflation_table <- data.frame(
  Variable = rownames(GPI_Inflation_summary$coefficients),
  Estimate = GPI_Inflation_summary$coefficients[, "Estimate"],
  p_value = GPI_Inflation_summary$coefficients[, "Pr(>|t|)"]
)
GPI_Inflation_table$Multiple_R_squared <- GPI_Inflation_summary$r.squared
if (file.exists("GPI_Inflation_table.xlsx")) file.remove("GPI_Inflation_table.xlsx")
write_xlsx(GPI_Inflation_table, path = "processed-data/GPI_Inflation_table.xlsx")
#We calculate the predicted values for GPI according to the model established with the Inflation regression and the residuals 
fitted_vals_GPI_Inflation <- fitted(GPI_Inflation_Regression)
residuals_vals_GPI_Inflation <- residuals(GPI_Inflation_Regression)
#We plot the residuals to see if there is any pattern 
plot(fitted_vals_GPI_Inflation, residuals_vals_GPI_Inflation,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "GPI-Inflation Residuals vs Fitted Values",
     pch = 16, col = "blue")
abline(h = 0, col = "black", lwd = 2)
#We plot a histogram to see if there is a normal distribution 
hist(residuals_vals_GPI_Inflation,
     breaks = 10,             
     col = "blue",        
     border = "black",          
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

#We trace trace the correlation between GDPpc and GPI
cor(Data_2ndAssignment$GDPpc, Data_2ndAssignment$GPI)
#We make a regression between GDPpc and GPI 
GPI_GDPpc_Regression <- lm(GPI ~ GDPpc, data = Data_2ndAssignment)
summary(GPI_GDPpc_Regression)
GPI_GDPpc_summary <- summary(GPI_GDPpc_Regression)
#We make a table from the regression data we are interested in 
GPI_GDPpc_table <- data.frame(
  Variable = rownames(GPI_GDPpc_summary$coefficients),
  Estimate = GPI_GDPpc_summary$coefficients[, "Estimate"],
  p_value = GPI_GDPpc_summary$coefficients[, "Pr(>|t|)"]
)
GPI_GDPpc_table$Multiple_R_squared <- GPI_GDPpc_summary$r.squared
if (file.exists("GPI_GDPpc_table.xlsx")) file.remove("GPI_GDPpc_table.xlsx")
write_xlsx(GPI_GDPpc_table, path = "processed-data/GPI_GDPpc_table.xlsx")
#We calculate the predicted values for GPI according to the model established with the GDPpc regression and the residuals 
fitted_vals_GPI_GDPpc <- fitted(GPI_GDPpc_Regression)
residuals_vals_GPI_GDPpc <- residuals(GPI_GDPpc_Regression)
#We plot the residuals to see if there is any pattern 
plot(fitted_vals_GPI_GDPpc, residuals_vals_GPI_GDPpc,
     xlab = "Fitted values",
     ylab = "Residuals",
    main = "GPI-GDPpc Residuals vs Fitted Values",
    pch = 16, col = "brown")
abline(h = 0, col = "black", lwd = 2)
#We plot a histogram to see if there is a normal distribution 
hist(residuals_vals_GPI_GDPpc,
     breaks = 10,             
     col = "brown",        
     border = "black",          
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

################################################################################

#Exercise 5
#We read the dataset with the 5 new counties and its GDPpc
Five_more_country <- Five_more_country <- data.frame(
  Country = c("Austria", "Belarus", "India", "Australia", "Guinea"),
  GDPpc   = c(56.5059683, 7.82905314, 2.48484543, 64.7117656, 1.66393781)
)
#We predict for these 5 countries according to what we got in our previous model their GPI
predicted_GPI_Five_countries <- predict(GPI_GDPpc_Regression, newdata = Five_more_country)
Five_more_country$Predicted_GPI <- predicted_GPI_Five_countries
#We create a vector with the actual GPI for each of the countries 
actual_GPI_values <- c(1.313, 2.291, 2.319, 1.536, 2.423)
#We add the actual value to the Five_more_country table 
Five_more_country$Actual_GPI <- actual_GPI_values
#We pivot longer the table for its representation in a side to side bar chart 
Five_more_country_long <- Five_more_country %>%
  pivot_longer(cols = c(Predicted_GPI, Actual_GPI),
               names_to = "Type",
               values_to = "GPI_Value")

ggplot(Five_more_country_long, aes(x = Country, y = GPI_Value, fill = Type)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.6),
           width = 0.6) +                           
  labs(title = "Predicted vs Actual GPI by Country",
       x = "Country",
       y = "GPI Value") +
  scale_fill_manual(values = c("steelblue", "red"),
                    name = "GPI Type",
                    labels = c("Actual GPI", "Predicted GPI")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#We make a scatterplot with the predicited and actual values
ggplot(Five_more_country_long, aes(x = GDPpc, y = GPI_Value, color = Type)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Country),
                  size = 3,
                  max.overlaps = Inf,
                  nudge_y = 0.05,        
                  direction = "y",      
                  segment.color = 'grey50',
                  box.padding = 0.5,
                  point.padding = 0.5) +
  scale_color_manual(values = c("Predicted_GPI" = "red", "Actual_GPI" = "steelblue"),
                     labels = c("Actual", "Predicted"),
                     name = "GPI Type") +
  labs(title = "GPI vs GDP per Capita: Predicted vs Actual",
       x = "GDP per Capita (thousands USD)",
       y = "GPI") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



################################################################################

#Exercise 6
#Mutate division as a factor 
Data_2ndAssignment$Division <- as.factor(Data_2ndAssignment$Division)
Data_2ndAssignment$Division <- relevel(Data_2ndAssignment$Division, ref = "Europe")
#We trace a regression model with GDPpc and Division 
GPI_GDPpc_Division_Regression <- lm(GPI ~ GDPpc + Division, data = Data_2ndAssignment)
summary(GPI_GDPpc_Division_Regression)
GPI_Division_summary <- summary(GPI_GDPpc_Division_Regression)
#We make a table from the regression data we are interested in 
GPI_Division_table <- data.frame(
  Variable = rownames(GPI_Division_summary$coefficients),
  Estimate = GPI_Division_summary$coefficients[, "Estimate"],
  p_value = GPI_Division_summary$coefficients[, "Pr(>|t|)"]
)
GPI_Division_table$Adjusted_R_squared <- GPI_Division_summary$adj.r.squared
if (file.exists("GPI_Division_table.xlsx")) file.remove("GPI_Division_table.xlsx")
write_xlsx(GPI_Division_table, path = "processed-data/GPI_Division_table.xlsx")
#We create a new data frame from the original one to add a variable that would represent the predicted GPI according to the 
#GPI_GDPpc_Division_Regression model 
Data_2ndAssignment_PredictedGPI <- Data_2ndAssignment
Data_2ndAssignment_PredictedGPI$Predicted_GPI <- predict(GPI_GDPpc_Division_Regression)
#We create a new data frame just with the Country, Division, GDPpc, the Actual and the Predicted GPI. 
Data_2ndAssignment_PredictedGPI_cleaned <- Data_2ndAssignment_PredictedGPI %>%
  select(Country, Division, GDPpc, Actual_GPI = GPI, Predicted_GPI)
#We calculate the correlation between the Actual and Predicted GPI values and represent it with a scatterplot
correlation_GDPpc_Division <- cor(Data_2ndAssignment_PredictedGPI_cleaned$Actual_GPI, Data_2ndAssignment_PredictedGPI_cleaned$Predicted_GPI)
ggplot(Data_2ndAssignment_PredictedGPI_cleaned, aes(x = Predicted_GPI, y = Actual_GPI)) +
  geom_point(color = "steelblue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Actual vs Predicted GPI",
    subtitle = "Regression Model: GPI ~ GDPpc + Division",
    x = "Predicted GPI",
    y = "Actual GPI"
  ) +
  annotate(
    "text",
    x = min(Data_2ndAssignment_PredictedGPI_cleaned$Predicted_GPI),
    y = max(Data_2ndAssignment_PredictedGPI_cleaned$Actual_GPI),
    label = paste0("r = ", round(correlation_GDPpc_Division, 3)),
    hjust = 0, vjust = 1, size = 5, fontface = "bold"
  )

################################################################################

#Exercise 7
#We create a data frame called edu_data, reading our csv
edu_data <- read_csv("raw-data/expected_years_schooling.csv")
#We filter out dataset with the year and columns that we are interested in 
edu_2023 <- edu_data %>%
  filter(Year == 2023) %>%
  select(Country = Entity, Education_ExpYears_2023 = `Expected years of schooling`)
#We rename the countries that have different names to the ones in our original dataset to make the merging
edu_2023$Country <- recode(edu_2023$Country,
                                     "Turkey" = "Türkiye",
                                     "South Korea" = "Korea, South")
#We make a copy of our original dataset to add this new variable
Data_2ndAssignment_edu <- Data_2ndAssignment 
#We merge our education dataset and our original one by Country
Data_2ndAssignment_edu <- merge(
  Data_2ndAssignment_edu,
  edu_2023,
  by = "Country",
  all.x = TRUE
)
#We fill a missing value in Kosovo for education by the mean of the other values 
Data_2ndAssignment_edu$Education_ExpYears_2023[is.na(Data_2ndAssignment_edu$Education_ExpYears_2023)] <- 
  mean(Data_2ndAssignment_edu$Education_ExpYears_2023, na.rm = TRUE)
#We trace a first correlation between the education expected years and GPI 
cor(Data_2ndAssignment_edu$Education_ExpYears_2023,
Data_2ndAssignment_edu$GPI)
#We make now a regression between education expected years and GPI 
GPI_Education_Regression <- lm(GPI ~ Education_ExpYears_2023, data = Data_2ndAssignment_edu)
summary(GPI_Education_Regression)
GPI_Education_summary <- summary(GPI_Education_Regression)
#We make a table from the regression data we are interested in 
GPI_Education_table <- data.frame(
  Variable = rownames(GPI_Education_summary$coefficients),
  Estimate = GPI_Education_summary$coefficients[, "Estimate"],
  p_value = GPI_Education_summary$coefficients[, "Pr(>|t|)"]
)
GPI_Education_table$Multiple_R_squared <- GPI_Education_summary$r.squared
if (file.exists("GPI_Education_table.xlsx")) file.remove("GPI_Education_table.xlsx")
write_xlsx(GPI_Education_table, path = "processed-data/GPI_Education_table.xlsx")
#We calculate the predicted values for GPI according to the model established with the Education_ExpYears_2023 and the residuals 
fitted_vals_GPI_Education <- fitted(GPI_Education_Regression)
residuals_vals_GPI_Education <- residuals(GPI_Education_Regression)
#We plot the residuals to see if there is any pattern 
plot(fitted_vals_GPI_Education, residuals_vals_GPI_Education,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "GPI-Education Residuals vs Fitted Values",
     pch = 16, col = "yellow")
abline(h = 0, col = "black", lwd = 2)
#We plot a histogram to see if there is a normal distribution 
hist(residuals_vals_GPI_Education,
     breaks = 10,             
     col = "yellow",        
     border = "black",          
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

#We create a data frame called RDI_data, reading our csv
rdi_data <- read_delim("raw-data/religious_diversity_index.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
#We filter out dataset with the columns that we are interested in 
rdi_data_clean <- rdi_data %>%
  select(Country, RDI)
#We rename the countries that have different names to the ones in our original dataset to make the merging
rdi_data_clean$Country <- recode(rdi_data_clean$Country,
                                 "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
                                 "Ivory Coast" = "Cote d'Ivoire",
                                 "Czech Republic"= "Czechia",
                                 "Guinea Bissau"= "Guinea-Bissau",
                                 "South Korea"= "Korea, South",
                                 "Republic of Macedonia"= "North Macedonia",
                                 "Turkey"= "Türkiye")
#We make a copy of our original dataset to add this new variable
Data_2ndAssignment_edu_rdi <- Data_2ndAssignment_edu 
#We merge our education dataset and our original one by Country
Data_2ndAssignment_edu_rdi <- merge(
  Data_2ndAssignment_edu_rdi,
  rdi_data_clean,
  by = "Country",
  all.x = TRUE
)
#We trace a first correlation between the RDI and GPI 
cor(Data_2ndAssignment_edu_rdi$RDI,
    Data_2ndAssignment_edu$GPI)
#We make now a regression between RDI and GPI 
GPI_RDI_Regression <- lm(GPI ~ RDI, data = Data_2ndAssignment_edu_rdi)
summary(GPI_RDI_Regression)
GPI_RDI_summary <- summary(GPI_RDI_Regression)
#We make a table from the regression data we are interested in 
GPI_RDI_table <- data.frame(
  Variable = rownames(GPI_RDI_summary$coefficients),
  Estimate = GPI_RDI_summary$coefficients[, "Estimate"],
  p_value = GPI_RDI_summary$coefficients[, "Pr(>|t|)"]
)
GPI_RDI_table$Multiple_R_squared <- GPI_RDI_summary$r.squared
if (file.exists("GPI_RDI_table.xlsx")) file.remove("GPI_RDI_table.xlsx")
write_xlsx(GPI_RDI_table, path = "processed-data/GPI_RDI_table.xlsx")
#We calculate the predicted values for GPI according to the model established with the Education_ExpYears_2023 and the residuals 
fitted_vals_GPI_RDI <- fitted(GPI_RDI_Regression)
residuals_vals_GPI_RDI <- residuals(GPI_RDI_Regression)
#We plot the residuals to see if there is any pattern 
plot(fitted_vals_GPI_RDI, residuals_vals_GPI_RDI,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "GPI-RDI Residuals vs Fitted Values",
     pch = 16, col = "orange")
abline(h = 0, col = "black", lwd = 2)
#We plot a histogram to see if there is a normal distribution 
hist(residuals_vals_GPI_RDI,
     breaks = 10,             
     col = "orange",        
     border = "black",          
     main = "Histogram of Residuals",
     xlab = "Residuals",
     ylab = "Frequency")

#We now make a regression between OECD and GPI, previously factorizing it
Data_2ndAssignment$OECD <- as.factor(Data_2ndAssignment$OECD)
GPI_OECD_Regression <- lm(GPI ~ OECD, data = Data_2ndAssignment)
summary (GPI_OECD_Regression)
GPI_OECD_summary <- summary (GPI_OECD_Regression)
#We make a table from the regression data we are interested in
GPI_OECD_table <- data.frame(
  Variable = rownames(GPI_OECD_summary$coefficients),
  Estimate = GPI_OECD_summary$coefficients[, "Estimate"],
  p_value = GPI_OECD_summary$coefficients[, "Pr(>|t|)"]
)
GPI_OECD_table$Multiple_R_squared <- GPI_OECD_summary$r.squared
if (file.exists("GPI_OECD_table.xlsx")) file.remove("GPI_OECD_table.xlsx")
write_xlsx(GPI_OECD_table, path = "processed-data/GPI_OECD_table.xlsx")

#We make a multilinear regression that considers GPI as response variable and GDPpc, Education_ExpYears_2023, OECD, Inflation, MilitaryExpenditure and RDI as predictors.
#We also convert OECD in a factor in the new dataset
Data_2ndAssignment_edu_rdi$OECD <- as.factor(Data_2ndAssignment_edu_rdi$OECD)
Multilinear_regression_1 <- lm(GPI ~ GDPpc + Education_ExpYears_2023 + OECD + Inflation + MilitaryExpenditure + RDI,
                data = Data_2ndAssignment_edu_rdi)
summary (Multilinear_regression_1)
GPI_Multilinear_1_summary <- summary (Multilinear_regression_1)
#We make a table from the regression data we are interested in
GPI_Multilinear_1_table <- data.frame(
  Variable = rownames(GPI_Multilinear_1_summary$coefficients),
  Estimate = GPI_Multilinear_1_summary$coefficients[, "Estimate"],
  p_value = GPI_Multilinear_1_summary$coefficients[, "Pr(>|t|)"]
)
GPI_Multilinear_1_table$Adjusted_R_squared <- GPI_Multilinear_1_summary$adj.r.squared
if (file.exists("GPI_Multilinear_1_table.xlsx")) file.remove("GPI_Multilinear_1_table.xlsx")
write_xlsx(GPI_Multilinear_1_table, path = "processed-data/GPI_Multilinear_1_table.xlsx")

#We establish the correlation between all the numeric variables to get which ones are strongly correlated (multicollinearity)
cor(Data_2ndAssignment_edu_rdi[, c("GDPpc", "Education_ExpYears_2023", "Inflation", "MilitaryExpenditure", "RDI")], 
    use = "complete.obs")
#We run another regression but this time excluding OECD and Education_ExpYears_2023
Multilinear_regression_2 <- lm(GPI ~ GDPpc + Inflation + MilitaryExpenditure + RDI,
                               data = Data_2ndAssignment_edu_rdi)
summary (Multilinear_regression_2)
GPI_Multilinear_2_summary <- summary (Multilinear_regression_2)
#We make a table from the regression data we are interested in 
GPI_Multilinear_2_table <- data.frame(
  Variable = rownames(GPI_Multilinear_2_summary$coefficients),
  Estimate = GPI_Multilinear_2_summary$coefficients[, "Estimate"],
  p_value = GPI_Multilinear_2_summary$coefficients[, "Pr(>|t|)"]
)
GPI_Multilinear_2_table$Adjusted_R_squared <- GPI_Multilinear_2_summary$adj.r.squared
if (file.exists("GPI_Multilinear_2_table.xlsx")) file.remove("GPI_Multilinear_2_table.xlsx")
write_xlsx(GPI_Multilinear_2_table, path = "processed-data/GPI_Multilinear_2_table.xlsx")



################################################################################

#Exercise 8
#We establish again 5 countries dataset for the multilinear regression with all the necessary variables  
Five_more_country_multilinear <- data.frame(
  Country = c("Austria", "Belarus", "India", "Australia", "Guinea"),
  MilitaryExpenditure= c(0.84, 1.8, 2.44, 1.92, 2.13),
  Inflation= c(7.7, 5, 5.4, 5.6, 7.8),
  OECD= c(1, 0, 0, 1, 0),
  GDPpc= c(56.5059683, 7.82905314, 2.48484543, 64.7117656, 1.66393781),
  Division= c("Europe", "Europe", "Asia", "Oceania", "Africa"),
  Education_ExpYears_2023= c(16.284280, 13.718810, 12.954540, 20.654780, 10.393307),
  RDI= c(3.8, 4.7, 4.0, 5.6, 3.1)
)
#We factorize the OECD one 
Five_more_country_multilinear$OECD <- as.factor(Five_more_country_multilinear$OECD)

#We predict GPI for these 5 countries according to our multilinear model 
predicted_GPI_Five_countries_multilinear <- predict(Multilinear_regression_2, newdata = Five_more_country_multilinear)
Five_more_country_multilinear$Predicted_GPI <- predicted_GPI_Five_countries_multilinear
#We create a vector with the actual GPI for each of the countries 
actual_GPI_values <- c(1.313, 2.291, 2.319, 1.536, 2.423)
#We add the actual value to the Five_more_country_multilinear table 
Five_more_country_multilinear$Actual_GPI <- actual_GPI_values
#We pivot longer the table for its representation
Five_more_country_multilinear_long <- Five_more_country_multilinear %>%
  pivot_longer(cols = c(Predicted_GPI, Actual_GPI),
               names_to = "Type",
               values_to = "GPI_Value")
ggplot(Five_more_country_multilinear_long, aes(x = Country, y = GPI_Value, fill = Type)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.6),
           width = 0.6) +                           
  labs(title = "Predicted vs Actual GPI by Country",
       x = "Country",
       y = "GPI Value") +
  scale_fill_manual(values = c("steelblue", "red"),
                    name = "GPI Type",
                    labels = c("Actual GPI", "Predicted GPI")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






