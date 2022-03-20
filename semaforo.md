Semaforo Escuela Project
================
Irina Valenzuela

# 1 Data

### Update and install packages

``` r
library(tidyverse)
library(broom) # It will convert estimation into data frame
library(modelsummary) #Regresion table side-by-side
library(ggplot2)

library(magrittr)
library(dplyr)
```

### Working Directory

``` r
# Check working directory
getwd()
```

    ## [1] "E:/konpeitos/Documents/Applied Econometrics/Semaforo_Escuela/Semaforo_escuela"

``` r
# Set Working directory
 setwd("E:/konpeitos/Documents/Applied Econometrics/Semaforo_Escuela/Semaforo_escuela/")
```

### Importing data

First, let’s load some data

``` r
# From csv
 read.csv()

# From STATA
 read.dta()
```

``` r
# Load data of Semaforo Escuela
 semaforo_raw <- read.csv("Data/semaforo_escuela_ECE_10_16.csv")
```

### First step: Cleaning Data

``` r
# To Manipulate semaforo and keep original data as raw
semaforo <- semaforo_raw 
```

### Second step: explore the data

``` r
# Dimensions: number of row and columns
dim(semaforo)

# First six rows of all columns
head(semaforo)

# Last six rows of all columns
tail(semaforo)

# Preview data
glimpse(semaforo)

# Structure of the data frame
str(semaforo)

# Names of variables
names(semaforo)

# Look at summaries of the variable
table(semaforo_raw$n_visit_2015)

# Proportions table
prop.table(table(semaforo_raw$n_visit_2015))*100

# Compute specific metrics
c(mean(semaforo$read_average), sd(semaforo$read_average))

# Compute metrics for a specific group
mean(semaforo[semaforo$area == "Rural",]$read_average) # Criteria: area is rural

#If there is NA, it would not be able to compute the mean
mean(semaforo[semaforo$area == "Rural",]$read_average, na.rm = T)

# Visualize the data
# plot(semaforo$read_average)
```

### Third step: Manipulate Data

``` r
## Working with data with Tidyverse

# Pipe = %>% Tells R thay you are going to manipulate the data
# Shortcut: Ctrl + Shift + M (Windows) 

# Remove a single variable
 semaforo %>% 
  select(-variable_name)

# Select a single variable and count the number of rows in each level
semaforo %>% 
  count(n_visit_2015)

# Summarize based on single variable
semaforo %>% 
  group_by(n_visit_2015) %>% 
  summarise(mean(read_average, na.rm = TRUE), sd(read_average, na.rm = T)) # Remember that na.rm=T is for not couting NA

# Summarize and rename your output
semaforo %>% 
  group_by(n_visit_2015) %>% 
  summarise(Mean = mean(read_average, na.rm = TRUE), Std = sd(read_average, na.rm = T))

# Group by multiple variables
semaforo %>% 
  group_by(n_visit_2015,year) %>% 
  summarise(Mean = mean(read_average, na.rm = TRUE), Std = sd(read_average, na.rm = T))

# Rename variables
 semaforo %>% 
  rename(newname = oldname)

# Create new variable
 dataframeX %>% 
  mutate(newvariable = oldvariable) %>% 
  select(oldvariable, newvariable)

# Subset using filter: an example
 semaforo %>% 
   filter(var1 == "F" & var2>3 | var2 ==1) ## & = AND, | = OR
```

### Creating Variables for Semaforo Escuela

``` r
# Create binary treatment based on whether to exposed or not exposed to Semaforo Escuela

# Option 1: Using mutate. Mutate() works with dataframes, and then create a dataframe that is saved in the console. if you want to save it permanently, you need to name it using datamframe_new <- 
# semaforo_1 <- semaforo %>% 
#   mutate(treated = ifelse(n_visit_2015 >=1, 1, 0)) %>%
#   select(n_visit_2015, treated)

# Option 2: Creating the new variable as an object
semaforo$treated <- ifelse(semaforo$n_visit_2015 >= 1, 1, 0)

# Create treatment intensity 2015 as the number of visits
semaforo$treat_intensity <-  semaforo$n_visit_2015

# Create treatment only for 0 and 1 visit in 2015 is equal to treatment
semaforo$treated2 <- ifelse(semaforo$n_visit_2015 == 1, 1, ifelse(semaforo$n_visit_2015 ==0, 0, NA))

# Create variable before and after
semaforo$time <- ifelse(semaforo$year >= 2015, 1, 0)
```

### Fourth Step: Summary Statistics table

``` r
#Create dataframe with variables of interest

# Summary at the school level

semaforo_school <- semaforo %>% 
  filter(year == 2015) %>% 
  select(`N visit 2015`= n_visit_2015,
         `N visit 2016`= n_visit_2016,
         `Month 1st visit 2015`= m_1_2015, 
         `Month 2nd visit 2015`= m_2_2015,
         `Month 3rd visit 2015`= m_3_2015,
         `Month 4th visit 2015`= m_4_2015,
         `Month 5th visit 2015`= m_5_2015,
         `Month 6th visit 2015`= m_6_2015,
         `Month 7th visit 2015`= m_7_2015,
         `Month Last visit 2015`= last_visit_2015,
         `Rural`= rural,
         `Age of school`= age_school,
         `N students`= total_students,
         `N teachers`= total_teachers,
         `N computers with internet`= computer_internet,
         `N computers without internet`= computer_nointernet,
         `Has science lab`= science_lab,
         `Has library`= library,
         `Access to public electric network`= electric_red,
         `Access to public water system`= water_red,
         `Access to public sewage system`= toilet_red) 

# For the dataframe Semaforo_school, I need to recode some variables
# Month of X visit inclues 0, but it should be NA, and then the mean exclude NA for the mean

semaforo_school$`Month 1st visit 2015` <- ifelse(semaforo_school$`Month 1st visit 2015` ==0, NA, semaforo_school$`Month 1st visit 2015`)

semaforo_school$`Month 2nd visit 2015` <- ifelse(semaforo_school$`Month 2nd visit 2015` ==0, NA, semaforo_school$`Month 2nd visit 2015`)

semaforo_school$`Month 3rd visit 2015` <- ifelse(semaforo_school$`Month 3rd visit 2015` ==0, NA, semaforo_school$`Month 3rd visit 2015`)

semaforo_school$`Month 4th visit 2015` <- ifelse(semaforo_school$`Month 4th visit 2015` ==0, NA, semaforo_school$`Month 4th visit 2015`)
  
semaforo_school$`Month 5th visit 2015` <- ifelse(semaforo_school$`Month 5th visit 2015` ==0, NA, semaforo_school$`Month 5th visit 2015`)


semaforo_school$`Month 6th visit 2015` <- ifelse(semaforo_school$`Month 6th visit 2015` ==0, NA, semaforo_school$`Month 6th visit 2015`)

semaforo_school$`Month 7th visit 2015` <- ifelse(semaforo_school$`Month 7th visit 2015` ==0, NA, semaforo_school$`Month 7th visit 2015`)


# Create the summary

datasummary(All(as.data.frame(semaforo_school)) ~ N + Mean + SD, data=semaforo_school, output = 'table_school_summary.tex')
```
