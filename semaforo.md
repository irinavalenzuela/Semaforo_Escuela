Semaforo Escuela Project
================
Irina Valenzuela

# 1 Data

### Update and install packages

``` r
# Install packages
# install.packages("")

# stargazer is an R package that creates LATEX code,
# install.packages("stargazer")

# help you build common complex tables and manipulate table styles
 install.packages("kableExtra")

# For dev version
 install.packages("devtools"):
 devtools::install_github("haozhu233/kableExtra")

# Additionals
install.packages("cli")
install.packages("haven")
install.packages("estimatr")

# Only for DiD
install.packages("lfe")
install.packages("bacondecomp")
  
# Uninstall package:
# remove.packages("")

# Update packages:
# update.packages("")
```

``` r
library(tidyverse)
library(broom) # It will convert estimation into data frame
library(modelsummary) #Regresion table side-by-side
library(ggplot2)

library(magrittr)
library(dplyr)

library(cli)
library(haven)
library(estimatr)
library(lfe)
library(bacondecomp)

library(plm)
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

datasummary_skim(semaforo_school)
```

|                                   | Unique (#) | Missing (%) |  Mean |    SD |  Min | Median |    Max |
|:----------------------------------|-----------:|------------:|------:|------:|-----:|-------:|-------:|
| N visit 2015                      |          8 |           0 |   2.0 |   1.3 |  0.0 |    2.0 |    7.0 |
| N visit 2016                      |          6 |           0 |   1.2 |   0.8 |  0.0 |    1.0 |    5.0 |
| Month 1st visit 2015              |          9 |          14 |   5.9 |   1.9 |  4.0 |    5.0 |   11.0 |
| Month 2nd visit 2015              |          8 |          38 |   8.0 |   1.5 |  5.0 |    8.0 |   11.0 |
| Month 3rd visit 2015              |          7 |          63 |   9.6 |   1.3 |  6.0 |   10.0 |   11.0 |
| Month 4th visit 2015              |          6 |          90 |   9.6 |   0.9 |  7.0 |    9.0 |   11.0 |
| Month 5th visit 2015              |          5 |          96 |  10.1 |   0.6 |  8.0 |   10.0 |   11.0 |
| Month 6th visit 2015              |          3 |          99 |  10.3 |   0.5 | 10.0 |   10.0 |   11.0 |
| Month 7th visit 2015              |          2 |         100 |  11.0 |   0.0 | 11.0 |   11.0 |   11.0 |
| Month Last visit 2015             |         10 |           1 |   8.0 |   3.6 |  0.0 |    9.0 |   11.0 |
| Age of school                     |        122 |          20 |  37.5 |  16.3 |  0.0 |   38.0 |  187.0 |
| N students                        |       1001 |           0 | 212.8 | 238.2 |  2.0 |  110.0 | 1890.0 |
| N teachers                        |         66 |           0 |   9.4 |   9.1 |  0.0 |    6.0 |   90.0 |
| N computers with internet         |        164 |           0 |   7.1 |  23.9 |  0.0 |    0.0 |  695.0 |
| N computers without internet      |        136 |           0 |   9.0 |  18.3 |  0.0 |    0.0 |  390.0 |
| Has science lab                   |          3 |           5 |   0.2 |   0.4 |  0.0 |    0.0 |    1.0 |
| Has library                       |          3 |           5 |   0.5 |   0.5 |  0.0 |    0.0 |    1.0 |
| Access to public electric network |          3 |           2 |   0.9 |   0.3 |  0.0 |    1.0 |    1.0 |
| Access to public water system     |          3 |           2 |   0.7 |   0.4 |  0.0 |    1.0 |    1.0 |
| Access to public sewage system    |          3 |           2 |   0.6 |   0.5 |  0.0 |    1.0 |    1.0 |

``` r
# Create summary table of that dataframe

datasummary(All(as.data.frame(semaforo_school)) ~ N + Mean + SD, data=semaforo_school)
```

|                                   |    N |   Mean |     SD |
|:----------------------------------|-----:|-------:|-------:|
| N visit 2015                      | 9185 |   2.00 |   1.33 |
| N visit 2016                      | 9185 |   1.20 |   0.77 |
| Month 1st visit 2015              | 7900 |   5.87 |   1.94 |
| Month 2nd visit 2015              | 5679 |   7.95 |   1.47 |
| Month 3rd visit 2015              | 3429 |   9.61 |   1.35 |
| Month 4th visit 2015              |  926 |   9.57 |   0.94 |
| Month 5th visit 2015              |  377 |  10.10 |   0.61 |
| Month 6th visit 2015              |   60 |  10.30 |   0.46 |
| Month 7th visit 2015              |   29 |  11.00 |   0.00 |
| Month Last visit 2015             | 9132 |   8.00 |   3.59 |
| Age of school                     | 7320 |  37.49 |  16.26 |
| N students                        | 9174 | 212.77 | 238.19 |
| N teachers                        | 9174 |   9.42 |   9.09 |
| N computers with internet         | 9156 |   7.07 |  23.91 |
| N computers without internet      | 9156 |   9.00 |  18.31 |
| Has science lab                   | 8738 |   0.19 |   0.39 |
| Has library                       | 8756 |   0.48 |   0.50 |
| Access to public electric network | 8990 |   0.90 |   0.30 |
| Access to public water system     | 8994 |   0.75 |   0.43 |
| Access to public sewage system    | 8991 |   0.60 |   0.49 |

``` r
# Summary statistics for reading and math test scores

semaforo_test <- semaforo %>% 
  select(Year = year,
         `School-average reading test score`= read_average,
         `School-average math test score`= math_average)

# The string variable "Year" needs to be as categorical, then I use as.factor
datasummary((`School-average reading test score` + `School-average math test score`) * as.factor(Year)  ~ N + Mean + SD, data=semaforo_test, output = 'table_test_summary.tex')
```

``` r
datasummary( (`School-average reading test score` + `School-average math test score`) * as.factor(Year)  ~ N + Mean + SD, data=semaforo_test)
```

|                                   | as.factor(Year) |    N |   Mean |    SD |
|:----------------------------------|:----------------|-----:|-------:|------:|
| School-average reading test score | 2010            | 9185 | 507.22 | 63.13 |
|                                   | 2011            | 9185 | 504.18 | 60.25 |
|                                   | 2012            | 9185 | 511.22 | 60.80 |
|                                   | 2013            | 9185 | 521.25 | 60.01 |
|                                   | 2014            | 9185 | 544.48 | 63.13 |
|                                   | 2015            | 9185 | 567.29 | 64.10 |
|                                   | 2016            | 9185 | 559.58 | 58.48 |
| School-average math test score    | 2010            | 9184 | 505.39 | 78.55 |
|                                   | 2011            | 9184 | 499.60 | 75.54 |
|                                   | 2012            | 9185 | 505.93 | 74.62 |
|                                   | 2013            | 9184 | 514.63 | 75.47 |
|                                   | 2014            | 9185 | 541.73 | 92.68 |
|                                   | 2015            | 9185 | 566.05 | 86.25 |
|                                   | 2016            | 9185 | 585.37 | 87.84 |

# 2 Exploratory Graphical Analysis

### Semaforo Escuela Visits

#### Number of Semaforo Escuela Visits: 2015, 2016

``` r
# N of visits in 2015 for primary school (It is primary school because all of them has ECE 2 grade primary school test)
plot_nv2015 <- ggplot(semaforo_school, aes(x = `N visit 2015`)) +
  geom_bar(color = "white") +
  labs(x = "Number of Semaforo Escuela Visits 2015", y = "Number of schools")
plot_nv2015
```

![](semaforo_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# N of visits 2016
plot_nv2016 <- ggplot(semaforo_school, aes(x = `N visit 2016`)) +
  geom_bar(color = "white") +
  labs(x = "Number of Semaforo Escuela Visits 2016", y = "Number of schools")
plot_nv2016
```

![](semaforo_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
# Plotting N visit 2015 vs 2016. 
# First option
ggplot(semaforo_school, aes(x= `N visit 2015`, y = `N visit 2016`)) +
  geom_point()
```

![](semaforo_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
# The second option to overcome the problem of data points overlap is to use what is called a counts chart. Wherever there is more points overlap, the size of the circle gets bigger.

plot_nv1516 <- ggplot(semaforo_school, aes(x= `N visit 2015`, y = `N visit 2016`)) +
  geom_count(col="tomato3",show.legend=F)+
  labs(x = "N visits 2015", y="N visits 2016")+
  theme_bw()
plot_nv1516
```

![](semaforo_files/figure-gfm/unnamed-chunk-9-4.png)<!-- --> \#### N
visit 2015-2016

``` r
# Create variable N visits 2015-2016

semaforo_school$n_visit_15_16 <- semaforo_school$`N visit 2015`+ semaforo_school$`N visit 2016`
  
# Plot N visits 2015 - 2016

plot_nv2015_2016 <- ggplot(semaforo_school, aes(x = n_visit_15_16)) +
  geom_bar(color = "white") +
  labs(x = "Number of Semaforo Escuela Visits 2015-2016")+
  theme_bw()

plot_nv2015_2016
```

![](semaforo_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### N visit 2015: Distribution by month of visit

``` r
# N visit 2015: Distribution by month of visit
# If schools received just 1 visit in 2015

plot_month_v115 <- semaforo_school %>% 
  filter(`N visit 2015`==1) %>% 
  ggplot(aes(x = `Month 1st visit 2015`)) +
  geom_bar(fill = "#FF6666") +
  labs(x = "Month of visit (1 visit)")+
  theme_bw()
plot_month_v115
```

![](semaforo_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# If schools received More than 1 visit in 2015
# 1st Visit
plot_month_v12_15 <- semaforo_school %>% 
  filter(`N visit 2015`> 1) %>% 
  ggplot(aes(x = `Month 1st visit 2015`)) +
  geom_bar(fill = "#FF6666") +
  labs(x = "Month of 1st visit (+1 visit)")+
  theme_bw()
plot_month_v12_15
```

![](semaforo_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
# 2nd visit

plot_month_v22_15 <- semaforo_school %>% 
  filter(`N visit 2015`> 1) %>% 
  ggplot(aes(x = `Month 2nd visit 2015`)) +
  geom_bar(fill = "#FF6666") +
  labs(x = "Month of 2nd visit (+1 visit)")+
  theme_bw()
plot_month_v22_15
```

![](semaforo_files/figure-gfm/unnamed-chunk-12-3.png)<!-- -->

``` r
# 3rd visit
plot_month_v32_15 <- semaforo_school %>% 
  filter(`N visit 2015`> 1) %>% 
  ggplot(aes(x = `Month 3rd visit 2015`)) +
  geom_bar(fill = "#FF6666") +
  labs(x = "Month of 3rd visit (+1 visit)")+
  theme_bw()
plot_month_v32_15
```

    ## Warning: Removed 2250 rows containing non-finite values (stat_count).

![](semaforo_files/figure-gfm/unnamed-chunk-12-4.png)<!-- -->

``` r
graph1 <- ggplot(semaforo, aes(x = read_average)) +
  geom_histogram(color="white")
graph1
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](semaforo_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
## Let's have the histogram by two groups (treatemt)
graph2 <- ggplot(semaforo, aes(x = read_average)) +
  geom_histogram(color="white") +
  facet_wrap(vars(treatment))
graph2
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](semaforo_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
ggplot(semaforo, aes(x = math_average)) +
  geom_histogram(color="white") +
  facet_wrap(vars(treatment))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 3 rows containing non-finite values (stat_bin).

![](semaforo_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
ggplot(semaforo, aes(x= treatment, y=read_average)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult= 1.96)) +
  facet_wrap(vars(time))
```

# 3 Empirical Strategy: Differnce-in-difference

### Manual diff-in-diff

``` r
diffs <- semaforo %>%
  group_by(time, treatment) %>% 
  summarize(meanscore = mean(read_average))
```

    ## `summarise()` has grouped output by 'time'. You can override using the `.groups` argument.

``` r
diffs
```

    ## # A tibble: 6 x 3
    ## # Groups:   time [2]
    ##    time treatment meanscore
    ##   <dbl>     <int>     <dbl>
    ## 1     0         0      492.
    ## 2     0         1      499.
    ## 3     0        NA      531.
    ## 4     1         0      534.
    ## 5     1         1      544.
    ## 6     1        NA      578.

``` r
before_treatment <- diffs %>% 
  filter(time ==0, treatment == 1) %>% 
  pull(meanscore)

before_control <- diffs %>% 
  filter(time ==0, treatment == 0) %>% 
  pull(meanscore)

after_treatment <- diffs %>% 
  filter(time ==1, treatment == 1) %>% 
  pull(meanscore)

after_control <- diffs %>% 
  filter(time ==1, treatment == 0) %>% 
  pull(meanscore)

diff_treatment_before_after <-  after_treatment - before_treatment
diff_control_before_after <- after_control - before_control


diff_treatment_before_after - diff_control_before_after
```

    ## [1] 1.815065

``` r
ggplot(diffs, aes(x= time, y=meanscore, color = treatment)) +
  geom_point()+
  geom_line(aes(group= treatment))
```

![](semaforo_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
# Label variable treatment
semaforo$Group <- factor(semaforo$treatment, levels = c(0,1), labels=c("Control", "Treatment"))


plot1 <- ggplot(semaforo, aes(year, read_average, color= Group)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2014) +
  theme_minimal() 
plot1
```

    ## No summary function supplied, defaulting to `mean_se()`

![](semaforo_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
plot11 <- plot1 +
  labs(title = NULL, x= "year", y = "Reading score")
plot11
```

    ## No summary function supplied, defaulting to `mean_se()`

![](semaforo_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
# Save graph at high resolution
ggsave(plot11, file="plot11.png", dpi= 300)
```

    ## Saving 7 x 5 in image
    ## No summary function supplied, defaulting to `mean_se()`

``` r
plot2 <- ggplot(semaforo, aes(year, math_average, color= Group)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2014) +
  theme_minimal() 
plot2
```

    ## Warning: Removed 3 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`

![](semaforo_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
plot22 <- plot2 +
  labs(title = NULL, x= "year", y = "Math score")
plot22
```

    ## Warning: Removed 3 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`

![](semaforo_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
# Save graph at high resolution
ggsave(plot22, file="plot22.png", dpi= 300)
```

    ## Saving 7 x 5 in image

    ## Warning: Removed 3 rows containing non-finite values (stat_summary).

    ## No summary function supplied, defaulting to `mean_se()`
