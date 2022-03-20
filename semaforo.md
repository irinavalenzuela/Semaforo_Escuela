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
