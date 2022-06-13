Semaforo HTE
================
Irina Valenzuela
6/13/2022

## Setup

#### Working Directory

``` r
# Check working directory
getwd()
```

    ## [1] "C:/Users/vlnzlrm2/OneDrive - University of Illinois - Urbana/Documents/2022/Semaforo Escuela 2022/3_Code/Semaforo_escuela"

#### Install Packages

``` r
install.packages('tidyverse') # Data Manipulation, includes ggplot2
install.packages('haven')     # Loading data
install.packages('stargazer') # Tables
install.packages("GenericML") # GenericML
#install.packages("mlr3")      # GenericLM depends on mlr3
```

#### Loading Libraries

``` r
library(tidyverse)
library(haven)
library(stargazer)
library(GenericML)
```

## 2 Data

#### Load

``` r
# Path to original STATA data directory
data_folder <- "C:/Users/vlnzlrm2/OneDrive - University of Illinois - Urbana/Documents/2022/Semaforo Escuela 2022/2_Data/"

# Reading the data
semaforo_data <- read_dta(paste0(data_folder,"semaforo.dta"))

# Exploring the data
head(semaforo_data)
```

    ## # A tibble: 6 x 200
    ##   modular_code codlocal cen_edu_2014  codooii_2014 dre_ugel_2014 region_edu_2014
    ##          <dbl>    <dbl> <chr>                <dbl> <chr>         <chr>          
    ## 1       200014    53828 54387 COTAHU~        30007 UGEL GRAU     DRE APURIMAC   
    ## 2       200030    53847 54389 PATAPA~        30007 UGEL GRAU     DRE APURIMAC   
    ## 3       200048    53852 54390 CAMPAN~        30007 UGEL GRAU     DRE APURIMAC   
    ## 4       200071    53871 54393 CHAPIM~        30007 UGEL GRAU     DRE APURIMAC   
    ## 5       200089    54045 54394 SEÑOR ~        30007 UGEL GRAU     DRE APURIMAC   
    ## 6       200097    54050 54395 HUAYO          30007 UGEL GRAU     DRE APURIMAC   
    ## # ... with 194 more variables: cod_tur_2014 <dbl>, fechareg_2014 <chr>,
    ## #   director_2014 <chr>, cp_etnico_2014 <chr>, codgeo_2014 <dbl>,
    ## #   dpto_2014 <chr>, prov_2014 <chr>, dist_2014 <chr>, level_2014 <dbl+lbl>,
    ## #   type_2014 <dbl+lbl>, year_padron_2014 <dbl>, age_school_2014 <dbl>,
    ## #   month_padron_2014 <chr>, area_2014 <dbl+lbl>, JUNTOS_dist_2014 <dbl>,
    ## #   students_male_2014 <dbl>, students_female_2014 <dbl>, mean_age_2014 <dbl>,
    ## #   total_students_2014 <dbl>, level_school_2014 <dbl+lbl>, ...
