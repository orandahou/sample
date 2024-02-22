# sample
Short Code Sample
---
title: "API-202MA Problem Set 3"
author: PRETEST_Oranda
output:
  pdf_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, message=FALSE}
# Load packages
library(tidyverse)
```

# Part I Income Analysis

```{r, message=FALSE}
# Load data
nlsy <- read.csv("https://raw.githubusercontent.com/tsvoronos/API202-students/main/data/nlsy2000_2012.csv")

regressionA <- lm(income ~ age + male, data=nlsy) 
summary(regressionA)
```


```{r, message=FALSE}
regressionB <- lm(income ~ age + male+ yrs_educ, data=nlsy) 
summary(regressionB)
```


```{r, message=FALSE}
nlsy <- mutate(nlsy, age_male = age * male)
regressionC <- lm(income ~ age + male + yrs_educ + age_male, nlsy) 
summary(regressionC)
```


```{r, message=FALSE}
# Package for displaying regression results
check_installed <- require(modelsummary)
if(check_installed==F){
  install.packages("modelsummary")
  require(modelsummary)
}

modelsummary(list(regressionA, regressionB, regressionC))
```


```{r, message=FALSE}
regressionC2 <- lm(married ~ age + male + yrs_educ + age_male, nlsy) 
summary(regressionC2)
```


```{r, message=FALSE}
nlsy <- mutate(nlsy, married_male = married * male)
regressionD <- lm(income ~ married + male + married_male, nlsy)
summary(regressionD)
```


```{r, message=FALSE}
nlsy <- mutate(nlsy, urban_black = urban * black)
regressionE <- lm(income ~ black + urban + urban_black +yrs_educ,nlsy)
summary(regressionE)

```

# Part II HIV Analysis

```{r, message=FALSE}
burkinafaso <- read.csv("https://raw.githubusercontent.com/tsvoronos/API202-students/main/data/burkinafaso.csv")

regression1 <- lm(hivpositive ~wealth_index+ catholic+ secondary_educ+ age_at_first_sex+ rural, burkinafaso)
summary(regression1)
```


```{r, message=FALSE}
burkinafaso %>% select(secondary_educ,rural) %>% summary()
tibble(wealth_index=c(2), catholic=c(1), secondary_educ=c(0.07153), age_at_first_sex=c(18), rural=c(0.7196)) %>% mutate(hivpositive_hat = predict(regression1,newdata=.))

regressionA <- lm(hivpositive ~wealth_index+ age_at_first_sex, burkinafaso)
summary(regressionA)
```


```{r, message=FALSE}
burkinafaso<- burkinafaso %>% mutate(lowincome=ifelse(wealth_index<4, 1, 0))
regressionB <- lm(hivpositive~lowincome+age_at_first_sex,burkinafaso)
summary(regressionB)
```


```{r, message=FALSE}
burkinafaso<- burkinafaso %>% mutate(wealth= as_factor(case_when(wealth_index < 2 ~ "Low Income", wealth_index > 1 & wealth_index < 3  ~ "Middle-Low Income", wealth_index > 2 & wealth_index < 4  ~ "Middle Income",wealth_index > 3 & wealth_index < 5  ~ "Middle-High Income", wealth_index > 4 ~ "High Income" )))

burkinafaso$wealth <- relevel(burkinafaso$wealth, "Low Income", "Middle-Low Income",  "Middle Income", "Middle-High Income","High Income")
regressionC <- lm(hivpositive ~wealth+ age_at_first_sex, burkinafaso)
summary(regressionC)
```


```{r, message=FALSE}
regressionE <- lm(hivpositive ~ wealth + age_at_first_sex + lowincome,burkinafaso)
summary(regressionE)
