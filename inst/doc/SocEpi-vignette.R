## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SocEpi)

## ---- eval=F------------------------------------------------------------------
#  install.packages(c("dplyr", "tidyr", "Rcpp"), dependencies = TRUE)
#  library(dplyr)
#  library(tidyr)
#  library(Rcpp)

## ----eval=F-------------------------------------------------------------------
#  install.packages(SocEpi) # if not installed before
#  library(SocEpi)

## -----------------------------------------------------------------------------
data <- dep_data

# Calculate z-score, weighted mean, sd and store all data in object z_oc
z_oc <- zscore(data$total_pop, data$pcnt_overcrowding)

# Extract z-score
data$z_overcrowd <- z_oc$z.score
mean(data$z_overcrowd) #mean of z-score
sd(data$z_overcrowd) #sd z-score

# Compare weighted mean and sd to actual mean and sd
mean(data$pcnt_overcrowding)
z_oc$w.mean
sd(data$pcnt_overcrowding)
z_oc$w.sd

## -----------------------------------------------------------------------------
# Calculate deciles (default)
data$dec_overcrowd <- w_pcntile(data, total_pop, pcnt_overcrowding)

# Percent of people by decile - note minor variation
tapply(data$total_pop, data$dec_overcrowd, sum)/sum(data$total_pop)*100

# Average percent of overcrowding by decile
tapply(data$pcnt_overcrowding, data$dec_overcrowd, mean)

## -----------------------------------------------------------------------------
# Example of calculating RII/SII with long format data
d <- health_data

# Asian population compared to Scottish (reference)
smr(d, bad, pop, age, ethnicity, sets = c("Scot", "asian"))

# Asian, White British and Irish population compared to Scottish (reference)
smr(d, bad, pop, age, ethnicity, sets = c("Scot", "asian", "WB", "Irish"),
   age_group = c("15-29", "30-44"), CI = 99)

## -----------------------------------------------------------------------------
# Standardized rates for all people
st_rate(d, bad, pop, quintile, age, ethnicity == "all")

# Standardized rates for Scottish for ages 15-29 and 30-44 with 99% CI
st_rate(d, bad, pop, quintile, age, ethnicity == "Scot", 
        age_group=c("15-29", "30-44"), CI=99)

## -----------------------------------------------------------------------------
# RII with 95% CI
rii(d, bad, pop, quintile, age, ethnicity == "all")

# SII with 99% CI
rii(d, bad, pop, quintile, age, ethnicity == "all", RII=FALSE, CI=99)

# supply own population weights
new_w <- c(0.075, 0.075, 0.075, 0.06, 0.060, 0.060, 0.06, 0.070, 0.050, 
           0.050, 0.050, 0.06, 0.060, 0.055, 0.050, 0.040, 0.025, 0.025)

rii(d, bad, pop, quintile, age, ethnicity == "all", RII=FALSE, CI=99, st_pop=new_w)

# SII for new age groups with 95% CI
rii(d, bad, pop, quintile, age, ethnicity == "Scot" &  ur2fold == "Urban",
   age_group=c("0-19", "20-34", "35-49"), RII=FALSE)


## ----eval=FALSE---------------------------------------------------------------
#  # SII with 99% CI
#  rii_results <- rii(d, bad, pop, quintile, age, ethnicity == "all", RII=FALSE, CI=99)
#  View(rii_results)

