# created 2025-04-22 by eucarida
# ats updated 2025-04-22 by eucarida
# GOAL:
# Generate a model for the Many Analyst Exercise in BIO176


# clean up
rm(list = ls())
gc()


# libraries
library(boot)
library(lme4)  
library(pbkrtest)
library(merTools) 
library(MuMIn)
library(sjPlot)
library(performance)
library(tidyverse)
theme_set(theme_bw())
