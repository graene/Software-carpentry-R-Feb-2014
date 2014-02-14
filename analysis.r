rm(list=ls())
setwd("/Users/Graeme/Desktop/SWC")
source('R/functions.R')

dat <- read.csv("data/gapminder-FiveYearData.csv",stringsAsFactors=F)


create.project(minimal=T)