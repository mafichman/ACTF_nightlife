library(tidyverse)
library(sf)

# Grab SBA data from here: https://data.sba.gov/dataset/ppp-foia

test <- read.csv("https://data.sba.gov/dataset/8aa276e2-6cab-4f86-aca4-a7dde42adf24/resource/c84fa84d-c047-4b66-8056-5748f6a2bfca/download/public_150k_plus_220102.csv")

# next steps - clean out philly only data, do it again!