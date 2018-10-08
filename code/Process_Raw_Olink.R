##################################################################
# Name: Process_Raw_Olink.R
# Author: Ryan Johnson
# Date Created: 5 October, 2018
# Purpose: Process the raw Olink data into a tidy format for 
#           future analyses
##################################################################

library(tidyverse)
library(readxl)

## Read in raw Olink data ----------------------------------

olink <- read_xlsx("data/raw/20170276_Henry_M_Jackson_Foundation-Ventura_NPX_LOD_Updated_and_Revised_2.26.18.xlsx")

## Clean up data ------------------------------------------------------

# Subset sample IDs and proteins
olink_raw_data <- olink[7:333, 2:95]
olink_raw_data <- olink_raw_data %>%
  select(-`20170276_Henry M Jackson Foundation-Ventura`)
colnames(olink_raw_data) <- c("subject_ID", olink[3, 4:95]) # rename columns

# Make tidy
olink_tidy <- olink_raw_data %>%
  filter(!str_detect(subject_ID, "^C")) %>%
  gather(protein, olink_value, -subject_ID)

rm(olink_raw_data)

# Add in Limit of detection
LOD <- as.tibble(t(olink[c(3, 335), 4:95]))
LOD <- LOD %>%
  rename(protein = V1, LOD_value = V2)

olink_tidy <- olink_tidy %>%
  full_join(., LOD, by = "protein")

rm(LOD)

# Remove samples with "warning" in QC
qc_data <- olink %>% 
  select(X__2, X__95) %>%
  filter(!is.na(X__2) & X__2 != "Serum ID") %>%
  rename(subject_ID = X__2, qc = X__95)

bad_samples <- qc_data %>%
  filter(qc == "Warning") %>%
  pull(subject_ID)

olink_tidy <- olink_tidy %>%
  filter(!subject_ID %in% bad_samples)

# Split subject ID and visit number and clean up
olink_tidy <- olink_tidy %>%
  separate(subject_ID, into = c("subject_ID", "visit"), sep = "\\s*\\(") %>%
  mutate(subject_ID = gsub(" ", "", subject_ID)) %>%
  mutate(visit = gsub("\\)", "", visit)) %>%
  mutate(visit = gsub(" ", "", visit)) %>%
  mutate(visit = gsub("t", "t_", visit))
  



## Write to processed -------------------------------------
write_csv(x = olink_tidy, path = "data/processed/Olink_tidy.csv")


