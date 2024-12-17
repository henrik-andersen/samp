
# Clear working directory
rm(list = ls())


# --- Setup 

# Load packages
library(haven)
library(reshape2)
library(tidyverse)

# --- For validation: take the 

# Set working directory
# setwd("F:/nextcloud/Jochen-Henrik/amp/data")

# Load data
df <- read.csv("C:/Users/andeh/Nextcloud/Jochen-Henrik/amp/data/amp_data_prepared.csv")
# Prepared dataset with AMP raw data seems to be amp_data_prepared.csv
head(df)

# Make smaller dataframe with just AMP raw 
dfw <- df[, which(colnames(df) == "prime_1"):which(colnames(df) == "rl_28")]
dfw$id <- as.numeric(rownames(dfw))
head(dfw)

# Make vector of important variable names
resp_vars <- paste0("response_", 1:28)
prime_vars <- paste0("prime_", 1:28)
categ_vars <- paste0("category_", 1:28)


# --- Preparation 

# Make the responses into factors for the labels, 1 : neg, 2: pos 
# (https://www.soscisurvey.de/help/doku.php/de:create:questions:amp)
for(i in 1:28) {
  dfw[, resp_vars[[i]]] <- factor(dfw[, resp_vars[[i]]], 
                                  levels = 1:2, 
                                  labels = c("negative", "positive"))
}

# Make the categories into factors for the labels, 1: neutral, 2: stimulus 
for(i in 1:28) {
  dfw[, categ_vars[[i]]] <- factor(dfw[, categ_vars[[i]]],
                                   levels = 1:2, 
                                   labels = c("neutral", "stimulus"))
}


# Cut the dataset down again
dfw <- dfw[, c("id", prime_vars, categ_vars, resp_vars)]
head(dfw)

# --- Make long datasets

# Melt into long datasets
dfl <- melt(dfw, id.vars = "id", measure.vars = resp_vars)
names(dfl) <- c("id", "variable", "response")
head(dfl)

dfl2 <- melt(dfw, id.vars = "id", measure.vars = prime_vars)
dfl2 <- dfl2[, c("id", "value")]
names(dfl2) <- c("id", "prime")

dfl3 <- melt(dfw, id.vars = "id", measure.vars = categ_vars)
dfl3 <- dfl3[, c("id", "value")]
names(dfl3) <- c("id", "category")

# Sort cases by id for double-checking
dfl <- dfl[order(dfl$id), ]
dfl2 <- dfl2[order(dfl2$id), ]
dfl3 <- dfl3[order(dfl3$id), ]

head(dfl)
head(dfl2)
head(dfl3)

# The ids are in the same order, don't need merge, just cbind 
sum(dfl$id != dfl2$id, na.rm = TRUE)
sum(dfl2$id != dfl3$id, na.rm = TRUE)
sum(dfl$id != dfl3$id, na.rm = TRUE)


# Combine datasets
dfl <- cbind(dfl, dfl2)
dfl <- cbind(dfl, dfl3)

# Reorder columns
dfl <- dfl[, c("id", "variable", "response", "prime", "category")]
head(dfl)

# --- Add some information

# Make a numeric question number column
dfl$quest_nr <- as.numeric(sub(".*_", "", dfl$variable))

# Make a reference df for prime names - this is experimental treatment per id
dftemp <- data.frame(prime = names(table(dfl$prime)))
# Categorize primes by hand 
  # Neutral: 0
  # Muslim:  1
  # Gay:     2
  # Women:   3
dftemp$exp <- c(1, 0, 3, 2, 3, 0, 3, 0, 3, 2, 0, 3, 3, 2, 2, 2, 1, 0, 1, 1, 2, 3, 1, 1, 0, 1, 2, 0)

# Make a factor variable for reference 
dftemp$exp_char <- factor(dftemp$exp, levels = 0:3, labels = c("neutral", "muslim", "gay", "women"))

# Merge the exp dataframe with dfl
dfl <- merge(dfl, dftemp, by = "prime")
dfl <- dfl[order(dfl$id, dfl$quest_nr), ]
head(dfl)

# Note: We lose 81 cases due to missing on prime 
dim(dfl)[1] - sum(is.na(dfl$prime))

# Make indicator for t1 or t2
dfl <- data.frame(dfl %>%
  group_by(id) %>%
  mutate(trial = duplicated(prime)))

# Turn this into numeric
dfl$trial <- ifelse(dfl$trial == FALSE, 1, 2)

# Make a numeric response vector
dfl$response_num <- ifelse(dfl$response == "positive", 1, 
                           ifelse(dfl$response == "negative", 0, NA))

# SoSci: "Anteil positiver Bewertungen"

# Reorder the columns 
dfl <- dfl[, c("id", "quest_nr", "prime", "category", "exp_char", "response", "response_num", "trial")]
head(dfl)

# --- Make individual datasets 

# Neutral 
dfl_neutral <- dfl[dfl$exp_char == "neutral", c("id", "prime", "response_num", "trial")]; dim(dfl_neutral)

# Muslim
dfl_muslim <- dfl[dfl$exp_char == "muslim", c("id", "prime", "response_num", "trial")]; dim(dfl_muslim)

# Gay
dfl_gay <- dfl[dfl$exp_char == "gay", c("id", "prime", "response_num", "trial")]; dim(dfl_gay)

# Women 
dfl_women <- dfl[dfl$exp_char == "women", c("id", "prime", "response_num", "trial")]; dim(dfl_women)

# Turn them into wide dataframes
dfw_neutral <- spread(dfl_neutral, prime, response_num)
dfw_muslim <- spread(dfl_muslim, prime, response_num)
dfw_gay <- spread(dfl_gay, prime, response_num)
dfw_women <- spread(dfl_women, prime, response_num)

head(dfw_neutral)

library(tidyverse)

df$id <- as.numeric(rownames(df))

glimpse(df)

head(dfw_muslim)

# Demographics 
df$sex <- df$DG02
df$sex <- ifelse(df$sex == "[NA] keine Angabe" |
                   df$sex == "[NA] nicht beantwortet" | 
                   df$sex == "andere", NA, df$sex)
df$sex <- factor(df$sex, levels = c("maennlich", "weiblich"), labels = c("Male", "Female"))
# table(df$sex)

df$nationality <- df$DG05 
df$nationality <- ifelse(df$nationality == "[NA] keine Angabe" | 
                           df$nationality == "[NA] nicht beantwortet", NA, df$nationality)
# table(df$nationality)

df$chinese <- df$P206
df$chinese <- ifelse(df$chinese == "[NA] nicht beantwortet", NA, df$chinese)
# table(df$chinese)


df$DG20 <- ifelse(df$DG20 == "[NA] nicht beantwortet", NA, df$DG20)
df <- df %>%
  mutate(yob = coalesce(DG03_01, as.numeric(DG20)))

tmp <- df %>% 
  select(id, sex, yob, nationality, DG08, DG13, DG14, DG15, DG16)

dfw_muslim_socdem <- left_join(dfw_muslim, tmp, by = "id")
dfw_gay_socdem <- left_join(dfw_gay, tmp, by = "id")
dfw_women_socdem <- left_join(dfw_women, tmp, by = "id")
dfw_neutral_socdem <- left_join(dfw_neutral, tmp, by = "id")

# --- Save datasets

# Rds 
saveRDS(dfl, "dfl.Rda")

saveRDS(dfw_neutral, "dfw_neutral.Rda")
saveRDS(dfw_muslim, "dfw_muslim.Rda")
saveRDS(dfw_gay, "dfw_gay.Rda")
saveRDS(dfw_women, "dfw_women.Rda")

saveRDS(dfw_neutral_socdem, "C:/Users/andeh/Nextcloud/Jochen-Henrik/amp/amp-paper/data/dfw_neutral_socdem.Rda")
saveRDS(dfw_muslim_socdem, "C:/Users/andeh/Nextcloud/Jochen-Henrik/amp/amp-paper/data/dfw_muslim_socdem.Rda")
saveRDS(dfw_gay_socdem, "C:/Users/andeh/Nextcloud/Jochen-Henrik/amp/amp-paper/data/dfw_gay_socdem.Rda")
saveRDS(dfw_women_socdem, "C:/Users/andeh/Nextcloud/Jochen-Henrik/amp/amp-paper/data/dfw_women_socdem.Rda")

# Csv
write.table(dfl, file = "dfl.csv", na = "NA", row.names = FALSE, col.names = TRUE, sep = ",", dec = ".")
write.table(dfl, file = "dfl_no_colnames.csv", row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")

write.table(dfw_neutral, file = "dfw_neutral.csv", na = "NA", row.names = FALSE, col.names = TRUE, sep = ",", dec = ".")
write.table(dfw_muslim, file = "dfw_muslim.csv", na = "NA", row.names = FALSE, col.names = TRUE, sep = ",", dec = ".")
write.table(dfw_gay, file = "dfw_gay.csv", na = "NA", row.names = FALSE, col.names = TRUE, sep = ",", dec = ".")
write.table(dfw_women, file = "dfw_women.csv", na = "NA", row.names = FALSE, col.names = TRUE, sep = ",", dec = ".")

write.table(dfw_neutral, file = "dfw_neutral_no_colnames.csv", na = "NA", row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")
write.table(dfw_muslim, file = "dfw_muslim_no_colnames.csv", na = "NA", row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")
write.table(dfw_gay, file = "dfw_gay_no_colnames.csv", na = "NA", row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")
write.table(dfw_women, file = "dfw_women_no_colnames.csv", na = "NA", row.names = FALSE, col.names = FALSE, sep = ",", dec = ".")


