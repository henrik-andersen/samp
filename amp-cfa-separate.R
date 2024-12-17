


# Load packages
library(haven)
library(lavaan)
library(tidyverse)
library(semPlot)
library(reshape2)

# Set working directory
# setwd("F:/nextcloud/Jochen-Henrik/amp/data")
setwd("D:/daten/r/")

# Load data
dfw_neutral <- readRDS("dfw_neutral.Rda")
dfw_muslim <- readRDS("dfw_muslim.Rda")
dfw_gay <- readRDS("dfw_gay.Rda")
dfw_women <- readRDS("dfw_women.Rda")

# Get item names for CFA tables
head(dfw_neutral)
head(dfw_muslim)
head(dfw_gay)
head(dfw_women)

# Create column for 'mainliners' (either all good or all bad)
dfw_neutral$mainliner <- ifelse(rowMeans(dfw_neutral[, 3:9]) == 7 |
                                 rowMeans(dfw_neutral[, 3:9]) == 0, 1, 0)

dfw_muslim$mainliner <- ifelse(rowMeans(dfw_muslim[, 3:9]) == 7 |
                                 rowMeans(dfw_muslim[, 3:9]) == 0, 1, 0)

dfw_gay$mainliner <- ifelse(rowMeans(dfw_gay[, 3:9]) == 7 |
                                 rowMeans(dfw_gay[, 3:9]) == 0, 1, 0)

dfw_women$mainliner <- ifelse(rowMeans(dfw_women[, 3:9]) == 7 |
                                 rowMeans(dfw_women[, 3:9]) == 0, 1, 0)

# Number of 'mainliners' 
table(dfw_neutral$mainliner)
table(dfw_muslim$mainliner)
table(dfw_gay$mainliner)
table(dfw_women$mainliner)

# Set column names
names(dfw_neutral) <- c("id", "trial", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "mainliner")
names(dfw_muslim)  <- c("id", "trial", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "mainliner")
names(dfw_gay)     <- c("id", "trial", "h1", "h2", "h3", "h4", "h5", "h6", "h7", "mainliner")
names(dfw_women)   <- c("id", "trial", "w1", "w2", "w3", "w4", "w5", "w6", "w7", "mainliner")


# Make longitudinal wide dataframes ---------------------------------------

# --- Muslims 

# Use dcast to long format, one column at a time 
dfl_m1 <- dcast(dfw_muslim, id ~ trial, value.var = "m1")
dfl_m2 <- dcast(dfw_muslim, id ~ trial, value.var = "m2")
dfl_m3 <- dcast(dfw_muslim, id ~ trial, value.var = "m3")
dfl_m4 <- dcast(dfw_muslim, id ~ trial, value.var = "m4")
dfl_m5 <- dcast(dfw_muslim, id ~ trial, value.var = "m5")
dfl_m6 <- dcast(dfw_muslim, id ~ trial, value.var = "m6")
dfl_m7 <- dcast(dfw_muslim, id ~ trial, value.var = "m7")
dfl_ml <- dcast(dfw_muslim, id ~ trial, value.var = "mainliner") # ml for mainliner

# Merge back together 
dfl_m <- merge(dfl_m1, dfl_m2, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22")

dfl_m <- merge(dfl_m, dfl_m3, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32")

dfl_m <- merge(dfl_m, dfl_m4, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32", "m41", "m42")

dfl_m <- merge(dfl_m, dfl_m5, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32", "m41", "m42", "m51", "m52")

dfl_m <- merge(dfl_m, dfl_m6, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32", "m41", "m42", "m51", "m52", "m61", "m62")

dfl_m <- merge(dfl_m, dfl_m7, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32", "m41", "m42", "m51", "m52", "m61", "m62", "m71", "m72")

dfl_m <- merge(dfl_m, dfl_ml, by = "id")
names(dfl_m) <- c("id", "m11", "m12", "m21", "m22", "m31", "m32", "m41", "m42", "m51", "m52", "m61", "m62", "m71", "m72", "ml1", "ml2")


# --- Homosexuals

dfl_h1 <- dcast(dfw_gay, id ~ trial, value.var = "h1")
dfl_h2 <- dcast(dfw_gay, id ~ trial, value.var = "h2")
dfl_h3 <- dcast(dfw_gay, id ~ trial, value.var = "h3")
dfl_h4 <- dcast(dfw_gay, id ~ trial, value.var = "h4")
dfl_h5 <- dcast(dfw_gay, id ~ trial, value.var = "h5")
dfl_h6 <- dcast(dfw_gay, id ~ trial, value.var = "h6")
dfl_h7 <- dcast(dfw_gay, id ~ trial, value.var = "h7")
dfl_hl <- dcast(dfw_gay, id ~ trial, value.var = "mainliner") # hl for mainliner, doesnt make sense but who cares

dfl_h <- merge(dfl_h1, dfl_h2, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22")

dfl_h <- merge(dfl_h, dfl_h3, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32")

dfl_h <- merge(dfl_h, dfl_h4, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32", "h41", "h42")

dfl_h <- merge(dfl_h, dfl_h5, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32", "h41", "h42", "h51", "h52")

dfl_h <- merge(dfl_h, dfl_h6, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32", "h41", "h42", "h51", "h52", "h61", "h62")

dfl_h <- merge(dfl_h, dfl_h7, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32", "h41", "h42", "h51", "h52", "h61", "h62", "h71", "h72")

dfl_h <- merge(dfl_h, dfl_hl, by = "id")
names(dfl_h) <- c("id", "h11", "h12", "h21", "h22", "h31", "h32", "h41", "h42", "h51", "h52", "h61", "h62", "h71", "h72", "hl1", "hl2")


# --- Women

dfl_w1 <- dcast(dfw_women, id ~ trial, value.var = "w1")
dfl_w2 <- dcast(dfw_women, id ~ trial, value.var = "w2")
dfl_w3 <- dcast(dfw_women, id ~ trial, value.var = "w3")
dfl_w4 <- dcast(dfw_women, id ~ trial, value.var = "w4")
dfl_w5 <- dcast(dfw_women, id ~ trial, value.var = "w5")
dfl_w6 <- dcast(dfw_women, id ~ trial, value.var = "w6")
dfl_w7 <- dcast(dfw_women, id ~ trial, value.var = "w7")
dfl_wl <- dcast(dfw_women, id ~ trial, value.var = "mainliner")

dfl_w <- merge(dfl_w1, dfl_w2, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22")

dfl_w <- merge(dfl_w, dfl_w3, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32")

dfl_w <- merge(dfl_w, dfl_w4, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32", "w41", "w42")

dfl_w <- merge(dfl_w, dfl_w5, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32", "w41", "w42", "w51", "w52")

dfl_w <- merge(dfl_w, dfl_w6, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32", "w41", "w42", "w51", "w52", "w61", "w62")

dfl_w <- merge(dfl_w, dfl_w7, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32", "w41", "w42", "w51", "w52", "w61", "w62", "w71", "w72")

dfl_w <- merge(dfl_w, dfl_wl, by = "id")
names(dfl_w) <- c("id", "w11", "w12", "w21", "w22", "w31", "w32", "w41", "w42", "w51", "w52", "w61", "w62", "w71", "w72", "wl1", "wl2")


# --- Neutral 

dfl_n1 <- dcast(dfw_neutral, id ~ trial, value.var = "n1")
dfl_n2 <- dcast(dfw_neutral, id ~ trial, value.var = "n2")
dfl_n3 <- dcast(dfw_neutral, id ~ trial, value.var = "n3")
dfl_n4 <- dcast(dfw_neutral, id ~ trial, value.var = "n4")
dfl_n5 <- dcast(dfw_neutral, id ~ trial, value.var = "n5")
dfl_n6 <- dcast(dfw_neutral, id ~ trial, value.var = "n6")
dfl_n7 <- dcast(dfw_neutral, id ~ trial, value.var = "n7")
dfl_nl <- dcast(dfw_neutral, id ~ trial, value.var = "mainliner") # ml for mainliner

dfl_n <- merge(dfl_n1, dfl_n2, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22")

dfl_n <- merge(dfl_n, dfl_n3, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32")

dfl_n <- merge(dfl_n, dfl_n4, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32", "n41", "n42")

dfl_n <- merge(dfl_n, dfl_n5, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32", "n41", "n42", "n51", "n52")

dfl_n <- merge(dfl_n, dfl_n6, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32", "n41", "n42", "n51", "n52", "n61", "n62")

dfl_n <- merge(dfl_n, dfl_n7, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32", "n41", "n42", "n51", "n52", "n61", "n62", "n71", "n72")

dfl_n <- merge(dfl_n, dfl_nl, by = "id")
names(dfl_n) <- c("id", "n11", "n12", "n21", "n22", "n31", "n32", "n41", "n42", "n51", "n52", "n61", "n62", "n71", "n72", "nl1", "nl2")





# Make an average mainliner score for each respondent. 
# If they were mainliner in both trial 1 and 2, then 1, otherwise 
# if they were NOT mainliner in both trial 1 and 2, then 0, otherwise 0.5 
dfl_m$mainliner <- rowMeans(dfl_m[c("ml1", "ml2")], na.rm = TRUE)
dfl_h$mainliner <- rowMeans(dfl_h[c("hl1", "hl2")], na.rm = TRUE)
dfl_w$mainliner <- rowMeans(dfl_w[c("wl1", "wl2")], na.rm = TRUE)
dfl_n$mainliner <- rowMeans(dfl_n[c("nl1", "nl2")], na.rm = TRUE)
table(dfl_m$mainliner); table(dfl_h$mainliner); table(dfl_w$mainliner); table(dfl_n$mainliner) 

# --- 
# Note: I think dfl is the wrong name, because after the merge, it is actually dfw 
# --- 

# Make dataframes with mean scores per id over trial 1 and 2
dfw_neutral2 <- dfw_neutral %>%
  group_by(id) %>%
  summarise_at(vars("n1", "n2", "n3", "n4", "n5", "n6", "n7", "mainliner"), mean)

dfw_muslim2 <- dfw_muslim %>%
  group_by(id) %>%
  summarise_at(vars("m1", "m2", "m3", "m4", "m5", "m6", "m7", "mainliner"), mean)

dfw_gay2 <- dfw_gay %>%
  group_by(id) %>%
  summarise_at(vars("h1", "h2", "h3", "h4", "h5", "h6", "h7", "mainliner"), mean)

dfw_women2 <- dfw_women %>%
  group_by(id) %>%
  summarise_at(vars("w1", "w2", "w3", "w4", "w5", "w6", "w7", "mainliner"), mean)

# Average scores: Merge each experimental group the the neutral control 
dfw_m <- merge(dfw_neutral2, dfw_muslim2, by = "id")
dfw_h <- merge(dfw_neutral2, dfw_gay2, by = "id")
dfw_w <- merge(dfw_neutral2, dfw_women2, by = "id")

# Two trials: Merge each experimental group the the neutral control 
dfl_m <- merge(dfl_m, dfl_n, by = "id")
dfl_h <- merge(dfl_h, dfl_n, by = "id")
dfl_w <- merge(dfl_w, dfl_n, by = "id")


# --- Get rid of the mainliner - turn this on or off depending 

# # Average scores 
# 
# # Muslim
# dim(dfw_m)
# dfw_m$mainliner <- ifelse(dfw_m$mainliner.x == 1 & dfw_m$mainliner.y == 1, 1, 0)
# dfw_m <- dfw_m[dfw_m$mainliner != 1, ]
# dim(dfw_m)
# table(dfw_m$mainliner)
# 
# # Homosexual
# dim(dfw_h)
# dfw_h$mainliner <- ifelse(dfw_h$mainliner.x == 1 & dfw_h$mainliner.y == 1, 1, 0)
# dfw_h <- dfw_h[dfw_h$mainliner != 1, ]
# dim(dfw_h)
# table(dfw_h$mainliner)
# 
# # Women 
# dim(dfw_w)
# dfw_w$mainliner <- ifelse(dfw_w$mainliner.x == 1 & dfw_w$mainliner.y == 1, 1, 0)
# dfw_w <- dfw_w[dfw_w$mainliner != 1, ]
# dim(dfw_w)
# table(dfw_w$mainliner)
# 
# # Two separate trials - here 'mainliner' column is already an average score across two trials 
# # per construct (stimulus, neutral)
# 
# # Muslim
# dim(dfl_m)
# dfl_m$mainliner <- ifelse(dfl_m$mainliner.x == 1 & dfl_m$mainliner.y == 1, 1, 0)
# dfl_m <- dfl_m[dfl_m$mainliner != 1, ]
# dim(dfl_m)
# table(dfl_m$mainliner)
# 
# # Homosexual
# dim(dfl_h)
# dfl_h$mainliner <- ifelse(dfl_h$mainliner.x == 1 & dfl_h$mainliner.y == 1, 1, 0)
# dfl_h <- dfl_h[dfl_h$mainliner != 1, ]
# dim(dfl_h)
# table(dfl_h$mainliner)
# 
# # Women 
# dim(dfl_w)
# dfl_w$mainliner <- ifelse(dfl_w$mainliner.x == 1 & dfl_w$mainliner.y == 1, 1, 0)
# dfl_w <- dfl_w[dfl_w$mainliner != 1, ]
# dim(dfl_w)
# table(dfl_w$mainliner)


# CFAs --------------------------------------------------------------------

#saveRDS(dfw_m, "dfw_m.Rda")
#saveRDS(dfw_h, "dfw_h.Rda")
#saveRDS(dfw_w, "dfw_w.Rda")

df <- read.csv("D:/daten/r/amp_data_prepared.csv")
df$id <- as.numeric(rownames(df))

glimpse(df)

# Demographics 
df$sex <- df$DG02
df$sex <- ifelse(df$sex == "[NA] keine Angabe" |
                   df$sex == "[NA] nicht beantwortet" | 
                   df$sex == "andere", NA, df$sex)
df$sex <- factor(df$sex, levels = c("maennlich", "weiblich"), labels = c("Male", "Female"))
 table(df$sex)

df$nationality <- df$DG05 
df$nationality <- ifelse(df$nationality == "[NA] keine Angabe" | 
                           df$nationality == "[NA] nicht beantwortet", NA, df$nationality)
 table(df$nationality)

df$chinese <- df$P206
df$chinese <- ifelse(df$chinese == "[NA] nicht beantwortet", NA, df$chinese)
 table(df$chinese)


df$DG20 <- ifelse(df$DG20 == "[NA] nicht beantwortet", NA, df$DG20)
df <- df %>%
  mutate(yob = coalesce(DG03_01, as.numeric(DG20)))

glimpse(df)


tmp <- df %>% 
  select(id, sex, yob, nationality, DG08, DG13, DG14, DG15, DG16)

dfw_m_socdem <- left_join(dfw_m, tmp, by = "id")
dfw_h_socdem <- left_join(dfw_h, tmp, by = "id")
dfw_w_socdem <- left_join(dfw_w, tmp, by = "id")

#saveRDS(dfw_m, "dfw_m_socdem.Rda")
#saveRDS(dfw_h, "dfw_h_socdem.Rda")
#saveRDS(dfw_w, "dfw_w_socdem.Rda")



######################################################################

# --- Mean scores 

# - Separate constructs 

# Muslim
m_mus <- '
  Neutral =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 
  Muslim  =~ 1*m1 + m2 + m3 + m4 + m5 + m6 + m7
  Neutral ~~ Muslim
    Neutral ~ 0
    Muslim ~ 0
'
m_mus.fit <- sem(m_mus, dfw_m, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "m1", "m2", "m3", "m4", "m5", "m6", "m7")
                 )
m_mus_std.fit <- update(m_mus.fit, 
                        std.lv = TRUE, std.ov = TRUE)

summary(m_mus.fit, standardized = TRUE, fit.measures = TRUE)


############ mgoup by sex


m_mus.fit_conf <- sem(m_mus, data = dfw_m_socdem, group="sex",
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_conf, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_th <- sem(m_mus, data = dfw_m_socdem, group="sex", group.equal=c("thresholds"),
                      ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                  "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_th, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_met <- sem(m_mus, data = dfw_m_socdem, group="sex", group.equal=c("thresholds", "loadings"),
                      ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                  "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_met, standardized = TRUE, fit.measures = TRUE)

anova(m_mus.fit_conf, m_mus.fit_th, m_mus.fit_met)


############ mgoup by educ

dfw_m_socdem$educ <- ifelse(dfw_m_socdem$DG08 == "Fachhochschulreife" | dfw_m_socdem$DG08 == "Abitur / Hochschulreife / Fachabitur / Erweiterte Oberschule (EOS) (DDR-Abschluss)" , c("hi"), c("lo"))


m_mus.fit_educ_conf <- sem(m_mus, data = dfw_m_socdem, group="educ",
                      ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                  "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_educ_conf, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_educ_th <- sem(m_mus, data = dfw_m_socdem, group="educ", group.equal=c("thresholds"),
                    ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_educ_th, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_educ_met <- sem(m_mus, data = dfw_m_socdem, group="educ", group.equal=c("thresholds", "loadings"),
                     ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                 "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_educ_met, standardized = TRUE, fit.measures = TRUE)

anova(m_mus.fit_educ_conf, m_mus.fit_educ_th, m_mus.fit_educ_met)



############ mgoup by yob

#dfw_m_socdem$yob <- ifelse(dfw_m_socdem$yob < 1980, c("older"), c("younger"))
summary(dfw_m_socdem$yob)
dfw_m_socdem$yob2 <- as.numeric(dfw_m_socdem$yob)
dfw_m_socdem$yob3 <- ifelse(dfw_m_socdem$yob2 < 1975, c("older"), c("younger"))
#2020-45=1975

m_mus.fit_age_conf <- sem(m_mus, data = dfw_m_socdem, group="yob3",
                           ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                       "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_age_conf, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_age_th <- sem(m_mus, data = dfw_m_socdem, group="yob3", group.equal=c("thresholds"),
                         ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                     "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_age_th, standardized = TRUE, fit.measures = TRUE)


m_mus.fit_age_met <- sem(m_mus, data = dfw_m_socdem, group="yob3", group.equal=c("thresholds", "loadings"),
                          ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                      "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
summary(m_mus.fit_age_met, standardized = TRUE, fit.measures = TRUE)

anova(m_mus.fit_age_conf, m_mus.fit_age_th, m_mus.fit_age_met)





###############################
# Omega reliability
lambda <- lavInspect(m_mus.fit, "coef")$lambda[8:14, 2]
psi <- lavInspect(m_mus.fit, "coef")$psi[2, 2]
theta <- diag(lavInspect(m_mus.fit, "coef")$theta[8:14, 8:14])

(sum(lambda)^2 * psi) / (sum(lambda)^2 * psi + sum(theta))

# With semTools
semTools::reliability(m_mus.fit)

lambda <- lavInspect(m_mus.fit, "std")$lambda; lambda

A <- (matrix(rep(1, 14), nrow = 1, ncol = 14) %*% lambda)^2; A

theta <- lavInspect(m_mus.fit, "std")$theta; theta
# 14 x 14

B <- t(matrix(c(rep(c(1,0), each = 7), rep(c(0,1), each = 7)), nrow = 14, ncol = 2)) %*% theta %*% matrix(rep(1, 14), nrow = 14, ncol = 1)
t(B)

A / (A + t(B))

# Homosexual 
m_hom <- '
  Neutral    =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 
  Homosexual =~ 1*h1 + h2 + h3 + h4 + h5 + h6 + h7
  Neutral ~~ Homosexual
  
  Neutral ~ 0
  Homosexual ~ 0
'
m_hom.fit <- sem(m_hom, dfw_h, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "h1", "h2", "h3", "h4", "h5", "h6", "h7")
                 )
m_hom_std.fit <- update(m_hom.fit, 
                        std.lv = TRUE, std.ov = TRUE)

summary(m_hom.fit, standardized = TRUE, fit.measures = TRUE)


####################################

############ mgoup by sex


m_hom.fit_conf <- sem(m_hom, data = dfw_h_socdem, group="sex",
                      ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                  "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_conf, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_th <- sem(m_hom, data = dfw_h_socdem, group="sex", group.equal=c("thresholds"),
                    ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_th, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_met <- sem(m_hom, data = dfw_h_socdem, group="sex", group.equal=c("thresholds", "loadings"),
                     ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                 "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_met, standardized = TRUE, fit.measures = TRUE)

anova(m_hom.fit_conf, m_hom.fit_th, m_hom.fit_met)


############ mgoup by educ

dfw_h_socdem$educ <- ifelse(dfw_h_socdem$DG08 == "Fachhochschulreife" | dfw_h_socdem$DG08 == "Abitur / Hochschulreife / Fachabitur / Erweiterte Oberschule (EOS) (DDR-Abschluss)" , c("hi"), c("lo"))


m_hom.fit_educ_conf <- sem(m_hom, data = dfw_h_socdem, group="educ",
                           ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                       "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_educ_conf, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_educ_th <- sem(m_hom, data = dfw_h_socdem, group="educ", group.equal=c("thresholds"),
                         ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                     "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_educ_th, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_educ_met <- sem(m_hom, data = dfw_h_socdem, group="educ", group.equal=c("thresholds", "loadings"),
                          ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                      "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_educ_met, standardized = TRUE, fit.measures = TRUE)

anova(m_hom.fit_educ_conf, m_hom.fit_educ_th, m_hom.fit_educ_met)



############ mgoup by yob

#dfw_m_socdem$yob <- ifelse(dfw_m_socdem$yob < 1980, c("older"), c("younger"))
summary(dfw_h_socdem$yob)
dfw_h_socdem$yob2 <- as.numeric(dfw_h_socdem$yob)
dfw_h_socdem$yob3 <- ifelse(dfw_h_socdem$yob2 < 1975, c("older"), c("younger"))
#2020-45=1975

m_hom.fit_age_conf <- sem(m_hom, data = dfw_h_socdem, group="yob3",
                          ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                      "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_age_conf, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_age_th <- sem(m_hom, data = dfw_h_socdem, group="yob3", group.equal=c("thresholds"),
                        ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                    "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_age_th, standardized = TRUE, fit.measures = TRUE)


m_hom.fit_age_met <- sem(m_hom, data = dfw_h_socdem, group="yob3", group.equal=c("thresholds", "loadings"),
                         ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                     "h1", "h2", "h3", "h4", "h5", "h6", "h7"))
summary(m_hom.fit_age_met, standardized = TRUE, fit.measures = TRUE)

anova(m_hom.fit_age_conf, m_hom.fit_age_th, m_hom.fit_age_met)





# Omega reliability
lambda <- lavInspect(m_hom.fit, "coef")$lambda[8:14, 2]
psi <- lavInspect(m_hom.fit, "coef")$psi[2, 2]
theta <- diag(lavInspect(m_hom.fit, "coef")$theta[8:14, 8:14])

(sum(lambda)^2 * psi) / (sum(lambda)^2 * psi + sum(theta))

# With semTools
semTools::reliability(m_hom.fit)

lambda <- lavInspect(m_hom.fit, "std")$lambda; lambda

A <- (matrix(rep(1, 14), nrow = 1, ncol = 14) %*% lambda)^2; A

theta <- lavInspect(m_hom.fit, "std")$theta; theta

B <- t(matrix(c(rep(c(1,0), each = 7), rep(c(0,1), each = 7)), nrow = 14, ncol = 2)) %*% theta %*% matrix(rep(1, 14), nrow = 14, ncol = 1)
t(B)

A / (A + t(B))





# Women 
m_wom <- '
  Neutral =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 
  Women   =~ 1*w1 + w2 + w3 + w4 + w5 + w6 + w7
  Neutral ~~ Women
      Neutral ~ 0
      Women ~ 0
'
m_wom.fit <- sem(m_wom, dfw_w, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "w1", "w2", "w3", "w4", "w5", "w6", "w7")
                 )
m_wom_std.fit <- update(m_wom.fit, 
                 std.lv = TRUE, std.ov = TRUE)

summary(m_wom.fit, standardized = TRUE, fit.measures = TRUE)


############ mgoup by sex


m_wom.fit_conf <- sem(m_wom, data = dfw_w_socdem, group="sex",
                      ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                  "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_conf, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_th <- sem(m_wom, data = dfw_w_socdem, group="sex", group.equal=c("thresholds"),
                    ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_th, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_met <- sem(m_wom, data = dfw_w_socdem, group="sex", group.equal=c("thresholds", "loadings"),
                     ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                 "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_met, standardized = TRUE, fit.measures = TRUE)

anova(m_wom.fit_conf, m_wom.fit_th, m_wom.fit_met)


############ mgoup by educ

dfw_w_socdem$educ <- ifelse(dfw_w_socdem$DG08 == "Fachhochschulreife" | dfw_w_socdem$DG08 == "Abitur / Hochschulreife / Fachabitur / Erweiterte Oberschule (EOS) (DDR-Abschluss)" , c("hi"), c("lo"))


m_wom.fit_educ_conf <- sem(m_wom, data = dfw_w_socdem, group="educ",
                           ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                       "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_educ_conf, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_educ_th <- sem(m_wom, data = dfw_w_socdem, group="educ", group.equal=c("thresholds"),
                         ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                     "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_educ_th, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_educ_met <- sem(m_wom, data = dfw_w_socdem, group="educ", group.equal=c("thresholds", "loadings"),
                          ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                      "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_educ_met, standardized = TRUE, fit.measures = TRUE)

anova(m_wom.fit_educ_conf, m_wom.fit_educ_th, m_wom.fit_educ_met)



############ mgoup by yob

#dfw_m_socdem$yob <- ifelse(dfw_m_socdem$yob < 1980, c("older"), c("younger"))
summary(dfw_w_socdem$yob)
dfw_w_socdem$yob2 <- as.numeric(dfw_w_socdem$yob)
dfw_w_socdem$yob3 <- ifelse(dfw_w_socdem$yob2 < 1975, c("older"), c("younger"))
#2020-45=1975

m_wom.fit_age_conf <- sem(m_wom, data = dfw_w_socdem, group="yob3",
                          ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                      "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_age_conf, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_age_th <- sem(m_wom, data = dfw_w_socdem, group="yob3", group.equal=c("thresholds"),
                        ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                    "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_age_th, standardized = TRUE, fit.measures = TRUE)


m_wom.fit_age_met <- sem(m_wom, data = dfw_w_socdem, group="yob3", group.equal=c("thresholds", "loadings"),
                         ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                                     "w1", "w2", "w3", "w4", "w5", "w6", "w7"))
summary(m_wom.fit_age_met, standardized = TRUE, fit.measures = TRUE)

anova(m_wom.fit_age_conf, m_wom.fit_age_th, m_wom.fit_age_met)



#########################

### Fit-Maße muslims
fitMeasures(m_mus.fit_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_mus.fit_educ_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_educ_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_educ_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_mus.fit_age_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_age_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_mus.fit_age_met, c("cfi.scaled","rmsea.scaled"))

### Fit-Maße women
fitMeasures(m_wom.fit_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_wom.fit_educ_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_educ_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_educ_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_wom.fit_age_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_age_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_wom.fit_age_met, c("cfi.scaled","rmsea.scaled"))

### Fit-Maße homoph
fitMeasures(m_hom.fit_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_hom.fit_educ_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_educ_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_educ_met, c("cfi.scaled","rmsea.scaled"))

fitMeasures(m_hom.fit_age_conf, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_age_th, c("cfi.scaled","rmsea.scaled"))
fitMeasures(m_hom.fit_age_met, c("cfi.scaled","rmsea.scaled"))







# Omega reliability
lambda <- lavInspect(m_wom.fit, "coef")$lambda[8:14, 2]
psi <- lavInspect(m_wom.fit, "coef")$psi[2, 2]
theta <- diag(lavInspect(m_wom.fit, "coef")$theta[8:14, 8:14])

(sum(lambda)^2 * psi) / (sum(lambda)^2 * psi + sum(theta))

# With semTools
semTools::reliability(m_wom.fit)

lambda <- lavInspect(m_wom.fit, "std")$lambda; lambda
psi <- lavInspect(m_wom.fit, "std")$psi; psi

A <- (matrix(rep(1, 14), nrow = 1, ncol = 14) %*% lambda)^2; A
# A <- A %*% psi

theta <- lavInspect(m_wom.fit, "std")$theta; theta

B <- t(matrix(c(rep(c(1,0), each = 7), rep(c(0,1), each = 7)), nrow = 14, ncol = 2)) %*% theta %*% matrix(rep(1, 14), nrow = 14, ncol = 1)
t(B)

A / (A + t(B))

# - One construct (stimulus + neutral)

# Muslim
m_must <- ' # t for together 
  neutral =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 + m1 + m2 + m3 + m4 + m5 + m6 + m7
'
m_must.fit <- sem(m_must, dfw_m, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "m1", "m2", "m3", "m4", "m5", "m6", "m7")
                 )
summary(m_must.fit, standardized = TRUE, fit.measures = TRUE)

# Homosexual 
m_homt <- '
  neutral =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 + h1 + h2 + h3 + h4 + h5 + h6 + h7
'
m_homt.fit <- sem(m_homt, dfw_h, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "h1", "h2", "h3", "h4", "h5", "h6", "h7")
                 )
summary(m_homt.fit, standardized = TRUE, fit.measures = TRUE)

# Women 
m_womt <- '
  neutral =~ 1*n1 + n2 + n3 + n4 + n5 + n6 + n7 + w1 + w2 + w3 + w4 + w5 + w6 + w7
'
m_womt.fit <- sem(m_womt, dfw_w, 
                 ordered = c("n1", "n2", "n3", "n4", "n5", "n6", "n7",
                             "w1", "w2", "w3", "w4", "w5", "w6", "w7")
                 )
summary(m_womt.fit, standardized = TRUE, fit.measures = TRUE)


# # --- Two trials 
# 
# # - Separate constructs 
# 
# # Muslim
# m_mus2 <- '
#   neutral1 =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 
#   neutral2 =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 
#   muslim1  =~ 1*m11 + m21 + m31 + m41 + m51 + m61 + m71
#   muslim2  =~ 1*m12 + m22 + m32 + m42 + m52 + m62 + m72
#   neutral1 ~~ neutral2 + muslim1 + muslim2  
#   neutral2 ~~ muslim1 + muslim2
#   muslim1  ~~ muslim2
# '
# m_mus2.fit <- sem(m_mus2, dfl_m, 
#                  ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                              "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                              "m11", "m21", "m31", "m41", "m51", "m61", "m71", 
#                              "m12", "m22", "m32", "m42", "m52", "m62", "m72")
#                  )
# summary(m_mus2.fit, standardized = TRUE, fit.measures = TRUE)
# 
# # Homosexual 
# m_hom2 <- '
#   neutral1    =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 
#   neutral2    =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 
#   homosexual1 =~ 1*h11 + h21 + h31 + h41 + h51 + h61 + h71
#   homosexual2 =~ 1*h12 + h22 + h32 + h42 + h52 + h62 + h72
#   neutral1 ~~ neutral2 + homosexual1 + homosexual2  
#   neutral2 ~~ homosexual1 + homosexual2
#   homosexual1  ~~ homosexual2
# '
# m_hom2.fit <- sem(m_hom2, dfl_h, 
#                   ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                               "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                               "h11", "h21", "h31", "h41", "h51", "h61", "h71", 
#                               "h12", "h22", "h32", "h42", "h52", "h62", "h72")
# )
# summary(m_hom2.fit, standardized = TRUE, fit.measures = TRUE)
# 
# # Women 
# m_wom2 <- '
#   neutral1 =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 
#   neutral2 =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 
#   women1   =~ 1*w11 + w21 + w31 + w41 + w51 + w61 + w71
#   women2   =~ 1*w12 + w22 + w32 + w42 + w52 + w62 + w72
#   neutral1 ~~ neutral2 + women1 + women2  
#   neutral2 ~~ women1 + women2
#   women1  ~~ women2
# '
# m_wom2.fit <- sem(m_wom2, dfl_w, 
#                   ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                               "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                               "w11", "w21", "w31", "w41", "w51", "w61", "w71", 
#                               "w12", "w22", "w32", "w42", "w52", "w62", "w72")
# )
# summary(m_wom2.fit, standardized = TRUE, fit.measures = TRUE)
# 
# # - One construct (stimulus + neutral)
# 
# # Muslim
# m_mus2t <- '
#   neutral1 =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 + m11 + m21 + m31 + m41 + m51 + m61 + m71
#   neutral2 =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 + m12 + m22 + m32 + m42 + m52 + m62 + m72
#   neutral1 ~~ neutral2
# '
# m_mus2t.fit <- sem(m_mus2t, dfl_m, 
#                   ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                               "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                               "m11", "m21", "m31", "m41", "m51", "m61", "m71", 
#                               "m12", "m22", "m32", "m42", "m52", "m62", "m72")
# )
# summary(m_mus2t.fit, standardized = TRUE, fit.measures = TRUE)
# 
# # Homosexual 
# m_hom2t <- '
#   neutral1    =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 + h11 + h21 + h31 + h41 + h51 + h61 + h71
#   neutral2    =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 + h12 + h22 + h32 + h42 + h52 + h62 + h72
#   neutral1 ~~ neutral2 
# '
# m_hom2t.fit <- sem(m_hom2t, dfl_h, 
#                   ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                               "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                               "h11", "h21", "h31", "h41", "h51", "h61", "h71", 
#                               "h12", "h22", "h32", "h42", "h52", "h62", "h72")
# )
# summary(m_hom2t.fit, standardized = TRUE, fit.measures = TRUE)
# lavInspect(m_hom2t.fit, "cov.lv")
# 
# # Women 
# m_wom2t <- '
#   neutral1 =~ 1*n11 + n21 + n31 + n41 + n51 + n61 + n71 + w11 + w21 + w31 + w41 + w51 + w61 + w71
#   neutral2 =~ 1*n12 + n22 + n32 + n42 + n52 + n62 + n72 + w12 + w22 + w32 + w42 + w52 + w62 + w72
#   neutral1 ~~ neutral2 
# '
# m_wom2t.fit <- sem(m_wom2t, dfl_w, 
#                   ordered = c("n11", "n21", "n31", "n41", "n51", "n61", "n71",
#                               "n12", "n22", "n32", "n42", "n52", "n62", "n72",
#                               "w11", "w21", "w31", "w41", "w51", "w61", "w71", 
#                               "w12", "w22", "w32", "w42", "w52", "w62", "w72")
# )
# summary(m_wom2t.fit, standardized = TRUE, fit.measures = TRUE)
# lavInspect(m_wom2t.fit, "cov.lv")
# library(matrixcalc)
# matrixcalc::is.positive.definite(lavInspect(m_wom2t.fit, "cov.lv"))
# eigen(lavInspect(m_wom2t.fit, "cov.lv"))


