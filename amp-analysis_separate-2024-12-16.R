# Clear directory
rm(list = ls())

# NOTE: CHECK MISSINGS ON ALL VARIABLES IN REGRESSION
# COULD BE THAT -9 IS STILL BEING COUNTED AS VALID 

# Data --------------------------------------------------------------------

# Packages
library(dplyr)
library(psych)
library(lubridate)
library(ggplot2)

df <- read.table("data/amp_data_prepared.csv", header = TRUE, sep = ",")

# Get dates 
# df %>%
#   mutate(STARTED = substr(STARTED, 1, 10)) %>%
#   mutate(date_started = ymd(STARTED)) %>%
#   summarise(min = min(date_started, na.rm = TRUE),
#             max = max(date_started, na.rm = TRUE))

# Recodes -----------------------------------------------------------------

# ZU02c2SC: Muslim
# ZU05c2SC: Muslim (mobile)
# ZU06c2SC: Homosexual (mobile)
# ZU07c2SC: Woman (mobile)
# ZU03c2SC: Homosexual
# ZU04c2SC: Woman

# Identify experimental groups
df$group <- ifelse(df$ZU01 == 1, "Muslim", 
                   ifelse(df$ZU01 == 2, "Homosexual", 
                          ifelse(df$ZU01 == 3, "Woman", NA)))

# Mobile or not
df$mobile <- ifelse(df$QUESTNNR == "base", 0, 1)

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
table(df$nationality)

df$chinese <- df$P206
df$chinese <- ifelse(df$chinese == "[NA] nicht beantwortet", NA, df$chinese)
table(df$chinese)

df$DG20 <- ifelse(df$DG20 == "[NA] nicht beantwortet", NA, df$DG20)
df <- df %>%
  mutate(yob = coalesce(DG03_01, as.numeric(DG20)))


# Remove Chinese ----------------------------------------------------------

# This is important, was missing and leading to different descriptives than in the paper. 

df <- df %>% 
  filter(chinese == "Nein")

# Experiment --------------------------------------------------------------

# Combine appropriate columns
df <- df %>%
  mutate(stimulus = coalesce(ZU02c2SC, ZU05c2SC, ZU06c2SC, ZU07c2SC, ZU03c2SC, ZU04c2SC))
df <- df %>%
  mutate(neutral  = coalesce(ZU02c1SC, ZU05c1SC, ZU06c1SC, ZU07c1SC, ZU03c1SC, ZU04c1SC))

# sum(table(df$group))

# Some descriptive summaries
df_sum <- df %>%
  # group(group) %>%    
  group_by(group, sex) %>%                                  # New as of 16.12.2024
  filter(!is.na(sex)) %>%                                   # New as of 16.12.2024
  summarise(bard = mean(stimulus - neutral, na.rm = TRUE), 
            s_d = sd(stimulus - neutral, na.rm = TRUE),
            n_g = n())
df_sum$se_d = df_sum$s_d / sqrt(df_sum$n_g)
df_sum$t = df_sum$bard / df_sum$se_d
df_sum

# Just stimulus prime 
df_sum_stim <- df %>%
  # group_by(group) %>% 
  group_by(group, sex) %>%                                  # New as of 16.12.2024
  filter(!is.na(sex)) %>%                                   # New as of 16.12.2024
  summarise(bard = mean(stimulus, na.rm = TRUE), 
            s_d = sd(stimulus, na.rm = TRUE),
            n_g = n())
df_sum_stim$se_d = df_sum_stim$s_d / sqrt(df_sum_stim$n_g)
df_sum_stim$t = df_sum_stim$bard / df_sum_stim$se_d
df_sum_stim

# Create difference column
df$difference <- df$stimulus - df$neutral
# Negative values indicate outgroup evaluated LESS POSITIVELY than neutral = xenophobic

# Take a look 
# hist(df$difference)
# hist(df$stimulus)
# hist(df$neutral)
# df$acq <- ifelse(df$stimulus == 1 & df$neutral == 1 | df$stimulus == 0 & df$neutral == 0, 1, 0)
# table(df$acq)
# df %>% 
#   group_by(group) %>% 
#   count(acq)


# Direct questions --------------------------------------------------------

# Identify columns
islamophob <- paste0("XE33_0", 2:5)
sexist     <- paste0("XE33_", c("09", "10", "11", "12"))
homophob   <- paste0("XE33_", 13:16)

# Turn into numeric (get rid of value labels)
df[islamophob] <- apply(df[islamophob], 2, function(x) substr(x, 1, 1))
df[sexist]     <- apply(df[sexist], 2, function(x) substr(x, 1, 1))
df[homophob]   <- apply(df[homophob], 2, function(x) substr(x, 1, 1))

df[c(islamophob, sexist, homophob)] <- apply(df[c(islamophob, sexist, homophob)], 2, function(x) as.numeric(x))

# Recode positively worded items
pos_vars <- paste0("XE33_", c("03", "04", "10", "11", "14", "15"))
df[pos_vars] <- apply(df[pos_vars], 2, function(x) abs(x - 6))

# Correlations
# cor(df[islamophob], use = "complete.obs")
# cor(df[sexist], use = "complete.obs")
# cor(df[homophob], use = "complete.obs")

# Relatively low reliabilities 
# psych::alpha(df[islamophob])
# psych::alpha(df[sexist])
# psych::alpha(df[homophob])

# Make attitude scales 
df$islamophob_sum <- rowSums(df[islamophob], na.rm = FALSE)
df$sexist_sum     <- rowSums(df[sexist], na.rm = FALSE)
df$homophob_sum   <- rowSums(df[homophob], na.rm = FALSE)

df %>% 
  group_by(group) %>% 
  summarise(n = n(),
            cor_prop = cor(islamophob_sum, stimulus, use = "complete.obs"),
            cor_diff = cor(islamophob_sum, difference, use = "complete.obs")) %>% 
  na.omit()

df %>% 
  filter(group == "Woman") %>%
  group_by(sex) %>% 
  summarise(n = n(),
            cor_prop = cor(islamophob_sum, stimulus, use = "complete.obs"),
            cor_diff = cor(islamophob_sum, difference, use = "complete.obs")) %>% 
  na.omit()

# BIDR --------------------------------------------------------------------

# According to Stock? (2007): RTC employ impression management (not self-deception)
# Use only items from AM and IM (from Blasberg et al., 2013: Bidimensional Impression Management Index)
# Items BD02_01:08

# head(df$BD02_01)

# Identify items 
# bidr <- c(paste0("BD02_0", 1:9), paste0("BD02_", 10:16))
bimi <- paste0("BD02_0", 1:8)
# psych::alpha(df[bidr], check.keys = TRUE)
# psych::alpha(df[bimi], check.keys = TRUE)

# Items where higher scores indicate less NSA 
# pos_vars <- bidr[-c(2, 4, 7:10, 14, 16)]
pos_vars <- bimi[-c(2, 4, 7:8)]
df[pos_vars] <- apply(df[pos_vars], 2, function(x) abs(x - 6))

# Create scale 
# df$bidr <- rowSums(df[bidr], na.rm = FALSE)
df$bimi <- rowSums(df[bimi], na.rm = FALSE)
# hist(df$bidr)
# hist(df$bimi)

# Desirability beliefs ----------------------------------------------------

db_muslim     <- paste0("SB11_0", 1:4)
db_woman      <- paste0("SB04_0", 1:4)
db_homosexual <- paste0("SB05_0", 1:4) 

pos_vars <- c(paste0("SB11_0", 2:3),
              paste0("SB04_0", 2:3),
              paste0("SB05_0", 2:3))

df[pos_vars] <- apply(df[pos_vars], 2, function(x) abs(x - 6))

# Additive index: higher values = more acceptable 
df$db_muslim <- rowSums(df[db_muslim], na.rm = FALSE)
df$db_woman  <- rowSums(df[db_woman], na.rm = FALSE)
df$db_homosexual <- rowSums(df[db_homosexual], na.rm = FALSE)

df[c(db_muslim, db_woman, db_homosexual)] <- apply(df[c(db_muslim, db_woman, db_homosexual)], 2, function(x) x - 3)

# More acceptable person finds sexist opinion, more sexist they are 
# cor(df$db_woman, df$sexist_sum, use = "complete.obs")

# df %>% 
#   filter(group == "Muslim") %>%
#   summarise(correlation = cor(df$db_muslim, df$islamophob_sum, use = "complete.obs"))
# 
# df %>% 
#   filter(group == "Woman") %>%
#   summarise(correlation = cor(df$db_woman, df$difference, use = "complete.obs"))
# 
# df %>% 
#   filter(group == "Homosexual") %>%
#   summarise(correlation = cor(df$db_homosexual, df$difference, use = "complete.obs"))


# Correlations, impl. and expl.  ------------------------------------------

# df %>%
#   filter(group == "Muslim") %>%
#   summarise(correlation = cor(df$islamophob_sum, df$difference, use = "complete.obs"))
# 
# df %>%
#   filter(group == "Woman" & sex == "maennlich") %>%
#   summarise(correlation = cor(df$sexist_sum, df$difference, use = "complete.obs"))
# 
# df %>%
#   filter(group == "Homosexual") %>%
#   summarise(correlation = cor(df$homophob_sum, df$difference, use = "complete.obs"))

# Filter out the Chinese speakers? Probably a good idea - make backup first for analysis not 
# involving AMP 
df_full <- df
df <- df %>% 
  filter(df$chinese == "Nein")


# Islamophobia ------------------------------------------------------------

df_agg_mus <- df %>% 
  filter(group == "Muslim") %>% 
  group_by(islamophob_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(difference, na.rm = TRUE), 
            se = sd(difference, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()

pmus <- df_agg_mus %>% 
  ggplot(aes(x = islamophob_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit islamophobia scale score", 
                     limits = c(min(df_agg_mus$islamophob_sum) - 1, 
                                max(df_agg_mus$islamophob_sum) + 1),
                     breaks = min(df_agg_mus$islamophob_sum):
                       max(df_agg_mus$islamophob_sum)) + 
  scale_y_continuous(name = "Implicit AMP difference score") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # Descriptive Relationship: Explicit and Implicit Islamophobia",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
pmus

# Stimulus not difference
df_agg_mus_stim <- df %>% 
  filter(group == "Muslim") %>% 
  group_by(islamophob_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(stimulus, na.rm = TRUE), 
            se = sd(stimulus, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()
df_agg_mus_stim

pmus_stim <- df_agg_mus_stim %>% 
  ggplot(aes(x = islamophob_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit islamophobia scale score", 
                     limits = c(min(df_agg_mus$islamophob_sum) - 1, 
                                max(df_agg_mus$islamophob_sum) + 1),
                     breaks = min(df_agg_mus$islamophob_sum):
                       max(df_agg_mus$islamophob_sum)) + 
  scale_y_continuous(name = "Implicit AMP proportion positive") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # Descriptive Relationship: Explicit and Implicit Islamophobia",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
pmus_stim


# Sexism ------------------------------------------------------------------

df_agg_sex <- df %>% 
  filter(group == "Woman") %>% 
  group_by(sexist_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(difference, na.rm = TRUE), 
            se = sd(difference, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()
# df_agg_sex

psex <- df_agg_sex %>% 
  ggplot(aes(x = sexist_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit sexism scale score", 
                     limits = c(min(df_agg_sex$sexist_sum) - 1, 
                                max(df_agg_sex$sexist_sum) + 1),
                     breaks = min(df_agg_sex$sexist_sum):
                       max(df_agg_sex$sexist_sum)) + 
  scale_y_continuous(name = "Implicit AMP difference score") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # "Descriptive Relationship between Explicit and Explicit Sexism Measures",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
psex

df_agg_sex_stim <- df %>% 
  filter(group == "Woman") %>% 
  group_by(sexist_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(stimulus, na.rm = TRUE), 
            se = sd(stimulus, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()
# df_agg_sex

psex_stim <- df_agg_sex_stim %>% 
  ggplot(aes(x = sexist_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit sexism scale score", 
                     limits = c(min(df_agg_sex$sexist_sum) - 1, 
                                max(df_agg_sex$sexist_sum) + 1),
                     breaks = min(df_agg_sex$sexist_sum):
                       max(df_agg_sex$sexist_sum)) + 
  scale_y_continuous(name = "Implicit AMP proportion positive") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # "Descriptive Relationship between Explicit and Explicit Sexism Measures",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
psex_stim


# Homophobia --------------------------------------------------------------

df_agg_hom <- df %>% 
  filter(group == "Homosexual") %>% 
  group_by(homophob_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(difference, na.rm = TRUE), 
            se = sd(difference, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()
# df_agg_hom

phom <- df_agg_hom %>% 
  ggplot(aes(x = homophob_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit homophobia scale score", 
                     limits = c(min(df_agg_hom$homophob_sum) - 1, 
                                max(df_agg_hom$homophob_sum) + 1),
                     breaks = min(df_agg_hom$homophob_sum):
                       max(df_agg_hom$homophob_sum)) + 
  scale_y_continuous(name = "Implicit AMP difference score") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # "Descriptive Relationship between Explicit and Explicit Homophobia Measures",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
phom

df_agg_hom_stim <- df %>% 
  filter(group == "Homosexual") %>% 
  group_by(homophob_sum, sex) %>% 
  summarise(n = n(),
            mean = mean(stimulus, na.rm = TRUE), 
            se = sd(stimulus, na.rm = TRUE) / sqrt(n)) %>% 
  na.omit()
# df_agg_hom

phom_stim <- df_agg_hom_stim %>% 
  ggplot(aes(x = homophob_sum, 
             y = mean)) + 
  geom_point(shape = 16, 
             size = 2) + 
  geom_smooth(method = "loess", 
              formula = "y ~ x", 
              color = "red", 
              size = 1, 
              linetype = "dashed", 
              se = FALSE) + 
  # geom_smooth(method = "lm", 
  #             formula = "y ~ x", 
  #             color = "blue", 
  #             size = 1, 
  #             linetype = "twodash", 
  #             se = FALSE) + 
  scale_x_continuous(name = "Explicit homophobia scale score", 
                     limits = c(min(df_agg_hom$homophob_sum) - 1, 
                                max(df_agg_hom$homophob_sum) + 1),
                     breaks = min(df_agg_hom$homophob_sum):
                       max(df_agg_hom$homophob_sum)) + 
  scale_y_continuous(name = "Implicit AMP proportion positive") + 
  geom_errorbar(aes(ymin = mean - 1.96 * se, 
                    ymax = mean + 1.96 * se),
                width = .2) + 
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") + 
  ggtitle(label = "", # "Descriptive Relationship between Explicit and Explicit Homophobia Measures",
          subtitle = "With Loess curve and 95% CIs around conditional mean") +
  theme_bw() + 
  facet_grid(cols = vars(sex))
phom_stim


df_agg_sex <- df %>% 
  filter(group == "Women") %>% 
  group_by(sexist_sum) %>% 
  summarise(n = n(),
            mean = mean(difference, na.rm = TRUE), 
            se = sd(difference, na.rm = TRUE) / sqrt(n)) 
# df_agg_sex

# Regression --------------------------------------------------------------

# head(df[c("group", "difference", 
#           "islamophob_sum", "sexist_sum", "homophob_sum", 
#           "db_muslim", "db_woman", "db_homosexual", "bimi")])
# 
# df %>%
#   filter(group == "Muslim") %>%
#   lm(difference ~ bimi*db_muslim, data = .) %>%
#   summary()
# 
# df %>%
#   filter(group == "Woman", sex == "maennlich") %>%
#   lm(difference ~ bimi*db_woman, data = .) %>%
#   summary()
# 
# df %>%
#   filter(group == "Homosexual") %>%
#   lm(difference ~ bimi*db_homosexual, data = .) %>%
#   summary()
# 
# df %>%
#   filter(group == "Muslim") %>%
#   lm(islamophob_sum ~ bimi*db_muslim, data = .) %>%
#   summary()
# 
# df %>%
#   filter(group == "Woman") %>%
#   lm(sexist_sum ~ bimi*db_woman, data = .) %>%
#   summary()
# 
# df %>%
#   filter(group == "Homosexual") %>%
#   lm(homophob_sum ~ bimi*db_homosexual, data = .) %>%
#   summary()


df %>%
  group_by(sex) %>% 
  summarise(cor(stimulus, sexist_sum, use = "complete.obs"))

df %>%
  filter(sex == "Male") %>%
  summarise(correlation = cor(df$sexist_sum, df$stimulus, use = "complete.obs"))

df %>%
  filter(group == "Woman" & sex == "Female") %>%
  summarise(correlation = cor(sexist_sum, difference, use = "complete.obs"))

df %>%
  filter(group == "Woman" & sex == "Male") %>%
  summarise(correlation = cor(sexist_sum, difference, use = "complete.obs"))


# t-tests AMP by sex from Jochen ------------------------------------------

lm(sexist_sum ~ sex, data = subset(df, group == "Woman")) %>% 
  summary()

# Sexism
df_sexism <- subset(df, group == "Woman")

describe(df_sexism$sexist_sum)
describeBy(df_sexism$sexist_sum, df_sexism$sex)
t.test(df_sexism$sexist_sum ~ df_sexism$sex, var.equal = TRUE, alternative = "two.sided")

lm(difference ~ sex, data = subset(df, group == "Woman")) %>% 
  summary()

describe(df_sexism$difference)
describeBy(df_sexism$difference, df_sexism$sex)
t.test(df_sexism$difference ~ df_sexism$sex, var.equal = TRUE, alternative = "two.sided")

# Homosexual

lm(homophob_sum ~ sex, data = subset(df, group == "Homosexual")) %>% 
  summary()

df_homosexual <- subset(df, group == "Homosexual")
describe(df_homosexual$homophob_sum)
describeBy(df_homosexual$homophob_sum, df_homosexual$sex)
t.test(df_homosexual$homophob_sum ~ df_homosexual$sex, var.equal = TRUE, alternative = "two.sided")

lm(difference ~ sex, data = subset(df, group == "Homosexual")) %>% 
  summary()

describe(df_homosexual$difference)
describeBy(df_homosexual$difference, df_homosexual$sex)
t.test(df_homosexual$difference ~ df_homosexual$sex, var.equal = TRUE, alternative = "two.sided")

# Islamophob

lm(islamophob_sum ~ sex, data = subset(df, group == "Muslim")) %>% 
  summary()

df_islamophob <- subset(df, group == "Muslim")
describe(df_islamophob$islamophob_sum)
describeBy(df_islamophob$islamophob_sum, df_islamophob$sex)
t.test(df_islamophob$islamophob_sum ~ df_islamophob$sex, var.equal = TRUE, alternative = "two.sided")

lm(difference ~ sex, data = subset(df, group == "Muslim")) %>% 
  summary()

describe(df_islamophob$difference)
describeBy(df_islamophob$difference, df_islamophob$sex)
t.test(df_islamophob$difference ~ df_islamophob$sex, var.equal = TRUE, alternative = "two.sided")

summary(lm(islamophob_sum ~ sex, data = df_islamophob))


# Descriptives for explicit -----------------------------------------------



