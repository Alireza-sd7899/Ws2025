library(dplyr)
library(haven)
library(sandwich)
library(lmtest)
library(tableone) # for frequency tables

# Set working directory
setwd("E:/My education/UNI/edu uni/Semester 2/Data science for Business") 

# Load data
df <- read_dta("alldata.dta")

# Drop pie treatment 24
df <- df %>%
  filter(pietreatment != 24) %>%
  mutate(totaldelay = if_else(delay == 1 | pass == 0, 1, 0))

# Create 'num' within treatment-session-group-round-bargaininground
df <- df %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = n()) %>%
  ungroup()

# -------------------------------
# Large Budget delays (piesize != 24)
# -------------------------------
df_large <- df %>% filter(num == 1, piesize != 24)

# Frequency table of totaldelay by treatment
table(df_large$totaldelay, df_large$treatment)

# Probit regressions
# Using glm with family=binomial(link="probit")
# Cluster-robust standard errors using 'vcovCL' from sandwich

# u48
mod_u48 <- glm(totaldelay ~ u48, family = binomial(link = "probit"), data = df_large %>% filter(m48 == 1 | u48 == 1))
coeftest(mod_u48, vcov = vcovCL(mod_u48, cluster = ~uniquesession))

# u96
mod_u96 <- glm(totaldelay ~ u96, family = binomial(link = "probit"), data = df_large %>% filter(m96 == 1 | u96 == 1))
coeftest(mod_u96, vcov = vcovCL(mod_u96, cluster = ~uniquesession))

# -------------------------------
# Small Budget delays (piesize == 24)
# -------------------------------
df_small <- df %>% filter(num == 1, piesize == 24)

# Frequency table of totaldelay by treatment
table(df_small$totaldelay, df_small$treatment)

# Probit regressions
# u48
mod_u48_small <- glm(totaldelay ~ u48, family = binomial(link = "probit"), data = df_small %>% filter(m48 == 1 | u48 == 1))
coeftest(mod_u48_small, vcov = vcovCL(mod_u48_small, cluster = ~uniquesession))

# u96
mod_u96_small <- glm(totaldelay ~ u96, family = binomial(link = "probit"), data = df_small %>% filter(m96 == 1 | u96 == 1))
coeftest(mod_u96_small, vcov = vcovCL(mod_u96_small, cluster = ~uniquesession))
library(dplyr)
library(haven)
library(sandwich)
library(lmtest)

# Load the data
df <- read_dta("alldata.dta")

# Create totaldelay variable
df <- df %>%
  mutate(totaldelay = if_else(delay == 1 | pass == 0, 1, 0))

# Create 'num' within treatment-session-group-round-bargaininground
df <- df %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = n()) %>%
  ungroup()

# Filter for piesize 24 and pietreatment 24 and num==1
df_small <- df %>%
  filter(num == 1, piesize == 24, pietreatment == 24)

# Frequency table of totaldelay by treatment (column percentages)
table(df_small$totaldelay, df_small$treatment)
prop.table(table(df_small$totaldelay, df_small$treatment), margin = 2)  # column percentages

# Probit regression
# Only include rows where proposer equals membernumber
mod_u24 <- glm(totaldelay ~ u24, family = binomial(link = "probit"),
               data = df_small %>% filter(proposer == membernumber))

# Clustered standard errors by uniquesession
coeftest(mod_u24, vcov = vcovCL(mod_u24, cluster = ~uniquesession))
library(dplyr)
library(haven)
library(sandwich)
library(lmtest)

# Load the data
df <- read_dta("alldata.dta")

# Generate 'num' within treatment-session-group-round-bargaininground
df <- df %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = n()) %>%
  ungroup()

# Create smallpie and interaction variables
df <- df %>%
  mutate(
    smallpie = if_else(piesize == 24, 1, 0),
    interactionsmall = smallpie * unanimity
  )

# Filter for proposer==membernumber & (m48==1 | u48==1)
df_48 <- df %>%
  filter(proposer == membernumber & (m48 == 1 | u48 == 1))

# Probit regression: pass ~ smallpie + unanimity + interaction
mod_48 <- glm(pass ~ smallpie + unanimity + interactionsmall, 
              family = binomial(link = "probit"),
              data = df_48)

# Clustered standard errors by uniquesession
coeftest(mod_48, vcov = vcovCL(mod_48, cluster = ~uniquesession))

# Repeat for 96 treatment
df_96 <- df %>%
  filter(proposer == membernumber & (m96 == 1 | u96 == 1))

mod_96 <- glm(pass ~ smallpie + unanimity + interactionsmall,
              family = binomial(link = "probit"),
              data = df_96)

coeftest(mod_96, vcov = vcovCL(mod_96, cluster = ~uniquesession))

# ****************************************************
# Figure 1: piesize frequencies and probit on largepie
# ****************************************************

# Tabulate piesize by treatment when pass==1
df %>%
  filter(pass == 1) %>%
  count(treatment, piesize) %>%
  group_by(treatment) %>%
  mutate(prop = n / sum(n))

# Create largepie variable
df <- df %>%
  mutate(largepie = if_else(piesize != 24, 1, 0))

# Probit regressions for largepie
df_large_48 <- df %>% filter(proposer == membernumber & (m48 == 1 | u48 == 1) & pass == 1)
mod_large_48 <- glm(largepie ~ u48, family = binomial(link = "probit"), data = df_large_48)
coeftest(mod_large_48, vcov = vcovCL(mod_large_48, cluster = ~uniquesession))

df_large_96 <- df %>% filter(proposer == membernumber & (m96 == 1 | u96 == 1) & pass == 1)
mod_large_96 <- glm(largepie ~ u96, family = binomial(link = "probit"), data = df_large_96)
coeftest(mod_large_96, vcov = vcovCL(mod_large_96, cluster = ~uniquesession))
library(dplyr)
library(haven)
library(sandwich)
library(lmtest)

# Load the data
df <- read_dta("alldata.dta")

# Keep only passed proposals
df <- df %>% filter(pass == 1)

# Calculate low, high, and middle shares
df <- df %>%
  rowwise() %>%
  mutate(
    lowshare = min(c(share1, share2, share3), na.rm = TRUE),
    highshare = max(c(share1, share2, share3), na.rm = TRUE),
    middleshare = piesize - lowshare - highshare
  ) %>%
  ungroup()

# MWC / Equality indicators
df <- df %>%
  mutate(
    mwcequal = (highshare == 12 & middleshare == 12 & lowshare == 0) |
      (highshare == 24 & middleshare == 24 & lowshare == 0) |
      (highshare == 48 & middleshare == 48 & lowshare == 0),
    mwcunequal = (lowshare == 0 & mwcequal == FALSE),
    equal = (highshare == 8 & middleshare == 8 & lowshare == 8) |
      (highshare == 16 & middleshare == 16 & lowshare == 16) |
      (highshare == 32 & middleshare == 32 & lowshare == 32),
    grand = (lowshare != highshare & lowshare != 0),
    mwc = (lowshare == 0),
    all = (lowshare != 0)
  )

# Proposer shares
df <- df %>%
  mutate(
    propingroup = (membernumber == proposer),
    propshare = case_when(
      proposer == 1 ~ share1,
      proposer == 2 ~ share2,
      proposer == 3 ~ share3,
      TRUE ~ NA_real_
    )
  )

# -----------------------------
# Small budget (piesize == 24)
# -----------------------------
small_budget <- df %>% filter(piesize == 24)

# Column 1: mwc
table(small_budget$treatment, small_budget$mwc)

# Column 2: mwcequal among mwc==1
table(small_budget$treatment[small_budget$mwc == 1], 
      small_budget$mwcequal[small_budget$mwc == 1])

# Column 3: all
table(small_budget$treatment, small_budget$all)

# Column 4: equal among all==1
table(small_budget$treatment[small_budget$all == 1], 
      small_budget$equal[small_budget$all == 1])

# Column 5: proposer share by treatment
small_budget %>%
  filter(propingroup == TRUE) %>%
  group_by(treatment) %>%
  summarise(sum_propshare = sum(propshare, na.rm = TRUE))

# -----------------------------
# Large budget (piesize != 24)
# -----------------------------
large_budget <- df %>% filter(piesize != 24)

table(large_budget$treatment, large_budget$mwc)
table(large_budget$treatment[large_budget$mwc == 1], large_budget$mwcequal[large_budget$mwc == 1])
table(large_budget$treatment, large_budget$all)
table(large_budget$treatment[large_budget$all == 1], large_budget$equal[large_budget$all == 1])

large_budget %>%
  filter(propingroup == TRUE) %>%
  group_by(treatment) %>%
  summarise(sum_propshare = sum(propshare, na.rm = TRUE))

# -----------------------------
# Footnote 32: Probit regressions
# -----------------------------
# Equalreg = mwcequal OR equal
df <- df %>% mutate(equalreg = mwcequal | equal)

# Add num to replicate Stata's grouping
df <- df %>%
  group_by(sessioncode, group, round) %>%
  mutate(num = n()) %>%
  ungroup()

# Probit regressions by treatment and proposer
df_m24 <- df %>% filter(m24 == 1 & membernumber == proposer)
df_m48 <- df %>% filter(m48 == 1 & membernumber == proposer)
df_m96 <- df %>% filter(m96 == 1 & membernumber == proposer)

# Probit models with clustered SE
mod_m24 <- glm(equalreg ~ all, family = binomial(link = "probit"), data = df_m24)
coeftest(mod_m24, vcov = vcovCL(mod_m24, cluster = ~uniquesession))

mod_m48 <- glm(equalreg ~ all, family = binomial(link = "probit"), data = df_m48)
coeftest(mod_m48, vcov = vcovCL(mod_m48, cluster = ~uniquesession))

mod_m96 <- glm(equalreg ~ all, family = binomial(link = "probit"), data = df_m96)
coeftest(mod_m96, vcov = vcovCL(mod_m96, cluster = ~uniquesession))
library(dplyr)
library(ggplot2)
library(readr)

# Load your data
df <- read_dta("alldata.dta")

# ---------------------------------
# Panel A: ECDF of payoffs
# ---------------------------------

df_panelA <- df %>%
  mutate(
    payoff = ifelse(pass == 1, ownshare, NA_real_),
    payoff = ifelse(terminatedearly == 1, 0, payoff)
  ) %>%
  filter(!is.na(payoff))

# Compute ECDFs
df_panelA <- df_panelA %>%
  group_by(treatment) %>%
  arrange(treatment, payoff) %>%
  mutate(payoffcdf = cumsum(rep(1, n())) / n()) %>%
  ungroup()

# Plot Panel A
ggplot(df_panelA, aes(x = payoff, y = payoffcdf, color = treatment, linetype = treatment)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("black", "gray50", "black", "gray50")) +
  scale_linetype_manual(values = c("solid", "solid", "longdash", "longdash")) +
  labs(x = "Share", y = "ECDFs of Share", color = "Treatment", linetype = "Treatment") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "top")

# ---------------------------------
# Panel B: ECDF of Gini
# ---------------------------------

df_panelB <- df %>%
  filter(terminatedearly != 1) %>%
  group_by(treatment, session, round, group) %>%
  mutate(
    totalpayment = sum(ownshare),
    maxpayment = max(ownshare),
    minpayment = min(ownshare),
    middlepayment = totalpayment - maxpayment - minpayment,
    gini = (4 - 2 * (3*minpayment + 2*middlepayment + maxpayment) / totalpayment) / 3
  ) %>%
  ungroup()

# Compute ECDFs for Gini by treatment
df_panelB <- df_panelB %>%
  group_by(treatment) %>%
  arrange(treatment, gini) %>%
  mutate(ginicdf = cumsum(rep(1, n())) / n()) %>%
  ungroup()

# Plot Panel B
ggplot(df_panelB, aes(x = gini, y = ginicdf, color = treatment, linetype = treatment)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("black", "gray50", "black", "gray50")) +
  scale_linetype_manual(values = c("solid", "solid", "longdash", "longdash")) +
  labs(x = "GINI", y = "ECDFs of GINI", color = "Treatment", linetype = "Treatment") +
  theme_minimal(base_size = 14) +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "top")
# Load required packages
library(tidyverse)
library(haven)       # to read .dta if needed
library(margins)     # for marginal effects
library(broom)       # for tidy model outputs

# -----------------------------
# 1. Load the data
# -----------------------------
# If you have CSV:
data <- read_csv("alldata_withchatcoded_ready.csv")

# OR, if you want to read Stata directly:
# data <- read_dta("alldata_withchatcoded_ready.dta")

# -----------------------------
# 2. Prepare the data
# -----------------------------
data <- data %>%
  arrange(treatment, session, group, round, bargaininground) %>%
  group_by(treatment, session, group, round) %>%
  mutate(num = n()) %>%
  ungroup()

# -----------------------------
# 3. Code disagreement as "yes"
# -----------------------------
data <- data %>%
  mutate(
    riskterminationtalk      = if_else(agreev3 == 0, 1, riskterminationtalk),
    equalitytalk             = if_else(agreev4 == 0, 1, equalitytalk),
    equalityinmwctalk        = if_else(agreev10 == 0, 1, equalityinmwctalk),
    yestodelaytalk           = if_else(agreev8 == 0, 1, yestodelaytalk),
    bigpietalk               = if_else(agreev2 == 0, 1, bigpietalk),
    threatnounequaltalk      = if_else(agreev5 == 0, 1, threatnounequaltalk),
    threatnosmallbudgettalk  = if_else(agreev6 == 0, 1, threatnosmallbudgettalk)
  )

# -----------------------------
# 4. Table 3: Topic Frequency by Treatment
# -----------------------------
# Function to get frequency and probit for each topic
probit_topic <- function(topic_var, treatment_var){
  
  # Filter only one row per group per round
  df <- data %>% filter(num == 1, round < 6.5)
  
  # Frequency table
  freq <- df %>%
    group_by(treatment) %>%
    summarize(n_yes = sum(.data[[topic_var]] == 1, na.rm = TRUE),
              n_total = n(),
              prop_yes = n_yes / n_total)
  
  print(freq)
  
  # Probit regression by treatment
  treatments <- unique(df[[treatment_var]])
  models <- list()
  
  for (t in treatments){
    mod <- glm(
      formula = as.formula(paste(topic_var, "~", t)),
      data = df,
      family = binomial(link = "probit")
    )
    models[[t]] <- mod
    print(tidy(mod))
    # Marginal effects
    me <- margins(mod)
    print(summary(me))
  }
  
  return(models)
}

# Example: Big Pie Talk
models_bigpie <- probit_topic("bigpietalk", "m48")  # Stata uses m48, m96, u48, u96 indicators

# Repeat for other topics: yestodelaytalk, equalitytalk, etc.

# -----------------------------
# 5. Table 4: Effect of Topic on Delay (Round 1)
# -----------------------------
# Filter only first bargaining round & groups that talked
df_round1 <- data %>%
  filter(bargaininground == 1, num == 1, round < 6.5, didnottalk == 0)

# Function for treatment-specific probit with marginal effects
probit_delay <- function(df, treatment_indicator){
  mod <- glm(
    delay ~ riskterminationtalk + equalitytalk + equalityinmwctalk + yestodelaytalk + bigpietalk,
    data = df %>% filter(.data[[treatment_indicator]] == 1),
    family = binomial(link = "probit")
  )
  print(summary(mod))
  
  me <- margins(mod)
  print(summary(me))
  
  return(list(model = mod, margins = me))
}

# Run for each treatment indicator
results_M48 <- probit_delay(df_round1, "m48")
results_U48 <- probit_delay(df_round1, "u48")
results_M96 <- probit_delay(df_round1, "m96")
results_U96 <- probit_delay(df_round1, "u96")
# Load libraries
library(tidyverse)
library(haven)       # read Stata files
library(fixest)      # regressions with cluster robust SEs
library(quantreg)    # quantile regressions
library(margins)     # marginal effects for probit/logit

# --------------------------------
# Load data
# --------------------------------
data <- read_dta("alldata.dta")

# --------------------------------
# Table 5 & Table 6: Investment behavior
# --------------------------------
# Keep first observation per player
data_invest <- data %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

# Task 1: playerinvested25
# OLS and Quantile regressions
invest_tasks <- list("playerinvested25", "playerinvested30")
treatments <- c("m24","u24","m48","u48","m96","u96")

for(task in invest_tasks){
  for(trt in treatments){
    df <- data_invest %>% filter(.data[[trt]] == 1)
    
    # OLS with clustered SEs
    ols_model <- feols(as.formula(paste(task,"~",trt)), data=df, cluster="uniquesession")
    print(summary(ols_model))
    
    # Quantile regression (median)
    qr_model <- rq(as.formula(paste(task,"~",trt)), data=df, tau=0.5)
    print(summary(qr_model))
  }
}

# --------------------------------
# Table 7: Small budget delays (first bargaining round)
# --------------------------------
data_table7 <- data %>%
  filter(bargaininground==1) %>%
  mutate(totaldelay = if_else(delay==1 | pass==0, 1, 0)) %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = n()) %>%
  ungroup()

# Probit models for u48 and u96
model_u48 <- feglm(totaldelay ~ u48, data = data_table7 %>% filter(piesize==24, proposer==membernumber),
                   family = binomial("probit"), cluster = ~uniquesession)
summary(model_u48)

model_u96 <- feglm(totaldelay ~ u96, data = data_table7 %>% filter(piesize==24, proposer==membernumber),
                   family = binomial("probit"), cluster = ~uniquesession)
summary(model_u96)

# --------------------------------
# Table 8: Total committee earnings
# --------------------------------
data_table8 <- data %>%
  mutate(votingrule = votingtreatment=="unanimity",
         payout = if_else(pass==1, share1+share2+share3, 0)) %>%
  group_by(uniquesession, memberid) %>%
  mutate(lastround = max(bargaininground)) %>%
  ungroup() %>%
  filter(bargaininground==lastround) %>%
  group_by(treatment, uniquesession, group, round) %>%
  mutate(num = n()) %>%
  ungroup()

# Clustered regressions by treatment
for(trt in treatments){
  df <- data_table8 %>% filter(.data[[trt]]==1, num==1)
  reg_model <- feols(payout ~ .data[[trt]], data=df, cluster="uniquesession")
  print(summary(reg_model))
}

# --------------------------------
# Figure 13: Individual-level delays
# --------------------------------
data_delays <- data %>%
  mutate(wasproposer = (proposer==membernumber)) %>%
  filter(wasproposer==1) %>%
  group_by(uniquesession, memberid) %>%
  mutate(nbdelays = sum(delay, na.rm=TRUE),
         nbtimeprop = n(),
         fracdelay = nbdelays / nbtimeprop,
         num = row_number()) %>%
  filter(num==1) %>%
  ungroup()

# Histogram example: U48 vs M48
data_delays %>%
  filter(u48==1) %>%
  ggplot(aes(x=fracdelay)) +
  geom_histogram(aes(y=..density..), fill="green", alpha=0.5, binwidth=0.05) +
  geom_histogram(data=data_delays %>% filter(m48==1), aes(x=fracdelay, y=..density..),
                 fill=NA, color="black", binwidth=0.05) +
  labs(x="Fraction of Delays", y="Density") +
  theme_minimal()

# Regression on fraction of delays
reg_m96 <- feols(fracdelay ~ m96, data=data_delays %>% filter(m96==1 | u96==1))
summary(reg_m96)

reg_m48 <- feols(fracdelay ~ m48, data=data_delays %>% filter(m48==1 | u48==1))
summary(reg_m48)

# --------------------------------
# Tables 9 & 10: Pass/No pass by half rounds
# --------------------------------
data_small <- data %>%
  filter(piesize==24, proposer==membernumber) %>%
  mutate(nopass = if_else(delay==1 | pass==0, 1, 0))

# First half
data_first <- data_small %>% filter(round < 6.5)
# Second half
data_second <- data_small %>% filter(round > 6.5)

# Probit example
model_first_m48 <- feglm(nopass ~ m48, data=data_first %>% filter(m48==1 | u48==1),
                         family=binomial("probit"), cluster=~uniquesession)
summary(model_first_m48)

# --------------------------------
# Tables 11 & 12: Proposal types
# --------------------------------
data_proposals <- data_small %>%
  mutate(lowshare = pmin(share1, share2, share3),
         highshare = pmax(share1, share2, share3),
         middleshare = piesize - lowshare - highshare,
         mwcequal = (highshare==12 & middleshare==12 & lowshare==0),
         mwcunequal = (lowshare==0 & !mwcequal),
         equal = (highshare==8 & middleshare==8 & lowshare==8),
         grand = (lowshare<8 & lowshare!=0),
         proposaltype = case_when(
           mwcequal ~ 1,
           mwcunequal ~ 2,
           equal ~ 3,
           grand ~ 4
         ))

# Table 11
data_proposals %>%
  filter(proposer==membernumber) %>%
  count(proposaltype, treatment) %>%
  group_by(treatment) %>%
  mutate(prop = n/sum(n))

# Table 12
data_proposals %>%
  filter(proposer==membernumber) %>%
  count(proposaltype, pass, treatment) %>%
  group_by(treatment) %>%
  mutate(prop = n/sum(n))

# --------------------------------
# Table 13: Chat topics first bargaining round
# --------------------------------
data_chat <- read_dta("alldata_withchatcoded_ready.dta") %>%
  arrange(treatment, session, group, round, bargaininground) %>%
  group_by(treatment, session, group, round) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  mutate(
    riskterminationtalk      = if_else(agreev3==0, 1, riskterminationtalk),
    equalitytalk             = if_else(agreev4==0, 1, equalitytalk),
    equalityinmwctalk        = if_else(agreev10==0, 1, equalityinmwctalk),
    yestodelaytalk           = if_else(agreev8==0, 1, yestodelaytalk),
    bigpietalk               = if_else(agreev2==0, 1, bigpietalk)
  ) %>%
  filter(bargaininground==1, num==1)

# Probit by treatment pairings
model_m48_m96 <- feglm(bigpietalk ~ m48, data=data_chat %>% filter(m48==1 | m96==1),
                       family=binomial("probit"), cluster=~uniquesession)
summary(model_m48_m96)
print(summary(model_m48_m96))
# Load libraries
library(tidyverse)
library(haven)       # read Stata .dta files
library(fixest)      # regressions with cluster robust SEs
library(quantreg)    # quantile regressions
library(knitr)       # for printing tables nicely

# --------------------------------
# Load data
# --------------------------------
data <- read_dta("alldata.dta")

# --------------------------------
# Table 5 & 6: Investment behavior
# --------------------------------
data_invest <- data %>% group_by(id) %>% slice(1) %>% ungroup()
invest_tasks <- list("playerinvested25", "playerinvested30")
treatments <- c("m24","u24","m48","u48","m96","u96")

for(task in invest_tasks){
  cat("\n===", task, "===\n")
  for(trt in treatments){
    df <- data_invest %>% filter(.data[[trt]] == 1)
    cat("\nTreatment:", trt, "\n")
    
    # OLS
    ols_model <- feols(as.formula(paste(task,"~",trt)), data=df, cluster="uniquesession")
    print(summary(ols_model))
    
    # Quantile regression (median)
    qr_model <- rq(as.formula(paste(task,"~",trt)), data=df, tau=0.5)
    print(summary(qr_model))
  }
}

# --------------------------------
# Table 7: Small budget delays (first bargaining round)
# --------------------------------
data_table7 <- data %>%
  filter(bargaininground==1) %>%
  mutate(totaldelay = if_else(delay==1 | pass==0, 1, 0)) %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = n()) %>%
  ungroup()

cat("\n--- Table 7 ---\n")
model_u48 <- feglm(totaldelay ~ u48, data = data_table7 %>% filter(piesize==24, proposer==membernumber),
                   family = binomial("probit"), cluster = ~uniquesession)
print(summary(model_u48))

model_u96 <- feglm(totaldelay ~ u96, data = data_table7 %>% filter(piesize==24, proposer==membernumber),
                   family = binomial("probit"), cluster = ~uniquesession)
print(summary(model_u96))

# --------------------------------
# Table 8: Total committee earnings
# --------------------------------
data_table8 <- data %>%
  mutate(votingrule = votingtreatment=="unanimity",
         payout = if_else(pass==1, share1+share2+share3, 0)) %>%
  group_by(uniquesession, memberid) %>%
  mutate(lastround = max(bargaininground)) %>%
  ungroup() %>%
  filter(bargaininground==lastround) %>%
  group_by(treatment, uniquesession, group, round) %>%
  mutate(num = n()) %>%
  ungroup()

cat("\n--- Table 8 ---\n")
for(trt in treatments){
  df <- data_table8 %>% filter(.data[[trt]]==1, num==1)
  reg_model <- feols(payout ~ .data[[trt]], data=df, cluster="uniquesession")
  cat("\nTreatment:", trt, "\n")
  print(summary(reg_model))
}

# --------------------------------
# Table 9 & 10: Pass/No pass by half rounds
# --------------------------------
data_small <- data %>%
  filter(piesize==24, proposer==membernumber) %>%
  mutate(nopass = if_else(delay==1 | pass==0, 1, 0))

data_first <- data_small %>% filter(round < 6.5)
data_second <- data_small %>% filter(round > 6.5)

cat("\n--- Table 9 & 10 ---\n")
model_first_m48 <- feglm(nopass ~ m48, data=data_first %>% filter(m48==1 | u48==1),
                         family=binomial("probit"), cluster=~uniquesession)
print(summary(model_first_m48))

# --------------------------------
# Tables 11 & 12: Proposal types
# --------------------------------
data_proposals <- data_small %>%
  mutate(lowshare = pmin(share1, share2, share3),
         highshare = pmax(share1, share2, share3),
         middleshare = piesize - lowshare - highshare,
         mwcequal = (highshare==12 & middleshare==12 & lowshare==0),
         mwcunequal = (lowshare==0 & !mwcequal),
         equal = (highshare==8 & middleshare==8 & lowshare==8),
         grand = (lowshare<8 & lowshare!=0),
         proposaltype = case_when(
           mwcequal ~ 1,
           mwcunequal ~ 2,
           equal ~ 3,
           grand ~ 4
         ))

cat("\n--- Table 11: Proposal Type by Treatment ---\n")
tab11 <- data_proposals %>% filter(proposer==membernumber) %>% count(proposaltype, treatment) %>%
  group_by(treatment) %>% mutate(prop = n/sum(n))
print(kable(tab11))

cat("\n--- Table 12: Proposal Type vs Pass by Treatment ---\n")
tab12 <- data_proposals %>% filter(proposer==membernumber) %>% count(proposaltype, pass, treatment) %>%
  group_by(treatment) %>% mutate(prop = n/sum(n))
print(kable(tab12))

# --------------------------------
# Table 13: Chat topics first bargaining round
# --------------------------------
data_chat <- read_dta("alldata_withchatcoded_ready.dta") %>%
  arrange(treatment, session, group, round, bargaininground) %>%
  group_by(treatment, session, group, round) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  mutate(
    riskterminationtalk      = if_else(agreev3==0, 1, riskterminationtalk),
    equalitytalk             = if_else(agreev4==0, 1, equalitytalk),
    equalityinmwctalk        = if_else(agreev10==0, 1, equalityinmwctalk),
    yestodelaytalk           = if_else(agreev8==0, 1, yestodelaytalk),
    bigpietalk               = if_else(agreev2==0, 1, bigpietalk)
  ) %>%
  filter(bargaininground==1, num==1)

cat("\n--- Table 13: Chat Topics ---\n")
model_m48_m96 <- feglm(bigpietalk ~ m48, data=data_chat %>% filter(m48==1 | m96==1),
                       family=binomial("probit"), cluster=~uniquesession)
print(summary(model_m48_m96))
