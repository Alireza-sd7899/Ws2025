# -------------------------------
# Load required packages
# -------------------------------
# install.packages(c("haven", "dplyr", "janitor", "sandwich", "lmtest", "modelsummary"))
library(haven)
library(dplyr)
library(janitor)
library(sandwich)
library(lmtest)
library(modelsummary)

# -------------------------------
# Load dataset
# -------------------------------
alldata <- read_dta("E:/My education/UNI/edu uni/Semester 2/Data science for Business/alldata.dta")

# -------------------------------
# Prepare variables
# -------------------------------
data <- alldata %>%
  mutate(totaldelay = as.numeric(delay == 1 | pass == 0)) %>%
  group_by(treatment, session, group, round, bargaininground) %>%
  mutate(num = row_number()) %>%
  ungroup()

# -------------------------------
# Subsets
# -------------------------------
# Large budget: piesize != 24
large_budget <- data %>% filter(num == 1, piesize != 24)
# Small budget: piesize == 24
small_budget <- data %>% filter(num == 1, piesize == 24)

# -------------------------------
# Helper function to run probit with clustered SE
# -------------------------------
run_probit <- function(df, y, x, cluster_var) {
  formula <- as.formula(paste(y, "~", x))
  model <- glm(formula, data = df, family = binomial(link = "probit"))
  # Compute cluster-robust SEs
  cov <- vcovCL(model, cluster = df[[cluster_var]])
  list(model = model, vcov = cov)
}

# -------------------------------
# Run models
# -------------------------------
# Large budget
res_large_u48 <- run_probit(large_budget %>% filter(m48 == 1 | u48 == 1),
                            "totaldelay", "u48", "uniquesession")
res_large_u96 <- run_probit(large_budget %>% filter(m96 == 1 | u96 == 1),
                            "totaldelay", "u96", "uniquesession")

# Small budget
res_small_u48 <- run_probit(small_budget %>% filter(m48 == 1 | u48 == 1),
                            "totaldelay", "u48", "uniquesession")
res_small_u96 <- run_probit(small_budget %>% filter(m96 == 1 | u96 == 1),
                            "totaldelay", "u96", "uniquesession")

# -------------------------------
# Export table using modelsummary
# -------------------------------
# List of models
models <- list(
  "Large Budget u48" = res_large_u48$model,
  "Large Budget u96" = res_large_u96$model,
  "Small Budget u48" = res_small_u48$model,
  "Small Budget u96" = res_small_u96$model
)

# Corresponding cluster-robust SEs
vcovs <- list(
  res_large_u48$vcov,
  res_large_u96$vcov,
  res_small_u48$vcov,
  res_small_u96$vcov
)

# Generate HTML or Word table
modelsummary(models,
             vcov = vcovs,
             output = "probit_results.html")  # or "probit_results.docx"

