# Load the readxl package (install if needed)
install.packages("readxl")  # Run once if not installed
library(readxl)

# Import the Excel file, using the first row as column names
data <- read_excel("E:/My education/UNI/edu uni/Semester 2/Data science for Business/data.2019.04.12.xlsx")


# Load necessary package
install.packages("dplyr")  # Run once if not installed
library(dplyr)

# Assuming your dataset is called 'data'
data <- data %>%
  # Create 'id' as a grouped identifier like egen group()
  mutate(id = as.numeric(factor(paste(sessioncode, memberid, sep = "_"))),
         
         # Generate treatment indicators
         m24 = (votingtreatment == "majority" & pietreatment == 24),
         m48 = (votingtreatment == "majority" & pietreatment == 48),
         m96 = (votingtreatment == "majority" & pietreatment == 96),
         
         u24 = (votingtreatment == "unanimity" & pietreatment == 24),
         u48 = (votingtreatment == "unanimity" & pietreatment == 48),
         u96 = (votingtreatment == "unanimity" & pietreatment == 96))
# Load packages
library(dplyr)
library(haven)   # for saving .dta files if you need to

# Assuming your dataset is called 'data'
data <- data %>%
  # Total yes votes
  mutate(
    totalyes = vote1 + vote2 + vote3,
    
    # Pass condition
    pass = case_when(
      is.na(totalyes) ~ NA,
      (totalyes == 3 & votingtreatment == "unanimity") |
        (totalyes >= 2 & votingtreatment == "majority") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Treatment variable
    treatment = case_when(
      m24 == 1 ~ "m24",
      m48 == 1 ~ "m48",
      m96 == 1 ~ "m96",
      u24 == 1 ~ "u24",
      u48 == 1 ~ "u48",
      u96 == 1 ~ "u96",
      TRUE ~ ""
    ),
    
    # Unanimity dummy
    unanimity = (votingtreatment == "unanimity"),
    
    # Session assignment
    session = case_when(
      sessioncode %in% c("50catrnf","7f7v4l6r","js437fzd","m1","m5","q32zfvnq") ~ 1,
      sessioncode %in% c("9gzbr7vg","ax2x1ab5","ifm6r51c","krgxtmp1","m2","m6") ~ 2,
      sessioncode %in% c("6nf5jbi5","gx0695sj","jvzb19el","m3","p66q0jhd","w2npj4ao") ~ 3,
      sessioncode %in% c("9fuzcnjg","ixhiq3bn","job0mwmb","jwlbjm39","m4","wfwdfk01") ~ 4,
      TRUE ~ NA_real_
    )
  )

# Create unique session IDs
data <- data %>%
  mutate(
    uniquesession = case_when(
      votingtreatment == "majority" & pietreatment == 24 ~ as.numeric(paste0("240", session)),
      votingtreatment == "majority" & pietreatment == 48 ~ as.numeric(paste0("480", session)),
      votingtreatment == "majority" & pietreatment == 96 ~ as.numeric(paste0("960", session)),
      votingtreatment == "unanimity" & pietreatment == 24 ~ as.numeric(paste0("241", session)),
      votingtreatment == "unanimity" & pietreatment == 48 ~ as.numeric(paste0("481", session)),
      votingtreatment == "unanimity" & pietreatment == 96 ~ as.numeric(paste0("961", session)),
      TRUE ~ NA_real_
    )
  )

# Save as Stata .dta file
write_dta(data, "E:/My education/UNI/edu uni/Semester 2/Data science for Business/Bargaining.dta")
library(readxl)
library(dplyr)
library(haven)
library(janitor)
library(stringr)

# Define paths
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
excel_file <- file.path(base_path, "risk.xlsx")
output_file <- file.path(base_path, "risk.dta")

# Function to import one treatment group (e.g., U1_s1–U1_s4)
import_group <- function(prefix, treatment, unanimity, pie = NA, n_sessions = 4) {
  bind_rows(lapply(1:n_sessions, function(i) {
    sheet_name <- paste0(prefix, "_s", i)
    message("Importing sheet: ", sheet_name)
    
    df <- read_excel(excel_file, sheet = sheet_name) %>%
      clean_names() %>%
      mutate(session = i,
             treatment = treatment,
             unanimity = unanimity,
             pie = pie)
    return(df)
  }))
}

# ---- Import all treatment groups ----
risk_data <- bind_rows(
  import_group("U1", "u24", unanimity = 1, pie = 24),
  import_group("U2", "u48", unanimity = 1, pie = 48),
  import_group("U4", "u96", unanimity = 1, pie = 96),
  import_group("M1", "m24", unanimity = 0, pie = 24),
  import_group("M2", "m48", unanimity = 0, pie = 48),
  import_group("M4", "m96", unanimity = 0, pie = 96)
)

# ---- Fix long variable names for Stata ----
names(risk_data) <- names(risk_data) %>%
  str_sub(1, 32)  # shorten to first 32 characters (Stata’s max limit)

# ---- Clean up: drop rows with missing session_code ----
if ("session_code" %in% names(risk_data)) {
  risk_data <- risk_data %>%
    filter(!(is.na(session_code) | session_code == ""))
}

# ---- Save final combined dataset ----
write_dta(risk_data, output_file)
message("✅ Combined risk data saved to: ", output_file)
library(haven)

# Load the Stata dataset
risk_data <- read_dta("E:/My education/UNI/edu uni/Semester 2/Data science for Business/risk.dta")

# View the first few rows
head(risk_data)

# Optional: open in RStudio’s data viewer
View(risk_data)
library(dplyr)
library(tidyr)
library(haven)

# Load the risk dataset
risk_data <- read_dta("E:/My education/UNI/edu uni/Semester 2/Data science for Business/risk.dta")

# --- Rename columns to match Stata logic ---
risk_data <- risk_data %>%
  rename(
    participantcoderisk = participant_code,        # correct column name
    memberid = participant_id_in_session,         # correct column name
    playerinvested = player_invested              # correct column name
  )

# --- Create riskmultiplier ---
risk_data <- risk_data %>%
  mutate(
    riskmultiplier = case_when(
      subsession_investment_multiplier == 3 ~ 2,
      subsession_investment_multiplier == 2.5 ~ 1,
      TRUE ~ NA_real_
    )
  )

# --- Create unique ID like Stata's egen group() ---
risk_data <- risk_data %>%
  mutate(id = as.numeric(factor(paste(unanimity, pie, session, memberid, sep = "_"))))

# --- Keep only relevant columns ---
risk_data <- risk_data %>%
  select(participantcoderisk, memberid, session_code, riskmultiplier,
         playerinvested, id, session, unanimity)

# --- Reshape wide: playerinvested by riskmultiplier ---
risk_data_wide <- risk_data %>%
  pivot_wider(
    names_from = riskmultiplier,
    values_from = playerinvested,
    names_prefix = "playerinvested"
  ) %>%
  rename(
    playerinvested25 = playerinvested1,
    playerinvested30 = playerinvested2
  ) %>%
  select(-id) %>%
  arrange(session_code, memberid)

# --- Save as Stata .dta file ---
write_dta(risk_data_wide, "E:/My education/UNI/edu uni/Semester 2/Data science for Business/riskformatted.dta")
library(dplyr)
library(haven)

# Load datasets
bargaining <- read_dta("E:/My education/UNI/edu uni/Semester 2/Data science for Business/Bargaining.dta")
risk <- read_dta("E:/My education/UNI/edu uni/Semester 2/Data science for Business/riskformatted.dta")

risk <- risk %>% select(-session)  # remove the existing session column
risk <- risk %>% rename(session = session_code)

merged_data <- full_join(bargaining, risk,
                         by = c("session" = "session_risk", "memberid"),
                         suffix = c("_barg", "_risk"))

library(dplyr)
library(haven)

# Set working directory
setwd("E:/My education/UNI/edu uni/Semester 2/Data science for Business")

# ---- Load datasets ----
bargaining <- read_dta("Bargaining.dta")
risk <- read_dta("riskformatted.dta")

# ---- Step 1: Clean risk dataset ----
# Drop duplicate 'session' column
risk <- risk %>% select(-session)

# Rename 'session_code' to 'session'
risk <- risk %>% rename(session = session_code)

# Ensure session columns are character for merging
bargaining <- bargaining %>% mutate(session = as.character(session))
risk <- risk %>% mutate(session = as.character(session))

# ---- Step 2: Merge datasets ----
merged_data <- full_join(bargaining, risk,
                         by = c("session", "memberid"),
                         suffix = c("_barg", "_risk"))

# ---- Step 3: Save merged dataset ----
write_dta(merged_data, "alldata.dta")

message("✅ Merge complete. Saved as alldata.dta in working directory.")

