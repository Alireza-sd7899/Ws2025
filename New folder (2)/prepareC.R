# ============================================================
# Convert Stata looping code to R
# Works with: majorityChats.xlsx
# Location: E:/My education/UNI/edu uni/Semester 2/Data science for Business
# ============================================================

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(haven)
library(stringr)

# Define base path
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
input_file <- file.path(base_path, "majorityChats.xlsx")
output_path <- file.path(base_path, "Processed_Files")

# Create output directory if it doesn’t exist
if (!dir.exists(output_path)) dir.create(output_path)

# Loop through sessions j = 2, 4 and k = 1, 2, 3, 4
for (j in seq(2, 4, 2)) {
  for (k in 1:4) {
    
    sheet_name <- paste0("M", j, "_s", k)
    session_label <- paste0("m", j, "s", k)
    message("Processing sheet: ", sheet_name)
    
    # -----------------------------------------------
    # 1. Import data
    # -----------------------------------------------
    df <- read_excel(input_file, sheet = sheet_name)
    
    # -----------------------------------------------
    # 2. Drop unused columns (use your actual column names)
    # -----------------------------------------------
    df <- df %>%
      select(
        -participant.id_in_session,
        -participant.code,
        -participant.label,
        -session.code,
        -group.bigSize,
        -subsession.round_number
      )
    
    # -----------------------------------------------
    # 3. Rename for easier handling
    # -----------------------------------------------
    df <- df %>%
      rename(
        group = group.group_identifier,
        playerid_in_group = player.id_in_group,
        playerm_v11 = player.m_v11
      )
    
    # -----------------------------------------------
    # 4. Sort by group and player ID
    # -----------------------------------------------
    df <- df %>%
      arrange(group, playerid_in_group)
    
    # -----------------------------------------------
    # 5. Reshape from long to wide
    # -----------------------------------------------
    df_wide <- df %>%
      pivot_wider(
        id_cols = group,
        names_from = playerid_in_group,
        values_from = starts_with("player.m_v")
      )
    
    # -----------------------------------------------
    # 6. Clean up column names (replace "." with "_")
    # -----------------------------------------------
    names(df_wide) <- gsub("\\.", "_", names(df_wide))
    
    # -----------------------------------------------
    # 7. Convert specific variables to 0/1
    # -----------------------------------------------
    for (i in c(2, 4, 9, 10, 13)) {
      var1 <- paste0("player_m_v", i, "_1")
      var2 <- paste0("player_m_v", i, "_2")
      
      if (var1 %in% names(df_wide)) {
        df_wide[[var1]] <- ifelse(df_wide[[var1]] != "0", 1, 0)
      }
      if (var2 %in% names(df_wide)) {
        df_wide[[var2]] <- ifelse(df_wide[[var2]] != "0", 1, 0)
      }
    }
    
    # -----------------------------------------------
    # 8. Create agree variables (coder agreement)
    # -----------------------------------------------
    for (i in 1:10) {
      v1 <- paste0("player_m_v", i, "_1")
      v2 <- paste0("player_m_v", i, "_2")
      agree_name <- paste0("agreev", i)
      if (v1 %in% names(df_wide) && v2 %in% names(df_wide)) {
        df_wide[[agree_name]] <- as.numeric(df_wide[[v1]] == df_wide[[v2]])
      }
    }
    
    # Special case for question 11
    if (all(c("player_m_v11_1", "player_m_v11_2") %in% names(df_wide))) {
      
      df_wide$session <- session_label
      
      # -----------------------------------------------
      # 9. Save full dataset (Kappa version)
      # -----------------------------------------------
      save_dta_path1 <- file.path(output_path, paste0("Kappa_", session_label, ".dta"))
      write_dta(df_wide, save_dta_path1)
      
      # -----------------------------------------------
      # 10. Keep only selected variables for smaller dataset
      # -----------------------------------------------
      keep_vars <- c("group", paste0("player_m_v", 1:10, "_1"), "player_m_v11_1", paste0("agreev", 1:11))
      df_min <- df_wide %>%
        select(any_of(keep_vars)) %>%
        rename_with(~ str_replace(.x, "player_m_v(\\d+)_1", "valueq\\1")) %>%
        arrange(group)
      
      # -----------------------------------------------
      # 11. Save smaller dataset
      # -----------------------------------------------
      save_dta_path2 <- file.path(output_path, paste0(session_label, ".dta"))
      write_dta(df_min, save_dta_path2)
      
      message("✅ Saved: ", save_dta_path2)
    }
  }
  
  message("🎉 All sheets processed successfully!")
  # ============================================================
  # Combine session Kappa files and compute Cohen's Kappa per session
  # ============================================================
  
  library(haven)
  library(dplyr)
  library(stringr)
  library(irr)
  
  # Path setup
  base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business/Processed_Files"
  output_file <- file.path(base_path, "KappaMajority.dta")
  
  # ------------------------------------------------------------
  # 1. Load and append all 8 session files
  # ------------------------------------------------------------
  sessions <- c(
    "m2s1", "m2s2", "m2s3", "m2s4",
    "m4s1", "m4s2", "m4s3", "m4s4"
  )
  
  all_data <- lapply(sessions, function(sess) {
    read_dta(file.path(base_path, paste0("Kappa_", sess, ".dta")))
  })
  
  df_all <- bind_rows(all_data)
  
  # ------------------------------------------------------------
  # 2. Compute Cohen’s Kappa for each question in each session
  # ------------------------------------------------------------
  # Function to safely compute Kappa
  compute_kappa <- function(var1, var2, data) {
    if (all(is.na(data[[var1]])) || all(is.na(data[[var2]]))) return(NA)
    if (length(unique(data[[var1]])) < 2 || length(unique(data[[var2]])) < 2) return(NA)
    tryCatch({
      kappa2(data.frame(data[[var1]], data[[var2]]))$value
    }, error = function(e) NA)
  }
  
  # Initialize empty list for results
  kappa_results <- list()
  
  # Loop over each session
  for (sess in sessions) {
    dsub <- df_all %>% filter(session == sess)
    message("Processing ", sess)
    
    for (i in 1:10) {
      var1 <- paste0("player_m_v", i, "_1")
      var2 <- paste0("player_m_v", i, "_2")
      if (all(c(var1, var2) %in% names(dsub))) {
        kappa_value <- compute_kappa(var1, var2, dsub)
        newcol <- paste0("kap", i, sess)
        dsub[[newcol]] <- kappa_value
      }
    }
    
    # Question 11 (from player_m_v11)
    if (all(c("player_m_v11_1", "player_m_v11_2") %in% names(dsub))) {
      kappa_value <- compute_kappa("player_m_v11_1", "player_m_v11_2", dsub)
      dsub[[paste0("kap11", sess)]] <- kappa_value
    }
    
    kappa_results[[sess]] <- dsub
  }
  
  # ------------------------------------------------------------
  # 3. Combine all results
  # ------------------------------------------------------------
  final_df <- bind_rows(kappa_results)
  
  # ------------------------------------------------------------
  # 4. Save combined dataset
  # ------------------------------------------------------------
  write_dta(final_df, output_file)
  
  message("🎉 KappaMajority.dta saved successfully at:\n", output_file)
  # ============================================================
  # Process "unanimityChats.xlsx" into individual and combined datasets
  # ============================================================
  
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(haven)
  library(stringr)
  
  # ------------------------------------------------------------
  # 1. Paths
  # ------------------------------------------------------------
  base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
  input_file <- file.path(base_path, "unanimityChats.xlsx")
  output_path <- file.path(base_path, "Processed_Files")
  
  if (!dir.exists(output_path)) dir.create(output_path)
  
  # ------------------------------------------------------------
  # 2. Helper function to process each sheet
  # ------------------------------------------------------------
  process_unanimity_sheet <- function(j, k) {
    sheet_name <- paste0("U", j, "_s", k)
    session_label <- paste0("u", j, "s", k)
    message("Processing ", sheet_name)
    
    df <- read_excel(input_file, sheet = sheet_name)
    
    # Drop unwanted columns (based on your Excel names)
    df <- df %>%
      select(
        -participant.id_in_session,
        -participant.code,
        -participant.label,
        -session.code,
        -group.bigSize,
        -subsession.round_number
      )
    
    # Rename columns
    df <- df %>%
      rename(
        group = group.group_identifier,
        playerid_in_group = player.id_in_group
      )
    
    # Drop missing groups
    df <- df %>% filter(!is.na(group))
    
    # Sort
    df <- df %>% arrange(group, playerid_in_group)
    
    # Reshape wide
    df_wide <- df %>%
      pivot_wider(
        id_cols = group,
        names_from = playerid_in_group,
        values_from = starts_with("player.u_v")
      )
    
    # Clean up names
    names(df_wide) <- gsub("\\.", "_", names(df_wide))
    
    # Convert nonzero entries to 1 for q2 and q4
    for (i in c(2, 4)) {
      var1 <- paste0("player_u_v", i, "_1")
      var2 <- paste0("player_u_v", i, "_2")
      
      if (var1 %in% names(df_wide)) {
        df_wide[[var1]] <- ifelse(df_wide[[var1]] != "0", 1, 0)
      }
      if (var2 %in% names(df_wide)) {
        df_wide[[var2]] <- ifelse(df_wide[[var2]] != "0", 1, 0)
      }
    }
    
    # Add session label
    df_wide$session <- session_label
    
    # Create agreement variables
    for (i in 1:8) {
      v1 <- paste0("player_u_v", i, "_1")
      v2 <- paste0("player_u_v", i, "_2")
      agree_name <- paste0("agreev", i)
      if (all(c(v1, v2) %in% names(df_wide))) {
        df_wide[[agree_name]] <- as.numeric(df_wide[[v1]] == df_wide[[v2]])
      }
    }
    
    # Save Kappa version
    write_dta(df_wide, file.path(output_path, paste0("Kappa_", session_label, ".dta")))
    
    # Create reduced dataset
    keep_vars <- c("group", paste0("player_u_v", 1:8, "_1"), paste0("agreev", 1:8))
    df_min <- df_wide %>%
      select(any_of(keep_vars)) %>%
      rename_with(~ str_replace(.x, "player_u_v(\\d+)_1", "valueq\\1")) %>%
      arrange(group)
    
    # Save reduced version
    write_dta(df_min, file.path(output_path, paste0(session_label, ".dta")))
    
    return(invisible(NULL))
  }
  
  # ------------------------------------------------------------
  # 3. Run loops for U2 (1–4) and U4 (1–3)
  # ------------------------------------------------------------
  for (j in c(2, 4)) {
    k_max <- ifelse(j == 2, 4, 3)
    for (k in 1:k_max) {
      process_unanimity_sheet(j, k)
    }
  }
  
  # ------------------------------------------------------------
  # 4. Handle U4_s4 separately (special case)
  # ------------------------------------------------------------
  sheet_name <- "U4_s4"
  session_label <- "u4s4"
  message("Processing ", sheet_name)
  
  df <- read_excel(input_file, sheet = sheet_name) %>%
    select(
      -participant.id_in_session,
      -participant.code,
      -participant.label,
      -session.code,
      -group.bigSize,
      -subsession.round_number
    ) %>%
    rename(
      group = group.group_identifier,
      playerid_in_group = player.id_in_group
    ) %>%
    filter(!is.na(group)) %>%
    arrange(group, playerid_in_group) %>%
    pivot_wider(id_cols = group,
                names_from = playerid_in_group,
                values_from = starts_with("player.u_v"))
  names(df) <- gsub("\\.", "_", names(df))
  
  for (i in c(2, 4)) {
    var1 <- paste0("player_u_v", i, "_1")
    var2 <- paste0("player_u_v", i, "_2")
    if (var1 %in% names(df)) df[[var1]] <- ifelse(df[[var1]] != "0", 1, 0)
    if (var2 %in% names(df)) df[[var2]] <- ifelse(df[[var2]] != "0", 1, 0)
  }
  
  df$session <- session_label
  for (i in 1:8) {
    v1 <- paste0("player_u_v", i, "_1")
    v2 <- paste0("player_u_v", i, "_2")
    agree_name <- paste0("agreev", i)
    if (all(c(v1, v2) %in% names(df))) {
      df[[agree_name]] <- as.numeric(df[[v1]] == df[[v2]])
    }
  }
  
  write_dta(df, file.path(output_path, paste0("Kappa_", session_label, ".dta")))
  
  df_min <- df %>%
    select(any_of(c("group", paste0("player_u_v", 1:8, "_1"), paste0("agreev", 1:8)))) %>%
    rename_with(~ str_replace(.x, "player_u_v(\\d+)_1", "valueq\\1")) %>%
    arrange(group)
  write_dta(df_min, file.path(output_path, paste0(session_label, ".dta")))
  
  # ------------------------------------------------------------
  # 5. Combine all session files into one KappaUnanimity.dta
  # ------------------------------------------------------------
  sessions <- c("u2s1", "u2s2", "u2s3", "u2s4", "u4s1", "u4s2", "u4s3", "u4s4")
  all_data <- lapply(sessions, function(sess) {
    read_dta(file.path(output_path, paste0(sess, ".dta")))
  })
  combined_df <- bind_rows(all_data) %>% select(-group)
  write_dta(combined_df, file.path(output_path, "KappaUnanimity.dta"))
  
  message("🎉 KappaUnanimity.dta created successfully in:\n", output_path)
  # ============================================================
  # R translation of: KappaUnanimity.dta creation and kappas
  # ============================================================
  
  library(haven)
  library(dplyr)
  library(irr)   # for kappa2()
  
  # ------------------------------------------------------------
  # 1. Paths
  # ------------------------------------------------------------
  base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business/Processed_Files"
  output_file <- file.path(base_path, "KappaUnanimity.dta")
  
  # ------------------------------------------------------------
  # 2. Load and combine all session data
  # ------------------------------------------------------------
  sessions <- c("u2s1", "u2s2", "u2s3", "u2s4", "u4s1", "u4s2", "u4s3", "u4s4")
  
  all_data <- lapply(sessions, function(sess) {
    f <- file.path(base_path, paste0("Kappa_", sess, ".dta"))
    if (file.exists(f)) {
      df <- read_dta(f)
      df
    } else {
      warning("Missing file: ", f)
      NULL
    }
  })
  df_all <- bind_rows(all_data)
  df_all <- df_all %>% select(-group, everything())
  
  # ------------------------------------------------------------
  # 3. Helper: compute Cohen’s kappa for two columns
  # ------------------------------------------------------------
  get_kappa <- function(x, y) {
    d <- data.frame(r1 = x, r2 = y)
    d <- na.omit(d)
    if (nrow(d) == 0) return(NA)
    tryCatch(kappa2(d)$value, error = function(e) NA)
  }
  
  # ------------------------------------------------------------
  # 4. Define sessions and which questions to include
  # (mimicking “no instances of q7==1”, etc.)
  # ------------------------------------------------------------
  session_rules <- list(
    u2s1 = c(1:6, 8),
    u2s2 = c(1:5, 7:8),
    u2s3 = c(2:6, 8),
    u2s4 = c(1:8),
    u4s1 = c(1:8),
    u4s2 = c(1:6, 8),
    u4s3 = c(2:8),
    u4s4 = c(1:5, 7:8)
  )
  
  # ------------------------------------------------------------
  # 5. Loop over sessions and compute kappas
  # ------------------------------------------------------------
  kappa_results <- list()
  
  for (sess in names(session_rules)) {
    cat("Processing session:", sess, "\n")
    df_sess <- df_all %>% filter(session == sess)
    
    qs <- session_rules[[sess]]
    kappas <- sapply(qs, function(i) {
      v1 <- paste0("player_u_v", i, "1")
      v2 <- paste0("player_u_v", i, "2")
      if (all(c(v1, v2) %in% names(df_sess))) {
        get_kappa(df_sess[[v1]], df_sess[[v2]])
      } else NA
    })
    
    names(kappas) <- paste0("kap", qs, sess)
    kappa_results[[sess]] <- kappas
  }
  
  # ------------------------------------------------------------
  # 6. Combine all Kappa results into one data frame
  # ------------------------------------------------------------
  kappa_df <- do.call(cbind, kappa_results)
  kappa_df <- as.data.frame(kappa_df)
  write_dta(df_all, output_file)
  
  cat("✅ Combined KappaUnanimity.dta written to:\n", output_file, "\n")
  
  # ------------------------------------------------------------
  # 7. Optionally, export the Kappa summary table to CSV
  # ------------------------------------------------------------
  write.csv(kappa_df, file.path(base_path, "KappaUnanimity_Summary.csv"), row.names = FALSE)
  cat("✅ Kappa summary saved as KappaUnanimity_Summary.csv\n")
  # ============================================================
  # Merge chatdata_48and96.xlsx with majority/unanimity .dta files
  # ============================================================
  
  library(readxl)
  library(dplyr)
  library(haven)
  library(stringr)
  
  # ------------------------------------------------------------
  # 1. Define paths
  # ------------------------------------------------------------
  base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
  input_file <- file.path(base_path, "chatdata_48and96.xlsx")
  notfor_path <- file.path(base_path, "Processed_Files")
  
  # ------------------------------------------------------------
  # 2. Helper function to merge and save
  # ------------------------------------------------------------
  merge_chat_with_dta <- function(sheet_name, dta_prefix) {
    message("Processing sheet: ", sheet_name)
    
    # Read Excel sheet
    chat_df <- read_excel(input_file, sheet = sheet_name) %>%
      select(Sessioncode, GroupId) %>%
      rename(sessioncode = Sessioncode, group = GroupId) %>%
      arrange(group) %>%
      group_by(group) %>%
      mutate(num = row_number()) %>%
      filter(num == 1) %>%
      select(-num) %>%
      ungroup()
    
    # Build corresponding .dta path
    dta_file <- file.path(notfor_path, paste0(dta_prefix, ".dta"))
    
    # Read the existing .dta
    if (!file.exists(dta_file)) {
      warning("Missing file: ", dta_file)
      return(NULL)
    }
    
    dta_df <- read_dta(dta_file)
    
    # Merge 1:1 by group
    merged_df <- merge(chat_df, dta_df, by = "group", all.x = FALSE, all.y = TRUE)
    
    # Sort by sessioncode and group
    if ("sessioncode" %in% names(merged_df)) {
      merged_df <- merged_df %>% arrange(sessioncode, group)
    } else {
      merged_df <- merged_df %>% arrange(group)
    }
    
    # Save back to .dta
    write_dta(merged_df, dta_file)
    message("✅ Saved updated file: ", dta_file)
  }
  
  # ------------------------------------------------------------
  # 3. Loop for Majority sessions (M2_s1–M4_s4)
  # ------------------------------------------------------------
  for (j in c(2, 4)) {
    for (k in 1:4) {
      sheet_name <- paste0("M", j, "_s", k)
      dta_prefix <- paste0("m", j, "s", k)
      merge_chat_with_dta(sheet_name, dta_prefix)
    }
  }
  
  # ------------------------------------------------------------
  # 4. Loop for Unanimity sessions (U4_s3–U4_s4)
  # ------------------------------------------------------------
  for (j in 4:4) {
    for (k in 3:4) {
      sheet_name <- paste0("U", j, "_s", k)
      dta_prefix <- paste0("u", j, "s", k)
      merge_chat_with_dta(sheet_name, dta_prefix)
    }
  }
  
  message("\n🎉 All merges complete and .dta files updated successfully!")
# ============================================================
# Multistage sessions U2s1 and U2s2
# ============================================================

library(readxl)
library(dplyr)
library(haven)
library(stringr)

# ------------------------------------------------------------
# 1. Define base paths
# ------------------------------------------------------------
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
input_file <- file.path(base_path, "session1_070212.chatswithmembers.xlsx")
notfor_path <- file.path(base_path, "Processed_Files")

# ------------------------------------------------------------
# 2. Function to process one session (U2s1 or U2s2)
# ------------------------------------------------------------
process_u2_session <- function(session_name, session_label, drop_group = NULL) {
  message("Processing ", session_name, " ...")
  
  # Read Excel file
  df <- read_excel(input_file) %>%
    select(matchId, groupId, member1Id, member2Id, member3Id) %>%
    rename(matchid = matchId,
           groupid = groupId,
           memberid1 = member1Id,
           memberid2 = member2Id,
           memberid3 = member3Id) %>%
    mutate(
      group = paste0("m", matchid, "g", groupid)
    ) %>%
    arrange(group) %>%
    group_by(group) %>%
    mutate(num = row_number()) %>%
    filter(num == 1) %>%
    select(-num) %>%
    ungroup()
  
  # Merge with existing .dta
  dta_path <- file.path(notfor_path, paste0(session_name, ".dta"))
  if (!file.exists(dta_path)) {
    stop("Missing file: ", dta_path)
  }
  
  dta_df <- read_dta(dta_path)
  
  merged <- merge(df, dta_df, by = "group")
  
  # Optional: drop problematic group
  if (!is.null(drop_group)) {
    merged <- merged %>% filter(group != drop_group)
  }
  
  # Generate derived columns
  merged <- merged %>%
    mutate(
      sessioncode = session_label,
      memberidmax = pmax(memberid1, memberid2, memberid3, na.rm = TRUE),
      round = matchid + 1
    ) %>%
    rename(groupbis = group) %>%
    arrange(round, memberidmax)
  
  # Save back to .dta
  write_dta(merged, dta_path)
  message("✅ Saved updated file: ", dta_path)
}

# ------------------------------------------------------------
# 3. Run for both sessions
# ------------------------------------------------------------
process_u2_session("u2s1", session_label = "m1")

# Special case: u2s2 needs to drop group "m9g3"
process_u2_session("u2s2", session_label = "m2", drop_group = "m9g3")

message("\n🎉 U2s1 and U2s2 sessions processed successfully!")
# ============================================================
# Multistage sessions U2s3 and U2s4
# ============================================================

library(readxl)
library(dplyr)
library(haven)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
input_file2 <- file.path(base_path, "session2_070213.chatswithmembers.xlsx")
notfor_path <- file.path(base_path, "Processed_Files")

# ------------------------------------------------------------
# Function to process a session
# ------------------------------------------------------------
process_u2_session2 <- function(session_name, session_label) {
  message("Processing ", session_name, " ...")
  
  # Read Excel
  df <- read_excel(input_file2) %>%
    select(matchId, groupId, member1Id, member2Id, member3Id) %>%
    rename(matchid = matchId,
           groupid = groupId,
           memberid1 = member1Id,
           memberid2 = member2Id,
           memberid3 = member3Id) %>%
    mutate(
      group = paste0("m", matchid, "g", groupid)
    ) %>%
    arrange(group) %>%
    group_by(group) %>%
    mutate(num = row_number()) %>%
    filter(num == 1) %>%
    select(-num) %>%
    ungroup()
  
  # Merge with existing .dta
  dta_path <- file.path(notfor_path, paste0(session_name, ".dta"))
  if (!file.exists(dta_path)) stop("Missing file: ", dta_path)
  
  dta_df <- read_dta(dta_path)
  
  merged <- merge(df, dta_df, by = "group")
  
  # Keep only _m == 3 equivalent (all matched rows)
  # In R merge, we only keep matching rows, so this is automatically enforced
  # If needed, you could filter further if there is a column named _m
  
  # Add derived columns
  merged <- merged %>%
    mutate(
      sessioncode = session_label,
      memberidmax = pmax(memberid1, memberid2, memberid3, na.rm = TRUE),
      round = matchid + 1
    ) %>%
    rename(groupbis = group) %>%
    arrange(round, memberidmax)
  
  # Save back to .dta
  write_dta(merged, dta_path)
  message("✅ Saved updated file: ", dta_path)
}

# ------------------------------------------------------------
# Run for U2s3 and U2s4
# ------------------------------------------------------------
process_u2_session2("u2s3", session_label = "m3")
process_u2_session2("u2s4", session_label = "m4")

message("\n🎉 U2s3 and U2s4 sessions processed successfully!")
library(readxl)
library(dplyr)
library(haven)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
input_file3 <- file.path(base_path, "session1_070215.chatswithmembers.xlsx")
notfor_path <- file.path(base_path, "Processed_Files")

# ------------------------------------------------------------
# Function to process a U4 session
# ------------------------------------------------------------
process_u4_session <- function(session_name, session_label) {
  message("Processing ", session_name, " ...")
  
  # Read Excel
  df <- read_excel(input_file3) %>%
    select(matchId, groupId, member1Id, member2Id, member3Id) %>%
    rename(matchid = matchId,
           groupid = groupId,
           memberid1 = member1Id,
           memberid2 = member2Id,
           memberid3 = member3Id) %>%
    mutate(
      group = paste0("m", matchid, "g", groupid)
    ) %>%
    arrange(group) %>%
    group_by(group) %>%
    mutate(num = row_number()) %>%
    filter(num == 1) %>%
    select(-num) %>%
    ungroup()
  
  # Merge with existing .dta
  dta_path <- file.path(notfor_path, paste0(session_name, ".dta"))
  if (!file.exists(dta_path)) stop("Missing file: ", dta_path)
  
  dta_df <- read_dta(dta_path)
  
  merged <- merge(df, dta_df, by = "group")
  
  # Add derived columns
  merged <- merged %>%
    mutate(
      sessioncode = session_label,
      memberidmax = pmax(memberid1, memberid2, memberid3, na.rm = TRUE),
      round = matchid + 1
    ) %>%
    rename(groupbis = group) %>%
    arrange(round, memberidmax)
  
  # Save back to .dta
  write_dta(merged, dta_path)
  message("✅ Saved updated file: ", dta_path)
}

# ------------------------------------------------------------
# Run for U4s1 and U4s2
# ------------------------------------------------------------
process_u4_session("u4s1", session_label = "m5")
process_u4_session("u4s2", session_label = "m6")

message("\n🎉 U4s1 and U4s2 sessions processed successfully!")
library(dplyr)
library(haven)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
base_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business"
alldata_path <- file.path(base_path, "alldata.dta")
chatcoded_path <- file.path(base_path, "Raw Data Chat Coders/NotForSubmission")
output_path <- file.path(base_path, "Raw Data Chat Coders/NotForSubmission")

# ------------------------------------------------------------
# Helper function to process a single session merge
# ------------------------------------------------------------
merge_chatcoded <- function(vote_type, multiplier_value, session_num, j, k, is_multistage=FALSE) {
  
  # Load base alldata
  df <- read_dta(alldata_path) %>%
    mutate(
      multiplier = case_when(
        pietreatment == 24 ~ 1,
        pietreatment == 48 ~ 2,
        pietreatment == 96 ~ 4
      )
    ) %>%
    filter(votingtreatment == vote_type,
           multiplier == multiplier_value,
           session == session_num)
  
  # If multistage, use 'round' and 'memberidmax' for merge
  if(is_multistage){
    df <- df %>%
      group_by(sessioncode, round, group) %>%
      mutate(memberidmax = max(memberid, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(sessioncode, round, memberidmax)
    merge_vars <- c("sessioncode", "round", "memberidmax")
  } else {
    df <- df %>% arrange(sessioncode, group)
    merge_vars <- c("sessioncode", "group")
  }
  
  # Merge with chat-coded data
  chat_file <- paste0(chatcoded_path, "/", vote_type[1], j, "s", k, ".dta") # vote_type[1] -> 'm' or 'u'
  chat_df <- read_dta(chat_file)
  
  merged <- merge(df, chat_df, by = merge_vars)
  
  # Save intermediate file
  out_file <- paste0(output_path, "/alldata_withchatcoded_", vote_type[1], j, "s", k, ".dta")
  write_dta(merged, out_file)
  
  return(out_file)
}

# ------------------------------------------------------------
# Step 1: Create all session-specific merged files
# ------------------------------------------------------------

# Majority sessions (m)
majority_sessions <- expand.grid(j = c(2,4), k = 1:4)
majority_files <- mapply(function(j,k){
  merge_chatcoded("majority", j, session_num = k, j=j, k=k)
}, majority_sessions$j, majority_sessions$k)

# Unanimity sessions (u)
unanimity_sessions <- expand.grid(j = c(2,4), k = 1:4)
unanimity_files <- mapply(function(j,k){
  # Check which sessions are multistage
  is_multi <- (j==2 & k %in% 1:4) | (j==4 & k %in% 1:2)
  merge_chatcoded("unanimity", j, session_num = k, j=j, k=k, is_multistage = is_multi)
}, unanimity_sessions$j, unanimity_sessions$k)

# ------------------------------------------------------------
# Step 2: Append all merged files into one dataset
# ------------------------------------------------------------
all_files <- c(majority_files, unanimity_files)
alldata_merged <- do.call(rbind, lapply(all_files, read_dta))

# Save final combined dataset
final_file <- file.path(base_path, "alldata_withchatcoded.dta")
write_dta(alldata_merged, final_file)

message("🎉 All chat-coded data merged into ", final_file)
library(dplyr)
library(haven)

# Paths
input_file <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business/alldata_withchatcoded.dta"
output_file <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business/alldata_withchatcoded_ready.dta"

# Load data
df <- read_dta(input_file)

# Create new variables
df <- df %>%
  mutate(
    # total delay
    totaldelay = if_else(delay == 1 | pass == 0, 1, 0),
    
    # relevant talk / did not talk
    relevant_talk = if_else(
      !is.na(valueq1) | !is.na(valueq2) | !is.na(valueq3) | !is.na(valueq4) |
        !is.na(valueq5) | !is.na(valueq6) | !is.na(valueq7) | !is.na(valueq8) |
        !is.na(valueq9) | !is.na(valueq10) | !is.na(valueq11), 1, 0
    ),
    didnottalk = if_else(
      is.na(valueq1) & is.na(valueq2) & is.na(valueq3) & is.na(valueq4) &
        is.na(valueq5) & is.na(valueq6) & is.na(valueq7) & is.na(valueq8) &
        is.na(valueq9) & is.na(valueq10) & is.na(valueq11), 1, 0
    ),
    
    # Specific talk variables
    bigpietalk = if_else(valueq2 == 1 & agreev2 == 1, 1, NA_real_),
    yestodelaytalk = if_else(valueq8 == 1 & agreev8 == 1, 1, NA_real_),
    equalitytalk = if_else(valueq4 == 1 & agreev4 == 1, 1, NA_real_),
    threatnounequaltalk = if_else(valueq5 == 1 & agreev5 == 1, 1, NA_real_),
    threatnosmallbudgettalk = if_else(valueq6 == 1 & agreev6 == 1, 1, NA_real_),
    riskterminationtalk = if_else(valueq3 == 1 & agreev3 == 1, 1, NA_real_),
    equalityinmwctalk = if_else(valueq10 == 1 & agreev10 == 1, 1, NA_real_),
    unequalinmwctalk = if_else(valueq11 == 1 & agreev11 == 1, 1, NA_real_)
  )

# Save to .dta
write_dta(df, output_file)

message("✅ Ready file saved to: ", output_file)

  