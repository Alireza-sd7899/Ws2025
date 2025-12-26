setwd("E:/Dc1")
# --- REVISED R CODE FOR REPLICATING GRAPHS FROM EXCEL FILE ---

# Load libraries
library(tidyverse)
library(readxl)     # <- To read Excel files
library(scales)

# File path
file_path <- "E:/My education/UNI/edu uni/Semester 2/Data science for Business/figures.xlsx"

# --- FIGURE 1: Stacked Bar Chart (Proportions) ---

# Read data from first sheet (change sheet = 1 to another if needed)
df1 <- read_excel(file_path, sheet = 1, skip = 1, col_names = FALSE)
colnames(df1) <- c("Category", "M48", "U48", "DROP", "M96", "U96")

# Clean and prepare data
df1_clean <- df1 %>%
  select(-DROP) %>% 
  pivot_longer(
    cols = c(M48, U48, M96, U96),
    names_to = "Condition",
    values_to = "Proportion"
  ) %>%
  mutate(Proportion = as.numeric(Proportion))

# Generate Stacked Bar Plot
plot1 <- ggplot(df1_clean, aes(x = Condition, y = Proportion, fill = Category)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Figure 1: Proportion of Small vs. Big Category by Condition",
    x = "Experimental Condition",
    y = "Proportion",
    fill = "Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(plot1)

# --------------------------------------------------------------------

# --- FIGURE 2: Grouped Bar Chart (Independent Metrics) ---

# Read data from second sheet (change if needed)
df2 <- read_excel(file_path, sheet = 2, skip = 1, col_names = FALSE)
colnames(df2) <- c("Metric", "M48", "U48", "M96", "U96")

# Clean and prepare data
df2_clean <- df2 %>%
  pivot_longer(
    cols = c(M48, U48, M96, U96),
    names_to = "Condition",
    values_to = "Value"
  ) %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(Metric = gsub("Frac of ", "", Metric))

# Generate Grouped Bar Plot
plot2 <- ggplot(df2_clean, aes(x = Condition, y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_text(
    aes(label = round(Value, 3)),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_y_continuous(limits = c(0, max(df2_clean$Value, na.rm = TRUE) * 1.1)) +
  labs(
    title = "Figure 2: Comparison of Two Metrics by Condition",
    x = "Experimental Condition",
    y = "Fraction Value",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

print(plot2)

