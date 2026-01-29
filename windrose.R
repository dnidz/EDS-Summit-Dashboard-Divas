
# ============================================================================
# Package Installation Script
# Community-Based Science Success Survey Analysis
# ============================================================================

# This script installs all packages required for the analysis
# Run this ONCE before running the main analysis script

cat("Installing required packages...\n")
cat("This may take a few minutes.\n\n")

# List of required packages
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "readr",          # Reading CSV files
  "ggplot2",        # Creating plots
  "scales",         # Scale functions for visualization
  "RColorBrewer",   # Color palettes
  "gridExtra",      # Arranging multiple plots
  "tidytext",       # Text analysis
  "reshape2",       # Data reshaping
  "viridis"         # Color-blind friendly color palettes
)

# Function to install packages if not already installed
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Installing", package, "...\n"))
    install.packages(package, dependencies = TRUE, repos = "https://cran.rstudio.com/")
    cat(paste(package, "installed successfully!\n\n"))
  } else {
    cat(paste(package, "is already installed.\n"))
  }
}

# Install all packages
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Verify installation
cat("\n============================================\n")
cat("Verifying package installation...\n")
cat("============================================\n\n")

all_installed <- TRUE
for (pkg in required_packages) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("✓", pkg, "loaded successfully\n"))
  } else {
    cat(paste("✗", pkg, "failed to load\n"))
    all_installed <- FALSE
  }
}

cat("\n============================================\n")
if (all_installed) {
  cat("SUCCESS! All packages installed and loaded.\n")
  cat("You can now run: source('community_science_analysis.R')\n")
} else {
  cat("WARNING: Some packages failed to install.\n")
  cat("Please check error messages above.\n")
}
cat("============================================\n")

# ============================================================================
# Community-Based Science Success Survey Analysis
# ============================================================================

# Load required libraries
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(tidytext)
library(reshape2)
library(viridis)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12))

# ============================================================================
# 1. DATA LOADING AND CLEANING
# ============================================================================

# Read the data (skip first 2 rows with metadata)
data <- read_csv("/Users/karagansmith/Desktop/DashboardDivas/survey_data/V1_Community-Based Success_January 28, 2026_13.47.csv", 
                 skip = 1)
# Create shorter, cleaner column names for success metrics
success_cols <- grep("^Please rate", colnames(data), value = TRUE)

# Create a mapping for shorter names
short_names <- c(
  "Policy_influence" = "Research directly influences policy or management decisions",
  "Environmental_improvements" = "Results lead to measurable environmental improvements",
  "Data_skills" = "Community capacity for data skills increases",
  "Trust_building" = "Trust is built between communities and institutions",
  "Sustained_funding" = "Project secures sustained funding",
  "Accessible_findings" = "Findings are accessible and usable by non-scientists",
  "Peer_review_pub" = "Publishing a peer-reviewed article",
  "Detailed_report" = "Publishing a detailed report",
  "Public_publication" = "Publishing for a public audience (e.g., blog post, website)",
  "Interim_communication" = "Communicating interim findings during the project (not just final publications)",
  "Active_collaboration" = "Active collaboration with community members throughout the project lifecycle",
  "Traditional_knowledge" = "Traditional/local knowledge is integrated with scientific data",
  "Community_ownership" = "Community maintains ownership of data and findings",
  "Continued_collaboration" = "Collaborations continue beyond initial project",
  "Equitable_partnerships" = "Partnerships are equitable, collaborative, empowering, and address social inequalities",
  "Community_control" = "The community has meaningful say and control over the research process",
  "Compensation" = "Community members are compensated for their time and effort",
  "Clear_communication" = "There is clear communication on how data and outcomes will be used",
  "Integrated_perspectives" = "Community members' viewpoints and perspectives are integrated into the project"
)

# ============================================================================
# 2. DEMOGRAPHICS ANALYSIS
# ============================================================================
colnames(data)
# Clean role data
roles <- data %>%
  select(contains("primary role")) %>%
  filter(!is.na(`What is your primary role? - Selected Choice`)) %>%
  separate_rows(`What is your primary role? - Selected Choice`, sep = ",") %>%
  mutate(Role = trimws(`What is your primary role? - Selected Choice`)) %>%
  count(Role, sort = TRUE)

# Plot 1: Primary Roles
p1 <- ggplot(roles, aes(x = reorder(Role, n), y = n, fill = Role)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Primary Roles of Survey Respondents",
       x = "Role",
       y = "Count") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

ggsave("plot1_roles.png", p1, width = 8, height = 5, dpi = 300)

# Clean experience data
experience <- data %>%
  select(contains("years have you been")) %>%
  filter(!is.na(`How many years have you been involved in community-based science?`)) %>%
  count(Experience = `How many years have you been involved in community-based science?`)

# Plot 2: Years of Experience
p2 <- ggplot(experience, aes(x = Experience, y = n, fill = Experience)) +
  geom_bar(stat = "identity") +
  labs(title = "Years of Experience in Community-Based Science",
       x = "Years of Experience",
       y = "Count") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")

ggsave("plot2_experience.png", p2, width = 8, height = 5, dpi = 300)

# Geographic scope
geo_scope <- data %>%
  select(contains("geographic scope")) %>%
  filter(!is.na(`What is the geographic scope of your work? (select all that apply)`)) %>%
  separate_rows(`What is the geographic scope of your work? (select all that apply)`, sep = ",") %>%
  mutate(Scope = trimws(`What is the geographic scope of your work? (select all that apply)`)) %>%
  count(Scope, sort = TRUE)

# Plot 3: Geographic Scope
p3 <- ggplot(geo_scope, aes(x = reorder(Scope, n), y = n, fill = Scope)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Geographic Scope of Work",
       x = "Geographic Scope",
       y = "Count") +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "plasma")

ggsave("plot3_geographic_scope.png", p3, width = 8, height = 5, dpi = 300)

# ============================================================================
# 3. SUCCESS METRICS ANALYSIS
# ============================================================================

# Extract and reshape success metric data
success_data <- data %>%
  select(starts_with("Please rate")) %>%
  select(-contains("TEXT"))

# Create numeric versions and calculate means
success_numeric <- success_data %>%
  mutate(across(everything(), ~case_when(
    . == "Not at all important" ~ 1,
    . == "Slightly important" ~ 2,
    . == "Moderately important" ~ 3,
    . == "Very important" ~ 4,
    . == "Extremely important" ~ 5,
    TRUE ~ NA_real_
  )))

# Calculate mean importance for each metric
mean_importance <- success_numeric %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Mean_Importance") %>%
  mutate(Metric_Short = case_when(
    grepl("policy or management", Metric) ~ "Policy influence",
    grepl("environmental improvements", Metric) ~ "Environmental improvements",
    grepl("data skills", Metric) ~ "Data skills capacity",
    grepl("Trust is built", Metric) ~ "Trust building",
    grepl("sustained funding", Metric) ~ "Sustained funding",
    grepl("accessible and usable", Metric) ~ "Accessible findings",
    grepl("peer-reviewed", Metric) ~ "Peer-reviewed publication",
    grepl("detailed report", Metric) ~ "Detailed report",
    grepl("public audience", Metric) ~ "Public publication",
    grepl("interim findings", Metric) ~ "Interim communication",
    grepl("Active collaboration", Metric) ~ "Active collaboration",
    grepl("Traditional/local", Metric) ~ "Traditional knowledge",
    grepl("ownership of data", Metric) ~ "Community data ownership",
    grepl("continue beyond", Metric) ~ "Continued collaboration",
    grepl("equitable, collaborative", Metric) ~ "Equitable partnerships",
    grepl("meaningful say", Metric) ~ "Community control",
    grepl("compensated", Metric) ~ "Compensation",
    grepl("clear communication", Metric) ~ "Clear communication",
    grepl("viewpoints and perspectives", Metric) ~ "Integrated perspectives",
    TRUE ~ "Other"
  )) %>%
  arrange(desc(Mean_Importance))

# Plot 4: Mean Importance Rankings
p4 <- ggplot(mean_importance, aes(x = reorder(Metric_Short, Mean_Importance), 
                                  y = Mean_Importance, 
                                  fill = Mean_Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Mean Importance Ratings for Success Metrics",
       subtitle = "Scale: 1 (Not important) to 5 (Extremely important)",
       x = "Success Metric",
       y = "Mean Importance Rating") +
  scale_fill_gradient2(low = "lightblue", mid = "yellow", high = "darkred", 
                       midpoint = 3, name = "Importance") +
  geom_hline(yintercept = 3, linetype = "dashed", color = "gray50") +
  theme(legend.position = "bottom")

ggsave("plot4_mean_importance.png", p4, width = 10, height = 8, dpi = 300)

# ============================================================================
# 4. HEATMAP OF SUCCESS METRICS
# ============================================================================

# Prepare data for heatmap
heatmap_data <- success_numeric %>%
  mutate(ResponseID = row_number()) %>%
  pivot_longer(-ResponseID, names_to = "Metric", values_to = "Rating") %>%
  filter(!is.na(Rating)) %>%
  mutate(Metric_Short = case_when(
    grepl("policy or management", Metric) ~ "Policy influence",
    grepl("environmental improvements", Metric) ~ "Environmental improvements",
    grepl("data skills", Metric) ~ "Data skills",
    grepl("Trust is built", Metric) ~ "Trust building",
    grepl("sustained funding", Metric) ~ "Sustained funding",
    grepl("accessible and usable", Metric) ~ "Accessible findings",
    grepl("peer-reviewed", Metric) ~ "Peer-review pub",
    grepl("detailed report", Metric) ~ "Detailed report",
    grepl("public audience", Metric) ~ "Public pub",
    grepl("interim findings", Metric) ~ "Interim comm",
    grepl("Active collaboration", Metric) ~ "Active collab",
    grepl("Traditional/local", Metric) ~ "Traditional knowledge",
    grepl("ownership of data", Metric) ~ "Data ownership",
    grepl("continue beyond", Metric) ~ "Continued collab",
    grepl("equitable, collaborative", Metric) ~ "Equitable partnerships",
    grepl("meaningful say", Metric) ~ "Community control",
    grepl("compensated", Metric) ~ "Compensation",
    grepl("clear communication", Metric) ~ "Clear comm",
    grepl("viewpoints and perspectives", Metric) ~ "Integrated perspectives",
    TRUE ~ "Other"
  ))

# Create heatmap
p5 <- ggplot(heatmap_data, aes(x = factor(ResponseID), y = Metric_Short, fill = Rating)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "darkred", 
                       midpoint = 3, name = "Rating",
                       breaks = c(1,2,3,4,5),
                       labels = c("1\nNot\nimportant", "2\nSlightly", 
                                  "3\nModerately", "4\nVery", "5\nExtremely")) +
  labs(title = "Heatmap of Success Metric Importance Ratings",
       x = "Respondent",
       y = "Success Metric") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

ggsave("plot5_heatmap.png", p5, width = 12, height = 8, dpi = 300)

# ============================================================================
# 5. DISTRIBUTION OF RATINGS BY METRIC
# ============================================================================

# Count distribution for each rating level
rating_distribution <- heatmap_data %>%
  count(Metric_Short, Rating) %>%
  group_by(Metric_Short) %>%
  mutate(Percentage = n / sum(n) * 100)

# Plot 6: Stacked bar chart of rating distributions
p6 <- ggplot(rating_distribution, aes(x = reorder(Metric_Short, Rating, 
                                                  FUN = function(x) sum(x * rating_distribution$n[rating_distribution$Metric_Short == rating_distribution$Metric_Short[1]])), 
                                      y = Percentage, 
                                      fill = factor(Rating))) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  labs(title = "Distribution of Importance Ratings by Success Metric",
       x = "Success Metric",
       y = "Percentage of Responses",
       fill = "Rating") +
  scale_fill_manual(values = c("1" = "#d73027", "2" = "#fc8d59", 
                               "3" = "#fee08b", "4" = "#91cf60", 
                               "5" = "#1a9850"),
                    labels = c("1 - Not important", "2 - Slightly", 
                               "3 - Moderately", "4 - Very", "5 - Extremely")) +
  theme(legend.position = "bottom")

ggsave("plot6_rating_distribution.png", p6, width = 12, height = 8, dpi = 300)

# ============================================================================
# 6. TOP 3 PRIORITIES ANALYSIS
# ============================================================================

# Extract top 3 priorities
top3_data <- data %>%
  select(contains("TOP 3 priorities")) %>%
  filter(!is.na(`Based on your previously identified "extremely important" responses, given limited resources, time, and capacity, select your TOP 3 priorities for success in community-based projects.`)) %>%
  separate_rows(`Based on your previously identified "extremely important" responses, given limited resources, time, and capacity, select your TOP 3 priorities for success in community-based projects.`, sep = ",") %>%
  mutate(Priority = trimws(`Based on your previously identified "extremely important" responses, given limited resources, time, and capacity, select your TOP 3 priorities for success in community-based projects.`)) %>%
  count(Priority, sort = TRUE)

# Plot 7: Top 3 Priorities
p7 <- ggplot(top3_data, aes(x = reorder(Priority, n), y = n, fill = Priority)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Most Frequently Selected Top 3 Priorities",
       subtitle = "What matters most when resources are limited?",
       x = "Priority",
       y = "Number of Times Selected") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

ggsave("plot7_top3_priorities.png", p7, width = 10, height = 6, dpi = 300)

# ============================================================================
# 7. BARRIERS TO SUCCESS
# ============================================================================

# Extract barriers data
barriers_data <- data %>%
  select(contains("barriers to actionable outcomes? - Selected")) %>%
  filter(!is.na(`In your opinion/experience, what are the primary barriers to actionable outcomes? - Selected Choice`)) %>%
  separate_rows(`In your opinion/experience, what are the primary barriers to actionable outcomes? - Selected Choice`, sep = ",") %>%
  mutate(Barrier = trimws(`In your opinion/experience, what are the primary barriers to actionable outcomes? - Selected Choice`)) %>%
  count(Barrier, sort = TRUE)

# Plot 8: Primary Barriers
p8 <- ggplot(barriers_data, aes(x = reorder(Barrier, n), y = n, fill = Barrier)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Primary Barriers to Actionable Outcomes",
       x = "Barrier",
       y = "Count") +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

ggsave("plot8_barriers.png", p8, width = 10, height = 6, dpi = 300)

# ============================================================================
# 8. ACTIONABILITY SCORES
# ============================================================================

# Extract actionability ratings
actionability <- data %>%
  select(contains("how actionable would you say")) %>%
  filter(!is.na(`In your most recent community-based project, how actionable would you say the outcomes were? - 0 = Not Actionable, 5 = Extremely Actionable`)) %>%
  mutate(Score = as.numeric(`In your most recent community-based project, how actionable would you say the outcomes were? - 0 = Not Actionable, 5 = Extremely Actionable`))

# Plot 9: Distribution of Actionability Scores
p9 <- ggplot(actionability, aes(x = Score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Actionability Scores",
       subtitle = "How actionable were outcomes in most recent projects?",
       x = "Actionability Score (0 = Not Actionable, 5 = Extremely Actionable)",
       y = "Count") +
  scale_x_continuous(breaks = 0:5)

ggsave("plot9_actionability.png", p9, width = 8, height = 5, dpi = 300)

# ============================================================================
# 9. COMMUNICATION PREFERENCES
# ============================================================================

# Extract communication preferences
comm_prefs <- data %>%
  select(contains("How do you prefer to be approached")) %>%
  filter(!is.na(`How do you prefer to be approached and communicated with during collaborative projects? - Selected Choice`)) %>%
  separate_rows(`How do you prefer to be approached and communicated with during collaborative projects? - Selected Choice`, sep = ",") %>%
  mutate(Preference = trimws(`How do you prefer to be approached and communicated with during collaborative projects? - Selected Choice`)) %>%
  count(Preference, sort = TRUE)

# Plot 10: Communication Preferences
p10 <- ggplot(comm_prefs, aes(x = reorder(Preference, n), y = n, fill = Preference)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Preferred Communication Methods",
       x = "Communication Method",
       y = "Count") +
  theme(legend.position = "none") +
  scale_fill_viridis_d(option = "mako")

ggsave("plot10_communication.png", p10, width = 10, height = 5, dpi = 300)


#windrose slay 

# Get role data
roles_data <- data %>%
  select(
    role = `What is your primary role? - Selected Choice`,
    starts_with("Please rate how important")
  ) %>%
  filter(!is.na(role)) %>%
  separate_rows(role, sep = ",") %>%
  mutate(role = trimws(role)) %>%
  filter(!grepl("ImportId|QID", role, ignore.case = TRUE)) %>%
  filter(role != "")

# Get the Q6 columns (all the rating questions)
q6_cols <- grep("^Please rate", colnames(roles_data), value = TRUE)

# Create proper mapping from Q6 column text to short priority names
# Each Q6 column contains the priority text
priority_short_mapping <- list(
  "Research directly influences policy or management decisions" = "Policy influence",
  "Results lead to measurable environmental improvements" = "Environmental improvements",
  "Community capacity for data skills increases" = "Data skills capacity",
  "Trust is built between communities and institutions" = "Trust building",
  "Project secures sustained funding" = "Sustained funding",
  "Findings are accessible and usable by non-scientists" = "Accessible findings",
  "Publishing a peer-reviewed article" = "Peer-reviewed pub",
  "Publishing a detailed report" = "Detailed report",
  "Publishing for a public audience (e.g., blog post, website)" = "Public publication",
  "Communicating interim findings during the project (not just final publications)" = "Interim communication",
  "Active collaboration with community members throughout the project lifecycle" = "Active collaboration",
  "Traditional/local knowledge is integrated with scientific data" = "Traditional knowledge",
  "Community maintains ownership of data and findings" = "Community data ownership",
  "Collaborations continue beyond initial project" = "Continued collaboration",
  "Partnerships are equitable, collaborative, empowering, and address social inequalities" = "Equitable partnerships",
  "The community has meaningful say and control over the research process" = "Community control",
  "Community members are compensated for their time and effort" = "Compensation",
  "There is clear communication on how data and outcomes will be used" = "Clear communication",
  "Community members' viewpoints and perspectives are integrated into the project" = "Integrated perspectives"
)

# Reshape data to long format and filter for "Extremely important"
extremely_important_data <- roles_data %>%
  pivot_longer(
    cols = all_of(q6_cols),
    names_to = "question",
    values_to = "rating"
  ) %>%
  filter(rating == "Extremely important") %>%
  mutate(priority_short = NA_character_)  # Initialize column

# Match each question to its short label
for (long_name in names(priority_short_mapping)) {
  extremely_important_data <- extremely_important_data %>%
    mutate(
      priority_short = if_else(
        grepl(long_name, question, fixed = TRUE) & is.na(priority_short),
        priority_short_mapping[[long_name]],
        priority_short
      )
    )
}

# Filter out any that didn't match
extremely_important_data <- extremely_important_data %>%
  filter(!is.na(priority_short))

# Count extremely important ratings by role and priority
summary_table <- extremely_important_data %>%
  count(priority_short, role) %>%
  group_by(priority_short) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  rename(Priority = priority_short, Role = role, Count = n) %>%
  ungroup()

# Print summary
cat("\n=== EXTREMELY IMPORTANT RATINGS SUMMARY ===\n\n")
print(summary_table %>% arrange(Priority, desc(Count)), n = 50)

# Save summary table
write.csv(summary_table, "extremely_important_summary.csv", row.names = FALSE)

# Get unique roles and create angle mapping
unique_roles <- c("Academic", "Student", "Government", "Non-Profit")

# Assign specific positions
angle_mapping <- data.frame(
  Role = c("Academic", "Student", "Government", "Non-Profit"),
  angle = c(90, 0, 270, 180)
)

# Add any other roles
other_roles <- setdiff(unique(summary_table$Role), unique_roles)
if(length(other_roles) > 0) {
  other_angles <- seq(45, 315, length.out = length(other_roles))
  other_mapping <- data.frame(Role = other_roles, angle = other_angles)
  angle_mapping <- rbind(angle_mapping, other_mapping)
}

n_roles <- nrow(angle_mapping)

# Add angles to summary table
plot_data <- summary_table %>%
  left_join(angle_mapping, by = "Role")

# Get unique priorities
unique_priorities <- sort(unique(summary_table$Priority))

# Define color palette for ROLES
role_colors <- c(
  "Academic" = "#E41A1C",
  "Student" = "#377EB8", 
  "Government" = "#4DAF4A",
  "Non-Profit" = "#984EA3"
)

# Add any missing roles
all_roles <- unique(summary_table$Role)
missing_roles <- setdiff(all_roles, names(role_colors))
if(length(missing_roles) > 0) {
  additional_colors <- scales::hue_pal()(length(missing_roles))
  names(additional_colors) <- missing_roles
  role_colors <- c(role_colors, additional_colors)
}

# Create wind rose for each priority
windrose_plots <- list()

for (priority in unique_priorities) {
  priority_data <- plot_data %>% filter(Priority == priority)
  
  if(nrow(priority_data) == 0) next
  
  p <- ggplot(priority_data, aes(x = angle, y = Count, fill = Role)) +
    geom_bar(stat = "identity", width = 360/n_roles * 1) +
    coord_polar(start = 0) +
    scale_x_continuous(
      breaks = angle_mapping$angle,
      labels = NULL,
      limits = c(0, 360)
    ) +
    scale_y_continuous(breaks = NULL, limits = c(0, max(priority_data$Count) * 1.6)) +
    scale_fill_manual(values = role_colors, name = "Role") +
    labs(
      title = priority,
      y = "",
      x = ""
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
      plot.margin = margin(2, 2, 2, 2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  windrose_plots[[priority]] <- p
  
  safe_filename <- gsub("[^A-Za-z0-9]", "_", priority)
  
  ggsave(paste0("windrose_extremely_important_", safe_filename, ".png"), 
         p, width = 6, height = 6, dpi = 300)
}

# Create combined grid
n_priorities <- length(windrose_plots)
n_cols <- 3
n_rows <- ceiling(n_priorities / n_cols)

# Extract shared legend
dummy_plot <- ggplot(plot_data, aes(x = angle, y = Count, fill = Role)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = role_colors, name = "Role") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12))

shared_legend <- get_legend(dummy_plot)

# Create grid
grid_plots <- do.call(gridExtra::grid.arrange, c(windrose_plots, ncol = n_cols))

# Combine with legend
combined_plot <- gridExtra::grid.arrange(grid_plots, shared_legend, 
                                         nrow = 2, heights = c(50,2))

ggsave("windrose_extremely_important_combined.png", combined_plot, 
       width = n_cols * 6, height = n_rows * 6 + 1, dpi = 300)
