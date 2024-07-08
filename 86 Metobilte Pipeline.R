install.packages("dplyr") 
install.packages("tidyr") 
install.packages("tidyverse") 
install.packages("ggplot2") 
install.packages("plotly") 
install.packages("readxl")

library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(plotly) 
library(tidyverse) 
library(readr) 
library(readxl)

your_data <- readRDS("C:/Users/kamal/Downloads/metab_scaled.rds") 
your_data_df <- as.data.frame(your_data) 
your_data_dfnew <- your_data_df %>% rownames_to_column(var = "Metabolite")

excel_file <- "C:/Users/kamal/Downloads/demo_extracted_death_added_emr_added_vap_iss_added_physical_impaired_added_resilience_added_deidentified.xlsx"

data <- read_excel(excel_file)

selected_columns_data2 <- data[c("patient_id", "PaO2/FiO2", "COHORT", "sample_id", "sofa_max_within24hours_from_ICUadmission", "sample_group", "time_from_injury")]

long_data <- gather(your_data_dfnew, key = "sample_id", value = "value", starts_with("X"))

merged_data <- merge(long_data, selected_columns_data2, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)

####long_data <- long_data %>% rename(sample_id = 2)# Print the long-format data print(long_data)

str(merged_data)

data_less_than_3 <- merged_data[merged_data$time_from_injury < 3, ] 
data_3_to_6 <- merged_data[merged_data$time_from_injury >= 3 & merged_data$time_from_injury < 6, ] 
data_6_to_9 <- merged_data[merged_data$time_from_injury >= 6 & merged_data$time_from_injury < 9, ] 
data_9_to_12 <- merged_data[merged_data$time_from_injury >= 9 & merged_data$time_from_injury < 12, ] 
##data_0_6 <- merged_data[merged_data$time_from_injury < 6, ] 
##data_6_to_12 <- merged_data[merged_data$time_from_injury >= 6 & merged_data$time_from_injury < 12, ]


#Add time period column
data_less_than_3 <- data_less_than_3 %>% mutate(time_period = "< 3 days")
data_3_to_6 <- data_3_to_6 %>% mutate(time_period = "3 to 6 days")
data_6_to_9 <- data_6_to_9 %>% mutate(time_period = "6 to 9 days")
data_9_to_12 <- data_9_to_12 %>% mutate(time_period = "9 to 12 days")

#Combine all the above data 
combined_data <- bind_rows(data_less_than_3, data_3_to_6, data_6_to_9, data_9_to_12)

#Identify each unique metabolite
unique_metabolite1 <- unique(combined_data$Metabolite)

#generate plots for each metabolite over the time periods and combine in one visual of all 4 time points
generate_plots_for_combined_data <- function(data) { unique_metabolites <- unique(data$Metabolite)

for (metabolite_name in unique_metabolites) { subdata <- data %>% filter(Metabolite == metabolite_name)
p <- ggplot(subdata, aes(x = value, 
                         y = sofa_max_within24hours_from_ICUadmission, 
                         color = COHORT, 
                         linetype = COHORT)) +
  geom_point(shape = 16) +  # The points
  geom_smooth(method = "lm", se = TRUE) +  # Regression lines with confidence intervals
  scale_color_manual(values = c("ARDS" = "blue", "Non-ARDS" = "red")) +
  scale_linetype_manual(values = c("ARDS" = "solid", "Non-ARDS" = "dashed")) +
  labs(x = "Metabolite Levels Over Time", 
       y = "SOFA Score within 24 hours from ICU Admission", 
       title = paste("Correlation of", metabolite_name, "Levels Over Time with SOFA Score and ARDS Development"),
       color = "Cohort", 
       linetype = "Cohort") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +  # This will remove the legend title
  facet_wrap(~ time_period, scales = "free")

# Print the plot for each metabolite
print(p)
} 
}

generate_plots_for_combined_data(combined_data)


#Need to use na.omit because the above function gave errors due to NA values.

clean_data_less_than_3 <- na.omit(data_less_than_3) 
clean_data_3_to_6 <- na.omit(data_3_to_6) 
clean_data_6_to_9 <- na.omit(data_6_to_9) 
clean_data_9_to_12 <- na.omit(data_9_to_12)

# Add an identifying column
clean_data_less_than_3_tp <- clean_data_less_than_3 %>% mutate(time_period = "< 3 days") 
clean_data_3_to_6_tp <- clean_data_3_to_6 %>% mutate(time_period = "3 to 6 days") 
clean_data_6_to_9_tp <- clean_data_6_to_9 %>% mutate(time_period = "6 to 9 days") 
clean_data_9_to_12_tp <- clean_data_9_to_12 %>% mutate(time_period = "9 to 12 days")


#Combine the data
clean_combined_data <- bind_rows(clean_data_less_than_3_tp, clean_data_3_to_6_tp, clean_data_6_to_9_tp, clean_data_9_to_12_tp)

#identify unique metabolites
unique_metabolite <- unique(clean_combined_data$Metabolite)

#Saving plots as png files 
save_plots_for_combined_data <- function(data) { 
  unique_metabolites <- unique(data$Metabolite)
  
  for (metabolite_name in unique_metabolites) { subdata <- data %>% filter(Metabolite == metabolite_name)
  p <- ggplot(subdata, aes(x = value, 
                           y = sofa_max_within24hours_from_ICUadmission, 
                           color = COHORT, 
                           linetype = COHORT)) +
    geom_point(shape = 16) +  # The points
    geom_smooth(method = "lm", se = TRUE) +  # Regression lines with confidence intervals
    scale_color_manual(values = c("ARDS" = "blue", "Non-ARDS" = "red")) +
    scale_linetype_manual(values = c("ARDS" = "solid", "Non-ARDS" = "dashed")) +
    labs(x = "Metabolite Levels Over Time", 
         y = "SOFA Score within 24 hours from ICU Admission", 
         title = paste("Correlation of", metabolite_name, "Levels Over Time with SOFA Score and ARDS Development"),
         color = "Cohort", 
         linetype = "Cohort") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +  # This will remove the legend title
    facet_wrap(~ time_period, scales = "free")
  
  # Print the plot for each metabolite
  png(filename = paste0(metabolite_name, ".png"))
  print(p)  # This actually creates the plot in the file
  dev.off()  # This closes the device
  } 
}

save_plots_for_combined_data(clean_combined_data)



##############Regression Analysis#########################

# Separate the data into two cohorts: ARDS and Non-ARDS
data_ards <- clean_combined_data %>% filter(COHORT == "ARDS")
data_non_ards <- clean_combined_data %>% filter(COHORT == "Non-ARDS")

# Perform linear regression for each metabolite within each cohort and store results
results_ards <- data_ards %>%
  group_by(Metabolite) %>%
  do(tidy(lm(value ~ sofa_max_within24hours_from_ICUadmission, data = .)))

results_non_ards <- data_non_ards %>%
  group_by(Metabolite) %>%
  do(tidy(lm(value ~ sofa_max_within24hours_from_ICUadmission, data = .)))

# Filter results to keep only coefficients
coefficients_ards <- results_ards %>% filter(term == "sofa_max_within24hours_from_ICUadmission")
coefficients_non_ards <- results_non_ards %>% filter(term == "sofa_max_within24hours_from_ICUadmission")

# Add a column to identify the cohort
coefficients_ards <- coefficients_ards %>% mutate(COHORT = "ARDS")
coefficients_non_ards <- coefficients_non_ards %>% mutate(COHORT = "Non-ARDS")

# Combine the coefficients data
combined_coefficients <- bind_rows(coefficients_ards, coefficients_non_ards)

# Plot the coefficients using separate box plots for each cohort
ggplot(combined_coefficients, aes(x = Metabolite, y = estimate, fill = COHORT)) +
  geom_boxplot() +
  facet_wrap(~ COHORT) +
  labs(title = "Regression Coefficients of Metabolites vs. SOFA Score",
       x = "Metabolite",
       y = "Coefficient Estimate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot
ggsave("regression_coefficients_boxplot_by_cohort.png")

# Display the results
print(combined_coefficients)

### Using pvalue###
significant_ards <- combined_coefficients %>% filter(COHORT == "ARDS" & p.value < 0.05)
significant_non_ards <- combined_coefficients %>% filter(COHORT == "Non-ARDS" & p.value < 0.05)

print("Significant Metabolites in ARDS Cohort:")
print(significant_ards)

print("Significant Metabolites in Non-ARDS Cohort:")
print(significant_non_ards)

#####################################################

# Filter for strong relationships (|coefficient| >= 0.6 and p-value < 0.05)
strong_relationships_ards <- coefficients_ards %>% 
  filter(abs(estimate) >= 0.6 & p.value < 0.05)

strong_relationships_non_ards <- coefficients_non_ards %>% 
  filter(abs(estimate) >= 0.6 & p.value < 0.05)

print("Strong Relationships in ARDS Cohort:")
print(strong_relationships_ards)

print("Strong Relationships in Non-ARDS Cohort:")
print(strong_relationships_non_ards)

#########################################################################################################
install.packages("gridExtra")
library("gridExtra")

categorized_coefficients <- combined_coefficients %>%
  mutate(correlation = case_when(
    abs(estimate) >= 0.6 ~ "Strong",
    abs(estimate) >= 0.4 & abs(estimate) < 0.6 ~ "Moderate",
    abs(estimate) >= 0.2 & abs(estimate) < 0.4 ~ "Weak",
    abs(estimate) < 0.2 ~ "No"
  ))

# Display the categorized table
print("Categorized Correlations:")
print(categorized_coefficients)

# Save the categorized table as a CSV file
write.csv(categorized_coefficients, "categorized_correlations.csv", row.names = FALSE)

# Plot the categorized results using facet_wrap
plot <- ggplot(categorized_coefficients, aes(x = Metabolite, y = estimate, fill = correlation)) +
  geom_boxplot() +
  facet_wrap(~ COHORT) +
  labs(title = "Regression Coefficients for ARDS and Non-ARDS Cohorts",
       x = "Metabolite",
       y = "Coefficient Estimate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Display the plot
print(plot)
