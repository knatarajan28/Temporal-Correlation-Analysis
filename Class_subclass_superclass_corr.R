install.packages("dplyr") 
install.packages("tidyr") 
install.packages("tidyverse") 
install.packages("ggplot") 
install.packages("plotly") 
install.packages("readxl")

library(dplyr) 
library(tidyr) 
library(ggplot2) 
library(plotly) 
library(tidyverse) 
library(readr) 
library(readxl)

# Read the class CSV file
class_distinction_data <- read.csv("C:\\Users\\kamal\\Downloads\\Metabolites_Classification_Corrected.csv")

#Read the metab data
metab_data <- readRDS("C:/Users/kamal/Downloads/metab_scaled.rds") 
metab_data <- as.data.frame(metab_data) 
metab_data <- metab_data %>% rownames_to_column(var = "Metabolite")
long_data <- gather(metab_data, key = "sample_id", value = "value", starts_with("X"))

#Read Sofa data
sofa_data <- readRDS("C:/Users/kamal/Downloads/demo.rds")
all_data<- merge(long_data, sofa_data, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)
#write.csv(all_data, "all_data.csv", row.names = FALSE)

#Merge datasets based on selected columns
selected_columns_data <- sofa_data[c("patient_id", "s_ards", "sample_id", "sofa_max_within24hours_from_ICUadmission", "time_from_injury")]
merged_data <- merge(long_data, selected_columns_data, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)

# Removing NA values from the patient_id column
merged_data <- merged_data[!is.na(merged_data$patient_id), ]

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


#Checking for duplicate sample_ids for each patient_id
repeating_patients <- data_9_to_12 %>%
  group_by(patient_id, Metabolite, time_period) %>%
  filter(n() > 1) %>%
  arrange(patient_id, Metabolite)

# Print the results
print("Repeating patient IDs for the same metabolite:")
print(repeating_patients)


# Keeping first occurences
first_occurence_0_3 <- data_less_than_3 %>%
  group_by(patient_id, Metabolite, time_period) %>%
  slice(1) %>%
  ungroup()  

first_occurence_3_6 <- data_3_to_6 %>%
  group_by(patient_id, Metabolite, time_period) %>%
  slice(1) %>%
  ungroup() 


first_occurence_6_9 <- data_6_to_9 %>%
  group_by(patient_id, Metabolite, time_period) %>%
  slice(1) %>%
  ungroup() 


first_occurence_9_12 <- data_9_to_12 %>%
  group_by(patient_id, Metabolite, time_period) %>%
  slice(1) %>%
  ungroup() 


first_occurence_0_3 <- merge(class_distinction_data, first_occurence_0_3, by = "Metabolite") #for class correlation
first_occurence_3_6 <- merge(class_distinction_data, first_occurence_3_6, by = "Metabolite") #for class correlation
first_occurence_6_9 <- merge(class_distinction_data, first_occurence_6_9, by = "Metabolite") #for class correlation
first_occurence_9_12 <- merge(class_distinction_data, first_occurence_9_12, by = "Metabolite") #for class correlation


# Summing the 'value' columns grouped by 'patient_id', 'class' and 'time'
df_class_0_3 <-first_occurence_0_3  %>% 
  group_by(patient_id, Class, time_from_injury) %>% 
  summarise(
    total_value = sum(value, na.rm = TRUE),# Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_class_3_6 <- first_occurence_3_6  %>% 
  group_by(patient_id, Class, time_from_injury) %>% 
  summarise(
    total_value = sum(value, na.rm = TRUE),# Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_class_6_9 <- first_occurence_6_9  %>%
  group_by(patient_id, Class, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 


df_class_9_12 <- first_occurence_9_12  %>%
  group_by(patient_id, Class, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

#Subclass Distinction
df_subclass_0_3 <- first_occurence_0_3  %>%
  group_by(patient_id, Subclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_subclass_3_6 <- first_occurence_3_6  %>%
  group_by(patient_id, Subclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_subclass_6_9 <- first_occurence_6_9  %>%
  group_by(patient_id, Subclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_subclass_9_12 <- first_occurence_9_12  %>%
  group_by(patient_id, Subclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

#For superclass
df_superclass_0_3 <- first_occurence_0_3  %>%
  group_by(patient_id, Superclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_superclass_3_6 <- first_occurence_3_6  %>%
  group_by(patient_id, Superclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_superclass_6_9 <- first_occurence_6_9  %>%
  group_by(patient_id, Superclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 

df_superclass_9_12 <- first_occurence_9_12  %>%
  group_by(patient_id, Superclass, time_from_injury) %>%
  summarise(
    total_value = sum(value, na.rm = TRUE), # Sum of 'value' column, removing NA values
    SOFA_score = first(sofa_max_within24hours_from_ICUadmission) # Take the first encountered SOFA_score
  ) %>%
  ungroup() 


#Calculating the spearman correlations
calculate_spearman_correlations_for_superclasses <- function(data) {
  # Ensure that 'Superclass', 'total_value', and 'SOFA_score' are in the expected format
  if(!all(c('Superclass', 'total_value','SOFA_score') %in% names(data))) {
    stop("Data must include 'Superclass', 'total_value', and 'SOFA_score' columns")
  }
  
  results <- data %>%
    group_by(Superclass) %>%
    summarise(
      n = n(),  # Count the number of data points in each group
      Correlation = if(n() > 2) { 
        cor(total_value,SOFA_score, method = "spearman", use = "complete.obs")
      } else {
        NA_real_
      },
      P_value = if(n() > 2) {
        cor.test(total_value,SOFA_score, method = "spearman", exact = FALSE)$p.value
      } else {
        NA_real_
      },
      .groups = 'drop'  # Drop grouping for returned dataframe
    ) %>%
    filter(n > 2)  # Optionally remove groups with insufficient data for correlation analysis
  
  return(results)
}


superclasscorr_0_3 <- calculate_spearman_correlations_for_superclasses(df_superclass_0_3)
superclasscorr_3_6 <- calculate_spearman_correlations_for_superclasses(df_superclass_3_6)
superclasscorr_6_9 <- calculate_spearman_correlations_for_superclasses(df_superclass_6_9)
superclasscorr_9_12 <- calculate_spearman_correlations_for_superclasses(df_superclass_9_12)

calculate_spearman_correlations_for_classes <- function(data) {
  # Ensure that 'Class', 'total_value', and 'SOFA_score' are in the expected format
  if(!all(c('Class', 'total_value','SOFA_score') %in% names(data))) {
    stop("Data must include 'Class', 'total_value', and 'SOFA_score' columns")
  }
  
  results <- data %>%
    group_by(Class) %>%
    summarise(
      n = n(),  # Count the number of data points in each group
      Correlation = if(n() > 2) { 
        cor(total_value,SOFA_score, method = "spearman", use = "complete.obs")
      } else {
        NA_real_
      },
      P_value = if(n() > 2) {
        cor.test(total_value,SOFA_score, method = "spearman", exact = FALSE)$p.value
      } else {
        NA_real_
      },
      .groups = 'drop'  # Drop grouping for returned dataframe
    ) %>%
    filter(n > 2)  # Optionally remove groups with insufficient data for correlation analysis
  
  return(results)
}

# For class correlations
class_corr_0_3 <- calculate_spearman_correlations_for_classes(df_class_0_3)
class_corr_3_6 <- calculate_spearman_correlations_for_classes(df_class_3_6)
class_corr_6_9 <- calculate_spearman_correlations_for_classes(df_class_6_9)
class_corr_9_12 <- calculate_spearman_correlations_for_classes(df_class_9_12)


calculate_spearman_correlations_for_subclasses <- function(data) {
  # Ensure that 'Subclass', 'total_value', and 'SOFA_score' are in the expected format
  if(!all(c('Subclass', 'total_value', 'SOFA_score') %in% names(data))) {
    stop("Data must include 'Subclass', 'value', and 'sofa_max_within24hours_from_ICUadmission' columns")
  }
  
  results <- data %>%
    group_by(Subclass) %>%
    summarise(
      n = n(),  # Count the number of data points in each group
      Correlation = if(n() > 2) { 
        cor(total_value, SOFA_score, method = "spearman", use = "complete.obs")
      } else {
        NA_real_
      },
      P_value = if(n() > 2) {
        cor.test(total_value, SOFA_score, method = "spearman", exact = FALSE)$p.value
      } else {
        NA_real_
      },
      .groups = 'drop'  # Drop grouping for returned dataframe
    ) %>%
    filter(n > 2)  # Optionally remove groups with insufficient data for correlation analysis
  
  return(results)
}
subclass_corr_0_3 <- calculate_spearman_correlations_for_subclasses(df_subclass_0_3)
subclass_corr_3_6 <- calculate_spearman_correlations_for_subclasses(df_subclass_3_6)
subclass_corr_6_9 <- calculate_spearman_correlations_for_subclasses(df_subclass_6_9)
subclass_corr_9_12 <- calculate_spearman_correlations_for_subclasses(df_subclass_9_12)


#Merging correlation results with original datasets
df_subclass_0_3_merged <- inner_join(df_subclass_0_3, subclass_corr_0_3, by.x = "Subclass", by.y = "Subclass", all.x = TRUE)
df_subclass_3_6_merged <- inner_join(df_subclass_3_6, subclass_corr_3_6, by.x = "Subclass", by.y = "Subclass", all.x = TRUE)
df_subclass_6_9_merged <- inner_join(df_subclass_6_9, subclass_corr_6_9, by.x = "Subclass", by.y = "Subclass", all.x = TRUE)
df_subclass_9_12_merged <- inner_join(df_subclass_9_12, subclass_corr_9_12, by.x = "Subclass", by.y = "Subclass", all.x = TRUE)

merged_data <- inner_join(df1, df2, by = "Class")

df_class_0_3_merged <- inner_join(df_class_0_3, class_corr_0_3, by = "Class")
df_class_3_6_merged <- inner_join(df_class_3_6, class_corr_3_6, by = "Class")
df_class_0_3_merged <- inner_join(df_class_6_9, class_corr_6_9, by = "Class")
df_class_0_3_merged <- inner_join(df_class_9_12, class_corr_9_12, by = "Class")

df_superclass_0_3_merged <- inner_join(df_superclass_0_3, superclasscorr_0_3, by.x = "Superclass", by.y = "Superclass", all.x = TRUE)
df_superclass_3_6_merged <- inner_join(df_superclass_3_6, superclasscorr_3_6, by.x = "Superclass", by.y = "Superclass", all.x = TRUE)
df_superclass_6_9_merged <- inner_join(df_superclass_6_9, superclasscorr_6_9, by.x = "Superclass", by.y = "Superclass", all.x = TRUE)
df_superclass_9_12_merged <- inner_join(df_superclass_9_12, superclasscorr_9_12, by.x = "Superclass", by.y = "Superclass", all.x = TRUE)

#Box plots

#First get list of metabolites for each class
metabolites_by_class <- class_distinction_data %>%
  group_by(Class) %>%
  summarise(Combined_Metabolites = paste(unique(Metabolite), collapse = ", "),
            .groups = 'drop')  # Drop the grouping


metabolites_by_superclass <- class_distinction_data %>%
  group_by(Superclass) %>%
  summarise(Combined_Metabolites = paste(unique(Metabolite), collapse = ", "),
            .groups = 'drop')  # Drop the grouping

metabolites_by_subclass <- class_distinction_data %>%
  group_by(Subclass) %>%
  summarise(Combined_Metabolites = paste(unique(Metabolite), collapse = ", "),
            .groups = 'drop')  # Drop the grouping

# Merge with filtered_data to ensure all classes are covered
df_class_0_3 <- df_class_0_3   %>%
  left_join(metabolites_by_class, by = "Class")
df_class_3_6 <- df_class_3_6   %>%
  left_join(metabolites_by_class, by = "Class")
df_class_6_9 <- df_class_6_9   %>%
  left_join(metabolites_by_class, by = "Class")
df_class_9_12 <- df_class_9_12   %>%
  left_join(metabolites_by_class, by = "Class")


# Filter out the class named 'nil'
df_class_0_3 <- df_class_0_3 %>%
  filter(Class != "nil")
df_class_3_6 <- df_class_3_6 %>%
  filter(Class != "nil")
df_class_6_9 <- df_class_6_9 %>%
  filter(Class != "nil")
df_class_9_12 <- df_class_9_12 %>%
  filter(Class != "nil")

box_plot <- ggplot(df_class_0_3  , aes(x = SOFA_score, y = total_value, fill = Class)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Classes",
       subtitle = "0-3 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)
print(metabolites_by_class)

#Boxplots
box_plot <- ggplot(df_class_3_6, aes(x = SOFA_score, y = total_value, fill = Class)) +
  geom_boxplot() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Classes",
       subtitle = "3-6 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_class_6_9, aes(x = SOFA_score, y = total_value, fill = Class)) +
  geom_boxplot() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Classes",
       subtitle = "6-9 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_class_9_12 , aes(x = SOFA_score, y = total_value, fill = Class)) +
  geom_boxplot() +
theme(axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Classes",
       subtitle = "9-12 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)


# For Subclasses
df_subclass_0_3 <- df_subclass_0_3   %>%
  left_join(metabolites_by_subclass, by = "Subclass")
df_subclass_3_6 <- df_subclass_3_6   %>%
  left_join(metabolites_by_subclass, by = "Subclass")
df_subclass_6_9 <- df_subclass_6_9   %>%
  left_join(metabolites_by_subclass, by = "Subclass")
df_subclass_9_12 <- df_subclass_9_12   %>%
  left_join(metabolites_by_subclass, by = "Subclass")


# Filter out the class named 'nil'
df_subclass_0_3 <- df_subclass_0_3 %>%
  filter(Subclass != "nil")
df_subclass_3_6 <- df_subclass_3_6 %>%
  filter(Subclass != "nil")
df_subclass_6_9 <- df_subclass_6_9 %>%
  filter(Subclass != "nil")
df_subclass_9_12 <- df_subclass_9_12 %>%
  filter(Subclass != "nil")

#Boxplots

box_plot <- ggplot(df_subclass_0_3, aes(x = SOFA_score, y = total_value, fill = Subclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Subclasses",
       subtitle = "0-3 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)
print(metabolites_by_subclass)

box_plot <- ggplot(df_subclass_3_6, aes(x = SOFA_score, y = total_value, fill = Subclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Subclasses",
       subtitle = "3-6 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_subclass_6_9, aes(x = SOFA_score, y = total_value, fill = Subclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Subclasses",
       subtitle = "6-9 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_subclass_9_12, aes(x = SOFA_score, y = total_value, fill = Subclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Subclasses",
       subtitle = "9-12 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)


#For Superclasses

# Merge with filtered_data to ensure all classes are covered
df_superclass_0_3_merged <- df_superclass_0_3   %>%
  inner_join(metabolites_by_superclass, by = "Superclass")
df_superclass_3_6_merged <- df_superclass_3_6   %>%
  inner_join(metabolites_by_superclass, by = "Superclass")
df_superclass_6_9_merged <- df_superclass_6_9   %>%
  inner_join(metabolites_by_superclass, by = "Superclass")
df_superclass_9_12_merged <- df_superclass_9_12   %>%
  inner_join(metabolites_by_superclass, by = "Superclass")

# Filter out the class named 'nil'
df_superclass_0_3 <- df_superclass_0_3   %>%
  filter(Superclass != "nil")
df_superclass_3_6 <- df_superclass_3_6   %>%
  filter(Superclass != "nil")
df_superclass_6_9 <- df_superclass_6_9   %>%
  filter(Superclass != "nil")
df_superclass_9_12 <- df_superclass_9_12   %>%
  filter(Superclass != "nil")

#Boxplots

box_plot <- ggplot(df_superclass_0_3, aes(x = SOFA_score, y = total_value, fill = Superclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Superclasses",
       subtitle = "0-3 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)
print(metabolites_by_superclass)

box_plot <- ggplot(df_superclass_3_6, aes(x = SOFA_score, y = total_value, fill = Superclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Superclasses",
       subtitle = "3-6 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_superclass_6_9, aes(x = SOFA_score, y = total_value, fill = Superclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Superclasses",
       subtitle = "6-9 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)

box_plot <- ggplot(df_superclass_9_12, aes(x = SOFA_score, y = total_value, fill = Superclass)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Box Plot of Summed Concentrations Across Superclasses",
       subtitle = "9-12 Days",
       x = "SOFA Score",
       y = "Summed Concentration",
       caption = "Metabolite information is available in the dataset summary.") +
  scale_fill_viridis_d()

print(box_plot)


#Scatterplots

# Assuming df_superclass_corr stores the correlation results
significant_superclasses_0_3 <- superclasscorr_0_3[superclasscorr_0_3$P_value < 0.05, ]
significant_superclasses_3_6 <- superclasscorr_3_6[superclasscorr_3_6$P_value < 0.05, ]
significant_superclasses_6_9 <- superclasscorr_6_9[superclasscorr_6_9$P_value < 0.05, ]
significant_superclasses_9_12 <- superclasscorr_9_12[superclasscorr_9_12$P_value < 0.05, ]

#Classes
significant_classes_0_3 <- class_corr_0_3[class_corr_0_3$P_value < 0.05, ]
significant_classes_3_6 <- class_corr_3_6[class_corr_3_6$P_value < 0.05, ]
significant_classes_6_9 <- class_corr_6_9[class_corr_6_9$P_value < 0.05, ]
significant_classes_9_12 <- class_corr_9_12[class_corr_9_12$P_value < 0.05, ]

#Subclasses
significant_subclasses_0_3 <- subclass_corr_0_3[subclass_corr_0_3$P_value < 0.05, ]
significant_subclasses_3_6 <- subclass_corr_3_6[subclass_corr_3_6$P_value < 0.05, ]
significant_subclasses_6_9 <- subclass_corr_6_9[subclass_corr_6_9$P_value < 0.05, ]
significant_subclasses_9_12 <- subclass_corr_9_12[subclass_corr_9_12$P_value < 0.05, ]


#Scatter plot: Classes

pdf("scatter_plots_9_12_days_unique_class.pdf", width = 35, height = 14)


# Get unique classes that are significant
unique_classes <- unique(significant_classes_9_12$Class)

# Iterate through each class
for (class in unique_classes) {
 
  df_subset <- df_class_9_12 %>%
    filter(Class == class)
  
  # Check if there is data in the subset
  if(nrow(df_subset) > 0) {
    plot <- ggplot(df_subset, aes(x = total_value, y = SOFA_score)) +
      geom_point(size = 3, color = "blue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
      stat_cor(method = "spearman", 
               label.x = max(df_subset$total_value, na.rm = TRUE),  # Position at max value of x for visibility
               label.y = max(df_subset$SOFA_score, na.rm = TRUE),   # Position at max value of y
               label.sep = ", ") +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", class, "vs SOFA Score 9-12 Days"),  # Reflect the correct time period
           x = "Summed Value",
           y = "SOFA Score") +
      theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Center the title
            axis.title = element_text(size = 19))
    
    print(plot)
  } else {
    message("No data found for class: ", class)
  }
}

# Close the PDF device
dev.off()


##############################


#Scatter plot: Superclasses


pdf("scatter_plots_6_9_days_unique_superclass.pdf", width = 35, height = 14)

# Get unique superclasses that are significant
unique_superclasses <- unique(significant_superclasses_6_9$Superclass)

# Iterate through each superclass
for (superclass in unique_superclasses) {  # Use 'superclass' here to match the loop variable
  
  df_subset <- df_superclass_6_9 %>%
    filter(Superclass == superclass)  # Ensure case sensitivity is correct
  
  # Check if there is data in the subset
  if(nrow(df_subset) > 0) {
    plot <- ggplot(df_subset, aes(x = total_value, y = SOFA_score)) +
      geom_point(size = 3, color = "blue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
      stat_cor(method = "spearman", 
               label.x = max(df_subset$total_value, na.rm = TRUE),  # Position at max value of x for visibility
               label.y = max(df_subset$SOFA_score, na.rm = TRUE),   # Position at max value of y
               label.sep = ", ") +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", superclass, "vs SOFA Score 6-9 Days"),  # Use the loop variable in title
           x = "Summed Value",
           y = "SOFA Score") +
      theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Center the title
            axis.title = element_text(size = 19))
    
    print(plot)
  } else {
    message("No data found for Superclass: ", superclass)
  }
}

# Close the PDF device
dev.off()


##############################


#Scatter plot: Subclasses


pdf("scatter_plots_9_12_days_unique_subclass.pdf", width = 35, height = 14)

# Get unique subclasses that are significant
unique_subclasses <- unique(significant_subclasses_9_12$Subclass)

# Iterate through each superclass
for (subclass in unique_subclasses) {  
  
  df_subset <- df_subclass_9_12 %>%
    filter(Subclass == subclass)  # Ensure case sensitivity is correct
  
  # Check if there is data in the subset
  if(nrow(df_subset) > 0) {
    plot <- ggplot(df_subset, aes(x = total_value, y = SOFA_score)) +
      geom_point(size = 3, color = "blue") +
      geom_smooth(method = "lm", color = "red", se = FALSE) +  # Linear regression line
      stat_cor(method = "spearman", 
               label.x = max(df_subset$total_value, na.rm = TRUE),  # Position at max value of x for visibility
               label.y = max(df_subset$SOFA_score, na.rm = TRUE),   # Position at max value of y
               label.sep = ", ") +
      theme_minimal() +
      labs(title = paste("Scatter Plot of", subclass, "vs SOFA Score 9-12 Days"),  # Use the loop variable in title
           x = "Summed Value",
           y = "SOFA Score") +
      theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Center the title
            axis.title = element_text(size = 19))
    
    print(plot)
  } else {
    message("No data found for Subclass: ", subclass)
  }
}

# Close the PDF device
dev.off()



################################### Benjamin Correction (Not Used)#####################################################
# Applying Benjamini-Hochberg correction to the p-values
subclass_corr_0_3$P_value_adjusted <- p.adjust(subclass_corr_0_3$P_value, method = "BH")
subclass_corr_3_6$P_value_adjusted <- p.adjust(subclass_corr_3_6$P_value, method = "BH")
subclass_corr_6_9$P_value_adjusted <- p.adjust(subclass_corr_6_9$P_value, method = "BH")

# Filtering datasets to include only significant results after correction
significant_subclasses_0_3 <- subclass_corr_0_3[subclass_corr_0_3$P_value_adjusted < 0.05, ]
significant_subclasses_3_6 <- subclass_corr_3_6[subclass_corr_3_6$P_value_adjusted < 0.05, ]
significant_subclasses_6_9 <- subclass_corr_6_9[subclass_corr_6_9$P_value_adjusted < 0.05, ]

#For superclasses 
# Applying Benjamini-Hochberg correction to the p-values
superclasscorr_0_3$P_value_adjusted <- p.adjust(superclasscorr_0_3$P_value, method = "BH")
superclasscorr_3_6$P_value_adjusted <- p.adjust(superclasscorr_3_6$P_value, method = "BH")
superclasscorr_6_9$P_value_adjusted <- p.adjust(superclasscorr_6_9$P_value, method = "BH")

# Filtering datasets to include only significant results after correction
significant_superclasses_0_3 <- superclasscorr_0_3[superclasscorr_0_3$P_value_adjusted < 0.05, ]
significant_superclasses_3_6 <- superclasscorr_3_6[superclasscorr_3_6$P_value_adjusted < 0.05, ]
significant_superclasses_6_9 <- superclasscorr_6_9[superclasscorr_6_9$P_value_adjusted < 0.05, ]

#For Classes
# Applying Benjamini-Hochberg correction to the p-values
class_corr_0_3$P_value_adjusted <- p.adjust(class_corr_0_3$P_value, method = "BH")
class_corr_3_6$P_value_adjusted <- p.adjust(class_corr_3_6$P_value, method = "BH")
class_corr_6_9$P_value_adjusted <- p.adjust(class_corr_6_9$P_value, method = "BH")

# Filtering datasets to include only significant results after correction
significant_classes_0_3 <- class_corr_0_3[class_corr_0_3$P_value_adjusted < 0.05, ]
significant_classes_3_6 <- class_corr_3_6[class_corr_3_6$P_value_adjusted < 0.05, ]
significant_classes_6_9 <- class_corr_6_9[class_corr_6_9$P_value_adjusted < 0.05, ]
