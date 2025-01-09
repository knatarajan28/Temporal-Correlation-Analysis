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


your_data <- readRDS("C:/Users/kamal/Downloads/metab_scaled.rds") 
your_data <- as.data.frame(your_data) 
your_data <- your_data %>% rownames_to_column(var = "Metabolite")
sofa_data <- readRDS("C:/Users/kamal/Downloads/demo.rds")
all_data<- merge(long_data, sofa_data, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)
write.csv(all_data, "all_data.csv", row.names = FALSE)
selected_columns_data <- sofa_data[c("patient_id", "s_ards", "sample_id", "sofa_max_within24hours_from_ICUadmission", "time_from_injury")]

long_data <- gather(your_data, key = "sample_id", value = "value", starts_with("X"))

merged_data <- merge(long_data, selected_columns_data, by.x = "sample_id", by.y = "sample_id", all.x = TRUE)

####long_data <- long_data %>% rename(sample_id = 2)# Print the long-format data print(long_data)
# Merge the two data frames based on the "Metabolite" column
# Assuming 'class_data' and 'another_data' are your data frames and both contain the column "Metabolite"
merged_data <- merge(class_data, merged_data, by = "Metabolite") #for class correlation



data_less_than_3 <- merged_data[merged_data$time_from_injury < 3, ] 
data_3_to_6 <- merged_data[merged_data$time_from_injury >= 3 & merged_data$time_from_injury < 6, ] 
data_6_to_9 <- merged_data[merged_data$time_from_injury >= 6 & merged_data$time_from_injury < 9, ] 
data_9_to_12 <- merged_data[merged_data$time_from_injury >= 9 & merged_data$time_from_injury < 12, ] 
##data_0_6 <- merged_data[merged_data$time_from_injury < 6, ] 
##data_6_to_12 <- merged_data[merged_data$time_from_injury >= 6 & merged_data$time_from_injury < 12, ]



# using na.omit only on one column
clean_data_less_than_3 <- data_less_than_3[!is.na(data_less_than_3$sofa_max_within24hours_from_ICUadmission), ]
# For data_3_to_6
clean_data_3_to_6 <- data_3_to_6[!is.na(data_3_to_6$sofa_max_within24hours_from_ICUadmission), ]
# For data_6_to_9
clean_data_6_to_9 <- data_6_to_9[!is.na(data_6_to_9$sofa_max_within24hours_from_ICUadmission), ]
# For data_9_to_12
clean_data_9_to_12 <- data_9_to_12[!is.na(data_9_to_12$sofa_max_within24hours_from_ICUadmission), ]

#Add time period column
data_less_than_3 <- data_less_than_3 %>% mutate(time_period = "< 3 days")
data_3_to_6 <- data_3_to_6 %>% mutate(time_period = "3 to 6 days")
data_6_to_9 <- data_6_to_9 %>% mutate(time_period = "6 to 9 days")
data_9_to_12 <- data_9_to_12 %>% mutate(time_period = "9 to 12 days")

#Combine the data
clean_combined_data <- bind_rows(clean_data_less_than_3, clean_data_3_to_6, clean_data_6_to_9, clean_data_9_to_12)

#identify unique metabolites
unique_metabolite <- unique(clean_combined_data$Metabolite)

##############remove any duplicates if there#########
repeating_patients <- clean_combined_data %>%
  group_by(patient_id, Metabolite, time_from_injury) %>%
  filter(n() > 1) %>%
  arrange(patient_id, Metabolite)

# Print the results
print("Repeating patient IDs for the same metabolite:")
print(repeating_patients)


#Checking for duplicate sample_ids for each patient_id
repeating_patients <- first_occurence_9_12%>%
  group_by(patient_id, Metabolite, time_period) %>%
  filter(n() > 1) %>%
  arrange(patient_id, Metabolite)

# Print the results
print("Repeating patient IDs for the same metabolite:")
print(repeating_patients)

#Keep First Ocuurences
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

generate_plots_for_combined_data <- function(data) {

  unique_metabolites <- unique(data$Metabolite)
  
  for (metabolite_name in unique_metabolites) {
    subdata <- data %>% filter(Metabolite == metabolite_name)
    
    # Print diagnostic messages
    cat("Processing metabolite:", metabolite_name, "\n")
    cat("Unique values in s_ards:", unique(subdata$s_ards), "\n")
    
    p <- ggplot(subdata, aes(x = value, 
                             y = sofa_max_within24hours_from_ICUadmission, 
                             color = s_ards, 
                             linetype = s_ards)) +
      geom_point(shape = 16) +  # The points
      geom_smooth(method = "lm", se = TRUE) +  # Regression lines with confidence intervals
      scale_color_manual(values = c("ards" = "blue", "non_ards" = "red")) +
      scale_linetype_manual(values = c("ards" = "solid", "non_ards" = "dashed")) +
      labs(x = "Metabolite Levels Over Time", 
           y = "SOFA Score within 24 hours from ICU Admission", 
           title = paste("Correlation of", metabolite_name, "Levels Over Time with SOFA Score and ARDS Development"),
           color = "Cohort", 
           linetype = "Cohort") +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +  # This will remove the legend title
      facet_wrap(~ time_period, scales = "free")
    
    print(p)
  }
}

# clean_combined_data is the data frame
generate_plots_for_combined_data(clean_combined_data)


# Function to calculate Spearman correlation and p-value for each metabolite
calculate_spearman_correlations_for_pathways <- function(data) {
  unique_metabolites <- unique(data$Metabolite)
  results <- data.frame(Metabolite = character(),
                        Correlation = numeric(),
                        P_value = numeric(),
                        stringsAsFactors = FALSE)
  
  for (metabolite_name in unique_metabolites) {
    subdata <- data %>% filter(Metabolite == metabolite_name)
    
    if (nrow(subdata) > 2) {  # Need at least 3 points to calculate correlation
      correlation_test <- cor.test(subdata$value, subdata$sofa_max_within24hours_from_ICUadmission, method = "spearman")
      results <- rbind(results, data.frame(Metabolite = metabolite_name,
                                           Correlation = correlation_test$estimate,
                                           P_value = correlation_test$p.value))
    }
  }
  
  return(results)
}

# no distinction of cohorts
corr_less_than_3 <- calculate_spearman_correlations_for_pathways(first_occurence_0_3)
corr_3_to_6 <-calculate_spearman_correlations_for_pathways(first_occurence_3_6)
corr_6_to_9 <- calculate_spearman_correlations_for_pathways(first_occurence_6_9)
corr_9_to_12 <- calculate_spearman_correlations_for_pathways(first_occurence_9_12)


# Identify significant metabolites (e.g., p-value < 0.05)
sigmetab3s<- corr_less_than_3%>%filter(P_value <0.05)
sigmetab3to6s<- corr_3_to_6%>%filter(P_value <0.05)
sigmetab6to9s<- corr_6_to_9%>%filter(P_value <0.05)
sigmetab9to12s<- corr_9_to_12%>%filter(P_value <0.05)

#No distinction based on cohorts
create_regression_results <- function(data) {
  
  unique_metabolites <- unique(data$Metabolite)
  results <- data.frame(Metabolite = character(),
                        Intercept = numeric(),
                        Slope = numeric(),
                        P_value = numeric(),
                        stringsAsFactors = FALSE)
  
  for (metabolite_name in unique_metabolites) {
    subdata <- data %>% filter(Metabolite == metabolite_name)
    
    if (nrow(subdata) > 2) {  # Ensure there are enough data points
      model <- lm(sofa_max_within24hours_from_ICUadmission ~ value, data = subdata)
      summary_model <- summary(model)
      coef_model <- coef(summary_model)
      
      intercept <- coef_model[1, "Estimate"]
      slope <- coef_model[2, "Estimate"]
      p_value <- coef_model[2, "Pr(>|t|)"]
      
      # Append results to dataframe
      results <- rbind(results, data.frame(Metabolite = metabolite_name,
                                           Intercept = intercept,
                                           Slope = slope,
                                           P_value = p_value,
                                           stringsAsFactors = FALSE))
    }
  }
  
  # Return the results dataframe
  return(results)
}

regression_resultscorr3 <- create_regression_results(clean_data_less_than_3)
regression_resultscorr3to6 <- create_regression_results(clean_data_3_to_6)
regression_resultscorr6to9 <- create_regression_results(clean_data_6_to_9)
regression_resultscorr9to12 <- create_regression_results(clean_data_9_to_12)

significant_metabolitesreg3 <- filter_significant_metabolites(regression_resultscorr3, significance_threshold)
significant_metabolitesreg3to6 <- filter_significant_metabolites(regression_resultscorr3to6, significance_threshold)
significant_metabolites3reg6to9 <- filter_significant_metabolites(regression_resultscorr6to9, significance_threshold)
significant_metabolitesreg9to12 <- filter_significant_metabolites(regression_resultscorr9to12, significance_threshold)


#Distinction based on cohort
create_regression_results_cohort <- function(data) {
  
  unique_metabolites <- unique(data$Metabolite)
  results <- data.frame(Metabolite = character(),
                        Cohort = character(),
                        Intercept = numeric(),
                        Slope = numeric(),
                        P_value = numeric(),
                        stringsAsFactors = FALSE)
  
  for (metabolite_name in unique_metabolites) {
    subdata <- data %>% filter(Metabolite == metabolite_name)
    
    # Fit linear models and extract coefficients for each cohort
    for (cohort in unique(subdata$s_ards)) {
      cohort_data <- subdata %>% filter(s_ards == cohort)
      if (nrow(cohort_data) > 2) {  # Ensure there are enough data points
        model <- lm(sofa_max_within24hours_from_ICUadmission ~ value, data = cohort_data)
        summary_model <- summary(model)
        coef_model <- coef(summary_model)
        
        intercept <- coef_model[1, "Estimate"]
        slope <- coef_model[2, "Estimate"]
        p_value <- coef_model[2, "Pr(>|t|)"]
        
        # Append results to dataframe
        results <- rbind(results, data.frame(Metabolite = metabolite_name,
                                             Cohort = cohort,
                                             Intercept = intercept,
                                             Slope = slope,
                                             P_value = p_value,
                                             stringsAsFactors = FALSE))
      }
    }
  }
  
  # Return the results dataframe
  return(results)
}

# Performing Regression anlysis on the different datasets
regression_results1a <- create_regression_results(ards_data1)
regression_results2a <- create_regression_results(ards_data2)
regression_results3a <- create_regression_results(ards_data3)
regression_results4a <- create_regression_results(ards_data4)
regression_results1n <- create_regression_results(non_ards_data1)
regression_results2n <- create_regression_results(non_ards_data2)
regression_results3n <- create_regression_results(non_ards_data3)
regression_results4n <- create_regression_results(non_ards_data4)


# Setting significance threshold
significance_threshold <- 0.01

filter_significant_metabolites <- function(regression_results, threshold) {
  significant_metabolites <- regression_results %>%
    filter(P_value < threshold)
  return(significant_metabolites)
}

# Filter significant metabolites for each dataset seperated by cohort
significant_metabolites1ar <- filter_significant_metabolites(regression_results1a, significance_threshold)
significant_metabolites2ar <- filter_significant_metabolites(regression_results2a, significance_threshold)
significant_metabolites3ar <- filter_significant_metabolites(regression_results3a, significance_threshold)
significant_metabolites4ar <- filter_significant_metabolites(regression_results4a, significance_threshold)
significant_metabolites1nr <- filter_significant_metabolites(regression_results1n, significance_threshold)
significant_metabolites2nr <- filter_significant_metabolites(regression_results2n, significance_threshold)
significant_metabolites3nr <- filter_significant_metabolites(regression_results3n, significance_threshold)
significant_metabolites4nr <- filter_significant_metabolites(regression_results4n, significance_threshold)


###Histogram###

# Histogram of SOFA score

patient_id_unique <- clean_combined_data %>%
  distinct(patient_id, .keep_all = TRUE)

# Create a histogram of SOFA scores for unique patients
ggplot(patient_id_unique, aes(x = sofa_max_within24hours_from_ICUadmission)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of SOFA Scores", x = "SOFA Score", y = "Frequency")

sofa_median <- median(patient_id_unique$sofa_max_within24hours_from_ICUadmission, na.rm = TRUE)

# Print the median
print(paste("The median SOFA score is:", sofa_median))


#Concentration of Metabolite over time
unique_metabolites <- unique(clean_combined_data$Metabolite)

# Save all plots to a single PDF
pdf("metabolite_concentration_plots.pdf", width = 8, height = 6)

# Loop through each unique metabolite and generate a plot
for (metabolite in unique_metabolites) {
  
  # Subset the data for the current metabolite
  metabolite_data <- subset(clean_combined_data, Metabolite == metabolite)
  
  # Generate the plot
  p <- ggplot(metabolite_data, aes(x = time_from_injury, y = value, color = as.factor(sofa_max_within24hours_from_ICUadmission), shape = s_ards)) +
    geom_point(size = 3) + # Add points
    geom_line(aes(group = patient_id), size = 1) + # Add lines connecting points for each patient
    scale_color_discrete(name = "SOFA Score") +
    labs(title = paste("Concentration of", metabolite, "Over Time"), 
         x = "Time from Injury (days)", 
         y = "Concentration", 
         shape = "ARDS Status") +
    theme_minimal() +
    theme(legend.position = "right") # Position the legend
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF file
dev.off()

# Print a message indicating that the plots have been saved
cat("All metabolite concentration plots have been saved to 'metabolite_concentration_plots.pdf'.\n")
cat(unique_metabolites)


### Violin plots
# Get unique metabolites from the dataset
unique_metabolites <- unique(clean_combined_data$Metabolite)
# Save all plots to a single PDF
pdf("metabolite_box_violin_plots.pdf", width = 8, height = 6)

# Loop through each unique metabolite and generate both box plots and violin plots
for (metabolite in unique_metabolites) {
  
  # Subset the data for the current metabolite
  metabolite_data <- subset(clean_combined_data, Metabolite == metabolite)
  
  # Create Box Plot
  p_box <- ggplot(metabolite_data, aes(x = time_from_injury, y = value, fill = as.factor(sofa_max_within24hours_from_ICUadmission))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", metabolite, "Over Time"), 
         x = "Time from Injury (days)", 
         y = "Concentration", 
         fill = "SOFA Score") +
    theme_minimal() +
    theme(legend.position = "right") # Position the legend
  
  # Print the box plot to the PDF
  print(p_box)
  
  # Create Violin Plot
  p_violin <- ggplot(metabolite_data, aes(x = time_from_injury, y = value, fill = as.factor(sofa_max_within24hours_from_ICUadmission))) +
    geom_violin(trim = FALSE) + # Create violin plot
    labs(title = paste("Violin Plot of", metabolite, "Over Time"), 
         x = "Time from Injury (days)", 
         y = "Concentration", 
         fill = "SOFA Score") +
    theme_minimal() +
    theme(legend.position = "right") # Position the legend
  
  # Print the violin plot to the PDF
  print(p_violin)
}

# Close the PDF file
dev.off()

# Print a message indicating that the plots have been saved
cat("All metabolite box and violin plots have been saved to 'metabolite_box_violin_plots.pdf'.\n")


# Using base R
hist(clean_combined_data$value, main="Histogram of Data", xlab="Values")

# Using ggplot2
library(ggplot2)
ggplot(data, aes(x=value)) + geom_histogram(bins=30, fill="blue", color="black") + labs(title="Histogram of Data", x="Values", y="Frequency")



###############################################################################################

# Extract the 'metabolite' column from clean_data_less_than_3
metabolite_data <- unique(clean_data_less_than_3["Metabolite"])

# Save it as a new dataset (CSV file)
write.csv(metabolite_data, "metabolite_data.csv", row.names = FALSE)

# Optionally, view the new dataset in R
View(metabolite_data)

# Create a mapping table from the provided information
kegg_mapping <- data.frame(
  Metabolite = c(
    "2-Aminophenol", "5-methyluridine", "2-Aminoisobutyric Acid", "(R)-2,3-Dihydroxy-isovalerate", 
    "2-Hydroxypyridine", "2-Methylglutaric Acid", "17-Hydroxyprogesterone", "5-Methylthioadenosine", 
    "1,7-Dimethylxanthine", "1-Methylnicotinamide", "Acetylcarnitine", "4-Trimethylammoniobutanoate",
    "Creatine", "Aceturic acid", "Palmitoylcarnitine", "Pyrocatechol", "3R-hydroxy-isobutyric acid",
    "4-Guanidinobutanoate", "4-Methylcatechol", "4-Quinolinol", "Citrulline", "Thymine", "Trigonelline",
    "Urea", "Uridine", "mono-Methyl Glutarate", "3-Hydroxy-2-methylpyridine-5-carboxylate", 
    "3-Hydroxybutyric acid", "3-Methyl-2-buten-1-ol", "Riboflavin", "S-Adenosyl-L-homocysteine", 
    "Taurine", "Theobromine", "Theophylline", "L-Asparagine", "L-Glutamine", "L-Isoleucine", 
    "L-Kynurenine", "L-Leucine", "Adenine", "Adenosine", "Adonitol", "3-Methyl-L-Histidine", 
    "Hypoxanthine", "Imidazoleacetic acid", "Kynurenic acid", "L-Allothreonine", "L-Valine", 
    "N-Acetyltryptophan", "N-Acetyl-L-asparagine", "N-Acetylaspartate", "Nicotinamide", 
    "Norleucine", "O-Acetyl-L-Serine", "D-Alanine", "D-Glucose", "D-Glucuronic acid", 
    "D-Pantothenic acid", "DL-Methionine sulfoxide", "Dodecanoylcarnitine", "Epicatechin", 
    "Fructose", "Galacturonic Acid", "Glucosamine 6-Sulfate", "Glutaryl-L-carnitine", 
    "Glutathione Reduced", "Glycerophosphocholine", "Guanidinosuccinic acid", "Guanine", 
    "Hippuric acid", "Homoserine", "Caffeine", "Cholic acid", "Citric Acid", "L-Tyrosine", 
    "Norvaline", "L-Phenylalanine", "L-Pipecolic acid", "Creatinine", "Betaine", "L-Serine", 
    "L-Threonine", "L-Proline", "Cysteamine", "D-Mannose", "L-Tryptophan"
  ),
  KEGG = c(
    "C01987", NA, "C03665", "C04272", "C02502", "C02930", "C01176", "C00170", "C13747", "C02918",
    "C02571", "C01181", "C00300", NA, "C02990", "C00090", NA, "C01035", "C06730", "C06343", 
    "C00327", "C00178", "C01004", "C00086", "C00299", "C02930", "C01270", "C01089", "C01390", 
    "C00255", "C00021", "C00245", "C07480", "C07130", "C00152", "C00064", "C00407", "C00328", 
    "C00123", "C00147", "C00212", "C00474", "C01152", "C00262", "C02835", "C01717", "C05519", 
    "C00183", NA, NA, "C01042", "C00153", "C01933", "C00979", "C00133", "C00031", "C00191", 
    "C00864", "C02989", NA, "C09727", "C00095", "C00333", "C04132", NA, "C00051", "C00670", 
    "C03139", "C00242", "C01586", "C00263", "C07481", "C00695", "C00158", "C00082", "C01799", 
    "C00079", "C00408", "C00791", "C00719", "C00065", "C00188", "C00148", "C01678", "C00159", 
    "C00078"
  )
)
# Assuming unique_metabolite_data and kegg_mapping have the same row order and length

# Assign both KEGG IDs "C00031" and "C00267" to "D-Glucose"
kegg_mapping$KEGG[kegg_mapping$Metabolite == "D-Glucose"] <- "C00031, C00267"

# Verify that the assignment worked
kegg_mapping[kegg_mapping$Metabolite == "D-Glucose", ]


# View or save the updated dataset
View(unique_metabolite_data_with_kegg)
write.csv(unique_metabolite_data_with_kegg, "unique_metabolite_data_with_kegg.csv", row.names = FALSE)


# View the updated dataset
View(unique_metabolite_data_with_kegg)
write.csv(unique_metabolite_data_with_kegg, "unique_metabolite_data_with_kegg.csv", row.names = FALSE)

# Load the dataset
unique_metabolite_data_with_kegg <- read.csv("unique_metabolite_data_with_kegg.csv")

# Define pathway IDs
pathways <- c("map00051", "map00052", "map00520", "map00760", "map00500", "map00270")

# Function to retrieve compounds directly from pathway using KEGG REST API
get_pathway_compounds_via_rest <- function(pathway_id) {
  url <- paste0("https://rest.kegg.jp/link/compound/", pathway_id)
  
  # Read the data
  response <- tryCatch(readLines(url), error = function(e) NULL)
  
  if (is.null(response)) {
    message(paste("Failed to retrieve pathway:", pathway_id))
    return(NULL)
  }
  
  # Extract compound IDs from the response
  compounds <- gsub("cpd:", "", sapply(strsplit(response, "\t"), `[`, 2))
  return(compounds)
}

# Fetch compounds for each pathway
pathway_compounds <- lapply(pathways, get_pathway_compounds_via_rest)

# Name the list based on pathway names
names(pathway_compounds) <- c("Fructose and mannose metabolism",
                              "Galactose metabolism",
                              "Amino sugar and nucleotide sugar metabolism",
                              "Nicotinate and nicotinamide metabolism",
                              "Starch and sucrose metabolism",
                              "Methionine metabolism")

# Print the pathway compounds to verify
print(pathway_compounds)

# Load the metabolite data with KEGG IDs
unique_metabolite_data_with_kegg <- read.csv("unique_metabolite_data_with_kegg.csv")


# Name the pathways for easy reference
pathway_names <- c("Fructose and mannose metabolism",
                   "Galactose metabolism",
                   "Amino sugar and nucleotide sugar metabolism",
                   "Nicotinate and nicotinamide metabolism",
                   "Starch and sucrose metabolism",
                   "Methionine metabolism")

# Match KEGG IDs in unique_metabolite_data_with_kegg to compounds in each pathway
metabolite_matches <- unique_metabolite_data_with_kegg %>%
  rowwise() %>%
  mutate(
    in_pathways = list(na.omit(pathway_names[sapply(pathway_compounds, function(compounds) KEGG %in% compounds)]))
  ) %>%
  filter(length(in_pathways) > 0)  # Filter for rows where matches were found

# View or save the results
View(metabolite_matches)
write.csv(metabolite_matches, "metabolite_matches.csv", row.names = FALSE)

view(unique_metabolite_data_with_kegg)


# Filter for metabolites found in multiple pathways
compounds_multiple_pathways <- metabolite_matches %>%
  rowwise() %>%
  filter(length(in_pathways) > 1) %>%
  mutate(Pathways = paste(in_pathways, collapse = ", ")) %>%  # Concatenate pathway names
  ungroup()  # Remove rowwise grouping

# Select only relevant columns (optional)
compounds_multiple_pathways <- compounds_multiple_pathways %>%
  select(Metabolite, KEGG, Pathways)

# View or save the results
View(compounds_multiple_pathways)
print(compounds_multiple_pathways)
write.csv(compounds_multiple_pathways, "compounds_multiple_pathways.csv", row.names = FALSE)

##########################################################################################################################################
# Define the compounds of interest
compounds_of_interest <- c(
  "2-Methylglutaric Acid", "5-Deoxy-5-(methylthio)adenosine", "1-Methylnicotinamide", 
  "mono-Methyl Glutarate", "SAH S-Adenosyl-L-homocysteine", "Nicotinamide", 
  "O-Acetyl-L-Serine", "D-Glucose", "D-Glucuronic acid", "DL-Methionine sulfoxide", 
  "Fructose", "Galacturonic Acid", "Glutathione Reduced", "Homoserine", 
  "L-Serine", "D-(+)-Mannose"
)

# Filter for these compounds in clean_data_less_than_3
compounds_data <- clean_data_less_than_3 %>%
  filter(Metabolite %in% compounds_of_interest)

# Perform Spearman correlation analysis for each compound
correlation_results <- compounds_data%>%
  group_by(Metabolite) %>%
  summarize(
    Spearman_correlation = cor(value,sofa_max_within24hours_from_ICUadmission, method = "spearman", use = "complete.obs"),
    p_value = cor.test(value,sofa_max_within24hours_from_ICUadmission  , method = "spearman")$p.value
  )

# View or save the results
View(correlation_results)
write.csv(correlation_results, "spearman_correlation_results.csv", row.names = FALSE)


##### OG number of KEGG metabolites##############

# Define pathways of interest
pathway_ids <- c("map00051", # Fructose and mannose metabolism
                 "map00052", # Galactose metabolism
                 "map00520", # Amino sugar and nucleotide sugar metabolism
                 "map00760", # Nicotinate and nicotinamide metabolism
                 "map00500", # Starch and sucrose metabolism
                 "map00270") # Methionine metabolism

# Loop through each pathway to get the count of compounds
pathway_metabolite_counts <- sapply(pathway_ids, function(pathway_id) {
  pathway_info <- tryCatch({
    keggGet(pathway_id)[[1]]
  }, error = function(e) {
    return(NULL)
  })
  
  # If pathway info is retrieved successfully, count metabolites
  if (!is.null(pathway_info$COMPOUND)) {
    return(length(pathway_info$COMPOUND))
  } else {
    return(NA)
  }
})

# Print results
print(pathway_metabolite_counts)



##################################################################################

# Load the data (replace with your file path if needed)
data <- read.csv("C:/Users/kamal/OneDrive/Documents/all_data.csv")

# Define baseline confounders
baseline_conf <- c("age", "sex", "race2")  # Adjust if other confounders are relevant

# Extract unique metabolites
metabolite_names <- unique(data$Metabolite)

# Initialize a list to store model results
results <- list()

# Loop through each unique metabolite and fit a GLM
for (metabolite in metabolite_names) {
  # Subset the data for the current metabolite
  metabolite_data <- subset(data, Metabolite == metabolite)
  
  # GLM formula: Metabolite value ~ SOFA + baseline confounders
  formula <- as.formula(paste("value ~ sofa_max_within24hours_from_ICUadmission +", 
                              paste(baseline_conf, collapse = " + ")))
  
  # Fit the GLM model
  model <- glm(formula, data = metabolite_data, family = gaussian())
  
  # Save the model summary
  results[[metabolite]] <- summary(model)
}

# Display the results for each metabolite
for (metabolite in names(results)) {
  cat("Results for", metabolite, "\n")
  print(results[[metabolite]])
  cat("\n\n")
}
# Plot a histogram to visually inspect the distribution
hist(data$value, main = "Distribution of Metabolite Levels", xlab = "Metabolite Level")
shapiro.test(long_data$value)
# Take a random sample of 5000 observations
set.seed(123)  # For reproducibility
sample_data <- sample(long_data$value, 5000)

# Perform Shapiro-Wilk test on the sample
shapiro.test(sample_data)

# Install the e1071 package if you haven???t already
install.packages("e1071")
library(e1071)

# Calculate skewness
skewness_value <- skewness(long_data$value)
print(skewness_value)

#####################  Regular GLM not working so use mice###########


install.packages("mice")
library(mice)
miced_data <- readRDS("C:/Users/kamal/Downloads/miced.rds")
# Check the structure of miced_data
str(miced_data)

metab_data <- miced_data$metab





# Load necessary libraries
if (!requireNamespace("KEGGREST", quietly = TRUE)) install.packages("KEGGREST")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stats", quietly = TRUE)) install.packages("stats")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")

library(KEGGREST)
library(dplyr)
library(stats)
library(corrplot)

# Part 1: Confirm compounds in KEGG pathways
# Assuming 'compounds_df' is your data frame with compounds you want to confirm in KEGG
confirm_in_kegg <- function(compound_names) {
  kegg_compounds <- lapply(compound_names, function(compound) {
    tryCatch({
      res <- keggFind("compound", compound)
      if (length(res) > 0) return(compound)
    }, error = function(e) NA)
  })
  confirmed_compounds <- unlist(kegg_compounds[!is.na(kegg_compounds)])
  confirmed_compounds
}

# Example usage with compounds column in 'compounds_df'
confirmed_compounds <- confirm_in_kegg(metabolite_data$Metabolite)

# Part 2: Spearman Correlation with SOFA Scores
# Assuming 'data' is your data frame containing 'SOFA_scores' and 'compound_values' columns

correlations <- sapply(data %>% select(starts_with("compound")), function(compound) {
  cor.test(compound, data$SOFA_scores, method = "spearman")$estimate
})

# Convert to data frame for better handling and visualization
correlations_df <- as.data.frame(correlations)
colnames(correlations_df) <- c("Spearman_Correlation")
correlations_df$Compound <- rownames(correlations_df)

# Optional visualization
corrplot(as.matrix(correlations), method = "circle", title = "Spearman Correlation with SOFA")

# Part 3: Running GLM on a Separate Dataset
# Assuming 'glm_data' is your separate dataset with predictors and response variable

glm_model <- glm(response_variable ~ predictor1 + predictor2 + ..., data = glm_data, family = binomial)
summary(glm_model)

# Part 4: Finding all compounds and their total counts in specified KEGG pathways
# Define pathways of interest
pathways <- c("Fructose and mannose metabolism", "Galactose metabolism", 
              "Amino sugar and nucleotide sugar metabolism", "Nicotinate and nicotinamide metabolism",
              "Starch and sucrose metabolism", "Methionine metabolism")

# Load necessary libraries
if (!requireNamespace("KEGGREST", quietly = TRUE)) install.packages("KEGGREST")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(KEGGREST)
library(dplyr)

# Define pathways of interest
pathways <- c("Fructose and mannose metabolism", "Galactose metabolism", 
              "Amino sugar and nucleotide sugar metabolism", "Nicotinate and nicotinamide metabolism",
              "Starch and sucrose metabolism", "Methionine metabolism")

# Function to retrieve compound names and IDs for a pathway
retrieve_compounds <- function(pathway) {
  # Find the pathway ID
  pathway_id <- names(keggFind("pathway", pathway))
  
  # Check if pathway ID was found
  if (length(pathway_id) == 0) {
    return(data.frame(Pathway = pathway, Compound_ID = NA, Compound_Name = "Pathway not found in KEGG"))
  }
  
  # Retrieve compounds linked to the pathway
  compounds_in_pathway <- keggLink("compound", pathway_id)
  
  # Check if any compounds are found
  if (length(compounds_in_pathway) == 0) {
    return(data.frame(Pathway = pathway, Compound_ID = NA, Compound_Name = "No compounds found"))
  }
  
  # For each compound, get the compound name
  compound_names <- sapply(names(compounds_in_pathway), function(comp_id) {
    compound_data <- tryCatch(keggGet(comp_id), error = function(e) NULL)
    if (!is.null(compound_data) && !is.null(compound_data[[1]]$NAME)) {
      return(compound_data[[1]]$NAME[1])  # Use the first name if there are multiple names
    }
    return(NA)
  })
  
  # Compile results into a data frame
  data.frame(Pathway = pathway, Compound_ID = names(compounds_in_pathway), Compound_Name = compound_names)
}

# Apply the function to each pathway
pathway_compounds_details <- lapply(pathways, retrieve_compounds)

# Combine into a single data frame
all_pathway_compounds <- do.call(rbind, pathway_compounds_details)

# Display the unique compounds with their names and IDs for each pathway
print(all_pathway_compounds)


# Load the RDS file in R
miced_data <- readRDS("C:/Users/kamal/Downloads/miced.rds")

# Save it as a CSV
write.csv(miced_data, "miced_data.csv")

#############################################################################

# Load necessary libraries
library(mice)
library(mitools)

# Load the data
data <- read.csv("C:/Users/kamal/OneDrive/Documents/filtered_miced_data.csv")

# Extract metabolite columns, excluding demographic variables and SOFA score columns
metabolite_columns <- setdiff(metabolite_columns, c("metab.age", "metab.sex", "metab.race2", 
                                                    "metab.ethnicity", "metab.primary_admit_reason", 
                                                    "metab.discharge_status", "metab.resilience"))

# Specify demographic variables
demographic_vars <- c("metab.age", "metab.sex", "metab.race2", "metab.ethnicity", 
                      "metab.primary_admit_reason", "metab.discharge_status", "metab.resilience")

# Initialize list to store models
models_list <- list()

# Run a GLM for each metabolite with specified demographic variables and SOFA score per imputation
for (i in 1:10) {
  sofa_var <- paste0("metab.sofa_max_within24hours_from_ICUadmission.", i)
  
  if (!sofa_var %in% names(data)) next  # Skip if SOFA variable is not in the data
  
  for (metab in metabolite_columns) {
    formula <- as.formula(
      paste(sofa_var, "~", metab, "+", paste(demographic_vars, collapse = " + "))
    )
    
    # Fit GLM for each imputed dataset
    models <- with(data, glm(formula, family = "gaussian"))
    
    # Pool the results
    pooled_results <- pool(models)
    
    # Store the pooled results for each metabolite and SOFA score variable
    models_list[[paste(metab, sofa_var, sep = "_")]] <- summary(pooled_results)
  }
}

# Print the results for each metabolite-SOFA combination
models_list


# Load the dplyr package
library(dplyr)

# Count unique occurrences of patient_id
unique_patient_count <- merged_data %>% 
  distinct(patient_id) %>% 
  nrow()

# Print the result
print(unique_patient_count)


