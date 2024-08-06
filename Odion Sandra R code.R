# Importing required libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stats)
library(tidyverse)
library(visdat)
library(inspectdf)
library(corrplot)
library(gridExtra)
library(ggrepel)
library(factoextra)
library(caret)
library(glmnet)
library(randomForest)
library(xgboost)
library(nnet)
library(kernlab)
library(class)
library(MASS)
library(skimr)
library(devtools)
library(gsubfn)
library(purrr)
library(viridis)
library(mlr)
library(Metrics)
library(fclust)
library(e1071)



#Data Integration, merging and Manipulation
# Specify the directory path where your files are located
directory <- "C:/Users/Sandra Odion Imhanze/Downloads/CPS Case Outcomes"

# Use list.files to find files matching the pattern in the directory
files <- list.files(path = directory, pattern = "Principal_Offence_Category_", full.names = TRUE)
print(files)

#Create an empty DataFrame to store the merged data
merged_df <- data.frame()

#loop through each file
for (file in files) {
  #Read the CSV file
  df <- read_csv(file, show_col_types = FALSE)
  
  #Extract month and year from the filename
  filename_parts <- unlist(strsplit(basename(file), "_")) # Split filename by "_" and "."
  
  #Extract month and year from the matched groups
  month <- filename_parts[4]
  year <- substr(filename_parts[5], 1, 4)  # Extract the first four characters (the year)
  
  #Create new columns for month and year and fill them with its appropriate values
  df <- mutate(df, Month = as.factor(month), Year = as.factor(year))
  
  #Rename the first column to "District"
  colnames(df)[1] <- "District"
  
  #Combine Dataframes
  merged_df <- bind_rows(merged_df, df)
}

#View the files that have been merged
View(merged_df)



# Count the number of duplicates rows based on all columns
rows_num_duplicates <- sum(duplicated(merged_df))

# Print the number of duplicates
print(rows_num_duplicates)


# Count the number of duplicates based on specific columns (e.g., 'District' and 'Month')
num_duplicates <- sum(duplicated(merged_df[, c("District", "Month")]))

# Print the number of duplicates
print(num_duplicates)

# Extract columns with "Percentage" in their names to percentage_df
percentage_columns <- grep("Percentage", names(merged_df), value = TRUE)
percentage_df <- merged_df[, percentage_columns, drop = FALSE]
# Print columns containing "Percentage" in their names
print(percentage_columns)
# Remove columns with "Percentage" from merged_df
new_merged_df <- merged_df[, setdiff(names(merged_df), names(percentage_df)), drop = FALSE]
# Add "District" column to percentage_df and move it to the first position
percentage_df <- cbind(District = merged_df$District, percentage_df[, -1])
# Remove "District" column from the end of percentage_df
percentage_df <- percentage_df[, -ncol(percentage_df)]
# Rename the first column to "District"
colnames(percentage_df)[1] <- "District"
# View the modified DataFrames
View(percentage_df)
View(new_merged_df)



# Check for missing data in the new_merged_df DataFrame
missing_values <- colSums(is.na(new_merged_df))

# Display the number of missing values in each column
print(missing_values)

#Display the summation of the missing values in the whole dataset
print(sum(missing_values))



# Renaming of columns for eaasier interpretation
column_names_mapping <- c(
  "Number of Homicide Convictions" = "Homicide",
  "Number of Homicide Unsuccessful" = "Homicide_Unsuccessful",
  "Number of Offences Against The Person Convictions" = "Person_Offences",
  "Number of Offences Against The Person Unsuccessful" = "Person_Offences_Unsuccessful",
  "Number of Sexual Offences Convictions" = "Sexual_Offences",
  "Number of Sexual Offences Unsuccessful" = "Sexual_Offences_Unsuccessful",
  "Number of Burglary Convictions" = "Burglary",
  "Number of Burglary Unsuccessful" = "Burglary_Unsuccessful",
  "Number of Robbery Convictions" = "Robbery",
  "Number of Robbery Unsuccessful" = "Robbery_Unsuccessful",
  "Number of Theft And Handling Convictions" = "Theft_Handling",
  "Number of Theft And Handling Unsuccessful" = "Theft_Handling_Unsuccessful",
  "Number of Fraud And Forgery Convictions" = "Fraud_Forgery",
  "Number of Fraud And Forgery Unsuccessful" = "Fraud_Forgery_Unsuccessful",
  "Number of Criminal Damage Convictions" = "Criminal_Damage",
  "Number of Criminal Damage Unsuccessful" = "Criminal_Damage_Unsuccessful",
  "Number of Drugs Offences Convictions" = "Drugs_Offences",
  "Number of Drugs Offences Unsuccessful" = "Drugs_Offences_Unsuccessful",
  "Number of Public Order Offences Convictions" = "Public_Order_Offences",
  "Number of Public Order Offences Unsuccessful" = "Public_Order_Offences_Unsuccessful",
  "Number of All Other Offences (excluding Motoring) Convictions" = "Other_Offences",
  "Number of All Other Offences (excluding Motoring) Unsuccessful" = "Other_Offences_Unsuccessful",
  "Number of Motoring Offences Convictions" = "Motoring_Offences",
  "Number of Motoring Offences Unsuccessful" = "Motoring_Offences_Unsuccessful",
  "Number of Admin Finalised Unsuccessful" = "Admin_Finalised_Unsuccessful"
)

# Filter out the column names that are not being renamed
columns_to_keep <- c("District", "Month", "Year")

# Rename columns of new_merged_df using the named vector
names_to_change <- names(new_merged_df)[!names(new_merged_df) %in% columns_to_keep]
new_column_names <- column_names_mapping[names_to_change]
names(new_merged_df)[!names(new_merged_df) %in% columns_to_keep] <- new_column_names

print(new_column_names)
View(new_merged_df)

# Check the data type of columns in the DataFrame
str(new_merged_df)

# Create a new column MonthInNumbers and convert months to numbers
new_merged_df$MonthInNumbers <- match(new_merged_df$Month, month.abb)

# Convert Month to character and Year to numeric
new_merged_df$MonthInNumbers <- as.numeric(new_merged_df$MonthInNumbers)
new_merged_df$Year <- as.numeric(as.character(new_merged_df$Year))
new_merged_df$Month <- as.character(new_merged_df$Month)

# # Check the data type of columns in the DataFrame
str(new_merged_df)


#Plotting the different columns showing their dataypes
inspect_types(new_merged_df) %>% show_plot()
vis_dat(new_merged_df)

#other code to check the datafrme
glimpse(new_merged_df)
summary(new_merged_df)
distinct(new_merged_df)

#Plot an overview of the dataframe using histogram
inspect_num(new_merged_df[, !names(new_merged_df) %in% c("MonthInNumbers", "Year")]) %>% show_plot()







# Filter the DataFrame to extract only rows where District is "National"
national_data <- new_merged_df[new_merged_df$District == "National", ]
# View the extracted data
View(national_data)
# Filter the DataFrame to remove rows where District is "National"
new_merged_df <- subset(new_merged_df, District != "National")
# Save the extracted data to a CSV file
write.csv(national_data, "national_data.csv", row.names = FALSE)
# From the national data df, define new column names that are "successful"
national_convicted_column_names <- c("District", "Month", "MonthInNumbers", "Year", "Homicide", "Person_Offences",
                             "Sexual_Offences", "Burglary", "Robbery", "Theft_Handling",
                             "Fraud_Forgery", "Criminal_Damage", "Drugs_Offences",
                             "Public_Order_Offences", "Other_Offences", "Motoring_Offences")
# Select columns for successful crime analysis
national_convicted_df <- national_data %>%
  dplyr::select(all_of(national_convicted_column_names))
# View the extracted DataFrames
View(national_convicted_df)
# Save DataFrame to CSV file
write.csv(national_convicted_df, "national_successful_crime_data.csv", row.names = FALSE)
#reset the index numerically in a chronological order
row.names(national_convicted_df) <- NULL
view(national_convicted_df)



# Define new column names for national unsuccessful columns
national_unsuccessful_column_names <- c("District", "Month", "Year", 
                                   "MonthInNumbers","Homicide_Unsuccessful",
                                   "Person_Offences_Unsuccessful",
                                   "Sexual_Offences_Unsuccessful",
                                   "Burglary_Unsuccessful",
                                   "Robbery_Unsuccessful",
                                   "Theft_Handling_Unsuccessful",
                                   "Fraud_Forgery_Unsuccessful",
                                   "Criminal_Damage_Unsuccessful",
                                   "Drugs_Offences_Unsuccessful",
                                   "Public_Order_Offences_Unsuccessful",
                                   "Other_Offences_Unsuccessful",
                                   "Motoring_Offences_Unsuccessful",
                                   "Admin_Finalised_Unsuccessful")
# Select columns for national unsuccessful crime analysis
national_unsuccessful_df <- national_data %>%
  dplyr::select(all_of(national_unsuccessful_column_names))
# View the extracted DataFrames
View(national_unsuccessful_df)
# Save DataFrame to CSV file
write.csv(national_unsuccessful_df, "national_unsuccessful_crime_data.csv", row.names = FALSE)
#reset the index numerically in a chronological order
row.names(national_unsuccessful_df) <- NULL
view(national_unsuccessful_df)



# Load the dplyr package
library(dplyr)
# Define new column names that are "successful"
counties_convicted_column_names <- c("District", "Month", "MonthInNumbers", "Year", "Homicide", "Person_Offences",
                             "Sexual_Offences", "Burglary", "Robbery", "Theft_Handling",
                             "Fraud_Forgery", "Criminal_Damage", "Drugs_Offences",
                             "Public_Order_Offences", "Other_Offences", "Motoring_Offences")
# Select columns for successful crime analysis
county_convicted_df <- new_merged_df %>%
  dplyr::select(all_of(counties_convicted_column_names))
# View the extracted DataFrames
View(county_convicted_df)
# Save DataFrame to CSV file
write.csv(county_convicted_df, "county_convicted_df.csv", row.names = FALSE)
#reset the index numerically in a chronological order
row.names(county_convicted_df) <- NULL
view(county_convicted_df)




# Define new column names for unsuccessful columns
county_unsuccessful_column_names <- c("District", "Month", "Year", 
                                   "MonthInNumbers","Homicide_Unsuccessful",
                                   "Person_Offences_Unsuccessful",
                                   "Sexual_Offences_Unsuccessful",
                                   "Burglary_Unsuccessful",
                                   "Robbery_Unsuccessful",
                                   "Theft_Handling_Unsuccessful",
                                   "Fraud_Forgery_Unsuccessful",
                                   "Criminal_Damage_Unsuccessful",
                                   "Drugs_Offences_Unsuccessful",
                                   "Public_Order_Offences_Unsuccessful",
                                   "Other_Offences_Unsuccessful",
                                   "Motoring_Offences_Unsuccessful",
                                   "Admin_Finalised_Unsuccessful")
# Select columns for unsuccessful crime analysis
county_unsuccessful_df <- new_merged_df %>%
  dplyr::select(all_of(county_unsuccessful_column_names))  # Select columns with new column names
# View the extracted DataFrames
View(county_unsuccessful_df)
# Save DataFrame to CSV file
write.csv(county_unsuccessful_df, "district_unsuccessful_crime_data.csv", row.names = FALSE)
#reset the index numerically in a chronological order
row.names(county_unsuccessful_df) <- NULL
view(county_unsuccessful_df)

#Plot an overview of the convicted and Unsuccessful dataframe using histogram
inspect_num(county_convicted_df[, !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]) %>% show_plot()
inspect_num(county_unsuccessful_df[, !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]) %>% show_plot()




#DESCRIPTIVE ANALYSIS: MEAN OF THE COUNTIES CONVICTED CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the means
numeric_columns <- sapply(county_convicted_df, is.numeric)
mean_of_sc <- colMeans(county_convicted_df[, numeric_columns & !names(county_convicted_df) %in% c("MonthInNumbers", "Year")])
# Convert the result to a dataframe
means_of_convictions <- data.frame(variable = names(mean_of_sc), mean_value = mean_of_sc)
print(means_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(mean_of_sc))
# Create plot using ggplot
ggplot(means_of_convictions, aes(x = variable, y = mean_value, fill = variable, label = round(mean_value, 2))) + 
  geom_bar(stat = "identity") +
  geom_text(size = 3, vjust = -0.3) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Crimes", y = "Mean Value", title = "Mean Values of the Counties Convicted Crime Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees
# Output mean values for each crime rounded to 2 decimal points
for (crime in names(mean_of_sc)) {
  print(paste("The mean value of the Counties Convicted Crime Cases for", crime, "is", round(mean_of_sc[crime],2)))
}




#DESCRIPTIVE ANALYSIS: MEAN OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the means
numeric_columns <- sapply(county_unsuccessful_df, is.numeric)
mean_of_sc <- colMeans(county_unsuccessful_df[, numeric_columns & !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")])
# Convert the result to a dataframe
means_of_convictions <- data.frame(variable = names(mean_of_sc), mean_value = mean_of_sc)
print(means_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(mean_of_sc))
# Create plot using ggplot
ggplot(means_of_convictions, aes(x = variable, y = mean_value, fill = variable, label = round(mean_value, 2))) + 
  geom_bar(stat = "identity") +
  geom_text(size = 3, vjust = -0.3) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = "Crimes", y = "Mean Value", title = "Mean Values of the Counties Unsuccessful Crime Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees
# Output mean values for each crime rounded to 2 decimal points
for (crime in names(mean_of_sc)) {
  print(paste("The mean value of the Counties Unsuccessful Crime Cases for", crime, "is", round(mean_of_sc[crime],2)))
}







#DESCRIPTIVE ANALYSIS: MEDIAN OF THE COUNTIES CONVICTED CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_convicted_df, is.numeric)
data_for_analysis <- county_convicted_df[, numeric_columns & !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Calculate median
median_of_sc <- sapply(data_for_analysis, median)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(median_of_sc), median_value = median_of_sc)
print(statistics_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(median_of_sc))
# Create plot using ggplot for medians (pie chart)
ggplot(statistics_of_convictions, aes(x = "", y = median_value, fill = variable, label = round(median_value, 2))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Median Values of the Counties Convicted Crime Cases")
# Output median values for each crime rounded to 2 decimal points
for (crime in names(median_of_sc)) {
  print(paste("The median value of the Counties Convicted Crime Cases for", crime, "is", round(median_of_sc[crime],2)))
}




#DESCRIPTIVE ANALYSIS: MEDIAN OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_unsuccessful_df, is.numeric)
data_for_analysis <- county_unsuccessful_df[, numeric_columns & !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Calculate median
median_of_sc <- sapply(data_for_analysis, median)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(median_of_sc), median_value = median_of_sc)
print(statistics_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(median_of_sc))
# Create plot using ggplot for medians (pie chart)
ggplot(statistics_of_convictions, aes(x = "", y = median_value, fill = variable, label = round(median_value, 2))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Median Values of the Counties Unsuccessful Crime Cases")
# Output median values for each crime rounded to 2 decimal points
for (crime in names(median_of_sc)) {
  print(paste("The median value of the Counties Unsuccessful Crime Cases for", crime, "is", round(median_of_sc[crime],2)))
}




#DESCRIPTIVE ANALYSIS: MODE OF THE COUNTIES CONVICTED CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_convicted_df, is.numeric)
data_for_analysis <- county_convicted_df[, numeric_columns & !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Calculate mean, median, and mode
mode_of_sc <- apply(data_for_analysis, 2, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(mode_of_sc), mode_value = mode_of_sc)
print(statistics_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(mode_of_sc))
# Create plot using ggplot for modes (horizontal bar plot)
ggplot(statistics_of_convictions, aes(x = mode_value, y = variable, fill = variable, label = mode_value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = mode_value), hjust = -0.2, size = 2.5) +
  labs(x = "Mode Value", y = "Crimes", title = "Mode Values of the Counties Convicted Crime Cases") +
  theme(axis.text.y = element_text(size = 8))
# Output mode values for each crime rounded to 2 decimal points
for (crime in names(mode_of_sc)) {
  print(paste("The mode value of the Counties Convicted Crime Cases for", crime, "is", round(mode_of_sc[crime],2)))
}



#DESCRIPTIVE ANALYSIS: MODE OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_unsuccessful_df, is.numeric)
data_for_analysis <- county_unsuccessful_df[, numeric_columns & !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Calculate mean, median, and mode
mode_of_sc <- apply(data_for_analysis, 2, function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
})
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(mode_of_sc), mode_value = mode_of_sc)
print(statistics_of_convictions)
# Ensure the viridis library is loaded
colors_sc <- viridis::viridis(length(mode_of_sc))
# Create plot using ggplot for modes (horizontal bar plot)
ggplot(statistics_of_convictions, aes(x = mode_value, y = variable, fill = variable, label = mode_value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = mode_value), hjust = -0.2, size = 2.5) +
  labs(x = "Mode Value", y = "Crimes", title = "Mode Values of the Counties Unsuccessful Crime Cases") +
  theme(axis.text.y = element_text(size = 8))
# Output mode values for each crime rounded to 2 decimal points
for (crime in names(mode_of_sc)) {
  print(paste("The mode value of the Counties Unsuccessful Crime Cases for", crime, "is", round(mode_of_sc[crime],2)))
}




#DESCRIPTIVE ANALYSIS: VARIANCE OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_convicted_df, is.numeric)
data_for_analysis <- county_convicted_df[, numeric_columns & !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Calculate variance
variance_of_sc <- sapply(data_for_analysis, var)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(variance_of_sc), variance_value = variance_of_sc)
print(statistics_of_convictions)
# Output variance values for each crime rounded to 2 decimal points
for (crime in names(variance_of_sc)) {
  print(paste("The variance value of the Counties Convicted Crime Cases for", crime, "is", round(variance_of_sc[crime],2)))
}


#DESCRIPTIVE ANALYSIS: VARIANCE OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_unsuccessful_df, is.numeric)
data_for_analysis <- county_unsuccessful_df[, numeric_columns & !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Calculate variance
variance_of_sc <- sapply(data_for_analysis, var)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(variable = names(variance_of_sc), variance_value = variance_of_sc)
print(statistics_of_convictions)
# Output Variance values for each crime rounded to 2 decimal points
for (crime in names(variance_of_sc)) {
  print(paste("The variance value of the Counties Unsuccessful Crime Cases for", crime, "is", round(variance_of_sc[crime],2)))
}






#DESCRIPTIVE ANALYSIS: STANDARD DEVIATION OF THE COUNTIES CONVICTED CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_convicted_df, is.numeric)
data_for_analysis <- county_convicted_df[, numeric_columns & !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Calculate standard deviation
std_dev_of_sc <- sapply(data_for_analysis, sd)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(CrimeType = names(std_dev_of_sc), StdDev = std_dev_of_sc)
# Create a plot that represents standard deviation using line segments for each crime type
ggplot(statistics_of_convictions, aes(x = CrimeType, y = StdDev)) +
  geom_segment(aes(xend = CrimeType, yend = 0), color = "black") +  # Line from 0 to the std dev value
  geom_point(color = "blue", size = 4) +  # Points at the end of each segment
  geom_text(aes(label = round(StdDev, 2)), vjust = -0.5, color = "black") +  # Labels showing the std dev values
  labs(x = "Crime Type", y = "Standard Deviation", title = "Standard Deviation of Convicted Crime Cases across Counties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Output Standard Deviation values for each crime rounded to 2 decimal points
for (crime in names(std_dev_of_sc)) {
  print(paste("The Standard Deviation value of the Counties Convicted Crime Cases for", crime, "is", round(std_dev_of_sc[crime],2)))
}




#DESCRIPTIVE ANALYSIS: STANDARD DEVIATION OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Exclude 'year' and 'months' from the numeric columns before calculating the statistics
numeric_columns <- sapply(county_unsuccessful_df, is.numeric)
data_for_analysis <- county_unsuccessful_df[, numeric_columns & !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Calculate standard deviation
std_dev_of_sc <- sapply(data_for_analysis, sd)
# Convert the results to a dataframe
statistics_of_convictions <- data.frame(CrimeType = names(std_dev_of_sc), StdDev = std_dev_of_sc)
# Create a plot that represents standard deviation using line segments for each crime type
ggplot(statistics_of_convictions, aes(x = CrimeType, y = StdDev)) +
  geom_segment(aes(xend = CrimeType, yend = 0), color = "black") +  # Line from 0 to the std dev value
  geom_point(color = "blue", size = 4) +  # Points at the end of each segment
  geom_text(aes(label = round(StdDev, 2)), vjust = -0.5, color = "black") +  # Labels showing the std dev values
  labs(x = "Crime Type", y = "Standard Deviation", title = "Standard Deviation of Unsuccessful Crime Cases across Counties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
# Output Standard Deviation values for each crime rounded to 2 decimal points
for (crime in names(std_dev_of_sc)) {
  print(paste("The Standard Deviation value of the Counties Unsuccessful Crime Cases for", crime, "is", round(std_dev_of_sc[crime],2)))
}



#DESCRIPTIVE ANALYSIS: COVARIANCE OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Select numeric columns excluding "MonthInNumbers" and "Year"
numeric_columns <- county_convicted_df[sapply(county_convicted_df, is.numeric) & 
                                         !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Print the numeric columns
print(numeric_columns)
#covariance
covariance_matrix <- cov(numeric_columns)
print(covariance_matrix)
# Plot covariance matrix with adjusted margins
heatmap(covariance_matrix, labRow = rownames(covariance_matrix), cexRow = 1.0, margins = c(12, 9), main = "Covariance Matrix Convicted Crime Cases")


#DESCRIPTIVE ANALYSIS: COVARIANCE OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Select numeric columns excluding "MonthInNumbers" and "Year"
numeric_columns <- county_unsuccessful_df[sapply(county_unsuccessful_df, is.numeric) & 
                                         !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Print the numeric columns
print(numeric_columns)
#covariance
covariance_matrix <- cov(numeric_columns)
print(covariance_matrix)

# Plot covariance matrix with adjusted margins
heatmap(covariance_matrix, labRow = rownames(covariance_matrix), cexRow = 1.0, margins = c(12, 9), main = "Covariance Matrix Unsuccessful Crime Cases")




#DESCRIPTIVE ANALYSIS: CORRELATION OF THE COUNTIES CONVICTED CRIME CASES
# Select numeric columns excluding "MonthInNumbers" and "Year"
numeric_columns <- county_convicted_df[sapply(county_convicted_df, is.numeric) & 
                                         !names(county_convicted_df) %in% c("MonthInNumbers", "Year")]
# Print the numeric columns
print(numeric_columns)
#correlation matrix
correlation_matrix <- cor(numeric_columns)
print(correlation_matrix)
#plot the correlation matrix
corrplot((correlation_matrix), main = "Correlation Matrix Convicted Crime Cases", mar = c(2,2,2,10))



#DESCRIPTIVE ANALYSIS: CORRELATION OF THE COUNTIES UNSUCCESSFUL CRIME CASES
# Select numeric columns excluding "MonthInNumbers" and "Year"
numeric_columns <- county_unsuccessful_df[sapply(county_unsuccessful_df, is.numeric) & 
                                         !names(county_unsuccessful_df) %in% c("MonthInNumbers", "Year")]
# Print the numeric columns
print(numeric_columns)
#correlation matrix
correlation_matrix <- cor(numeric_columns)
print(correlation_matrix)
#plot the correlation matrix
corrplot((correlation_matrix), main = "Correlation Matrix Unsuccessful Crime Cases", mar = c(2,2,2,10))



#mean_Person_Offences

#mean_Person_Offences

# Plotting the average of the Person offences (Convicted) for each of the Counties
mean_Person_Offences <- county_convicted_df %>%
  group_by(District) %>%
  summarise(mean_Person_Offences = mean(Person_Offences, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_Person_Offences, y = reorder(District, mean_Person_Offences))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(mean_Person_Offences, 2)), hjust = -0.07, size = 3, color = "black") +  # Add mean labels
  labs(x = "Averages", y = "District", title = "Average of Person Offence Conviction across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

mean_Person_Offences

# Plotting the average of the Person offences (Unsuccessful) for each of the Counties
mean_Person_Offences <- county_unsuccessful_df %>%
  group_by(District) %>%
  summarise(mean_Person_Offences = mean(Person_Offences_Unsuccessful, na.rm = TRUE)) %>%
  ggplot(aes(x = mean_Person_Offences, y = reorder(District, mean_Person_Offences))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = round(mean_Person_Offences, 2)), hjust = -0.07, size = 3, color = "black") +  # Add mean labels
  labs(x = "Averages", y = "District", title = "Average of Person Offence Unsuccessful across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

mean_Person_Offences




# Plotting the Total Sum of Homicide Convictions for each of the Counties
sum_Homicide <- county_convicted_df %>%
  group_by(District) %>%
  summarise(sum_Homicide = sum(Homicide, na.rm = TRUE)) %>%
  ggplot(aes(x = sum_Homicide, y = reorder(District, sum_Homicide))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(sum_Homicide, 2)), hjust = -0.07, size = 3, color = "black") +  # Add sum labels
  labs(x = "Total Sum", y = "District", title = "Total Sum of Homicide Conviction across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

sum_Homicide

# Plotting the Total Sum of Homicide Unsuccessful for each of the Counties
sum_Homicide <- county_unsuccessful_df %>%
  group_by(District) %>%
  summarise(sum_Homicide = sum(Homicide_Unsuccessful, na.rm = TRUE)) %>%
  ggplot(aes(x = sum_Homicide, y = reorder(District, sum_Homicide))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(sum_Homicide, 2)), hjust = -0.07, size = 3, color = "black") +  # Add sum labels
  labs(x = "Total Sum", y = "District", title = "Total Sum of Homicide Unsuccessful across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

sum_Homicide




# Plotting the Maximum Count of Motoring_Offences Convictions for each of the Counties
Max_Motoring_Offences <- county_convicted_df %>%
  group_by(District) %>%
  summarise(Max_Motoring_Offences = max(Motoring_Offences, na.rm = TRUE)) %>%
  ggplot(aes(x = Max_Motoring_Offences, y = reorder(District, Max_Motoring_Offences))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Max_Motoring_Offences, 2)), hjust = -0.07, size = 3, color = "black") +  # Add max labels
  labs(x = "Maximum", y = "District", title = "Maximum Count of Motoring_Offences Conviction across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

Max_Motoring_Offences

# Plotting the Maximum Count of Motoring_Offences Unsuccessful for each of the Counties
Max_Motoring_Offences <- county_unsuccessful_df %>%
  group_by(District) %>%
  summarise(Max_Motoring_Offences = max(Motoring_Offences_Unsuccessful, na.rm = TRUE)) %>%
  ggplot(aes(x = Max_Motoring_Offences, y = reorder(District, Max_Motoring_Offences))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Max_Motoring_Offences, 2)), hjust = -0.07, size = 3, color = "black") +  # Add max labels
  labs(x = "Maximum Count", y = "District", title = "Maximum Count of Motoring_Offences Unsuccessful across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

Max_Motoring_Offences


# Plotting the Minimum Count of Theft_Handling Convictions for each of the Counties
Min_Theft_Handling <- county_convicted_df %>%
  group_by(District) %>%
  summarise(Min_Theft_Handling = min(Theft_Handling, na.rm = TRUE)) %>%
  ggplot(aes(x = Min_Theft_Handling, y = reorder(District, Min_Theft_Handling))) +
  geom_histogram(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(Min_Theft_Handling, 2)), hjust = -0.07, size = 3, color = "black") +  # Add max labels
  labs(x = "Minimum", y = "District", title = "Minimum Count of Theft_Handling Conviction across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

Min_Theft_Handling

# Plotting the Minimum Count of Theft_Handling Unsuccessful for each of the Counties
Min_Theft_Handling <- county_unsuccessful_df %>%
  group_by(District) %>%
  summarise(Min_Theft_Handling = min(Theft_Handling_Unsuccessful, na.rm = TRUE)) %>%
  ggplot(aes(x = Min_Theft_Handling, y = reorder(District, Min_Theft_Handling))) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Min_Theft_Handling, 2)), hjust = -0.07, size = 3, color = "black") +  # Add max labels
  labs(x = "Minimum Count", y = "District", title = "Minimum Count of Theft_Handling Unsuccessful across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

Min_Theft_Handling


# Plotting the Total Sum of Robbery Convictions for each of the Counties
sum_Robbery <- county_convicted_df %>%
  group_by(District) %>%
  summarise(sum_Robbery = sum(Robbery, na.rm = TRUE)) %>%
  ggplot(aes(x = sum_Robbery, y = reorder(District, sum_Robbery))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(sum_Robbery, 2)), hjust = -0.07, size = 3, color = "black") +  # Add sum labels
  labs(x = "Total Sum", y = "District", title = "Total Sum of Robbery Conviction across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

sum_Robbery

# Plotting the Total Sum of Robbery Unsuccessful for each of the Counties
sum_Robbery <- county_unsuccessful_df %>%
  group_by(District) %>%
  summarise(sum_Robbery = sum(Robbery_Unsuccessful, na.rm = TRUE)) %>%
  ggplot(aes(x = sum_Robbery, y = reorder(District, sum_Robbery))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = round(sum_Robbery, 2)), hjust = -0.07, size = 3, color = "black") +  # Add sum labels
  labs(x = "Total Sum", y = "District", title = "Total Sum of Robbery Unsuccessful across the Counties") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) 

sum_Robbery







library(ggplot2)

# Plotting the average of the sexual offences for each of the districts
district_successful_df %>%
  group_by(District) %>%
  summarise(mean_Sexual_Offences = mean(Sexual_Offences, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(District, mean_Sexual_Offences), y = mean_Sexual_Offences)) +
  geom_bar(stat = "identity") +
  labs(x = "District", y = "Averages", title = "Average of Sexual Offence Conviction for UK 28 Counties") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

mean_sexual_offences


# Load necessary libraries
library(ggplot2)
library(dplyr)






# FEATURE ENGINEERING ON THE COUNTY CONVICTED DATAFRAME
# Define the thresholds for crime region categories
low_threshold <- 0
moderate_threshold <- 10000
moderately_high_threshold <- 15000
high_threshold <- 20000
very_high_threshold <- 30000
# Extract the list of districts with the total sum of several crime types and categorize them
total_sum_convictions <- county_convicted_df %>%
  group_by(District) %>%
  summarise(
    sum_Robbery = sum(Robbery, na.rm = TRUE),
    sum_Homicide = sum(Homicide, na.rm = TRUE),
    sum_Motoring_Offences = sum(Motoring_Offences, na.rm = TRUE),
    sum_Person_Offences = sum(Person_Offences, na.rm = TRUE),
    sum_Theft_Handling = sum(Theft_Handling, na.rm = TRUE),
    Total = sum_Robbery + sum_Homicide + sum_Motoring_Offences + sum_Person_Offences + sum_Theft_Handling
  ) %>%
  mutate(
    Crime_Region_Categories = case_when(
      Total >= very_high_threshold ~ "Very High Crime Region",
      Total >= high_threshold & Total < very_high_threshold ~ "High Crime Region",
      Total >= moderately_high_threshold & Total < high_threshold ~ "Moderately High Crime Region",
      Total >= moderate_threshold & Total < moderately_high_threshold ~ "Moderate Crime Region",
      Total >= low_threshold & Total < moderate_threshold ~ "Low Crime Region"
    )
  )
#Print the results
print(total_sum_convictions, n=Inf)
# Merge the new column to the original dataframe
county_convicted_df <- left_join(county_convicted_df, total_sum_convictions, by = "District")
# View the updated dataframe
View(county_convicted_df)






#VISUALIZE THE CRIME CATEGORIES OF THE CONVICTED CRIME CASES ACROSS THE COUNTIES
# Calculate value counts for each crime region category
crime_region_counts <- table(county_convicted_df$Crime_Region_Categories)
# Convert table to data frame
crime_region_counts_df <- as.data.frame(crime_region_counts)
names(crime_region_counts_df) <- c("Crime_Region_Categories", "Frequency")
# Calculate percentage
crime_region_counts_df$Percentage <- crime_region_counts_df$Frequency / sum(crime_region_counts_df$Frequency) * 100
# Plotting pie chart
pie_chart <- ggplot(crime_region_counts_df, aes(x = "", y = Frequency, fill = Crime_Region_Categories)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("Distribution of Counties Across Convicted Crime Region Categories") +
  scale_fill_manual(values = c("skyblue", "lightgreen", "lightblue", "lightpink", "red")) +
  theme_void() +
  geom_text(aes(label = paste0(sprintf("%.1f", Percentage), "%")), position = position_stack(vjust = 0.5))
print(pie_chart)

# Plotting histogram (bar chart)
bar_chart <- ggplot(crime_region_counts_df, aes(x = Crime_Region_Categories, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Distribution of Counties Across Convicted Crime Region Categories") +
  xlab("Crime Region Categories Label") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5))
print(bar_chart)





# Plotting the average of the sexual offences for each of the districts
county_convicted_df %>%
  group_by(District) %>%
  summarise(max_Sexual_Offences = max(Motoring_Offences, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(District, max_Sexual_Offences), y = max_Sexual_Offences)) +
  geom_bar(stat = "identity") +
  labs(x = "Counties", y = "Sex Conviction", title = "Counties with the highest number of the sexual offence Convictions") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extract the list of districts with the highest number of sexual offence convictions
max_sexual_offences <- county_convicted_df %>%
  group_by(District) %>%
  summarise(max_Sexual_Offences = max(Motoring_Offences, na.rm = TRUE)) %>%
  arrange(desc(max_Sexual_Offences))

max_sexual_offences



#VISUALIZATION OF THE VARIABLES RELATIONSHIP
# Create a scatter plot with regression line to visualize the relationship between Person_Offences and Motoring_Offences
ggplot(county_convicted_df, aes(x = Motoring_Offences, y = Person_Offences)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(x = "Motoring Offences", y = "Person Offences", title = "Relationship Between Person Offences and Motoring Offences") +
  theme_minimal()

#Homicide vs. Person_Offences:
ggplot(county_convicted_df, aes(x = Person_Offences, y = Homicide)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Person Offences", y = "Homicide", title = "Relationship Between Homicide and Person Offences") +
  theme_minimal()

#Motoring_Offences vs. Person_Offences:
ggplot(county_convicted_df, aes(x = Person_Offences, y = Motoring_Offences)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Person Offences", y = "Motoring Offences", title = "Relationship Between Motoring Offences and Person Offences") +
  theme_minimal()

#Theft_Handling vs. Public_Order_Offences:
ggplot(county_convicted_df, aes(x = Public_Order_Offences, y = Theft_Handling)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Public Order Offences", y = "Theft Handling", title = "Relationship Between Theft Handling and Public Order Offences") +
  theme_minimal()

#Robbery vs. Person_Offences:
ggplot(county_convicted_df, aes(x = Person_Offences, y = Robbery)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Person Offences", y = "Robbery", title = "Relationship Between Robbery and Person Offences") +
  theme_minimal()



# Load necessary libraries
library(dplyr)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Function to perform linear regression and evaluate the model
perform_linear_regression <- function(response_var, explanatory_var) {
  # Split the data into training and testing sets
  train_index <- createDataPartition(county_convicted_df[[response_var]], p = 0.8, list = FALSE)
  train_data <- county_convicted_df[train_index, ]
  test_data <- county_convicted_df[-train_index, ]
  
  # Fit a linear regression model
  model <- lm(paste(response_var, "~", explanatory_var), data = train_data)
  
  # Make predictions on the test data
  predicted_values <- predict(model, newdata = test_data)
  
  # Create a data frame with actual and predicted values
  prediction_data <- data.frame(Actual = test_data[[response_var]], Predicted = predicted_values)
  
  # Display the head of prediction_data
  cat("Head of prediction_data for", response_var, ":\n")
  print(head(prediction_data))
  
  # Calculate the RMSE
  rmse <- sqrt(mean((prediction_data$Actual - prediction_data$Predicted)^2))
  print(paste("Root Mean Squared Error (RMSE) for", response_var, ":", rmse))
  
  # Plot the actual vs. predicted values
  plot_title <- paste("Actual vs. Predicted Values for", response_var)
  plot <- ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(x = "Actual", y = "Predicted", title = plot_title) +
    theme_minimal()
  
  print(plot)
  
  # Return the model
  return(model)
}

# Perform linear regression for each scenario
model1 <- perform_linear_regression("Person_Offences", "Motoring_Offences")
summary(model1)

model2 <- perform_linear_regression("Homicide", "Person_Offences")
summary(model2)

model3 <- perform_linear_regression("Motoring_Offences", "Person_Offences")
summary(model3)

model4 <- perform_linear_regression("Theft_Handling", "Public_Order_Offences")
summary(model4)

model5 <- perform_linear_regression("Robbery", "Person_Offences")
summary(model5)


#VISUALIZING MULTI VARIABLES RELATIONSHIP
# Motoring_Offences vs. Person_Offences and Year
ggplot(county_convicted_df, aes(x = Person_Offences, y = Motoring_Offences, color = as.factor(Year))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Person_Offences", y = "Motoring_Offences", title = "Relationship Between Motoring_Offences and Person_Offences/Year") +
  theme_minimal()

# Theft Handling vs Public Order Offences and Year
ggplot(county_convicted_df, aes(x = Public_Order_Offences, y = Theft_Handling, color = as.factor(Year))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Public Order Offences", y = "Theft Handling", title = "Relationship Between Theft Handling and Public Order Offences/Year") +
  theme_minimal()




#MULTI LINEAR REGRESSION
# Function to perform linear regression and evaluate the model
perform_linear_regression <- function(response_var, explanatory_vars) {
  # Split the data into training and testing sets
  set.seed(123)
  train_index <- createDataPartition(county_convicted_df[[response_var]], p = 0.8, list = FALSE)
  train_data <- county_convicted_df[train_index, ]
  test_data <- county_convicted_df[-train_index, ]
  
  # Fit a linear regression model
  model <- lm(paste(response_var, "~", paste(explanatory_vars, collapse = " + ")), data = train_data)
  
  # Make predictions on the test data
  predicted_values <- predict(model, newdata = test_data)
  
  # Create a data frame with actual and predicted values
  prediction_data <- data.frame(Actual = test_data[[response_var]], Predicted = predicted_values)
  
  # Display the head of prediction_data
  cat("Head of prediction_data for", response_var, ":\n")
  print(head(prediction_data))
  
  # Calculate the RMSE
  rmse <- sqrt(mean((prediction_data$Actual - prediction_data$Predicted)^2))
  print(paste("Root Mean Squared Error (RMSE) for", response_var, ":", rmse))
  
  # Plot the actual vs. predicted values
  plot_title <- paste("Actual vs. Predicted Values for", response_var)
  plot <- ggplot(prediction_data, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(x = "Actual", y = "Predicted", title = plot_title) +
    theme_minimal()
  
  print(plot)
  
  # Return the model
  return(model)
}

# Perform multi linear regression for Motoring_Offences using Person Offences and Year
explanatory_vars <- c("Year", "Person_Offences")
model6 <- perform_linear_regression("Motoring_Offences", explanatory_vars)
summary(model6)

# Perform multi linear regression for Theft_Handling using Public_Order_Offences and Year
explanatory_vars <- c("Year", "Public_Order_Offences")
model7 <- perform_linear_regression("Theft_Handling", explanatory_vars)
summary(model7)

#CLASSIFICATION
distinct_categories <- county_convicted_df %>%
  distinct(Crime_Region_Categories)

print(distinct_categories)


#SVM classification
# Ensure that your target variable is a factor
training_data$Crime_Region_Categories <- factor(training_data$Crime_Region_Categories)
# Train the SVM model
svm_model <- svm(Crime_Region_Categories ~ ., data = training_data, kernel = "radial")
# Print the model summary
print(svm_model)

# Make predictions on the test data
predictions <- predict(svm_model, newdata = testing_data)
head(predictions)
# Convert testing_data$Crime_Region_Categories to factor and set the levels
testing_data$Crime_Region_Categories <- factor(testing_data$Crime_Region_Categories, 
                                               levels = levels(predictions))



# Now, run the confusion matrix
confusionMatrix(predictions, testing_data$Crime_Region_Categories)


#PLOTTING THE CONFUSION MATRIX
conf_matrix1 <- confusionMatrix(predictions, testing_data$Crime_Region_Categories)
# Increase the margins to make room for the labels
par(mar = c(3, 5, 2, 8) + 0.1)
# Plot confusion matrix with rotated labels and adjusted label size
plot(conf_matrix1$table, col = conf_matrix1$byClass, main = "Confusion Matrix for SVM Model", las = 1.5, cex.axis = 0.55)

#PLOTTING THE CONFUSION MATRIX USING HEATMAP
# Convert the confusion matrix to a table for plotting
conf_table <- as.table(conf_matrix1$table)
# Plot the confusion matrix as a heatmap
heatmap_colors <- colorRampPalette(c("white", "blue"))(100)  # Adjust color palette as needed
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust the margins
image(1:nrow(conf_table), 1:ncol(conf_table), t(as.matrix(conf_table)), col = heatmap_colors, 
      main = "Confusion Matrix for SVM Model using Heatmap", xlab="Predicted", ylab="Actual", axes=FALSE)
# Adding counts on the plot
text(expand.grid(x = seq_len(ncol(conf_table)), y = seq_len(nrow(conf_table))), 
     labels = as.character(conf_table), cex = 0.8, col = "red")



#randomforest classification
county_convicted_df$Crime_Region_Categories <- factor(county_convicted_df$Crime_Region_Categories)
set.seed(123)  # for reproducibility
train_index <- createDataPartition(county_convicted_df$Crime_Region_Categories, p = 0.8, list = FALSE)
train_data <- county_convicted_df[train_index, ]
test_data <- county_convicted_df[-train_index, ]
library(randomForest)
model <- randomForest(Crime_Region_Categories ~ ., data = train_data, ntree = 500)
predictions <- predict(model, newdata = test_data)
conf_matrix2 <- confusionMatrix(predictions, test_data$Crime_Region_Categories)
print(conf_matrix2)

#PLOTTING THE CONFUSION MATRIX
conf_matrix2 <- confusionMatrix(predictions, testing_data$Crime_Region_Categories)
# Increase the margins to make room for the labels
par(mar = c(3, 5, 2, 8) + 0.1)
# Plot confusion matrix with rotated labels and adjusted label size
plot(conf_matrix1$table, col = conf_matrix2$byClass, main = "Confusion Matrix for RandomForest Model", las = 1.5, cex.axis = 0.55)

#PLOTTING THE CONFUSION MATRIX USING HEATMAP
# Convert the confusion matrix to a table for plotting
conf_table <- as.table(conf_matrix2$table)
# Plot the confusion matrix as a heatmap
heatmap_colors <- colorRampPalette(c("white", "blue"))(100)  # Adjust color palette as needed
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust the margins
image(1:nrow(conf_table), 1:ncol(conf_table), t(as.matrix(conf_table)), col = heatmap_colors, 
      main = "Confusion Matrix for RandomForest Model using Heatmap", xlab="Predicted", ylab="Actual", axes=FALSE)
# Adding counts on the plot
text(expand.grid(x = seq_len(ncol(conf_table)), y = seq_len(nrow(conf_table))), 
     labels = as.character(conf_table), cex = 0.8, col = "red")






#Clustering Analysis
# Perform scaling and K-means clustering
# Select numerical columns for clustering
cluster_cols <- county_convicted_df[, c("Homicide", "Person_Offences", "Sexual_Offences", "Burglary", "Robbery", 
                                        "Theft_Handling", "Fraud_Forgery", "Criminal_Damage", "Drugs_Offences", 
                                        "Public_Order_Offences", "Other_Offences", "Motoring_Offences")]
# Set random seed for reproducibility
set.seed(123)
# Perform K-means clustering with 3 clusters and 25 random starts
res.km <- kmeans(scale(cluster_cols), 3, nstart = 25)
# Get cluster assignments for each data point
res.km$cluster
# Visualize the clustering results using a scatter plot with ellipses
fviz_cluster(res.km, 
             data = cluster_cols,  # Specify the data used for clustering
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),  # Define colors for clusters
             geom = "point",  # Use points to represent data points
             ellipse.type = "convex",  # Use convex hulls to represent cluster shapes
             ggtheme = theme_bw()  # Set the plot theme to black and white
)


#THE ELBOW METHOD
# Normalize the data
normalized_data <- scale(cluster_cols)
# Determine optimal number of clusters using the elbow method
wss <- c()
for (i in 1:10) {
  kmeans_model <- kmeans(normalized_data, centers = i)
  wss[i] <- sum(kmeans_model$withinss)
}
# Plot the elbow curve
plot(1:10, wss, type = "b", xlab = "Number of Clusters (K)", ylab = "Within-cluster Sum of Squares (WSS)", main = "Elbow Method")

# From the plot, determine the optimal value of K

# Set random seed for reproducibility
set.seed(123)
# Perform K-means clustering with 5 clusters and 25 random starts
res.km <- kmeans(scale(cluster_cols), 5, nstart = 25)
# Get cluster assignments for each data point
res.km$cluster
# Visualize the clustering results using a scatter plot with ellipses
fviz_cluster(res.km, 
             data = cluster_cols,  # Specify the data used for clustering
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FF5733", "#7CFC00"),  # Define colors for clusters
             geom = "point",  # Use points to represent data points
             ellipse.type = "convex",  # Use convex hulls to represent cluster shapes
             ggtheme = theme_bw(),  # Set the plot theme to black and white
             main = "Cluster Plot for the optimal value of k"  # Title for the plot
)




#PAM CLUSTERING
require(cluster)
pam.res <- pam(normalized_data, 5)
# Visualize the PAM clustering
fviz_cluster(pam.res, geom="point", ellipse.type = "norm") +
  ggtitle("Cluster Plot with Partitioning Around Medoids (PAM)")



#HIERARCHICAL CLUSTERING
# I have already normalized my data
#Use hcut() which compute hclust and cut the tree
hc.cut <- hcut(normalized_data, k = 5, hc_method = "complete")
#visualise the cluster
fviz_cluster(hc.cut, ellipse.type = "convex") +
  ggtitle("Cluster Plot with Hierarchical Clustering")
