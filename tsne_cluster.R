install.packages("Rtsne")
install.packages("ggplot2")
library(Rtsne)
library(ggplot2)


# Selecting only the specified columns for t-SNE
data_for_tsne <- df[, c("ARC", "HellaSwag", "MMLU", "TruthfulQA", "Winogrande", "GSM8K")]

# Remove any duplicate rows
data_for_tsne_unique <- unique(data_for_tsne)

# Running t-SNE on the unique data
set.seed(42)  # for reproducibility
tsne_results <- Rtsne(data_for_tsne_unique, dims = 2, perplexity=30, verbose=TRUE)

# Assuming "X.Param. B" is now numeric, recategorize it
df$X.Param.B.Categorized <- cut(df$`X.Param.B`,
                                breaks = c(-Inf, 1.5, 3, 7, 13, 35, 60, 70, Inf),  # 9 breaks define 8 intervals
                                labels = c("~1.5", "~3", "~7", "~13", "~35", "~60", "~70", "70+"),  # 8 labels for 8 intervals
                                right = FALSE)




# Ensure df is filtered or adjusted to match data_for_tsne_unique
# This might involve removing duplicates or applying the same filters
df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Now, df_filtered should have the same number of rows as data_for_tsne_unique
# Confirm this:
if (nrow(df_filtered) == nrow(data_for_tsne_unique)) {
  # Convert the t-SNE results into a data frame
  tsne_data <- as.data.frame(tsne_results$Y)

  # Add the categorized "X.Param. B" to the t-SNE data
  tsne_data$X.Param.B <- df_filtered$X.Param.B.Categorized

  # Continue with plotting...
} else {
  stop("Row numbers don't match between t-SNE data and original data.")
}


# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="red", "~3"="blue", "~7"="green", "~13"="yellow", "~35"="orange", "~60"="purple", "70+"="brown")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Parameters (Billion)")


#
# Open a PostScript device
postscript("tsne_visualization.eps", width = 6, height = 6)  # Adjust size as needed
# Ensure the data is prepared and t-SNE is run as in previous steps

# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="grey", "~3"="skyblue", "~7"="blue", "~13"="red", "~35"="orange", "~60"="purple", "70+"="black")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Params (Billion)")

dev.off()



###

tsne_data <- as.data.frame(tsne_results$Y)

# Ensure the "Types" categories match the rows used in t-SNE
df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Add the "Types" categories to the t-SNE data
tsne_data$Type <- df_filtered$Type

# Open a PostScript device
postscript("tsne_types_visualization.eps", width = 6, height = 6)

# Ensure the data is prepared and t-SNE is run as in previous steps

# Plotting using ggplot2 with specified colors
ggplot(tsne_data, aes(x = V1, y = V2, color = Type)) +
  geom_point() +
  scale_color_manual(values = c("grey", "red", "blue", "yellow", "black")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by model training types")

# Close the PostScript device
dev.off()

##

install.packages("umap")
install.packages("ggplot2")
library(umap)
library(ggplot2)
# Selecting only the specified columns for UMAP
data_for_umap <- df[, c("ARC", "HellaSwag", "MMLU", "TruthfulQA", "Winogrande", "GSM8K")]

# Remove any duplicate rows
data_for_umap_unique <- unique(data_for_umap)

# Ensure "Architecture" is a factor representing categories
df$Architecture <- as.factor(df$Architecture)
# Running UMAP on the unique data
set.seed(42)  # for reproducibility
umap_results <- umap(data_for_umap_unique)
# Convert the UMAP results into a data frame
umap_data <- as.data.frame(umap_results$layout)

# Ensure the "Architecture" categories match the rows used in UMAP
df_filtered <- df[rownames(df) %in% rownames(data_for_umap_unique), ]

# Add the "Architecture" categories to the UMAP data
umap_data$Architecture <- df_filtered$Architecture

# Plotting using ggplot2 with specified colors
ggplot(umap_data, aes(x = V1, y = V2, color = Architecture)) +
  geom_point() +
  theme_minimal() +
  ggtitle("UMAP visualization by Architecture")

# Open a PostScript device
postscript("umap_architecture_visualization.eps", width = 6, height = 6)

# Plotting using ggplot2 with specified colors
ggplot(umap_data, aes(x = V1, y = V2, color = Architecture)) +
  geom_point() +
  theme_minimal() +
  ggtitle("UMAP visualization by Architecture")

# Close the PostScript device
dev.off()

# Open a larger PostScript device
postscript("umap_architecture_visualization1.eps", width = 10, height = 8)  # Adjust size as needed
# Plotting using ggplot2 with externalized legend
ggplot(umap_data, aes(x = V1, y = V2, color = Architecture)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "right") +  # Adjust legend position
  guides(color = guide_legend(override.aes = list(size=0.5))) +  # Adjust legend aesthetics
  ggtitle("UMAP visualization by Architecture")



dev.off()


###

# Selecting only the specified columns for t-SNE
data_for_tsne <- df[, c("TruthfulQA", "Winogrande")]

# Remove any duplicate rows
data_for_tsne_unique <- unique(data_for_tsne)

# Running t-SNE on the unique data
set.seed(688)  # for reproducibility
tsne_results <- Rtsne(data_for_tsne_unique, dims = 2, perplexity=30, verbose=TRUE)

# Assuming "X.Param. B" is now numeric, recategorize it
df$X.Param.B.Categorized <- cut(df$`X.Param.B`,
                                breaks = c(-Inf, 1.5, 3, 7, 13, 35, 60, 70, Inf),  # 9 breaks define 8 intervals
                                labels = c("~1.5", "~3", "~7", "~13", "~35", "~60", "~70", "70+"),  # 8 labels for 8 intervals
                                right = FALSE)




# Ensure df is filtered or adjusted to match data_for_tsne_unique
# This might involve removing duplicates or applying the same filters
df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Now, df_filtered should have the same number of rows as data_for_tsne_unique
# Confirm this:
if (nrow(df_filtered) == nrow(data_for_tsne_unique)) {
  # Convert the t-SNE results into a data frame
  tsne_data <- as.data.frame(tsne_results$Y)

  # Add the categorized "X.Param. B" to the t-SNE data
  tsne_data$X.Param.B <- df_filtered$X.Param.B.Categorized

  # Continue with plotting...
} else {
  stop("Row numbers don't match between t-SNE data and original data.")
}




#
# Open a PostScript device
postscript("tsne_visualization_tq_wi.eps", width = 6, height = 6)  # Adjust size as needed
# Ensure the data is prepared and t-SNE is run as in previous steps

# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="grey", "~3"="skyblue", "~7"="blue", "~13"="red", "~35"="orange", "~60"="purple", "70+"="black")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Params (Billion) with TQ_Wi")

dev.off()



##############################################
scaled data
############################################################


library(Rtsne)
library(ggplot2)


# Selecting only the specified columns for t-SNE
data_for_tsne <- df1[, c("ARCScaled1", "HellaSwagScaled1", "MMLUScaled1", "TruthfulQAScaled1", "WinograndeScaled1", "GSM8KScaled1")]

# Remove any duplicate rows
data_for_tsne_unique <- unique(data_for_tsne)
# Ensure data_for_tsne_unique is a dataframe
data_for_tsne_unique <- as.data.frame(data_for_tsne_unique)

# Replace infinite values with NA in each column
for (col in names(data_for_tsne_unique)) {
    data_for_tsne_unique[[col]][is.infinite(data_for_tsne_unique[[col]])] <- NA
}

# Remove rows with any NA values
data_for_tsne_clean <- na.omit(data_for_tsne_unique)

# Running t-SNE on the clean data
# Running t-SNE on the cleaned data
set.seed(688)  # for reproducibility
tsne_results <- Rtsne(data_for_tsne_clean, dims = 2, perplexity = 30, verbose = TRUE)

# Assuming "X.Param. B" is now numeric, recategorize it
df1$X.Param.B.Categorized <- cut(df1$`X.Param.B.`,
                                breaks = c(-Inf, 1.5, 3, 7, 13, 35, 60, 70, Inf),  # 9 breaks define 8 intervals
                                labels = c("~1.5", "~3", "~7", "~13", "~35", "~60", "~70", "70+"),  # 8 labels for 8 intervals
                                right = FALSE)

# Ensure df1 is filtered or adjusted to match data_for_tsne_clean
# This might involve removing duplicates or applying the same filters
df_filtered <- df1[rownames(df1) %in% rownames(data_for_tsne_clean), ]

# Sort df_filtered to match the order of data_for_tsne_clean
df_filtered <- df_filtered[match(rownames(data_for_tsne_clean), rownames(df_filtered)), ]

# Now, df_filtered should have the same number of rows as data_for_tsne_clean
# Confirm this:
if (nrow(df_filtered) == nrow(data_for_tsne_clean)) {
    tsne_data <- as.data.frame(tsne_results$Y)
    tsne_data$X.Param.B <- df_filtered$X.Param.B.Categorized
    # Continue with plotting...
} else {
    stop("Row numbers don't match between t-SNE data and original data.")
}



# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="red", "~3"="blue", "~7"="green", "~13"="yellow", "~35"="orange", "~60"="purple", "70+"="brown")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Parameters (Billion) (scaled data)")


#
# Open a PostScript device
postscript("tsne_visualization_scaled_para2.eps", width = 6, height = 6)  # Adjust size as needed
# Ensure the data is prepared and t-SNE is run as in previous steps

# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="beige", "~3"="grey", "~7"="blue", "~13"="red", "~35"="yellow", "~60"="green", "70+"="black")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by model params (Billion) (scaled data)") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

dev.off()




###types

#tsne_data <- as.data.frame(tsne_results$Y)

# Ensure the "Types" categories match the rows used in t-SNE
#df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Add the "Types" categories to the t-SNE data
tsne_data$Type <- df_filtered$Type



# Open a PostScript device
postscript("tsne_types_visualization_scaled2.eps", width = 6, height = 6)

# Ensure the data is prepared and t-SNE is run as in previous steps

# Add a new category "Unknown" to the data for empty values in "Type"
#tsne_data$Type[is.na(tsne_data$Type)] <- "Unknown"

# Plotting using ggplot2 with specified colors
gg <- ggplot(tsne_data, aes(x = V1, y = V2, color = Type)) +
  geom_point() +
  scale_color_manual(values = c("grey", "red", "blue", "yellow", "black"), 
                     labels = c("Unknown", "fine-tuned", "instruction-tuned", "pretrained", "RL-tuned")) +  # Added "Unknown" label
  theme_minimal() +
  ggtitle("t-SNE visualization by model training types (scaled data)") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

# Print the plot
print(gg)

# Close the PostScript device
dev.off()




###architetures

#tsne_data <- as.data.frame(tsne_results$Y)

# Ensure the "Types" categories match the rows used in t-SNE
#df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Add the "Types" categories to the t-SNE data
tsne_data$Architecture <- df_filtered$Category1



# Open a PostScript device
postscript("tsne_architecture_visualization_scaled2.eps", width = 6, height = 6)

# Ensure the data is prepared and t-SNE is run as in previous steps

# Add a new category "Unknown" to the data for empty values in "Type"
#tsne_data$Type[is.na(tsne_data$Type)] <- "Unknown"

# Plotting using ggplot2 with specified colors
gg <- ggplot(tsne_data, aes(x = V1, y = V2, color = Architecture)) +
  geom_point() +
  scale_color_manual(values = c("grey", "red",  "yellow", "gold", "green", "cyan","blue", "coral", "black", "cornsilk", "plum", "snow4")) +  # Added "Unknown" label
  theme_minimal() +
  ggtitle("t-SNE visualization by model training architectures (scaled data)") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

# Print the plot
print(gg)

# Close the PostScript device
dev.off()

################
raw data
######################################


library(Rtsne)
library(ggplot2)


# Selecting only the specified columns for t-SNE
data_for_tsne <- df1[, c("ARC", "HellaSwag", "MMLU", "TruthfulQA", "Winogrande", "GSM8K")]

# Remove any duplicate rows
data_for_tsne_unique <- unique(data_for_tsne)
# Ensure data_for_tsne_unique is a dataframe
data_for_tsne_unique <- as.data.frame(data_for_tsne_unique)

# Replace infinite values with NA in each column
for (col in names(data_for_tsne_unique)) {
    data_for_tsne_unique[[col]][is.infinite(data_for_tsne_unique[[col]])] <- NA
}

# Remove rows with any NA values
data_for_tsne_clean <- na.omit(data_for_tsne_unique)

# Running t-SNE on the clean data
# Running t-SNE on the cleaned data
set.seed(688)  # for reproducibility
tsne_results <- Rtsne(data_for_tsne_clean, dims = 2, perplexity = 30, verbose = TRUE)

# Assuming "X.Param. B" is now numeric, recategorize it
df1$X.Param.B.Categorized <- cut(df1$`X.Param.B.`,
                                breaks = c(-Inf, 1.5, 3, 7, 13, 35, 60, 70, Inf),  # 9 breaks define 8 intervals
                                labels = c("~1.5", "~3", "~7", "~13", "~35", "~60", "~70", "70+"),  # 8 labels for 8 intervals
                                right = FALSE)

# Ensure df1 is filtered or adjusted to match data_for_tsne_clean
# This might involve removing duplicates or applying the same filters
df_filtered <- df1[rownames(df1) %in% rownames(data_for_tsne_clean), ]

# Sort df_filtered to match the order of data_for_tsne_clean
df_filtered <- df_filtered[match(rownames(data_for_tsne_clean), rownames(df_filtered)), ]

# Now, df_filtered should have the same number of rows as data_for_tsne_clean
# Confirm this:
if (nrow(df_filtered) == nrow(data_for_tsne_clean)) {
    tsne_data <- as.data.frame(tsne_results$Y)
    tsne_data$X.Param.B <- df_filtered$X.Param.B.Categorized
    # Continue with plotting...
} else {
    stop("Row numbers don't match between t-SNE data and original data.")
}



# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="red", "~3"="blue", "~7"="green", "~13"="yellow", "~35"="orange", "~60"="purple", "70+"="brown")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Parameters (Billion)")


#
# Open a PostScript device
postscript("tsne_visualization_para2.eps", width = 6, height = 6)  # Adjust size as needed
# Ensure the data is prepared and t-SNE is run as in previous steps

# Plotting using ggplot2
ggplot(tsne_data, aes(x = V1, y = V2, color = X.Param.B)) +
  geom_point() +
  scale_color_manual(values = c("~1.5"="beige", "~3"="grey", "~7"="blue", "~13"="red", "~35"="yellow", "~60"="green", "70+"="black")) +
  theme_minimal() +
  ggtitle("t-SNE visualization by Model Params (Billion)") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

dev.off()




###types

#tsne_data <- as.data.frame(tsne_results$Y)

# Ensure the "Types" categories match the rows used in t-SNE
#df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Add the "Types" categories to the t-SNE data
tsne_data$Type <- df_filtered$Type



# Open a PostScript device
postscript("tsne_types_visualization2.eps", width = 6, height = 6)

# Ensure the data is prepared and t-SNE is run as in previous steps

# Add a new category "Unknown" to the data for empty values in "Type"
#tsne_data$Type[is.na(tsne_data$Type)] <- "Unknown"

# Plotting using ggplot2 with specified colors
gg <- ggplot(tsne_data, aes(x = V1, y = V2, color = Type)) +
  geom_point() +
  scale_color_manual(values = c("grey", "red", "blue", "yellow", "black"), 
                     labels = c("Unknown", "fine-tuned", "instruction-tuned", "pretrained", "RL-tuned")) +  # Added "Unknown" label
  theme_minimal() +
  ggtitle("t-SNE visualization by model training types") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

# Print the plot
print(gg)

# Close the PostScript device
dev.off()




###architetures

#tsne_data <- as.data.frame(tsne_results$Y)

# Ensure the "Types" categories match the rows used in t-SNE
#df_filtered <- df[rownames(df) %in% rownames(data_for_tsne_unique), ]

# Add the "Types" categories to the t-SNE data
tsne_data$Architecture <- df_filtered$Category1



# Open a PostScript device
postscript("tsne_architecture_visualization2.eps", width = 6, height = 6)

# Ensure the data is prepared and t-SNE is run as in previous steps

# Add a new category "Unknown" to the data for empty values in "Type"
#tsne_data$Type[is.na(tsne_data$Type)] <- "Unknown"

# Plotting using ggplot2 with specified colors
gg <- ggplot(tsne_data, aes(x = V1, y = V2, color = Architecture)) +
  geom_point() +
  scale_color_manual(values = c("grey", "red",  "yellow", "gold", "green", "cyan","blue", "coral", "black", "cornsilk", "plum", "snow4")) +  # Added "Unknown" label
  theme_minimal() +
  ggtitle("t-SNE visualization by model training architectures") +
  theme(
    legend.position = "bottom",  # Place the legend at the bottom
    legend.direction = "horizontal"  # Use a horizontal legend
  )

# Print the plot
print(gg)

# Close the PostScript device
dev.off()



