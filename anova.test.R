
head(dfx)
  Average   ARC HellaSwag  MMLU TruthfulQA Winogrande GSM8K              Type
1   52.68 56.66     81.09 53.30      43.99      73.01  8.04        fine-tuned
2   52.68 29.10     82.27 71.37      55.97      77.35  0.00        fine-tuned
3   52.66 54.10     77.91 54.49      49.36      70.17  9.93 instruction-tuned
4   52.66 59.22     81.02 53.73      39.70      73.64  8.64        fine-tuned
5   52.65 57.25     81.94 53.65      38.03      76.09  8.95        fine-tuned
6   52.62 52.47     78.02 48.42      45.47      72.69 18.65 instruction-tuned
  X.Param.B. Category    Range
1      12.85    Llama   [7,13)
2      46.74    Other [35,177]
3       7.11  Mistral   [7,13)
4      12.85    Llama   [7,13)
5      12.85    Llama   [7,13)
6       7.00    Llama   [7,13)
> colnames(dfx)
 [1] "Average"    "ARC"        "HellaSwag"  "MMLU"       "TruthfulQA"
 [6] "Winogrande" "GSM8K"      "Type"       "X.Param.B." "Category"  
[11] "Range" 


table(dfx$Range)

 [0,1.5)  [1.5,3)    [3,7)   [7,13)  [13,35) [35,177] 
     299       64      464      234      128       19

arcv1=dfx[dfx$Range=="[0,1.5)",]$ARC
arcv2=dfx[dfx$Range=="[1.5,3)",]$ARC

 arcv3=dfx[dfx$Range=="[3,7)",]$ARC

arcv4=dfx[dfx$Range=="[7,13)",]$ARC
 arcv5=dfx[dfx$Range=="[13,35)",]$ARC
arcv6=dfx[dfx$Range=="[35,177)",]$ARC



I want to do the same thing for the columns "HllaSwag", "MMLU", "TruthfulQA", "Winogrande" and "GSM8K"


##function to extract the data using range of parameters

dfx$Range <- cut(dfx$X.Param.B., breaks, include.lowest = TRUE, right = FALSE)

subset_by_range <- function(dataframe, column_name, ranges) {
  # Create an empty list to store the subsets
  subsets <- list()

  # Loop through each range and extract the subset for the given column
  for (range in ranges) {
    subsets[[range]] <- dataframe[dataframe$Range == range,][[column_name]]
  }

  return(subsets)
}

# Define the ranges
ranges <- c("[0,1.5)", "[1.5,3)", "[3,7)", "[7,13)", "[13,35)", "[35,177)")

# Example usage for the "ARC" column
arc_subsets <- subset_by_range(dfx, "ARC", ranges)

# Example usage for the "HellaSwag" column
hswag_subsets <- subset_by_range(dfx, "HellaSwag", ranges)

# And so on for other columns...

mmlu_subsets <- subset_by_range(dfx, "MMLU", ranges)

truthfulqa_subsets <- subset_by_range(dfx, "TruthfulQA", ranges)

winogrande_subsets <- subset_by_range(dfx, "Winogrande", ranges)

gsm8k_subsets <- subset_by_range(dfx, "GSM8K", ranges)



##testing

perform_anova <- function(subsets, column_name) {
  # Filter out empty subsets
  non_empty_subsets <- subsets[sapply(subsets, function(x) length(x) > 0)]

  # Create a list of data frames only for non-empty subsets
  data_list <- lapply(names(non_empty_subsets), function(group) {
    data.frame(value = non_empty_subsets[[group]], group = group)
  })

  # Combine into a single data frame
  combined_data <- bind_rows(data_list)

  # Check if combined data is not empty
  if (nrow(combined_data) > 0) {
    # Perform ANOVA
    anova_result <- aov(value ~ group, data = combined_data)
    anova_summary <- summary(anova_result)

    # Extract the p-value
    p_value <- anova_summary[[1]]["group", "Pr(>F)"]

    # Check if the p-value is less than 0.05 and proceed with post-hoc test
    if (p_value < 0.05) {
      post_hoc_result <- TukeyHSD(anova_result)
      cat("Post-hoc results for", column_name, ":\n")
      print(post_hoc_result)
    } else {
      cat("ANOVA result for", column_name, "is not significant; no need for post-hoc test.\n")
    }
  } else {
    cat("No data available for ANOVA in", column_name, "\n")
  }
}

# Example usage
perform_anova(hswag_subsets, "HellaSwag")

#
Post-hoc results for HellaSwag :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                     diff       lwr       upr     p adj
[1.5,3)-[0,1.5) 17.054605 11.409646 22.699564 0.0000000
[13,35)-[0,1.5) 19.277418 14.948230 23.606606 0.0000000
[3,7)-[0,1.5)   30.526744 27.487254 33.566235 0.0000000
[7,13)-[0,1.5)  28.405136 24.827854 31.982417 0.0000000
[13,35)-[1.5,3)  2.222813 -4.051826  8.497451 0.8696093
[3,7)-[1.5,3)   13.472139  8.007002 18.937276 0.0000000
[7,13)-[1.5,3)  11.350530  5.568994 17.132067 0.0000010
[3,7)-[13,35)   11.249327  7.157380 15.341273 0.0000000
[7,13)-[13,35)   9.127718  4.621892 13.633544 0.0000004
[7,13)-[3,7)    -2.121609 -5.407808  1.164590 0.3953885


perform_anova(arc_subsets, "ARC")
 perform_anova(arc_subsets, "ARC")
Post-hoc results for ARC :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff       lwr       upr     p adj
[1.5,3)-[0,1.5)  7.4093327  4.274415 10.544251 0.0000000
[13,35)-[0,1.5) 14.4483952 12.044189 16.852602 0.0000000
[3,7)-[0,1.5)   18.6471721 16.959197 20.335148 0.0000000
[7,13)-[0,1.5)  18.9918580 17.005221 20.978495 0.0000000
[13,35)-[1.5,3)  7.0390625  3.554453 10.523672 0.0000004
[3,7)-[1.5,3)   11.2378394  8.202785 14.272893 0.0000000
[7,13)-[1.5,3)  11.5825254  8.371759 14.793291 0.0000000
[3,7)-[13,35)    4.1987769  1.926322  6.471232 0.0000051
[7,13)-[13,35)   4.5434629  2.041160  7.045765 0.0000080
[7,13)-[3,7)     0.3446859 -1.480299  2.169671 0.9857806

##

 perform_anova(mmlu_subsets, "MMLU")
#Post-hoc results for MMLU :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff        lwr       upr     p adj
[1.5,3)-[0,1.5)  0.5318865 -2.7997472  3.863520 0.9924826
[13,35)-[0,1.5) 10.0071209  7.4520505 12.562191 0.0000000
[3,7)-[0,1.5)   11.2175007  9.4236050 13.011396 0.0000000
[7,13)-[0,1.5)  13.3182460 11.2069477 15.429544 0.0000000
[13,35)-[1.5,3)  9.4752344  5.7719663 13.178502 0.0000000
[3,7)-[1.5,3)   10.6856142  7.4601109 13.911118 0.0000000
[7,13)-[1.5,3)  12.7863595  9.3741183 16.198601 0.0000000
[3,7)-[13,35)    1.2103798 -1.2046716  3.625431 0.6475804
[7,13)-[13,35)   3.3111251  0.6518034  5.970447 0.0062181
[7,13)-[3,7)     2.1007453  0.1612430  4.040248 0.0260882

##
 perform_anova(truthfulqa_subsets, "TruthfulQA")

#Post-hoc results for TruthfulQA :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff        lwr        upr     p adj
[1.5,3)-[0,1.5) -3.9523061 -6.8639119 -1.0407004 0.0020322
[13,35)-[0,1.5) -0.2984780 -2.5314239  1.9344679 0.9962070
[3,7)-[0,1.5)   -0.8061046 -2.3738391  0.7616299 0.6247158
[7,13)-[0,1.5)  -0.1570030 -2.0021243  1.6881183 0.9993538
[13,35)-[1.5,3)  3.6538281  0.4174409  6.8902154 0.0177704
[3,7)-[1.5,3)    3.1462015  0.3273459  5.9650571 0.0198257
[7,13)-[1.5,3)   3.7953032  0.8132522  6.7773541 0.0047670
[3,7)-[13,35)   -0.5076266 -2.6182061  1.6029529 0.9653253
[7,13)-[13,35)   0.1414750 -2.1825790  2.4655290 0.9998286
[7,13)-[3,7)     0.6491016 -1.0458825  2.3440858 0.8337023

###
 perform_anova(winogrande_subsets, "Winogrande")

#Post-hoc results for Winogrande :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff        lwr       upr     p adj
[1.5,3)-[0,1.5)  5.8337119  3.3288132  8.338611 0.0000000
[13,35)-[0,1.5) 12.4065244 10.4854871 14.327562 0.0000000
[3,7)-[0,1.5)   13.6935125 12.3447667 15.042258 0.0000000
[7,13)-[0,1.5)  13.3772575 11.7898716 14.964643 0.0000000
[13,35)-[1.5,3)  6.5728125  3.7884994  9.357126 0.0000000
[3,7)-[1.5,3)    7.8598006  5.4346964 10.284905 0.0000000
[7,13)-[1.5,3)   7.5435457  4.9780420 10.109049 0.0000000
[3,7)-[13,35)    1.2869881 -0.5287755  3.102752 0.2985599
[7,13)-[13,35)   0.9707332 -1.0286858  2.970152 0.6747727
[7,13)-[3,7)    -0.3162550 -1.7744756  1.141966 0.9762190


###
 perform_anova(gsm8k_subsets, "GSM8K")

##
Post-hoc results for GSM8K :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff       lwr         upr     p adj
[1.5,3)-[0,1.5)  0.2919779 -1.345216 1.929171417 0.9885453
[13,35)-[0,1.5)  4.7310404  3.475457 5.986624037 0.0000000
[3,7)-[0,1.5)    3.5533411  2.671805 4.434876899 0.0000000
[7,13)-[0,1.5)   4.2648532  3.227343 5.302363364 0.0000000
[13,35)-[1.5,3)  4.4390625  2.619245 6.258880326 0.0000000
[3,7)-[1.5,3)    3.2613631  1.676323 4.846403255 0.0000002
[7,13)-[1.5,3)   3.9728753  2.296071 5.649679990 0.0000000
[3,7)-[13,35)   -1.1776994 -2.364476 0.009077727 0.0529244
[7,13)-[13,35)  -0.4661872 -1.773001 0.840626362 0.8666828
[7,13)-[3,7)     0.7115121 -0.241576 1.664600259 0.2477577

###test

# Load dplyr for bind_rows
library(dplyr)

# Create a list of data frames
data_list <- list(
  data.frame(value = arcv1, group = "arcv1"),
  data.frame(value = arcv2, group = "arcv2"),
  data.frame(value = arcv3, group = "arcv3"),
  data.frame(value = arcv4, group = "arcv4"),
  data.frame(value = arcv5, group = "arcv5")
)

# Combine into a single data frame
combined_data <- bind_rows(data_list)

# Now you can perform ANOVA
# Perform ANOVA
anova_result <- aov(value ~ group, data = combined_data)
anova_summary <- summary(anova_result)

# Extract the p-value
p_value <- anova_summary[[1]]["group", "Pr(>F)"]

# Check if the p-value is less than 0.05 and proceed with post-hoc test
if (p_value < 0.05) {
  post_hoc_result <- TukeyHSD(anova_result)
  print(post_hoc_result)
} else {
  print("ANOVA result is not significant; no need for post-hoc test.")
}



##################################################################
##############################################################



##function to extract the data using types

subset_by_type <- function(dataframe, column_name, types) {
  # Create an empty list to store the subsets
  subsets <- list()

  # Loop through each range and extract the subset for the given column
  for (type in types) {
    subsets[[type]] <- dataframe[dataframe$Type == type,][[column_name]]
  }

  return(subsets)
}

# Define the ranges
types <- c("fine-tuned", "instruction-tuned", "pretrained", "RL-tuned", "")

# Example usage for the "ARC" column
arc1_subsets <- subset_by_type(dfx, "ARC", types)

# Example usage for the "HellaSwag" column
hswag1_subsets <- subset_by_type(dfx, "HellaSwag", types)

# And so on for other columns...



# Corrected usage for other columns
mmlu1_subsets <- subset_by_type(dfx, "MMLU", types)
truthfulqa1_subsets <- subset_by_type(dfx, "TruthfulQA", types)
winogrande1_subsets <- subset_by_type(dfx, "Winogrande", types)
gsm8k1_subsets <- subset_by_type(dfx, "GSM8K", types)


##testing
##testing
perform_anova <- function(subsets, column_name) {
  # Filter out empty subsets
  non_empty_subsets <- subsets[sapply(subsets, function(x) length(x) > 0)]

  # Create a list of data frames only for non-empty subsets
  data_list <- lapply(names(non_empty_subsets), function(group) {
    if (length(non_empty_subsets[[group]]) > 0) {
      return(data.frame(value = non_empty_subsets[[group]], group = group))
    }
  })

  # Remove NULL elements (which occur if a subset was empty)
  data_list <- Filter(Negate(is.null), data_list)

  # Combine into a single data frame
  combined_data <- bind_rows(data_list)

  # Check if combined data is not empty
  if (nrow(combined_data) > 0) {
    # Perform ANOVA
    anova_result <- aov(value ~ group, data = combined_data)
    anova_summary <- summary(anova_result)

    # Extract the p-value
    p_value <- anova_summary[[1]]["group", "Pr(>F)"]

    # Check if the p-value is less than 0.05 and proceed with post-hoc test
    if (p_value < 0.05) {
      post_hoc_result <- TukeyHSD(anova_result)
      cat("Post-hoc results for", column_name, ":\n")
      print(post_hoc_result)
    } else {
      cat("ANOVA result for", column_name, "is not significant; no need for post-hoc test.\n")
    }
  } else {
    cat("No data available for ANOVA in", column_name, "\n")
  }
}

# Example usage
perform_anova(hswag1_subsets, "HellaSwag")


#Post-hoc results for HellaSwag :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                                    diff       lwr        upr     p adj
instruction-tuned-fine-tuned  -0.5327538  -4.63521  3.5697028 0.9871419
pretrained-fine-tuned        -10.5038331 -14.73593 -6.2717401 0.0000000
RL-tuned-fine-tuned          -11.5358046 -22.40249 -0.6691219 0.0324468
pretrained-instruction-tuned  -9.9710793 -15.29474 -4.6474220 0.0000098
RL-tuned-instruction-tuned   -11.0030508 -22.33952  0.3334172 0.0608708
RL-tuned-pretrained           -1.0319715 -12.41599 10.3520510 0.9955318

##
perform_anova(arc1_subsets, "ARC")
#Post-hoc results for ARC :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                                   diff        lwr        upr     p adj
instruction-tuned-fine-tuned -0.7306568  -3.135547  1.6742333 0.8627760
pretrained-fine-tuned        -8.1775760 -10.658460 -5.6966921 0.0000000
RL-tuned-fine-tuned          -5.5219401 -11.892069  0.8481891 0.1157081
pretrained-instruction-tuned -7.4469192 -10.567686 -4.3261522 0.0000000
RL-tuned-instruction-tuned   -4.7912833 -11.436804  1.8542375 0.2482975
RL-tuned-pretrained           2.6556359  -4.017762  9.3290335 0.7354807

###
perform_anova(mmlu1_subsets, "MMLU")
Post-hoc results for MMLU :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                                   diff        lwr       upr     p adj
instruction-tuned-fine-tuned  0.4362427  -1.808535  2.681020 0.9590616
pretrained-fine-tuned        -6.8448772  -9.160589 -4.529165 0.0000000
RL-tuned-fine-tuned          -3.3710938  -9.317114  2.574926 0.4630107
pretrained-instruction-tuned -7.2811200 -10.194113 -4.368127 0.0000000
RL-tuned-instruction-tuned   -3.8073366 -10.010413  2.395740 0.3909314
RL-tuned-pretrained           3.4737834  -2.755314  9.702881 0.4777870


###
perform_anova(truthfulqa1_subsets, "TruthfuflQA")
ANOVA result for TruthfuflQA is not significant; no need for post-hoc test.


> perform_anova(winogrande1_subsets, "Winogrande")
Post-hoc results for Winogrande :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                                   diff       lwr        upr     p adj
instruction-tuned-fine-tuned -0.3880087 -2.253808  1.4777910 0.9505005
pretrained-fine-tuned        -5.1907209 -7.115479 -3.2659625 0.0000000
RL-tuned-fine-tuned          -4.6331338 -9.575307  0.3090399 0.0753851
pretrained-instruction-tuned -4.8027122 -7.223915 -2.3815098 0.0000023
RL-tuned-instruction-tuned   -4.2451251 -9.400957  0.9107073 0.1478202
RL-tuned-pretrained           0.5575871 -4.619873  5.7350473 0.9925704

> perform_anova(gsm8k1_subsets, "GSM8K")
Post-hoc results for GSM8K :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                                   diff         lwr        upr     p adj
instruction-tuned-fine-tuned  1.0325763 -0.02707363  2.0922263 0.0593403
pretrained-fine-tuned        -1.2992075 -2.39234205 -0.2060729 0.0122091
RL-tuned-fine-tuned          -0.4450669 -3.25189247  2.3617586 0.9770644
pretrained-instruction-tuned -2.3317838 -3.70686555 -0.9567020 0.0000826
RL-tuned-instruction-tuned   -1.4776433 -4.40581268  1.4505262 0.5641361
RL-tuned-pretrained           0.8541405 -2.08631204  3.7945931 0.8777964

> 


############################################################################
############################################################################


> dim(dfx)
[1] 1208   11


##function to extract the data using categories

subset_by_cat <- function(dataframe, column_name, categories) {
  # Create an empty list to store the subsets
  subsets <- list()

  # Loop through each range and extract the subset for the given column
  for (category in categories) {
    subsets[[category]] <- dataframe[dataframe$Category == category,][[column_name]]
  }

  return(subsets)
}

# Define the ranges
categories <- c("Bloom", "GPT", "Llama", "Mistral", "Other")

# Example usage for the "ARC" column
arc2_subsets <- subset_by_cat(dfx, "ARC", categories)

# Example usage for the "HellaSwag" column
hswag2_subsets <- subset_by_cat(dfx, "HellaSwag", categories)

# And so on for other columns...



# Corrected usage for other columns
mmlu2_subsets <- subset_by_cat(dfx, "MMLU", categories)
truthfulqa2_subsets <- subset_by_cat(dfx, "TruthfulQA", categories)
winogrande2_subsets <- subset_by_cat(dfx, "Winogrande", categories)
gsm8k2_subsets <- subset_by_cat(dfx, "GSM8K", categories)


##testing
perform_anova <- function(subsets, column_name) {
  # Filter out empty subsets
  non_empty_subsets <- subsets[sapply(subsets, function(x) length(x) > 0)]

  # Create a list of data frames only for non-empty subsets
  data_list <- lapply(names(non_empty_subsets), function(group) {
    if (length(non_empty_subsets[[group]]) > 0) {
      return(data.frame(value = non_empty_subsets[[group]], group = group))
    }
  })

  # Remove NULL elements (which occur if a subset was empty)
  data_list <- Filter(Negate(is.null), data_list)

  # Combine into a single data frame
  combined_data <- bind_rows(data_list)

  # Check if combined data is not empty
  if (nrow(combined_data) > 0) {
    # Perform ANOVA
    anova_result <- aov(value ~ group, data = combined_data)
    anova_summary <- summary(anova_result)

    # Extract the p-value
    p_value <- anova_summary[[1]]["group", "Pr(>F)"]

    # Check if the p-value is less than 0.05 and proceed with post-hoc test
    if (p_value < 0.05) {
      post_hoc_result <- TukeyHSD(anova_result)
      cat("Post-hoc results for", column_name, ":\n")
      print(post_hoc_result)
    } else {
      cat("ANOVA result for", column_name, "is not significant; no need for post-hoc test.\n")
    }
  } else {
    cat("No data available for ANOVA in", column_name, "\n")
  }
}

# Example usage
perform_anova(hswag2_subsets, "HellaSwag")

##Post-hoc results for HellaSwag :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                    diff        lwr       upr     p adj
GPT-Bloom      -0.393874  -9.243496  8.455748 0.9999507
Llama-Bloom    17.129504   8.537954 25.721054 0.0000006
Mistral-Bloom   3.648039  -8.786362 16.082441 0.9301752
Other-Bloom     7.862174  -1.231739 16.956086 0.1266189
Llama-GPT      17.523378  14.057647 20.989110 0.0000000
Mistral-GPT     4.041913  -5.591928 13.675755 0.7818178
Other-GPT       8.256048   3.684855 12.827241 0.0000091
Mistral-Llama -13.481465 -22.878795 -4.084134 0.0008903
Other-Llama    -9.267330 -13.316371 -5.218290 0.0000000
Other-Mistral   4.214134  -5.644584 14.072853 0.7698163

 perform_anova(arc2_subsets, "ARC")
Post-hoc results for ARC :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                   diff        lwr        upr     p adj
GPT-Bloom     -1.255027 -6.2083058  3.6982510 0.9582047
Llama-Bloom   11.402901  6.5940700 16.2117327 0.0000000
Mistral-Bloom  6.752493 -0.2072456 13.7122309 0.0622035
Other-Bloom    3.763811 -1.3262004  8.8538234 0.2568664
Llama-GPT     12.657929 10.7181021 14.5977554 0.0000000
Mistral-GPT    8.007520  2.6153011 13.3997390 0.0005070
Other-GPT      5.018839  2.4602672  7.5774106 0.0000010
Mistral-Llama -4.650409 -9.9102488  0.6094313 0.1118491
Other-Llama   -7.639090 -9.9054044 -5.3727753 0.0000000
Other-Mistral -2.988681 -8.5067677  2.5294054 0.5759576

> perform_anova(mmlu2_subsets, "MMLU")
Post-hoc results for MMLU :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                   diff        lwr       upr     p adj
GPT-Bloom     -1.479409  -5.963924  3.005105 0.8964631
Llama-Bloom   10.464579   6.110841 14.818317 0.0000000
Mistral-Bloom 11.224315   4.923226 17.525405 0.0000127
Other-Bloom    3.273524  -1.334784  7.881833 0.2963543
Llama-GPT     11.943989  10.187741 13.700236 0.0000000
Mistral-GPT   12.703725   7.821809 17.585640 0.0000000
Other-GPT      4.752934   2.436498  7.069370 0.0000003
Mistral-Llama  0.759736  -4.002328  5.521800 0.9925029
Other-Llama   -7.191055  -9.242892 -5.139218 0.0000000
Other-Mistral -7.950791 -12.946662 -2.954920 0.0001451

> perform_anova(truthfulqa2_subsets, "TruthfulQA")
Post-hoc results for TruthfulQA :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                    diff        lwr        upr     p adj
GPT-Bloom      0.2019615 -3.5703066  3.9742296 0.9998972
Llama-Bloom    2.5423303 -1.1199312  6.2045919 0.3196993
Mistral-Bloom  0.9672709 -4.3330567  6.2675986 0.9875065
Other-Bloom   -1.2893697 -5.1657699  2.5870305 0.8936290
Llama-GPT      2.3403688  0.8630551  3.8176826 0.0001584
Mistral-GPT    0.7653094 -3.3412426  4.8718614 0.9864788
Other-GPT     -1.4913312 -3.4398626  0.4572002 0.2245953
Mistral-Llama -1.5750594 -5.5807957  2.4306768 0.8198398
Other-Llama   -3.8317000 -5.5576571 -2.1057429 0.0000000
Other-Mistral -2.2566406 -6.4590496  1.9457684 0.5842166

> perform_anova(winogrande2_subsets, "Winogrande")
Post-hoc results for Winogrande :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                    diff        lwr        upr     p adj
GPT-Bloom     -0.4311111 -4.2224944  3.3602722 0.9979798
Llama-Bloom    9.5853446  5.9045252 13.2661639 0.0000000
Mistral-Bloom  5.0662069 -0.2609792 10.3933930 0.0713175
Other-Bloom    4.0380412  0.1419981  7.9340843 0.0378697
Llama-GPT     10.0164557  8.5316559 11.5012554 0.0000000
Mistral-GPT    5.4973180  1.3699568  9.6246792 0.0026418
Other-GPT      4.4691523  2.5107471  6.4275576 0.0000000
Mistral-Llama -4.5191377 -8.5451722 -0.4931031 0.0187841
Other-Llama   -5.5473033 -7.2820064 -3.8126002 0.0000000
Other-Mistral -1.0281657 -5.2518696  3.1955383 0.9637919

> perform_anova(gsm8k2_subsets, "GSM8K")
Post-hoc results for GSM8K :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                    diff        lwr        upr     p adj
GPT-Bloom      0.3792747 -1.9463255  2.7048748 0.9918457
Llama-Bloom    4.2329858  1.9752045  6.4907670 0.0000035
Mistral-Bloom  4.6736158  1.4059681  7.9412634 0.0009337
Other-Bloom    1.7620427 -0.6277548  4.1518403 0.2596440
Llama-GPT      3.8537111  2.9429484  4.7644738 0.0000000
Mistral-GPT    4.2943411  1.7626552  6.8260271 0.0000391
Other-GPT      1.3827681  0.1815000  2.5840361 0.0146733
Mistral-Llama  0.4406300 -2.0289031  2.9101631 0.9885257
Other-Llama   -2.4709431 -3.5349942 -1.4068919 0.0000000
Other-Mistral -2.9115731 -5.5023548 -0.3207913 0.0185706


###############################################################
###########################################################

############################################################################
############################################################################

 colnames(df1)
 [1] "T"                 "Model"             "Average"          
 [4] "ARC"               "HellaSwag"         "MMLU"             
 [7] "TruthfulQA"        "Winogrande"        "GSM8K"            
[10] "Type"              "Architecture"      "Precision"        
[13] "Hub.License"       "X.Param.B."        "log_Param"        
[16] "log_GSM8K"         "log_Winogrande"    "log_HellaSwag"    
[19] "log_MMLU"          "log_ARC"           "log_TruthfulQA"   
[22] "Category"          "ScaledAverage"     "ARCScaled"        
[25] "HellaSwagScaled"   "MMLUScaled"        "TruthfulQAScaled" 
[28] "WinograndeScaled"  "GSM8KScaled"       "log_ARC_sc"       
[31] "log_HellaSwag_sc"  "log_MMLU_sc"       "log_TruthfulQA_sc"
[34] "log_Winogrande_sc" "log_GSM8K_sc"      "Range"            
[37] "Category1"         "ScaledAverage1"    "ARCScaled1"       
[40] "HellaSwagScaled1"  "MMLUScaled1"       "TruthfulQAScaled1"
[43] "WinograndeScaled1" "GSM8KScaled1"     
> dfx=df1[,c(3:9, 10,11, 12, 14, 36, 37)]
dim(dfx)
[1] 1182   13


##function to extract the data using categories

subset_by_cat <- function(dataframe, column_name, categories) {
  # Create an empty list to store the subsets
  subsets <- list()

  # Loop through each range and extract the subset for the given column
  for (category in categories) {
    subsets[[category]] <- dataframe[dataframe$Category1 == category,][[column_name]]
  }

  return(subsets)
}

# Define the ranges
categories <- c("Bloom", "GPT2", "GPTJ", "Falcon", "Llama", "GPTNeo", "GLM", "Mistral","Rwkv", "OPT",  "Other")

# Example usage for the "ARC" column
arc3_subsets <- subset_by_cat(dfx, "ARC", categories)

# Example usage for the "HellaSwag" column
hswag3_subsets <- subset_by_cat(dfx, "HellaSwag", categories)

# And so on for other columns...



# Corrected usage for other columns
mmlu3_subsets <- subset_by_cat(dfx, "MMLU", categories)
truthfulqa3_subsets <- subset_by_cat(dfx, "TruthfulQA", categories)
winogrande3_subsets <- subset_by_cat(dfx, "Winogrande", categories)
gsm8k3_subsets <- subset_by_cat(dfx, "GSM8K", categories)


##testing
perform_anova <- function(subsets, column_name) {
  # Filter out empty subsets
  non_empty_subsets <- subsets[sapply(subsets, function(x) length(x) > 0)]

  # Create a list of data frames only for non-empty subsets
  data_list <- lapply(names(non_empty_subsets), function(group) {
    if (length(non_empty_subsets[[group]]) > 0) {
      return(data.frame(value = non_empty_subsets[[group]], group = group))
    }
  })

  # Remove NULL elements (which occur if a subset was empty)
  data_list <- Filter(Negate(is.null), data_list)

  # Combine into a single data frame
  combined_data <- bind_rows(data_list)

  # Check if combined data is not empty
  if (nrow(combined_data) > 0) {
    # Perform ANOVA
    anova_result <- aov(value ~ group, data = combined_data)
    anova_summary <- summary(anova_result)

    # Extract the p-value
    p_value <- anova_summary[[1]]["group", "Pr(>F)"]

    # Check if the p-value is less than 0.05 and proceed with post-hoc test
    if (p_value < 0.05) {
      post_hoc_result <- TukeyHSD(anova_result)
      cat("Post-hoc results for", column_name, ":\n")
      print(post_hoc_result)
    } else {
      cat("ANOVA result for", column_name, "is not significant; no need for post-hoc test.\n")
    }
  } else {
    cat("No data available for ANOVA in", column_name, "\n")
  }
}

# Example usage
perform_anova(hswag3_subsets, "HellaSwag")
#
perform_anova(hswag3_subsets, "HellaSwag")column_name, "\n")nt; no need for post
Post-hoc results for HellaSwag :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff         lwr        upr     p adj
Falcon-Bloom     7.0165275 -11.4334279 25.4664828 0.9794909
GLM-Bloom        7.2457582 -11.2041971 25.6957136 0.9741301
GPT2-Bloom      -9.9674420 -21.0414033  1.1065192 0.1232511
GPTJ-Bloom      16.3558929   3.2082768 29.5035089 0.0031075
GPTNeo-Bloom     2.8202338  -7.9796143 13.6200819 0.9990055
Llama-Bloom     17.1926491   7.3335250 27.0517731 0.0000013
Mistral-Bloom    2.5975714 -11.8049204 17.0000632 0.9999649
OPT-Bloom        4.7211429  -8.2795679 17.7218536 0.9854327
Other-Bloom      8.3872112  -2.5567675 19.3311900 0.3224030
Rwkv-Bloom       4.1361429 -14.8660921 23.1383778 0.9998021
GLM-Falcon       0.2292308 -22.0512071 22.5096686 1.0000000
GPT2-Falcon    -16.9839695 -33.6767726 -0.2911664 0.0420809
GPTJ-Falcon      9.3393654  -8.7955971 27.4743279 0.8550592
GPTNeo-Falcon   -4.1962937 -20.7085248 12.3159374 0.9992148
Llama-Falcon    10.1761216  -5.7367469 26.0889901 0.6046455
Mistral-Falcon  -4.4189560 -23.4832900 14.6453779 0.9996516
OPT-Falcon      -2.2953846 -20.3241266 15.7333573 0.9999987
Other-Falcon     1.3706838 -15.2361741 17.9775416 1.0000000
Rwkv-Falcon     -2.8803846 -25.6202613 19.8594920 0.9999988
GPT2-GLM       -17.2132003 -33.9060034 -0.5203972 0.0366291
GPTJ-GLM         9.1101346  -9.0248279 27.2450971 0.8737730
GPTNeo-GLM      -4.4255245 -20.9377556 12.0867066 0.9987556
Llama-GLM        9.9468908  -5.9659777 25.8597593 0.6378136
Mistral-GLM     -4.6481868 -23.7125207 14.4161471 0.9994541
OPT-GLM         -2.5246154 -20.5533573 15.5041266 0.9999968
Other-GLM        1.1414530 -15.4654049 17.7483109 1.0000000
Rwkv-GLM        -3.1096154 -25.8494920 19.6302613 0.9999975
GPTJ-GPT2       26.3233349  15.7825270 36.8641428 0.0000000
GPTNeo-GPT2     12.7876758   5.3791981 20.1961535 0.0000018
Llama-GPT2      27.1600911  21.2060043 33.1141780 0.0000000
Mistral-GPT2    12.5650135   0.4951919 24.6348351 0.0331050
OPT-GPT2        14.6885849   4.3315920 25.0455778 0.0002760
Other-GPT2      18.3546533  10.7376005 25.9717061 0.0000000
Rwkv-GPT2       14.1035849  -3.1976797 31.4048495 0.2348977
GPTNeo-GPTJ    -13.5356591 -23.7881091 -3.2832090 0.0011234
Llama-GPTJ       0.8367562  -8.4195033 10.0930157 1.0000000
Mistral-GPTJ   -13.7583214 -27.7550276  0.2383847 0.0589216
OPT-GPTJ       -11.6347500 -24.1844309  0.9149309 0.0980087
Other-GPTJ      -7.9686816 -18.3728483  2.4354850 0.3233124
Rwkv-GPTJ      -12.2197500 -30.9162990  6.4767990 0.5718440
Llama-GPTNeo    14.3724153   8.9451461 19.7996845 0.0000000
Mistral-GPTNeo  -0.2226623 -12.0414903 11.5961656 1.0000000
OPT-GPTNeo       1.9009091  -8.1624603 11.9642784 0.9999461
Other-GPTNeo     5.5669775  -1.6457609 12.7797158 0.3118318
Rwkv-GPTNeo      1.3159091 -15.8111997 18.4430179 1.0000000
Mistral-Llama  -14.5950776 -25.5609475 -3.6292078 0.0009685
OPT-Llama      -12.4715062 -21.5178873 -3.4251251 0.0004930
Other-Llama     -8.8054378 -14.5141331 -3.0967425 0.0000402
Rwkv-Llama     -13.0565062 -29.6065353  3.4935229 0.2799525
OPT-Mistral      2.1235714 -11.7352328 15.9823756 0.9999925
Other-Mistral    5.7896398  -6.1610357 17.7403153 0.8976327
Rwkv-Mistral     1.5385714 -18.0607374 21.1378803 1.0000000
Other-OPT        3.6660684  -6.5518254 13.8839622 0.9867004
Rwkv-OPT        -0.5850000 -19.1785370 18.0085370 1.0000000
Rwkv-Other      -4.2510684 -21.4694251 12.9672883 0.9993904
##

> perform_anova(arc3_subsets, "ARC")
Post-hoc results for ARC :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                        diff         lwr        upr     p adj
Falcon-Bloom     0.468593407  -9.8169703 10.7541571 1.0000000
GLM-Bloom       -1.745252747 -12.0308165  8.5403110 0.9999802
GPT2-Bloom      -6.400695418 -12.5742569 -0.2271339 0.0345822
GPTJ-Bloom       6.858285714  -0.4713062 14.1878776 0.0908981
GPTNeo-Bloom     0.535679654  -5.4850680  6.5564273 1.0000000
Llama-Bloom     11.516720497   6.0204119 17.0130291 0.0000000
Mistral-Bloom    6.058428571  -1.9707370 14.0875942 0.3459787
OPT-Bloom        0.463428571  -6.7842659  7.7111230 1.0000000
Other-Bloom      5.574063492  -0.5270348 11.6751618 0.1100347
Rwkv-Bloom      -0.004547619 -10.5979986 10.5889034 1.0000000
GLM-Falcon      -2.213846154 -14.6348444 10.2071521 0.9999686
GPT2-Falcon     -6.869288824 -16.1752675  2.4366898 0.3800111
GPTJ-Falcon      6.389692308  -3.7202678 16.4996524 0.6219072
GPTNeo-Falcon    0.067086247  -9.1382263  9.2723988 1.0000000
Llama-Falcon    11.048127090   2.1769499 19.9193043 0.0030528
Mistral-Falcon   5.589835165  -5.0382351 16.2179054 0.8376179
OPT-Falcon      -0.005164835 -10.0559086 10.0455790 1.0000000
Other-Falcon     5.105470085  -4.1525954 14.3635356 0.7928451
Rwkv-Falcon     -0.473141026 -13.1502693 12.2039872 1.0000000
GPT2-GLM        -4.655442671 -13.9614213  4.6505360 0.8767175
GPTJ-GLM         8.603538462  -1.5064217 18.7134986 0.1815073
GPTNeo-GLM       2.280932401  -6.9243801 11.4862449 0.9993708
Llama-GLM       13.261973244   4.3907960 22.1331504 0.0000856
Mistral-GLM      7.803681319  -2.8243889 18.4317516 0.3883335
OPT-GLM          2.208681319  -7.8420625 12.2594251 0.9997842
Other-GLM        7.319316239  -1.9387492 16.5773817 0.2769307
Rwkv-GLM         1.740705128 -10.9364231 14.4178334 0.9999974
GPTJ-GPT2       13.258981132   7.3826443 19.1353179 0.0000000
GPTNeo-GPT2      6.936375071   2.8062637 11.0664864 0.0000040
Llama-GPT2      17.917415915  14.5981049 21.2367269 0.0000000
Mistral-GPT2    12.459123989   5.7303858 19.1878622 0.0000002
OPT-GPT2         6.864123989   1.0902612 12.6379868 0.0062222
Other-GPT2      11.974758910   7.7283702 16.2211476 0.0000000
Rwkv-GPT2        6.396147799  -3.2490386 16.0413342 0.5495110
GPTNeo-GPTJ     -6.322606061 -12.0381878 -0.6070243 0.0164023
Llama-GPTJ       4.658434783  -0.5017862  9.8186558 0.1205418
Mistral-GPTJ    -0.799857143  -8.6028036  7.0030893 0.9999998
OPT-GPTJ        -6.394857143 -13.3911095  0.6013952 0.1096395
Other-GPTJ      -1.284222222  -7.0843836  4.5159392 0.9997691
Rwkv-GPTJ       -6.862833333 -17.2858692  3.5602025 0.5605260
Llama-GPTNeo    10.981040843   7.9554225 14.0066592 0.0000000
Mistral-GPTNeo   5.522748918  -1.0660642 12.1115620 0.1995036
OPT-GPTNeo      -0.072251082  -5.6824233  5.5379211 1.0000000
Other-GPTNeo     5.038383838   1.0173942  9.0593735 0.0027654
Rwkv-GPTNeo     -0.540227273 -10.0883246  9.0078700 1.0000000
Mistral-Llama   -5.458291925 -11.5715941  0.6550103 0.1307895
OPT-Llama      -11.053291925 -16.0965090 -6.0100749 0.0000000
Other-Llama     -5.942657005  -9.1251660 -2.7601480 0.0000001
Rwkv-Llama     -11.521268116 -20.7476525 -2.2948838 0.0029238
OPT-Mistral     -5.595000000 -13.3210682  2.1310682 0.4101500
Other-Mistral   -0.484365079  -7.1466812  6.1779510 1.0000000
Rwkv-Mistral    -6.062976190 -16.9892866  4.8633343 0.7863972
Other-OPT        5.110634921  -0.5856823 10.8069521 0.1262327
Rwkv-OPT        -0.467976190 -10.8335844  9.8976320 1.0000000
Rwkv-Other      -5.578611111 -15.1775777  4.0203555 0.7341623

> perform_anova(mmlu3_subsets, "MMLU")
Post-hoc results for MMLU :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff         lwr        upr     p adj
Falcon-Bloom    -2.9485495 -12.3871115  6.4900126 0.9955177
GLM-Bloom       -2.6093187 -12.0478808  6.8292434 0.9983781
GPT2-Bloom      -1.9496307  -7.6148082  3.7155467 0.9903356
GPTJ-Bloom       1.3561429  -5.3698675  8.0821532 0.9999015
GPTNeo-Bloom    -2.3020693  -7.8270169  3.2228783 0.9608793
Llama-Bloom     10.5380714   5.4943761 15.5817668 0.0000000
Mistral-Bloom   10.8123571   3.4443820 18.1803323 0.0001311
OPT-Bloom       -0.8970476  -7.5479046  5.7538094 0.9999978
Other-Bloom      6.2118779   0.6131965 11.8105593 0.0158021
Rwkv-Bloom      -3.2108571 -12.9319524  6.5102381 0.9929932
GLM-Falcon       0.3392308 -11.0589158 11.7373774 1.0000000
GPT2-Falcon      0.9989187  -7.5407257  9.5385632 0.9999994
GPTJ-Falcon      4.3046923  -4.9727269 13.5821115 0.9211423
GPTNeo-Falcon    0.6464802  -7.8007879  9.0937482 1.0000000
Llama-Falcon    13.4866209   5.3459726 21.6272692 0.0000059
Mistral-Falcon  13.7609066   4.0080429 23.5137703 0.0003074
OPT-Falcon       2.0515018  -7.1715774 11.2745811 0.9997593
Other-Falcon     9.1604274   0.6647505 17.6561042 0.0223228
Rwkv-Falcon     -0.2623077 -11.8954923 11.3708770 1.0000000
GPT2-GLM         0.6596880  -7.8799565  9.1993324 1.0000000
GPTJ-GLM         3.9654615  -5.3119576 13.2428807 0.9535211
GPTNeo-GLM       0.3072494  -8.1400186  8.7545175 1.0000000
Llama-GLM       13.1473901   5.0067418 21.2880384 0.0000121
Mistral-GLM     13.4216758   3.6688122 23.1745395 0.0005106
OPT-GLM          1.7122711  -7.5108082 10.9353503 0.9999541
Other-GLM        8.8211966   0.3255197 17.3168734 0.0340440
Rwkv-GLM        -0.6015385 -12.2347231 11.0316462 1.0000000
GPTJ-GPT2        3.3057736  -2.0866552  8.6982023 0.6650893
GPTNeo-GPT2     -0.3524385  -4.1424410  3.4375640 0.9999999
Llama-GPT2      12.4877022   9.4417317 15.5336726 0.0000000
Mistral-GPT2    12.7619879   6.5873518 18.9366240 0.0000000
OPT-GPT2         1.0525831  -4.2458102  6.3509765 0.9999141
Other-GPT2       8.1615086   4.2648040 12.0582132 0.0000000
Rwkv-GPT2       -1.2612264 -10.1121454  7.5896925 0.9999962
GPTNeo-GPTJ     -3.6582121  -8.9031238  1.5866995 0.4699352
Llama-GPTJ       9.1819286   4.4466445 13.9172126 0.0000000
Mistral-GPTJ     9.4562143   2.2958295 16.6165991 0.0011174
OPT-GPTJ        -2.2531905  -8.6733113  4.1669304 0.9887587
Other-GPTJ       4.8557350  -0.4667913 10.1782613 0.1112700
Rwkv-GPTJ       -4.5670000 -14.1317136  4.9977136 0.9060988
Llama-GPTNeo    12.8401407  10.0636778 15.6166036 0.0000000
Mistral-GPTNeo  13.1144264   7.0681927 19.1606601 0.0000000
OPT-GPTNeo       1.4050216  -3.7431608  6.5532041 0.9985466
Other-GPTNeo     8.5139472   4.8240803 12.2038140 0.0000000
Rwkv-GPTNeo     -0.9087879  -9.6706129  7.8530371 0.9999998
Mistral-Llama    0.2742857  -5.3355947  5.8841661 1.0000000
OPT-Llama      -11.4351190 -16.0630343 -6.8072038 0.0000000
Other-Llama     -4.3261935  -7.2466274 -1.4057596 0.0001058
Rwkv-Llama     -13.7489286 -22.2155332 -5.2823239 0.0000104
OPT-Mistral    -11.7094048 -18.7992422 -4.6195674 0.0000065
Other-Mistral   -4.6004792 -10.7141630  1.5132045 0.3501797
Rwkv-Mistral   -14.0232143 -24.0497585 -3.9966701 0.0003686
Other-OPT        7.1089255   1.8816920 12.3361590 0.0006439
Rwkv-OPT        -2.3138095 -11.8258246  7.1982055 0.9994652
Rwkv-Other      -9.4227350 -18.2312403 -0.6142298 0.0244690

> perform_anova(truthfulqa3_subsets, "TruthfulQA")
Post-hoc results for TruthfulQA :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                     diff         lwr        upr     p adj
Falcon-Bloom    0.5771648  -7.5507877  8.7051174 1.0000000
GLM-Bloom      -3.3197582 -11.4477108  4.8081943 0.9658996
GPT2-Bloom      1.4009892  -3.4775392  6.2795176 0.9977632
GPTJ-Bloom     -3.4291429  -9.2212002  2.3629145 0.7111983
GPTNeo-Bloom   -0.1808247  -4.9385951  4.5769458 1.0000000
Llama-Bloom     2.5562857  -1.7870577  6.8996291 0.7186437
Mistral-Bloom   1.0752143  -5.2696666  7.4200952 0.9999805
OPT-Bloom      -1.5887143  -7.3160539  4.1386253 0.9983307
Other-Bloom    -0.7525788  -5.5738446  4.0686871 0.9999910
Rwkv-Bloom     -2.6593095 -11.0305636  5.7119445 0.9948674
GLM-Falcon     -3.8969231 -13.7123583  5.9185121 0.9720485
GPT2-Falcon     0.8238244  -6.5300314  8.1776802 0.9999996
GPTJ-Falcon    -4.0063077 -11.9954931  3.9828777 0.8750268
GPTNeo-Falcon  -0.7579895  -8.0322960  6.5163170 0.9999998
Llama-Falcon    1.9791209  -5.0311422  8.9893839 0.9980624
Mistral-Falcon  0.4980495  -7.9005617  8.8966606 1.0000000
OPT-Falcon     -2.1658791 -10.1082701  5.7765119 0.9985565
Other-Falcon   -1.3297436  -8.6457370  5.9862499 0.9999623
Rwkv-Falcon    -3.2364744 -13.2543110  6.7813622 0.9941238
GPT2-GLM        4.7207475  -2.6331084 12.0746033 0.5989576
GPTJ-GLM       -0.1093846  -8.0985701  7.8798008 1.0000000
GPTNeo-GLM      3.1389336  -4.1353730 10.4132401 0.9504878
Llama-GLM       5.8760440  -1.1342191 12.8863070 0.1994963
Mistral-GLM     4.3949725  -4.0036386 12.7935837 0.8419420
OPT-GLM         1.7310440  -6.2113470  9.6734349 0.9997997
Other-GLM       2.5671795  -4.7488140  9.8831729 0.9887730
Rwkv-GLM        0.6604487  -9.3573879 10.6782853 1.0000000
GPTJ-GPT2      -4.8301321  -9.4737849 -0.1864793 0.0334036
GPTNeo-GPT2    -1.5818139  -4.8455484  1.6819206 0.8973800
Llama-GPT2      1.1552965  -1.4677199  3.7783129 0.9433502
Mistral-GPT2   -0.3257749  -5.6430202  4.9914703 1.0000000
OPT-GPT2       -2.9897035  -7.5523783  1.5729713 0.5679468
Other-GPT2     -2.1535680  -5.5091882  1.2020523 0.5993346
Rwkv-GPT2      -4.0602987 -11.6822065  3.5616090 0.8263019
GPTNeo-GPTJ     3.2483182  -1.2683013  7.7649376 0.4210965
Llama-GPTJ      5.9854286   1.9076714 10.0631857 0.0001304
Mistral-GPTJ    4.5043571  -1.6617588 10.6704730 0.3963943
OPT-GPTJ        1.8404286  -3.6882141  7.3690713 0.9925471
Other-GPTJ      2.6765641  -1.9068927  7.2600209 0.7283298
Rwkv-GPTJ       0.7698333  -7.4667537  9.0064204 0.9999999
Llama-GPTNeo    2.7371104   0.3461785  5.1280423 0.0105432
Mistral-GPTNeo  1.2560390  -3.9506334  6.4627113 0.9995031
OPT-GPTNeo     -1.4078896  -5.8412114  3.0254321 0.9948806
Other-GPTNeo   -0.5717541  -3.7492574  2.6057493 0.9999657
Rwkv-GPTNeo    -2.4784848 -10.0236699  5.0667002 0.9932921
Mistral-Llama  -1.4810714  -6.3119812  3.3498383 0.9961538
OPT-Llama      -4.1450000  -8.1302972 -0.1597028 0.0334329
Other-Llama    -3.3088645  -5.8237759 -0.7939530 0.0011958
Rwkv-Llama     -5.2155952 -12.5065533  2.0753629 0.4296115
OPT-Mistral    -2.6639286  -8.7692931  3.4414359 0.9467259
Other-Mistral  -1.8277930  -7.0925496  3.4369635 0.9896574
Rwkv-Mistral   -3.7345238 -12.3688131  4.8997655 0.9497130
Other-OPT       0.8361355  -3.6652605  5.3375316 0.9999539
Rwkv-OPT       -1.0705952  -9.2618013  7.1206108 0.9999984
Rwkv-Other     -1.9067308  -9.4921142  5.6786527 0.9992863

> perform_anova(Winogrande3_subsets, "Winogrande")
Error in perform_anova(Winogrande3_subsets, "Winogrande") : 
  object 'Winogrande3_subsets' not found
> perform_anova(winogrande3_subsets, "Winogrande")
Post-hoc results for Winogrande :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff          lwr        upr     p adj
Falcon-Bloom     1.7538462  -6.16978366  9.6774760 0.9997697
GLM-Bloom        0.5638462  -7.35978366  8.4874760 1.0000000
GPT2-Bloom      -3.9103774  -8.66626799  0.8455133 0.2236628
GPTJ-Bloom       5.7280000   0.08154500 11.3744550 0.0433612
GPTNeo-Bloom     0.7217424  -3.91642589  5.3599107 0.9999913
Llama-Bloom      9.6795031   5.44534385 13.9136624 0.0000000
Mistral-Bloom    4.5350000  -1.65038154 10.7203815 0.3906298
OPT-Bloom        2.6257143  -2.95764985  8.2090784 0.9143557
Other-Bloom      4.8625641   0.16249657  9.5626316 0.0353784
Rwkv-Bloom      -0.3475000  -8.50831515  7.8133152 1.0000000
GLM-Falcon      -1.1900000 -10.75869207  8.3786921 0.9999990
GPT2-Falcon     -5.6642235 -12.83321606  1.5047690 0.2778077
GPTJ-Falcon      3.9741538  -3.81419723 11.7625049 0.8622301
GPTNeo-Falcon   -1.0321037  -8.12354673  6.0593393 0.9999954
Llama-Falcon     7.9256570   1.09161985 14.7596941 0.0088395
Mistral-Falcon   2.7811538  -5.40633071 10.9686384 0.9912676
OPT-Falcon       0.8718681  -6.87086482  8.6146011 0.9999996
Other-Falcon     3.1087179  -4.02336403 10.2407999 0.9470840
Rwkv-Falcon     -2.1013462 -11.86735161  7.6646593 0.9998217
GPT2-GLM        -4.4742235 -11.64321606  2.6947690 0.6400438
GPTJ-GLM         5.1641538  -2.62419723 12.9525049 0.5497044
GPTNeo-GLM       0.1578963  -6.93354673  7.2493393 1.0000000
Llama-GLM        9.1156570   2.28161985 15.9496941 0.0009301
Mistral-GLM      3.9711538  -4.21633071 12.1586384 0.8969253
OPT-GLM          2.0618681  -5.68086482  9.8046011 0.9988225
Other-GLM        4.2987179  -2.83336403 11.4307999 0.6879906
Rwkv-GLM        -0.9113462 -10.67735161  8.8546593 0.9999999
GPTJ-GPT2        9.6383774   5.11145801 14.1652967 0.0000000
GPTNeo-GPT2      4.6321198   1.45042997  7.8138096 0.0001566
Llama-GPT2      13.5898805  11.03280219 16.1469587 0.0000000
Mistral-GPT2     8.4453774   3.26179852 13.6289562 0.0000095
OPT-GPT2         6.5360916   2.08811460 10.9840687 0.0001272
Other-GPT2       8.7729415   5.50167572 12.0442072 0.0000000
Rwkv-GPT2        3.5628774  -3.86742874 10.9931835 0.9036937
GPTNeo-GPTJ     -5.0062576  -9.40933700 -0.6031782 0.0115460
Llama-GPTJ       3.9515031  -0.02374628  7.9267525 0.0529888
Mistral-GPTJ    -1.1930000  -7.20411037  4.8181104 0.9999148
OPT-GPTJ        -3.1022857  -8.49194787  2.2873764 0.7456383
Other-GPTJ      -0.8654359  -5.33367245  3.6028007 0.9999320
Rwkv-GPTJ       -6.0755000 -14.10503343  1.9540334 0.3417468
Llama-GPTNeo     8.9577607   6.62693272 11.2885886 0.0000000
Mistral-GPTNeo   3.8132576  -1.26252800  8.8890432 0.3526838
OPT-GPTNeo       1.9039719  -2.41790382  6.2258475 0.9432688
Other-GPTNeo     4.1408217   1.04319530  7.2384481 0.0008931
Rwkv-GPTNeo     -1.0692424  -8.42475453  6.2862697 0.9999954
Mistral-Llama   -5.1445031  -9.85397213 -0.4350341 0.0191281
OPT-Llama       -7.0537888 -10.93890255 -3.1686751 0.0000003
Other-Llama     -4.8169390  -7.26862994 -2.3652481 0.0000000
Rwkv-Llama     -10.0270031 -17.13467907 -2.9193271 0.0003084
OPT-Mistral     -1.9092857  -7.86117188  4.0426005 0.9944485
Other-Mistral    0.3275641  -4.80484553  5.4599737 1.0000000
Rwkv-Mistral    -4.8825000 -13.29973814  3.5347381 0.7364622
Other-OPT        2.2368498  -2.15138888  6.6250885 0.8630237
Rwkv-OPT        -2.9732143 -10.95850752  5.0120789 0.9824465
Rwkv-Other      -5.2100641 -12.60476408  2.1846359 0.4538145

> perform_anova(gsm8k3_subsets, "GSM8K")
Post-hoc results for GSM8K :
  Tukey multiple comparisons of means
    95% family-wise confidence level

Fit: aov(formula = value ~ group, data = combined_data)

$group
                      diff          lwr         upr     p adj
Falcon-Bloom    0.14624176  -4.81548018  5.10796370 1.0000000
GLM-Bloom      -0.52298901  -5.48471095  4.43873293 0.9999998
GPT2-Bloom     -0.19329380  -3.17139947  2.78481187 1.0000000
GPTJ-Bloom      1.26085714  -2.27491375  4.79662803 0.9873035
GPTNeo-Bloom    0.47722078  -2.42716800  3.38160955 0.9999854
Llama-Bloom     4.24644720   1.59504610  6.89784831 0.0000152
Mistral-Bloom   4.82764286   0.95439991  8.70088580 0.0030131
OPT-Bloom       0.03204762  -3.46421621  3.52831145 1.0000000
Other-Bloom     2.87769475  -0.06545488  5.82084438 0.0619039
Rwkv-Bloom     -0.23914286  -5.34938861  4.87110289 1.0000000
GLM-Falcon     -0.66923077  -6.66107926  5.32261773 0.9999996
GPT2-Falcon    -0.33953556  -4.82870889  4.14963777 1.0000000
GPTJ-Falcon     1.11461538  -3.76239595  5.99162672 0.9996929
GPTNeo-Falcon   0.33097902  -4.10963332  4.77159136 1.0000000
Llama-Falcon    4.10020545  -0.17922110  8.37963200 0.0743076
Mistral-Falcon  4.68140110  -0.44554484  9.80834704 0.1105201
OPT-Falcon     -0.11419414  -4.96263972  4.73425144 1.0000000
Other-Falcon    2.73145299  -1.73460719  7.19751317 0.6683092
Rwkv-Falcon    -0.38538462  -6.50078938  5.73002015 1.0000000
GPT2-GLM        0.32969521  -4.15947812  4.81886854 1.0000000
GPTJ-GLM        1.78384615  -3.09316518  6.66085749 0.9846252
GPTNeo-GLM      1.00020979  -3.44040255  5.44082213 0.9997305
Llama-GLM       4.76943622   0.49000966  9.04886277 0.0149391
Mistral-GLM     5.35063187   0.22368592 10.47757781 0.0322250
OPT-GLM         0.55503663  -4.29340895  5.40348221 0.9999995
Other-GLM       3.40068376  -1.06537642  7.86674394 0.3321214
Rwkv-GLM        0.28384615  -5.83155861  6.39925092 1.0000000
GPTJ-GPT2       1.45415094  -1.38057450  4.28887639 0.8581435
GPTNeo-GPT2     0.67051458  -1.32183748  2.66286664 0.9918792
Llama-GPT2      4.43974101   2.83851636  6.04096566 0.0000000
Mistral-GPT2    5.02093666   1.77501560  8.26685771 0.0000374
OPT-GPT2        0.22534142  -2.55995090  3.01063374 1.0000000
Other-GPT2      3.07098855   1.02254467  5.11943243 0.0000801
Rwkv-GPT2      -0.04584906  -4.69865511  4.60695700 1.0000000
GPTNeo-GPTJ    -0.78363636  -3.54081411  1.97354138 0.9979497
Llama-GPTJ      2.98559006   0.49631652  5.47486361 0.0054640
Mistral-GPTJ    3.56678571  -0.19732984  7.33090127 0.0817706
OPT-GPTJ       -1.22880952  -4.60377852  2.14615947 0.9851440
Other-GPTJ      1.61683761  -1.18114108  4.41481629 0.7410054
Rwkv-GPTJ      -1.50000000  -6.52803804  3.52803804 0.9969274
Llama-GPTNeo    3.76922643   2.30967815  5.22877470 0.0000000
Mistral-GPTNeo  4.35042208   1.17200042  7.52884374 0.0005682
OPT-GPTNeo     -0.44517316  -3.15150169  2.26115537 0.9999852
Other-GPTNeo    2.40047397   0.46076185  4.34018609 0.0033777
Rwkv-GPTNeo    -0.71636364  -5.32233421  3.88960694 0.9999913
Mistral-Llama   0.58119565  -2.36784113  3.53023243 0.9999202
OPT-Llama      -4.21439959  -6.64723081 -1.78156836 0.0000016
Other-Llama    -1.36875246  -2.90398428  0.16647937 0.1321765
Rwkv-Llama     -4.48559006  -8.93636737 -0.03481276 0.0463042
OPT-Mistral    -4.79559524  -8.52262500 -1.06856547 0.0017722
Other-Mistral  -1.94994811  -5.16382736  1.26393115 0.6791661
Rwkv-Mistral   -5.06678571 -10.33760175  0.20403033 0.0721766
Other-OPT       2.84564713   0.09776254  5.59353172 0.0350160
Rwkv-OPT       -0.27119048  -5.27152562  4.72914467 1.0000000
Rwkv-Other     -3.11683761  -7.74734736  1.51367215 0.5262534

> 


