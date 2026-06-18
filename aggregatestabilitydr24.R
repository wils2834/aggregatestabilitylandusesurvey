library(readxl)
library(googlesheets4)
library(lattice)
library(tidyverse)
library(plyr)
library(readr)
library(nlme)
library(emmeans)
library(multcomp)
library(multcompView)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(knitr)
library(rstatix)

aggstability2024 <- read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Wilson Aggregate Stability_GC - 2024 - CombinedAS_GC.csv")
head(aggstability2024)
str(aggstability2024)
print(aggstability2024$mm2.per)

# Check for missing values
sum(is.na(aggstability2024$mm2.per))

# Check if `mm2.per` is numeric
str(aggstability2024$mm2.per)

# Convert to numeric if necessary
aggstability2024$mm2.per <- as.numeric(aggstability2024$mm2.per)
# Remove rows with missing `mm2.per` values
aggstability2024 <- aggstability2024[!is.na(aggstability2024$mm2.per), ]
#type as factor
aggstability2024$Type <- as.factor(aggstability2024$Type)



# Bar graph for mm2.per and type
aggstability2024$Type <- factor(aggstability2024$Type, levels = c("Conventional","Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP"))


ggplot(aggstability2024, aes(x = Type, y = mm2.per)) +
  geom_boxplot(aes(fill = Type), width = 0.1, color = "black", alpha = 0.6) +  # Adjusted alpha
  labs(
    title = "Prelimary Data on Aggregate Stability By Management ",
    x = NULL,
    y = "Aggregates > 2mm (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "black"),  # Increased size and bolded x-axis labels
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),  # Bolded y-axis labels
    axis.title = element_text(size = 16, face = "bold", color = "black"),  # Bolded axis titles
    axis.title.x = element_text(size = 18, color = "black"),  # Increased size for x-axis title
    axis.title.y = element_text(size = 18, color = "black"),  # Increased size for y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),  # Increased size and bolded plot title
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  # Border around the graph
    legend.position = "none"
  ) 


# Convert columns to numeric where possible
aggstability2024 <- aggstability2024 %>%
  mutate(
    mm1.per = as.numeric(mm1.per),
    mm2.per = as.numeric(mm2.per),
    um250.per = as.numeric(um250.per),
    um53.per = as.numeric(um53.per),
    mmless0053.per = as.numeric(mmless0053.per)
  )

#pairwise t test ----


# Perform pairwise comparisons
pairwise_results <- aggstability2024 %>%
  pairwise_t_test(
    mm2.per ~ Type, # Dependent variable vs group
    p.adjust.method = "bonferroni" # Adjust for multiple comparisons
  )

# Display the results
pairwise_results


# Create the pairwise_results tibble (example data)
pairwise_results <- tibble::tibble(
  .y. = c("mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per", "mm2.per"),
  group1 = c("Conve", "Conve", "New_C", "Conve", "New_C", "Old_C", "Conve", "New_C", "Old_C", "Rotat"),
  group2 = c("New_C", "Old_C", "Old_C", "Rotat", "Rotat", "Rotat", "Soil_", "Soil_", "Soil_", "Soil_"),
  n1 = c(9, 9, 9, 9, 9, 2, 9, 9, 2, 11),
  n2 = c(9, 2, 2, 11, 11, 11, 9, 9, 9, 9),
  p = c(6.04e-4, 1.11e-1, 5.29e-1, 5.15e-4, 8.99e-1, 5.71e-1, 1.67e-1, 2.41e-2, 4.38e-1, 2.49e-2),
  p.signif = c("***", "ns", "ns", "***", "ns", "ns", "ns", "*", "ns", "*"),
  p.adj = c(0.00604, 1, 1, 0.00515, 1, 1, 1, 0.241, 1, 0.249),
  p.adj.signif = c("**", "ns", "ns", "**", "ns", "ns", "ns", "ns", "ns", "ns")
)

# Create a table
kable(pairwise_results, 
      format = "html", 
      caption = "Pairwise Comparison Results",
      col.names = c("Response Variable", "Group 1", "Group 2", "n1", "n2", "p-value", "Significance", "Adjusted p-value", "Adjusted Significance"))


# Assuming 'aggstability2024' is your dataset
# Perform ANOVA
anova_res <- aov(mm2.per ~ Type, data = aggstability2024)
anova_res
# Tukey HSD post-hoc test
tukey_res <- TukeyHSD(anova_res)
print(tukey_res)

# Extract p-values and generate letters for significant differences
letters <- multcompLetters4(anova_res, tukey_res)$Type

# Extract grouping letters from the 'letters' object
letters_df <- data.frame(
  Type = names(letters$Letters),  # Extract names (factor levels)
  label = letters$Letters         # Extract the letters
)

# Merge the letters with the original dataset
aggstability2024 <- aggstability2024 %>%
  left_join(letters_df, by = "Type")

# Create the plot
ggplot(aggstability2024, aes(x = Type, y = mm2.per)) +
  geom_boxplot(aes(fill = Type), width = 0.1, color = "black", alpha = 0.6) +  # Adjusted alpha
  geom_text(data = letters_df, aes(x = Type, y = max(aggstability2024$mm2.per) + 5, label = label), 
            inherit.aes = FALSE, size = 5, color = "black") +  # Add grouping letters
  labs(
    title = "Water Stable Aggregates By Management ",
    x = NULL,
    y = "Aggregates > 2mm (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold", color = "black"),  # Increased size and bolded x-axis labels
    axis.text.y = element_text(size = 12, face = "bold", color = "black"),  # Bolded y-axis labels
    axis.title = element_text(size = 16, face = "bold", color = "black"),  # Bolded axis titles
    axis.title.x = element_text(size = 18, color = "black"),  # Increased size for x-axis title
    axis.title.y = element_text(size = 18, color = "black"),  # Increased size for y-axis title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "black"),  # Increased size and bolded plot title
    panel.border = element_rect(color = "black", fill = NA, size = 1.5),  # Border around the graph
    legend.position = "none"
  )

class(aggstability2024)
#stacked bar
df_long <- aggstability2024 %>%
  dplyr::select(County, Type, mm2.per, mm1.per, um250.per, um53.per, mmless0053.per) %>%
  tidyr::pivot_longer(
    cols = c(mm2.per, mm1.per, um250.per, um53.per, mmless0053.per),
    names_to = "Fraction",
    values_to = "Percentage"
  )

# Create the stacked bar plot
ggplot(df_long, aes(x = Type, y = Percentage, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Management Type", y = "Percentage", fill = "Soil Fraction") +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_long_mean <- df_long %>%
  group_by(County, Type, Fraction) %>%
  reframe(Mean_Percentage = mean(Percentage, na.rm = TRUE)) %>% 
  left_join(letters_df, by = c("Type" = "type"))


print(df_long_mean)

df_long_mean$Fraction<- factor(df_long_mean$Fraction, levels = c("mm2.per", "mm1.per","um250.per","um53.per","mmless0053.per"))
df_long_mean$Type <- factor(df_long_mean$Type, levels = c("Conventional","Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP"))

# First, get total height (sum of all fractions) per Type so we can place letters above bars
label_pos <- df_long_mean %>%
  group_by(Type) %>%
  summarise(y_pos = sum(Mean_Percentage)) %>%
  left_join(letters_df, by = "Type")

#stackedbarwithletters2mm----
ggplot(df_long_mean, aes(x = Type, y = Mean_Percentage, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(
    data = label_pos,
    aes(x = Type, y = y_pos + 3, label = label),   # place label a bit above the bar
    inherit.aes = FALSE,
    size = 8,
    fontface = "bold"
  ) +
  theme_minimal() +
  scale_x_discrete(labels = c("Conventional", "Soil Health", "Rotational Grazing", "New CRP", "Old CRP")) + 
  labs(
    y = "Mean Percentage",
    fill = "Soil Fraction",
    title = "Water Stable Aggregates By Management",
    x = NULL
  ) +
  scale_fill_brewer(palette = "Reds") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold", color = "black"), 
    axis.text.y = element_text(size = 25, face = "bold", color = "black"),  
    axis.title.y = element_text(size = 27, face = "bold", color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "black"),
    legend.text = element_text(size = 25, face = "bold", color = "black"),
    legend.title = element_text(size = 27, face = "bold", color = "black")
  )

ggplot(df_long_mean, aes(x = Type, y = Mean_Percentage, fill = Fraction)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +  # Scales to 100%
  theme_minimal() +
  scale_x_discrete(labels = c("Conventional", "Soil Health", "Rotational Grazing", "New CRP", "Old CRP")) + 
  labs(y = "Mean Percentage", fill = "Soil Fraction", title = "Water Stable Aggregates By Management ",
       x = NULL) +
  scale_fill_brewer(palette = "Reds") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25, face = "bold", color = "black"), 
    axis.text.y = element_text(size = 25, face = "bold", color = "black"),  
    axis.title.y = element_text(size = 27, face = "bold", color = "black"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5, color = "black"),
    legend.text = element_text(size = 25, face = "bold", color = "black"),
    legend.title = element_text(size = 27, face = "bold", color = "black")
  ) +
  scale_y_continuous(labels = scales::percent_format())

#mehmets code no color ----
grouped_ggbetweenstats(
  data             = aggstability2024,
  x                = Type,
  y                = mm2.per,
  grouping.var     = Type,
  ggsignif.args    = list(textsize = 4, tip_length = 0.01),
  p.adjust.method  = "bonferroni",
  palette          = "default_jama",
  package          = "ggsci",
  plotgrid.args    = list(nrow = 1),
  annotation.args  = list(title = "Differences in Avg Runs by Management Type"),
  comparisons      = list(
    c("Conventional", "Soil_Health"),
    c("Conventional", "Rotational_Grazing"),
    c("Conventional", "New_CRP"),
    c("Conventional", "Old_CRP"),
    c("Soil_Health", "Rotational_Grazing"),
    c("Soil_Health", "New_CRP"),
    c("Soil_Health", "Old_CRP"),
    c("Rotational_Grazing", "New_CRP"),
    c("Rotational_Grazing", "Old_CRP"),
    c("New_CRP", "Old_CRP")
  ),
  label            = "p.signif",
  hide.ns          = FALSE
)

#County----
df_long_mean <- df_long %>%
  group_by(County, Type, Fraction) %>%
  reframe(Mean_Percentage = mean(Percentage, na.rm = TRUE)) %>% 
  left_join(letters_df, by = "Type")

df_long_mean$Fraction <- factor(df_long_mean$Fraction,
                                levels = c("mm2.per", "mm1.per", "um250.per", "um53.per", "mmless0053.per")
)

df_long_mean$Type <- factor(df_long_mean$Type, levels = c("Conventional","Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP"))

ggplot(df_long_mean, aes(x = Type, y = Mean_Percentage, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  theme_minimal() +
  scale_x_discrete(labels = c(
    "Conventional" = "Conventional Crops & Tillage",
    "Soil_Health" = "No Till and/or Cover Crops",
    "Rotational_Grazing" = "Rotational Grazing",
    "New_CRP" = "0–10 years CRP",
    "Old_CRP" = "10+ years CRP"
  )) +
  scale_fill_brewer(
    palette = "Reds",
    name = "Percentage of Soil Fraction",   
    labels = c(
      "mm2.per" = "% >2 mm aggregates",
      "mm1.per" = "% 1–2 mm aggregates",
      "um250.per" = "% 250–1000 µm aggregates",
      "um53.per" = "% 53–250 µm aggregates",
      "mmless0053.per" = "% <53 µm aggregates"
    )
  ) +
  labs(
    y = "Mean Percentage",
    title = "Water Stable Aggregates by Management and County",
    x = NULL
  ) +
  facet_wrap(~County, scales = "fixed") +
  theme(
    axis.text.x = element_text(
      angle = 50,         # slightly steep but compact
      hjust = 1,          # aligns neatly under bars
      vjust = 1,
      size = 13,           # compact readable font
      face = "bold",
      color = "black"
    ),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black"),
    legend.text = element_text(size = 20, face = "bold", color = "black"),
    legend.title = element_text(size = 22, face = "bold", color = "black"),
    strip.text = element_text(size = 20, face = "bold", color = "black"),
    plot.margin = margin(10, 20, 30, 10)
  )

df_long_mean <- df_long %>%
  group_by(Type, Fraction) %>%
  reframe(Mean_Percentage = mean(Percentage, na.rm = TRUE)) %>% 
  left_join(letters_df, by = c("Type" = "type"))

print(df_long_mean)

df_long_mean$Fraction <- factor(df_long_mean$Fraction, 
                                levels = c("mm2.per", "mm1.per", "um250.per", "um53.per", "mmless0053.per"))
df_long_mean$Type <- factor(df_long_mean$Type, 
                            levels = c("Conventional", "Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP"))
ggplot(df_long_mean, aes(x = Type, y = Mean_Percentage, fill = Fraction)) +
  geom_bar(stat = "identity", position = "fill", color = "black") + 
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_x_discrete(labels = c(
    "Conventional" = "Conventional Crops & Tillage",
    "Soil_Health" = "No Till and/or Cover Crops",
    "Rotational_Grazing" = "Rotational Grazing",
    "New_CRP" = "0–10 years CRP",
    "Old_CRP" = "10+ years CRP"
  )) +
  scale_fill_brewer(
    palette = "Reds",
    name = "Percentage of Soil Fraction",
    labels = c(
      "mm2.per" = "% >2 mm aggregates",
      "mm1.per" = "% 1–2 mm aggregates",
      "um250.per" = "% 250–1000 µm aggregates",
      "um53.per" = "% 53–250 µm aggregates",
      "mmless0053.per" = "% <53 µm aggregates"
    )
  ) +
  labs(
    y = "Mean Percentage",
    title = "Water Stable Aggregates by Management",
    x = NULL
  ) +
  theme(
    axis.text.x = element_text(
      angle = 50,
      hjust = 1,
      vjust = 1,
      size = 16,
      face = "bold",
      color = "black"
    ),
    axis.text.y = element_text(size = 18, face = "bold", color = "black"),
    axis.title.y = element_text(size = 25, face = "bold", color = "black"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black"),
    legend.text = element_text(size = 20, face = "bold", color = "black"),
    legend.title = element_text(size = 22, face = "bold", color = "black"),
    plot.margin = margin(10, 20, 30, 10)
  )

