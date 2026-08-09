# Aggregate Stability Analysis — 2024 CRP Project
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(knitr)
library(MuMIn)

# Factor levels
type_levels <- c("Conventional","Soil_Health","Rotational_Grazing","New_CRP","Old_CRP")
type_labels  <- c("Conventional\nCrops & Tillage",
                  "No Till and/or\nCover Crops",
                  "Rotational\nGrazing",
                  "0-10 Years\nCRP",
                  "10+ Years\nCRP")

fraction_levels <- c("mm2.per","mm1.per","um250.per","um53.per","mmless0053.per")
fraction_labels <- c("> 2 mm","1-2 mm","250-1000 um","53-250 um","< 53 um")

fill_colors <- c(
  "Conventional"        = "#E69F00",
  "Soil_Health"         = "#56B4E9",
  "Rotational_Grazing"  = "#009E73",
  "New_CRP"             = "#F0E442",
  "Old_CRP"             = "#0072B2"
)

type_labels_legend <- c(
  "Conventional"        = "Conventional Cropping & Tillage",
  "Soil_Health"         = "No Till and/or Cover Crops",
  "Rotational_Grazing"  = "Rotational Grazing",
  "New_CRP"             = "0-10 years CRP",
  "Old_CRP"             = "10+ years CRP"
)

# RG/CRP farm_name -> lab-data Farmer name and CRP contract-age split.
rg_crp_farmer_map <- c(
  "5G_Farms"           = "5G_Farms_LLC",
  "Cliff"              = "Cliff",
  "Cliff_2"            = "Cliff_2",
  "Collins"            = "Steve_Collins",
  "Derouin"            = "Michael_Derouin",
  "Eiken"              = "Kay_Eiken",
  "Eiken_2"            = "Kay_Eiken_2",
  "Haugen"             = "Bonnie_Haugen",
  "Lee"                = "Carter_Lee",
  "Lee_2"              = "Carter_Lee_2",
  "McLean"             = "Maurice_McLean",
  "Menne"              = "Kevin_Menne",
  "Mitchell"           = "Thomas_Mitchell",
  "Reichling_Brothers" = "Reichling_Brothers",
  "RLH_Inc"            = "RLH_Inc",
  "RLH_Inc_2"          = "RLH_Inc_2",
  "Schmidt"            = "Dennis_Schmidt",
  "UW_AG"              = "UW_AG",
  "UW_AG_2"            = "UW_AG_2"
)

crp_type_map <- c(
  "Schmidt"             = "Old_CRP",
  "Reichling_Brothers"  = "Old_CRP",
  "McLean"              = "Old_CRP",
  "Eiken"               = "Old_CRP",
  "Eiken_2"             = "Old_CRP",
  "Derouin"             = "Old_CRP",
  "Collins"             = "New_CRP",
  "Menne"               = "New_CRP",
  "5G_Farms"            = "New_CRP",
  "Mitchell"            = "New_CRP",
  "RLH_Inc"             = "New_CRP",
  "RLH_Inc_2"           = "New_CRP"
)

# ---- Data import --------------------------------------------------------
aggstability2024 <- read.csv(
  "C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Wilson Aggregate Stability_GC - 2024 - CombinedAS_GC.csv",
  header = FALSE
)

colnames(aggstability2024) <- as.character(aggstability2024[1, ])
aggstability2024 <- aggstability2024[-c(1, 2), ]
rownames(aggstability2024) <- NULL
colnames(aggstability2024) <- make.unique(colnames(aggstability2024))

aggstability2024 <- aggstability2024 %>%
  dplyr::select(Project, UID, County, Farmer, Type, Rep,
                mm2.per, mm1.per, um250.per, um53.per, mmless0053.per)

# Data cleaning 
fraction_cols <- c("mm2.per","mm1.per","um250.per","um53.per","mmless0053.per")

aggstability2024 <- aggstability2024 %>%
  mutate(across(all_of(fraction_cols), as.numeric)) %>%
  filter(!is.na(mm2.per)) %>%
  mutate(
    Type   = factor(Type,   levels = type_levels),
    Farmer = as.factor(Farmer),
    County = as.factor(County)
  )

# Build true field identifier
aggstability2024 <- aggstability2024 %>%
  mutate(Field = paste(Farmer, Type, sep = "_"))

cat("Distinct fields:", n_distinct(aggstability2024$Field), "(expect 31)\n")
stopifnot(n_distinct(aggstability2024$Field) == 31)

#  Mixed models 
model_AS     <- lmer(mm2.per      ~ Type + (1 | Field), data = aggstability2024)
model_AS_log <- lmer(log(mm2.per) ~ Type + (1 | Field), data = aggstability2024)

shapiro_raw <- shapiro.test(residuals(model_AS))
shapiro_log <- shapiro.test(residuals(model_AS_log))
cat("Shapiro-Wilk, raw-scale residuals:  p =", signif(shapiro_raw$p.value, 3), "\n")
cat("Shapiro-Wilk, log-scale residuals:  p =", signif(shapiro_log$p.value, 3), "\n")

use_log    <- shapiro_log$p.value > shapiro_raw$p.value
final_AS   <- if (use_log) model_AS_log else model_AS
cat("Using", if (use_log) "LOG-transformed" else "RAW-scale", "model for inference.\n\n")

summary(final_AS)

# Emmeans and CLD for > 2 mm fraction
emmeans_AS <- emmeans(final_AS, ~ Type, type = "response")

cld_AS <- cld(emmeans_AS, Letters = letters, adjust = "sidak") %>%
  as.data.frame() %>%
  rename(emmean = if ("response" %in% names(.)) "response" else "emmean") %>%
  mutate(
    Type   = factor(Type, levels = type_levels),
    .group = trimws(.group)
  )

pairs_AS <- pairs(emmeans_AS, adjust = "sidak")
kable(as.data.frame(pairs_AS), digits = 4,
      caption = "Pairwise comparisons - Aggregates > 2 mm (LMM, Tukey-adjusted)")

print(cld_AS)

#  Emmeans for all fractions
emmeans_all <- map_dfr(fraction_levels, function(frac) {
  aggstability2024[[frac]] <- aggstability2024[[frac]] + 0.001  # avoid log(0)
  mod <- lmer(reformulate("Type + (1 | Field)", response = paste0("log(", frac, ")")),
              data = aggstability2024)
  emmeans(mod, ~ Type, type = "response") %>%
    as.data.frame() %>%
    rename(emmean = response) %>%
    mutate(Fraction = frac)
}) %>%
  mutate(
    Type     = factor(Type,     levels = type_levels),
    Fraction = factor(Fraction, levels = fraction_levels)
  )

# Label positions (Tukey letters from > 2 mm model) 
label_pos <- emmeans_all %>%
  dplyr::group_by(Type) %>%
  dplyr::summarise(y_pos = sum(emmean), .groups = "drop") %>%
  dplyr::left_join(cld_AS %>% dplyr::select(Type, .group), by = "Type")

# Shared theme 
fill_scale <- scale_fill_brewer(palette = "Reds", name = "Soil Fraction",
                                labels = fraction_labels)

theme_agg <- theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 13,
                                face = "bold", color = "black"),
    axis.text.y  = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5,
                                color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
    legend.position = "none"
  )

# Plot 1: Boxplot with emmeans and CLD letters 
ggplot() +
  geom_boxplot(data = aggstability2024,
               aes(x = Type, y = mm2.per, fill = Type),
               width = 0.5, color = "black", alpha = 0.4) +
  geom_point(data = cld_AS, aes(x = Type, y = emmean),
             size = 3, color = "black") +
  geom_errorbar(data = cld_AS,
                aes(x = Type, ymin = lower.CL, ymax = upper.CL),
                width = 0.15, color = "black", linewidth = 0.8) +
  geom_text(data = cld_AS,
            aes(x = Type, y = upper.CL + 3, label = .group),
            size = 5, fontface = "bold", color = "black") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  labs(title = "Water Stable Aggregates by Management",
       x = NULL, y = "Aggregates > 2 mm (%)") +
  theme_agg

# Plot 2: Stacked bar 
ggplot(emmeans_all, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  scale_y_continuous(labels = scales::percent_format()) +
  fill_scale +
  labs(title = "Water Stable Aggregates by Management",
       x = NULL, y = "Percent Stable Aggregates") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 13,
                                face = "bold", color = "black"),
    axis.text.y  = element_text(size = 13, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    legend.text  = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 13, face = "bold", color = "black"),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5,
                                color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)
  )

# Plot 3: Stacked bar faceted by county 
emmeans_county <- map_dfr(fraction_levels, function(frac) {
  mod <- lmer(reformulate("Type + County + (1 | Field)", response = frac),
              data = aggstability2024)
  emmeans(mod, ~ Type | County) %>%
    as.data.frame() %>%
    mutate(Fraction = frac)
}) %>%
  mutate(
    Type     = factor(Type,     levels = type_levels),
    Fraction = factor(Fraction, levels = fraction_levels)
  )

ggplot(emmeans_county, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  facet_wrap(~ County, scales = "fixed") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  fill_scale +
  labs(title = "Water Stable Aggregates by Management and County",
       x = NULL, y = "Percent Stable Aggregates (%)") +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 13,
                                face = "bold", color = "black"),
    axis.text.y  = element_text(size = 13, face = "bold", color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    legend.text  = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 13, face = "bold", color = "black"),
    plot.title   = element_text(size = 18, face = "bold", hjust = 0.5,
                                color = "black"),
    strip.text = element_text(size = 14, face = "bold", color = "black"))

# ---- Metadata export ----------------------------------------------------
print(pairs_AS)
anova_AS <- anova(final_AS)

AS_meta <- as.data.frame(emmeans_AS) %>%
  dplyr::rename(Mean = if ("response" %in% names(emmeans_AS)) "response" else "emmean") %>%
  dplyr::left_join(
    as.data.frame(cld_AS) %>% dplyr::select(Type, CLD = .group),
    by = "Type"
  ) %>%
  mutate(
    Metric        = "Aggregate_Stability",
    Scale         = if (use_log) "log" else "raw",
    Shapiro_raw_p = signif(shapiro_raw$p.value, 3),
    Shapiro_log_p = signif(shapiro_log$p.value, 3),
    LMM_F         = round(anova_AS$`F value`[1], 3),
    LMM_p         = signif(anova_AS$`Pr(>F)`[1], 3)
  ) %>%
  dplyr::select(Metric, Scale, Shapiro_raw_p, Shapiro_log_p,
                LMM_F, LMM_p, Type, Mean, SE, CLD)

write.csv(AS_meta, "AS_metadata.csv", row.names = FALSE)
cat("Saved AS_metadata.csv\n")

# ---- Supplemental table
all_scored <- read.csv(
  "C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\managementoflandusesurvey2024\\soil_health_scores_2024_all_types.csv",
  stringsAsFactors = FALSE
)

all_scored <- all_scored %>%
  mutate(
    Farmer = ifelse(farm_name %in% names(rg_crp_farmer_map),
                    rg_crp_farmer_map[farm_name],
                    Farmer),
    Type = ifelse(Type == "CRP" & farm_name %in% names(crp_type_map),
                  crp_type_map[farm_name],
                  Type)
  )

supp_table <- all_scored %>%
  dplyr::select(
    Farmer, Type, county, state,
    tillage_score, crop_diversity_score,
    cover_crop_score, manure_score,
    soil_health_score
  ) %>%
  dplyr::rename(
    Management     = Type,
    County         = county,
    State          = state,
    Tillage        = tillage_score,
    Crop_Diversity = crop_diversity_score,
    Cover_Crop     = cover_crop_score,
    Manure         = manure_score,
    Total_Score    = soil_health_score
  ) %>%
  arrange(Management, Farmer)

print(supp_table)

write.csv(supp_table,
          "C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\supplemental_management_scores_all_types.csv",
          row.names = FALSE)
cat("Saved supplemental_management_scores_all_types.csv\n")

# PART 4 Indexed (continuous management score) version

# scores_full reuses the same corrected all_scored table built above
scores_full <- all_scored
scores_full <- scores_full %>%
  mutate(Type = recode(Type,
                       "CRP_New" = "New_CRP",
                       "CRP_Old" = "Old_CRP"))
cat("Corrected Farmer/Type for RG/CRP rows:\n")
print(scores_full %>% filter(Type %in% c("Rotational_Grazing","Old_CRP","New_CRP")) %>%
        dplyr::select(farm_name, Farmer, Type))

# Join management score onto the aggregate stability data
aggstability2024_indexed <- aggstability2024 %>%
  mutate(Farmer = as.character(Farmer), Type = as.character(Type)) %>%
  left_join(
    scores_full %>% dplyr::select(Farmer, Type, soil_health_score),
    by = c("Farmer", "Type")
  )

cat("\nRows with missing soil_health_score after join:",
    sum(is.na(aggstability2024_indexed$soil_health_score)), "\n")

# Mixed model with continuous management score 

model_AS_idx     <- lmer(mm2.per      ~ soil_health_score + (1 | Field), data = aggstability2024_indexed)
model_AS_idx_log <- lmer(log(mm2.per) ~ soil_health_score + (1 | Field), data = aggstability2024_indexed)

shapiro_idx_raw_AS <- shapiro.test(residuals(model_AS_idx))
shapiro_idx_log_AS <- shapiro.test(residuals(model_AS_idx_log))
cat("Indexed model - Shapiro-Wilk, raw-scale residuals:  p =", signif(shapiro_idx_raw_AS$p.value, 3), "\n")
cat("Indexed model - Shapiro-Wilk, log-scale residuals:  p =", signif(shapiro_idx_log_AS$p.value, 3), "\n")

use_log_idx_AS  <- shapiro_idx_log_AS$p.value > shapiro_idx_raw_AS$p.value
final_AS_idx    <- if (use_log_idx_AS) model_AS_idx_log else model_AS_idx
cat("Using", if (use_log_idx_AS) "LOG-transformed" else "RAW-scale", "model for indexed inference.\n\n")

summary(final_AS_idx)
anova_AS_idx <- anova(final_AS_idx)
print(anova_AS_idx)

# Visual diagnostic: QQ plot for the selected model
par(mfrow = c(1, 2))
qqnorm(residuals(final_AS_idx), main = "QQ Plot: Indexed Aggregate Stability Model")
qqline(residuals(final_AS_idx), col = "red", lwd = 2)
plot(fitted(final_AS_idx), residuals(final_AS_idx),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Plot: scatter + fitted line 

fixed_est_AS   <- summary(final_AS_idx)$coefficients["soil_health_score", "Estimate"]
fixed_p_AS     <- anova_AS_idx$`Pr(>F)`[1]
r2_marginal_AS <- MuMIn::r.squaredGLMM(final_AS_idx)[1, "R2m"]

sig_lab_AS <- paste0("R\u00b2(marginal) = ", round(r2_marginal_AS, 2),
                     "  (", ifelse(fixed_p_AS < 0.001, "p < 0.001", paste0("p = ", round(fixed_p_AS, 3))), ")")

plot_data_AS <- aggstability2024_indexed %>%
  mutate(y_plot = if (use_log_idx_AS) log(mm2.per) else mm2.per)

p_indexed_AS <- ggplot(plot_data_AS, aes(x = soil_health_score, y = y_plot)) +
  geom_smooth(method = "lm", color = "gray50", fill = "gray85",
              alpha = 0.3, linewidth = 0.8, se = TRUE) +
  geom_point(aes(color = Type, shape = County), size = 3.5, alpha = 0.9) +
  annotate("text", x = Inf, y = Inf, label = sig_lab_AS,
           hjust = 1.05, vjust = 1.8, size = 4.2, color = "gray30", fontface = "italic") +
  scale_color_manual(values = fill_colors, labels = type_labels_legend, name = "Management") +
  scale_shape_manual(values = c("Grant" = 16, "Fillmore" = 17), name = "County") +
  labs(
    title = "Aggregate Stability (> 2 mm) vs. Soil Health Management Score",
    x = "Soil Health Management Score  (higher = more favorable)",
    y = if (use_log_idx_AS) "log(Aggregates > 2 mm, %)" else "Aggregates > 2 mm (%)"
  ) +
  theme_bw() +
  theme(
    axis.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.title      = element_text(size = 13, face = "bold"),
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines")
  )

print(p_indexed_AS)
ggsave("AS_vs_soil_health_score.png", p_indexed_AS, width = 8.5, height = 6.5, dpi = 300)

cat("\nSaved AS_vs_soil_health_score.png\n")

# ---- Metadata export (indexed version) ------------------------------------

AS_meta_indexed <- data.frame(
  Metric         = "Aggregate_Stability",
  Model          = "indexed (soil_health_score continuous)",
  Scale          = if (use_log_idx_AS) "log" else "raw",
  Shapiro_raw_p  = signif(shapiro_idx_raw_AS$p.value, 3),
  Shapiro_log_p  = signif(shapiro_idx_log_AS$p.value, 3),
  Fixed_Estimate = round(fixed_est_AS, 4),
  LMM_F          = round(anova_AS_idx$`F value`[1], 3),
  LMM_p          = signif(fixed_p_AS, 3),
  R2_marginal    = round(r2_marginal_AS, 3)
)

write.csv(AS_meta_indexed, "AS_metadata_indexed.csv", row.names = FALSE)
cat("Saved AS_metadata_indexed.csv\n")

# PART 5 — Field-averaged scatter (n=31 or fewer, simple lm, ordinary R^2)

as_field_avg <- aggstability2024_indexed %>%
  group_by(Farmer, Type, County, soil_health_score) %>%
  summarise(Agg_Stability = mean(mm2.per, na.rm = TRUE), .groups = "drop")

lm_fit_avg  <- lm(Agg_Stability ~ soil_health_score, data = as_field_avg)
r2_avg      <- summary(lm_fit_avg)$r.squared
pval_avg    <- summary(lm_fit_avg)$coefficients[2, 4]
sig_lab_avg <- paste0("R\u00b2 = ", round(r2_avg, 2),
                      "  (", ifelse(pval_avg < 0.001, "p < 0.001", paste0("p = ", round(pval_avg, 3))), ")")

cat("Field-averaged Aggregate Stability model: n =", nrow(as_field_avg),
    ", R2 =", round(r2_avg, 3), ", p =", signif(pval_avg, 3), "\n")

p_field_avg <- ggplot(as_field_avg, aes(x = soil_health_score, y = Agg_Stability)) +
  geom_smooth(method = "lm", color = "gray50", fill = "gray85",
              alpha = 0.3, linewidth = 0.8, se = TRUE) +
  geom_point(aes(color = Type, shape = County), size = 4, alpha = 0.9) +
  annotate("text", x = Inf, y = Inf, label = sig_lab_avg,
           hjust = 1.05, vjust = 1.8, size = 4.2, color = "gray30", fontface = "italic") +
  scale_color_manual(values = fill_colors, labels = type_labels_legend, name = "Management") +
  scale_shape_manual(values = c("Grant" = 16, "Fillmore" = 17), name = "County") +
  labs(
    title = "Aggregate Stability (> 2 mm) by Management",
    x = "Soil Health Management Score  (higher = more favorable)",
    y = "Aggregates > 2 mm (%)"
  ) +
  theme_bw() +
  theme(
    axis.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.title      = element_text(size = 13, face = "bold"),
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "lines")
  )

print(p_field_avg)
ggsave("AS_field_avg_vs_score.png", p_field_avg, width = 8.5, height = 6.5, dpi = 300)
cat("Saved AS_field_avg_vs_score.png\n")