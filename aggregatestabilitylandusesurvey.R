# Aggregate Stability Analysis — 2024 CRP Project
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(multcompView)
library(knitr)

# ---- Factor levels used throughout --------------------------------------
type_levels <- c("Conventional","Soil_Health","Rotational_Grazing","New_CRP","Old_CRP")
type_labels  <- c("Conventional\nCrops & Tillage",
                  "No Till and/or\nCover Crops",
                  "Rotational\nGrazing",
                  "0-10 Years\nCRP",
                  "10+ Years\nCRP")

fraction_levels <- c("mm2.per","mm1.per","um250.per","um53.per","mmless0053.per")
fraction_labels <- c("> 2 mm","1-2 mm","250-1000 um","53-250 um","< 53 um")

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
# ---- Data cleaning ------------------------------------------------------
fraction_cols <- c("mm2.per","mm1.per","um250.per","um53.per","mmless0053.per")

aggstability2024 <- aggstability2024 %>%
  mutate(across(all_of(fraction_cols), as.numeric)) %>%
  filter(!is.na(mm2.per)) %>%
  mutate(
    Type   = factor(Type,   levels = type_levels),
    Farmer = as.factor(Farmer),
    County = as.factor(County)
  )

# Build true field identifier — same fix applied to ACE, POXC, and PMC.
# RLH_Inc and UW_AG each contributed two separate fields under different
# management types; using Farmer alone as the random effect would
# incorrectly pool those fields into one group.
aggstability2024 <- aggstability2024 %>%
  mutate(Field = paste(Farmer, Type, sep = "_"))

cat("Distinct fields:", n_distinct(aggstability2024$Field), "(expect 31)\n")
stopifnot(n_distinct(aggstability2024$Field) == 31)

# ---- Mixed models -------------------------------------------------------
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

# ---- Emmeans and CLD for > 2 mm fraction --------------------------------
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

# ---- Emmeans for all fractions ------------------------------------------
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

# ---- Label positions (Tukey letters from > 2 mm model) ------------------
label_pos <- emmeans_all %>%
  group_by(Type) %>%
  summarise(y_pos = sum(emmean), .groups = "drop") %>%
  left_join(cld_AS %>% select(Type, .group), by = "Type")

# ---- Shared theme -------------------------------------------------------
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

# ---- Plot 1: Boxplot with emmeans and CLD letters -----------------------
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

# ---- Plot 2: Stacked bar — absolute emmeans -----------------------------
ggplot(emmeans_all, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(data = label_pos,
            aes(x = Type, y = y_pos + 3, label = .group),
            inherit.aes = FALSE, size = 8, fontface = "bold") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  fill_scale +
  labs(title = "Water Stable Aggregates by Management",
       x = NULL, y = "Estimated Marginal Mean (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13,
                                   face = "bold", color = "black"))

# ---- Plot 3: Stacked bar — proportional (100%) --------------------------
ggplot(emmeans_all, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  scale_y_continuous(labels = scales::percent_format()) +
  fill_scale +
  labs(title = "Water Stable Aggregates by Management (Proportional)",
       x = NULL, y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13,
                                   face = "bold", color = "black"))

# ---- Plot 4: Stacked bar faceted by county ------------------------------
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
       x = NULL, y = "Estimated Marginal Mean (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13,
                               face = "bold", color = "black"),
    strip.text  = element_text(size = 14, face = "bold", color = "black")
  )

# ---- Metadata export ----------------------------------------------------
anova_AS <- anova(final_AS)

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
                LMM_F, LMM_p, Type, Mean, CLD)

write.csv(AS_meta, "AS_metadata.csv", row.names = FALSE)
cat("Saved AS_metadata.csv\n")

write.csv(AS_meta, "AS_metadata.csv", row.names = FALSE)
cat("Saved AS_metadata.csv\n")

