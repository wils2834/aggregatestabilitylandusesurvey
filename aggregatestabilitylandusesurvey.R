# Aggregate Stability Analysis – 2024 CRP Project

# LIBRARIES -------------------------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)   # p-values for LMM fixed effects
library(emmeans)
library(multcomp)   # cld()
library(multcompView)
library(rstatix)
library(knitr)

# DATA IMPORT -----------------------------------------------------------------
aggstability2024 <- read.csv("Wilson Aggregate Stability_GC - 2024 - CombinedAS_GC.csv",
                             header = FALSE)

# Row 1 = real column names, Row 2 = descriptions, Row 3+ = data
colnames(aggstability2024) <- as.character(aggstability2024[1, ])

# Drop both the name row and description row
aggstability2024 <- aggstability2024[-c(1, 2), ]

# Reset row numbers
rownames(aggstability2024) <- NULL

# Make duplicate column names unique
colnames(aggstability2024) <- make.unique(colnames(aggstability2024))

# Keep only the columns we actually need
aggstability2024 <- aggstability2024 %>%
  select(Project, UID, County, Farmer, Type, Rep,
         mm2.per, mm1.per, um250.per, um53.per, mmless0053.per)

# Check
names(aggstability2024)
head(aggstability2024)

# DATA CLEANING ---------------------------------------------------------------
fraction_cols <- c("mm2.per", "mm1.per", "um250.per", "um53.per", "mmless0053.per")

aggstability2024 <- aggstability2024 %>%
  mutate(across(all_of(fraction_cols), as.numeric)) %>%
  filter(!is.na(mm2.per)) %>%
  mutate(
    Type   = factor(Type,   levels = type_levels),
    Farmer = as.factor(Farmer),
    County = as.factor(County)
  )

# FACTOR LEVELS & LABELS (used throughout) ------------------------------------
type_levels <- c("Conventional", "Soil_Health", "Rotational_Grazing", "New_CRP", "Old_CRP")
type_labels <- c("Conventional\nCrops & Tillage",
                 "No Till and/or\nCover Crops",
                 "Rotational\nGrazing",
                 "0–10 Years\nCRP",
                 "10+ Years\nCRP")

fraction_levels <- c("mm2.per", "mm1.per", "um250.per", "um53.per", "mmless0053.per")
fraction_labels <- c("> 2 mm", "1–2 mm", "250–1000 µm", "53–250 µm", "< 53 µm")

# Re-apply factor with confirmed levels after cleaning
aggstability2024$Type <- factor(aggstability2024$Type, levels = type_levels)

# LINEAR MIXED MODELS ---------------------------------------------------------
# Farmer as random effect to account for repeated measures / field-level clustering

model_AS     <- lmer(mm2.per       ~ Type + (1 | Farmer), data = aggstability2024)
model_AS_log <- lmer(log(mm2.per)  ~ Type + (1 | Farmer), data = aggstability2024)

# Compare fits – use whichever has lower AIC
AIC(model_AS, model_AS_log)
summary(model_AS)

# Emmeans on log scale - back-transform to original scale with type = "response"
emmeans_AS <- emmeans(model_AS_log, ~ Type, type = "response")

cld_AS <- cld(emmeans_AS, Letters = letters, adjust = "tukey") %>%
  as.data.frame() %>%
  rename(emmean = response) %>%   # back-transformed column is called "response"
  mutate(
    Type   = factor(Type, levels = type_levels),
    .group = trimws(.group)
  )
# PAIRWISE TABLE (for reporting) ----------------------------------------------
pairs_AS <- pairs(emmeans_AS, adjust = "tukey")
kable(as.data.frame(pairs_AS),
      digits  = 4,
      caption = "Pairwise comparisons – Aggregates > 2 mm (LMM, Tukey-adjusted)")

# EMMEANS FOR ALL FRACTIONS ---------------------------------------------------
# Fit a separate LMM per fraction and extract emmeans
emmeans_all <- map_dfr(fraction_levels, function(frac) {
  # Add small constant to avoid log(0) for mmless0053.per
  aggstability2024[[frac]] <- aggstability2024[[frac]] + 0.001
  
  mod <- lmer(reformulate("Type + (1 | Farmer)", response = paste0("log(", frac, ")")),
              data = aggstability2024)
  emmeans(mod, ~ Type, type = "response") %>%
    as.data.frame() %>%
    rename(emmean = response) %>%   # rename to emmean for consistency
    mutate(Fraction = frac)
}) %>%
  mutate(
    Type     = factor(Type,     levels = type_levels),
    Fraction = factor(Fraction, levels = fraction_levels)
  )
# Stacked bar label positions (top of bar) with Tukey letters from >2mm model
label_pos <- emmeans_all %>%
  group_by(Type) %>%
  summarise(y_pos = sum(emmean), .groups = "drop") %>%
  left_join(cld_AS %>% select(Type, .group), by = "Type")

# =============================================================================
# SHARED THEME & SCALES
# =============================================================================

fill_scale <- scale_fill_brewer(
  palette = "Reds",
  name    = "Soil Fraction",
  labels  = fraction_labels
)

# Standard figure theme
theme_agg <- theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 13,
                                   face = "bold", color = "black"),
    axis.text.y     = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y    = element_text(size = 16, face = "bold", color = "black"),
    plot.title      = element_text(size = 18, face = "bold", hjust = 0.5,
                                   color = "black"),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 1.5),
    legend.position = "none"
  )

# Poster/presentation theme (larger text)
theme_poster <- theme_minimal() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 16,
                                face = "bold", color = "black"),
    axis.text.y  = element_text(size = 18, face = "bold", color = "black"),
    axis.title.y = element_text(size = 22, face = "bold", color = "black"),
    plot.title   = element_text(size = 26, face = "bold", hjust = 0.5,
                                color = "black"),
    legend.text  = element_text(size = 18, face = "bold", color = "black"),
    legend.title = element_text(size = 20, face = "bold", color = "black"),
    plot.margin  = margin(10, 20, 30, 10)
  )

# PLOTS
# PLOT 1: Boxplot – raw data + LMM emmean ± 95% CI + Tukey letters ------------
ggplot() +
  geom_boxplot(
    data  = aggstability2024,
    aes(x = Type, y = mm2.per, fill = Type),
    width = 0.5, color = "black", alpha = 0.4
  ) +
  geom_point(
    data  = cld_AS,
    aes(x = Type, y = emmean),
    size  = 3, color = "black"
  ) +
  geom_errorbar(
    data  = cld_AS,
    aes(x = Type, ymin = lower.CL, ymax = upper.CL),
    width = 0.15, color = "black", linewidth = 0.8
  ) +
  geom_text(
    data  = cld_AS,
    aes(x = Type, y = upper.CL + 3, label = .group),
    size  = 5, fontface = "bold", color = "black"
  ) +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  labs(
    title = "Water Stable Aggregates by Management",
    x     = NULL,
    y     = "Aggregates > 2 mm (%)"
  ) +
  theme_agg

# PLOT 2: Stacked bar – emmeans absolute, Tukey letters on top ----------------
ggplot(emmeans_all, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(
    data        = label_pos,
    aes(x = Type, y = y_pos + 3, label = .group),
    inherit.aes = FALSE,
    size        = 8, fontface = "bold"
  ) +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  fill_scale +
  labs(
    title = "Water Stable Aggregates by Management",
    x     = NULL,
    y     = "Estimated Marginal Mean (%)"
  ) +
  theme_poster

# PLOT 3: Stacked bar – proportional (100%) -----------------------------------
ggplot(emmeans_all, aes(x = Type, y = emmean, fill = Fraction)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  scale_y_continuous(labels = scales::percent_format()) +
  fill_scale +
  labs(
    title = "Water Stable Aggregates by Management (Proportional)",
    x     = NULL,
    y     = "Estimated Marginal Mean (%)"
  ) +
  theme_poster

# PLOT 4: Stacked bar faceted by County ---------------------------------------
# Fit county-level emmeans (Type + County as fixed effects)
emmeans_county <- map_dfr(fraction_levels, function(frac) {
  mod <- lmer(reformulate("Type + County + (1 | Farmer)", response = frac),
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
  facet_wrap(~County, scales = "fixed") +
  scale_x_discrete(labels = setNames(type_labels, type_levels)) +
  fill_scale +
  labs(
    title = "Water Stable Aggregates by Management and County",
    x     = NULL,
    y     = "Estimated Marginal Mean (%)"
  ) +
  theme_poster +
  theme(strip.text = element_text(size = 20, face = "bold", color = "black"))
