master_metadata <- bind_rows(
  read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\ACElandusesurvey\\ACE_metadata.csv"),
  read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\POXClandusesurvey\\POXC_metadata.csv"),
  read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\PMClandusesurvey\\PMC_metadata.csv"),
  read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\aggregatestabilitylandusesurvey\\AS_metadata.csv"),
  read.csv("C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\POXClandusesurvey\\BD_metadata.csv")
) %>%
  mutate(
    Type = factor(Type, levels = c("Conventional","Soil_Health",
                                   "Rotational_Grazing","New_CRP","Old_CRP")),
    Metric = factor(Metric, levels = c("ACE","POXC","PMC",
                                       "Aggregate_Stability","Bulk_Density"))
  ) %>%
  arrange(Metric, Type)

write.csv(master_metadata, 
          "C:\\Users\\swils\\Downloads\\soil_health_LMM_metadata.csv", 
          row.names = FALSE)
cat("Saved\n")
print(master_metadata)

#--------------adding CSV---------------------------------------------
master <- read.csv("C:\\Users\\swils\\Downloads\\soil_health_LMM_metadata.csv",
                   stringsAsFactors = FALSE)

# Format each cell as "mean ± SE letter"
master <- master_metadata %>%
  mutate(
    Mean = round(Mean, 2),
    SE   = round(SE,   2),
    CLD  = trimws(CLD),
    cell = paste0(Mean, " +/- ", SE, " ", CLD),
    Metric = factor(Metric, levels = c("ACE","POXC","PMC",
                                       "Aggregate_Stability","Bulk_Density")),
    Type   = factor(Type,   levels = c("Conventional","Soil_Health",
                                       "Rotational_Grazing","New_CRP","Old_CRP"))
  )

table_wide <- master %>%
  dplyr::select(Metric, Type, cell) %>%
  pivot_wider(names_from = Type, values_from = cell)

table_wide <- table_wide %>%
  mutate(Units = case_when(
    Metric == "ACE"                 ~ "g Protein kg-1 soil",
    Metric == "POXC"                ~ "moles permanganate kg-1 soil",
    Metric == "PMC"                 ~ "ug C g-1 day-1",
    Metric == "Aggregate_Stability" ~ "% > 2 mm",
    Metric == "Bulk_Density"        ~ "g cm-3"
  )) %>%
  dplyr::select(Metric, Units, Conventional, Soil_Health,
                Rotational_Grazing, New_CRP, Old_CRP)

print(table_wide)

write.csv(table_wide,
          "C:\\Users\\swils\\Downloads\\01 Projects\\2024 CRP\\Landusesurveygithub\\soil_health_summary_table.csv",
          row.names = FALSE)
cat("Saved soil_health_summary_table.csv\n")
