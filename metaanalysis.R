# Combine all five metric metadata files into one master CSV
# Run this after running all five individual metric scripts

library(dplyr)

master_metadata <- bind_rows(
  read.csv("ACE_metadata.csv"),
  read.csv("POXC_metadata.csv"),
  read.csv("PMC_metadata.csv"),
  read.csv("AS_metadata.csv"),
  read.csv("BD_metadata.csv")
) %>%
  mutate(
    Type = factor(Type, levels = c("Conventional","Soil_Health",
                                   "Rotational_Grazing","New_CRP","Old_CRP")),
    Metric = factor(Metric, levels = c("ACE","POXC","PMC",
                                       "Aggregate_Stability","Bulk_Density"))
  ) %>%
  arrange(Metric, Type)

write.csv(master_metadata, "soil_health_LMM_metadata.csv", row.names = FALSE)
cat("Saved soil_health_LMM_metadata.csv\n")
print(master_metadata)
