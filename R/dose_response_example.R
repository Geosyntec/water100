# Dose Response Example using ecotox data

# Setup -----------------------------------------------
rm(list = ls())
library(dplyr)
library(drc)
library(readr)
library(here)
library(readxl)
library(ggplot2)
library(gt)


# Helper Functions ------------------------------------

geom_mean <- function(x) {
  exp(mean(log(x)))
}

drop_empties <- function(df) {
  df <- df %>% select_if(function(x) !(all(is.na(x)) | all(x == "") | all(x == "NR")))
  return(df)
}

cas_to_numeric <- function(df) {
  df <- df %>%
    mutate_if(is.integer64, as.numeric)
  return(df)
}

# use this to select the columns we care about
relevant_cols <- function(df) {
  return(df %>%
    dplyr::select(
      any_of(
        c(
          "test_id",
          "reference_number",
          "chemical_name",
          "Chemical_name",
          "test_cas",
          "species_number",
          "organism_habitat",
          "exposure_type",
          "control_type",
          "media_type",
          "genus",
          "common_name",
          "latin_name",
          "species",
          "ecotox_group",
          "obs_duration_mean",
          "obs_duration_unit",
          "obs_duration_label",
          "exposure_duration_mean",
          "exposure_duration_unit",
          "exposure_duration_label",
          "endpoint",
          "effect",
          "measurement",
          "conc1_mean",
          "conc1_unit",
          "conversion_factors",
          "conc_ug_L",
          "taxonomic.distance",
          "r", "R2", "intercept", "slope", "MSE",
          "endpoint_type", "US-EPA New Chemical Categories", "CAS", "ASTER",
          "chemical_name",
          "ecotox_group",
          "dtxsid", "epaName", "formatted_cas", "dose", "percent_val", "obs_hours"
        )
      )
    ))
}



# Import Data -----------------------------------------


tests.df <- read_rds(here("data", "ecotox_mort_tests.rds")) # previously extracted salmon mortality tests

# previously classified endpoint percentage values
endpoint_classifications <- read_excel(here("xlsx", "endpoint_classifications.xlsx")) %>%
  dplyr::select(c(endpoint, endpoint_type = Type, percent_val)) %>%
  filter(endpoint_type != "out")

# Chemical information
chemicals <- read_rds(here("data", "selected_chemical_info.rds"))

# Data Cleanup  ---------------------------------------

## Convert concentration values to ug/L ----------------

unit_names <- c(
  "ug/L", "ppm", "mg/L", "AI ug/L", "ppb",
  "AI ppb", "mmol/L", "AI ppm", "ng/L", "mg/kg bdwt",
  "mg/kg", "uM", "AI mg/L", "ul/L", "ug/kg bdwt",
  "ng/ml", "uCI/g org", "ml/L", "g/kg"
)

conversion_factors <- c(
  1, 1e3, 1e3, 1, 1,
  1, NA, 1e3, 1e-3, NA,
  NA, NA, 1e3, NA, NA,
  1, NA, 1000, NA
)

unit_conversions.df <- data.frame(unit_names, conversion_factors) %>% filter(!is.na(conversion_factors))



## Convert duration units ----------------------------
obs_duration_unit <- c("h", "d", "NR", "dpu", "mo", "dph", "wk", "mi")
# Time_Unit <- c("hours", "days", "NR", "days post swim up", "months", "days post hatch", "weeks", "minutes")
time_conversion_factors <- c(1, 24, NA, 24, 720, 24, 168, 0.016666667)

duration_conversions.df <- data.frame(obs_duration_unit, time_conversion_factors)

cleaned_results <- tests.df %>%
  left_join(unit_conversions.df, by = join_by(conc1_unit == unit_names)) %>%
  mutate(conc_ug_L = conversion_factors * as.numeric(conc1_mean)) %>%
  left_join(duration_conversions.df, by = join_by(obs_duration_unit)) %>%
  mutate(obs_hours = paste(time_conversion_factors * as.numeric(obs_duration_mean)),'hours') %>%
  left_join(endpoint_classifications) %>% # %>%
  rename(dose = conc_ug_L) %>%
  mutate(obs_duration_label = paste(obs_duration_mean, obs_duration_unit, sep = "-")) %>%
  distinct() %>%
  filter(!is.na(endpoint_type)) %>% 
  mutate_at(vars(obs_hours), ~replace(., is.na(.), 0))


## Join with chemical information ----------------------
chem_results <- chemicals %>% right_join(cleaned_results, by = join_by(test_cas))



## Summarize results  ----------------------------------
# This command exports to the clipboard for viewing in excel
results_summarized <-
  chem_results %>%
  group_by(epaName, ecotox_group, ASTER, `US-EPA New Chemical Categories`, EPA_aquatic_criteria, formatted_cas, effect, test_cas) %>%
  summarize(
    tests = n(),
    endpoint_types = n_distinct(endpoint)
  ) %>%
  clipr::write_clip() %>%
  ungroup()

# Choose only chemicals that have a LC50 and two other endpoints
selected_chemicals <- results_summarized %>% filter(endpoint_types >= 3)

selected_chemicals %>% gt()


#  Generate Dose Response Curve  ----------------------
cocs <- selected_chemicals$epaName %>% unique()
cocs <- c(
# "1,1,1-Trichloroethane",
# "2,4-Dichlorophenol", 
# "Chlorpyrifos", 
"Chlorobenzene", 
 "Carbaryl", 
# "Cadmium", 
# "Atrazine", 
 "Acrolein",
# "3,4-Dichloroaniline",
"Atrazine",
"Toxaphene",
"Aroclor 1254",
"Naphthalene",
"Tris(2-chloroethyl) phosphate")


for (i in 1:length(cocs)) {
  coc <- selected_chemicals %>%
    filter(epaName == cocs[i])

  # filter for coc
  sel_tests <- cleaned_results %>%
    filter(test_cas == coc$test_cas) %>%
    left_join(endpoint_classifications) #%>%
    #relevant_cols() #%>%
    #drop_empties() #%>%
    #filter(!is.na(dose))

  tryCatch(
    {
      model.1 <- drm(percent_val ~ log10(dose),
        data = sel_tests,
        fct = LL.2()
      )


      ## Plot model ------------------------------------------
      plot(model.1, main = coc$epaName, xlab = "dose ug/L", ylab = "response (percent mortality)")

      ## Summarize Model -------------------------------------
      model.1 %>% summary()
    },
    error = function(e) {
      print(paste("error on ", cocs[i]))
    }
  )
  tryCatch(
    {
      # Dose response by duration -----------------------------------
      model.2 <- drm(percent_val ~ log10(dose),
        data = sel_tests, curveid = exposure_duration_mean,
        fct = LL.2()
      )

      plot(model.2, main = paste(coc$epaName, "by duration"), xlab = "dose ug/L", ylab = "response (percent mortality)")
    },
    error = function(e) {
      print(paste("error on ", cocs[i], "by duration"))
    }
  )
}


mylogit <- glm(percent_val ~ dose, data = sel_tests, family = "binomial")
plot(mylogit)


