## HEADER -----------------------------------------------------
##  R file METADATA
##  algorithm name          cancer_dashboard / app.R
##  project:                BNR
##  analysts:               Kern Rocke
##  date first created      18-AUG-2025
##  date last modified      07-FEB-2026
##  algorithm task          Create Dashboard for Barbados Cancer Registry
##  status                  Completed
##  objective               To have a dashboard for monitoring cancer registry data
##  methods                 See additional information on dashboard. 

#-------------------------------------------------------------------------------
######################
### Libraries ###
#####################
# Note: Add any new libraries to the list of libaries in libs

#List of libaries needed
#libs <- c("shiny", "shinydashboard", "shinyauthr", "shinyjs", "sodium", "dplyr",
#          "ggplot2", "DT", "lubridate", "survival", "plotly", "tidyr", "purrr",
#          "readxl", "qcc", "officer", "rvg", "sf", "leaflet", "viridis") 

#Install missing libraries
#installed_libs <- libs %in% rownames(installed.packages())
#if (any(installed_libs == F)) {
#  install.packages(libs[!installed_libs])
#}

#Load libraries
#invisible(lapply(libs, library, character.only = T))

library(shiny)
library(shinydashboard)
library(shinyauthr)
library(shinyjs)
library(sodium)
library(ggplot2)
library(DT)
library(lubridate)
library(survival)
library(plotly)
library(tidyr)
library(purrr)
library(readxl)
library(qcc)
library(officer)
library(rvg)
library(sf)          
library(leaflet)     
library(viridis)
library(dplyr)      # loaded last so dplyr::select() wins over sf/rvg/MASS conflicts
# MASS is used via MASS::glm.nb() only — not attached to avoid masking dplyr::select()
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")

#-------------------------------------------------------------------------------

# User credentials
user_base <- tibble::tibble(
  user = "bnr_cancer",
  password = sodium::password_store("cancer!001")
)

# Read the CSV files
data <- read.csv("data/cancer_2013_2022_bnr.csv", stringsAsFactors = FALSE)
mortality_data <- read.csv("data/cancer_death_2008_2024.csv", stringsAsFactors = FALSE)

# Load parish geo shapefile
parish_shapefile <- st_read("data/Barbados_Parish.shp") 
st_crs(parish_shapefile) <- 3857
parish_shapefile <-  st_transform(parish_shapefile, 4326)
parish_shapefile <- st_make_valid(parish_shapefile)

# Load population data from WPP.xlsx
years <- c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)
pop_data <- map2_dfr(seq_along(years), years, ~{
  read_excel("data/WPP.xlsx", sheet = .x) %>%
    mutate(year = .y)
})
pop_data$sex <- ifelse(pop_data$sex == 1, "male", "female")

# Load parish population data
parish_pop_raw <- read.csv("data/parish_population_by_age.csv", stringsAsFactors = FALSE)

# Fix typo in Christ Church name
parish_pop_raw$parish[parish_pop_raw$parish == "Christ Churc"] <- "Christ Church"

# Convert 10-year age bands to 5-year age bands for WHO standardization
# We'll approximate by splitting each 10-year band evenly
parish_pop_by_age5 <- parish_pop_raw %>%
  mutate(
    # Ages 0-4 (half of 0-9)
    age_0_4 = age_0_9 * 0.5,
    # Ages 5-9 (half of 0-9)
    age_5_9 = age_0_9 * 0.5,
    # Ages 10-14 (half of 10-19)
    age_10_14 = age_10_19 * 0.5,
    # Ages 15-19 (half of 10-19)
    age_15_19 = age_10_19 * 0.5,
    # Ages 20-24 (half of 20-29)
    age_20_24 = age_20_29 * 0.5,
    # Ages 25-29 (half of 20-29)
    age_25_29 = age_20_29 * 0.5,
    # Ages 30-34 (half of 30-39)
    age_30_34 = age_30_39 * 0.5,
    # Ages 35-39 (half of 30-39)
    age_35_39 = age_30_39 * 0.5,
    # Ages 40-44 (half of 40-49)
    age_40_44 = age_40_49 * 0.5,
    # Ages 45-49 (half of 40-49)
    age_45_49 = age_40_49 * 0.5,
    # Ages 50-54 (half of 50-59)
    age_50_54 = age_50_59 * 0.5,
    # Ages 55-59 (half of 50-59)
    age_55_59 = age_50_59 * 0.5,
    # Ages 60-64 (half of 60-69)
    age_60_64 = age_60_69 * 0.5,
    # Ages 65-69 (half of 60-69)
    age_65_69 = age_60_69 * 0.5,
    # Ages 70-74 (half of 70-79)
    age_70_74 = age_70_79 * 0.5,
    # Ages 75-79 (half of 70-79)
    age_75_79 = age_70_79 * 0.5,
    # Ages 80-84 (half of 80-89)
    age_80_84 = age_80_89 * 0.5,
    # Ages 85+ (half of 80-89 + all 90+)
    age_85_plus = age_80_89 * 0.5 + age_90_99 + age_100_plus
  ) %>%
  dplyr::select(parish, total_pop, starts_with("age_"))

# Reshape to long format for easier merging
parish_pop_long <- parish_pop_by_age5 %>%
  dplyr::select(parish, age_0_4:age_85_plus) %>%
  pivot_longer(
    cols = starts_with("age_"),
    names_to = "age_band",
    values_to = "population"
  ) %>%
  mutate(
    age_group = case_when(
      age_band == "age_0_4" ~ 1,
      age_band == "age_5_9" ~ 2,
      age_band == "age_10_14" ~ 3,
      age_band == "age_15_19" ~ 4,
      age_band == "age_20_24" ~ 5,
      age_band == "age_25_29" ~ 6,
      age_band == "age_30_34" ~ 7,
      age_band == "age_35_39" ~ 8,
      age_band == "age_40_44" ~ 9,
      age_band == "age_45_49" ~ 10,
      age_band == "age_50_54" ~ 11,
      age_band == "age_55_59" ~ 12,
      age_band == "age_60_64" ~ 13,
      age_band == "age_65_69" ~ 14,
      age_band == "age_70_74" ~ 15,
      age_band == "age_75_79" ~ 16,
      age_band == "age_80_84" ~ 17,
      age_band == "age_85_plus" ~ 18
    )
  ) %>%
  dplyr::select(parish, age_group, population)

# WHO 2000 standard population weights for 18 age groups (0-4 to 85+)
who_weights <- c(8860, 8690, 8590, 8470, 8220, 7930, 7610, 7150, 6590, 6040, 5380, 4550, 3720, 2960, 2210, 1520, 900, 600) / 100000

# Function to compute ASIR
compute_asir <- function(cancer_data, pop_data, who_weights, site, sex_group) {
  if (site == "All cancers") {
    cancer_df <- cancer_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    cancer_df <- cancer_data %>% filter(siteiarc == site)
  }
  if (sex_group != "Both") {
    cancer_df <- cancer_df %>% filter(sex == tolower(sex_group))
  }
  if (nrow(cancer_df) == 0) {
    return(data.frame(year = integer(), asir = numeric()))
  }
  cancer_df <- cancer_df %>%
    mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), labels = 1:18, right = FALSE))) %>%
    filter(!is.na(age_group)) %>%
    group_by(year = dxyr, age_group) %>%
    summarise(counts = n(), .groups = 'drop')
  
  years <- unique(cancer_data$dxyr)
  age_groups <- 1:18
  full_df <- expand_grid(year = years, age_group = age_groups) %>%
    left_join(cancer_df, by = c("year", "age_group")) %>%
    mutate(counts = coalesce(counts, 0))
  
  if (sex_group == "Both") {
    pop_df <- pop_data %>%
      group_by(year, age5) %>%
      summarise(pop = sum(pop_wpp), .groups = 'drop') %>%
      rename(age_group = age5)
  } else {
    pop_df <- pop_data %>%
      filter(sex == tolower(sex_group)) %>%
      dplyr::select(year, age_group = age5, pop = pop_wpp)
  }
  
  full_df <- full_df %>%
    left_join(pop_df, by = c("year", "age_group")) %>%
    mutate(pop = coalesce(pop, 0),
           age_rate = ifelse(pop > 0, counts / pop * 100000, 0)) %>%
    group_by(year) %>%
    summarise(asir = sum(age_rate * who_weights[age_group]), .groups = 'drop')
  
  full_df
}

# Function to compute Cumulative Incidence (0-74 years)
compute_cuminc <- function(cancer_data, pop_data, site, sex_group) {
  if (site == "All cancers") {
    cancer_df <- cancer_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    cancer_df <- cancer_data %>% filter(siteiarc == site)
  }
  if (sex_group != "Both") {
    cancer_df <- cancer_df %>% filter(sex == tolower(sex_group))
  }
  if (nrow(cancer_df) == 0) {
    return(data.frame(year = integer(), cuminc = numeric()))
  }
  cancer_df <- cancer_df %>%
    mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), labels = 1:18, right = FALSE))) %>%
    filter(!is.na(age_group)) %>%
    group_by(year = dxyr, age_group) %>%
    summarise(counts = n(), .groups = 'drop')
  
  years <- unique(cancer_data$dxyr)
  age_groups <- 1:18
  full_df <- expand_grid(year = years, age_group = age_groups) %>%
    left_join(cancer_df, by = c("year", "age_group")) %>%
    mutate(counts = coalesce(counts, 0))
  
  if (sex_group == "Both") {
    pop_df <- pop_data %>%
      group_by(year, age5) %>%
      summarise(pop = sum(pop_wpp), .groups = 'drop') %>%
      rename(age_group = age5)
  } else {
    pop_df <- pop_data %>%
      filter(sex == tolower(sex_group)) %>%
      dplyr::select(year, age_group = age5, pop = pop_wpp)
  }
  
  full_df <- full_df %>%
    left_join(pop_df, by = c("year", "age_group")) %>%
    mutate(pop = coalesce(pop, 0),
           age_rate = ifelse(pop > 0, counts / pop * 100000, 0)) %>%
    group_by(year) %>%
    summarise(cuminc = sum(age_rate[age_group %in% 1:15] * 5) / 100000 * 100, .groups = 'drop')
  
  full_df
}

# Function to compute Crude Incidence Rate
compute_crude_incidence <- function(cancer_data, pop_data, site, sex_group) {
  if (site == "All cancers") {
    cancer_df <- cancer_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    cancer_df <- cancer_data %>% filter(siteiarc == site)
  }
  if (sex_group != "Both") {
    cancer_df <- cancer_df %>% filter(sex == tolower(sex_group))
  }
  if (nrow(cancer_df) == 0) {
    return(data.frame(year = integer(), crude_rate = numeric()))
  }
  
  # Count cases by year
  cancer_counts <- cancer_df %>%
    group_by(year = dxyr) %>%
    summarise(counts = n(), .groups = 'drop')
  
  # Get population data
  if (sex_group == "Both") {
    pop_df <- pop_data %>%
      group_by(year) %>%
      summarise(pop = sum(pop_wpp), .groups = 'drop')
  } else {
    pop_df <- pop_data %>%
      filter(sex == tolower(sex_group)) %>%
      group_by(year) %>%
      summarise(pop = sum(pop_wpp), .groups = 'drop')
  }
  
  # Merge cancer counts with population data
  crude_df <- cancer_counts %>%
    left_join(pop_df, by = "year") %>%
    mutate(
      pop = coalesce(pop, 0),
      crude_rate = ifelse(pop > 0, counts / pop * 100000, 0)
    ) %>%
    select(year, crude_rate)
  
  crude_df
}

# Function to compute ASR trends for top 5 cancer sites
compute_top5_asr_trends <- function(data, pop_data, who_weights) {
  # Get top 5 cancer sites by total frequency (excluding O&U)
  top5_sites <- data %>%
    filter(siteiarc != "Other and unspecified (O&U)") %>%
    count(siteiarc) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    pull(siteiarc)
  
  # Compute ASR for each of the top 5 sites
  asr_trends <- map_dfr(top5_sites, ~{
    site_data <- compute_asir(data, pop_data, who_weights, .x, "Both")
    if(nrow(site_data) > 0) {
      site_data$cancer_site <- .x
      return(site_data)
    } else {
      return(NULL)
    }
  })
  
  return(asr_trends)
}

# Function to compute prevalence data
compute_prevalence <- function(data, pop_data, site, sex_group, prevalence_date = "2022-12-31") {
  # Filter for selected site
  if (site == "All cancers") {
    cancer_df <- data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    cancer_df <- data %>% filter(siteiarc == site)
  }
  
  # Filter for sex if specified
  if (sex_group != "Both") {
    cancer_df <- cancer_df %>% filter(sex == tolower(sex_group))
  }
  
  if (nrow(cancer_df) == 0) {
    return(list(survivors = 0, prevalence_rate = 0, pop_total = 0, age_data = data.frame()))
  }
  
  # Parse dates and calculate survival status at prevalence date
  cancer_df <- cancer_df %>%
    mutate(
      dx_date = as.Date(parse_incidence(IncidenceDate)),
      death_date = if_else(deceased == "dead", as.Date(dmy(dod), quiet = TRUE), as.Date(NA)),
      last_contact = as.Date(dmy(dlc), quiet = TRUE),
      prevalence_date_calc = as.Date(prevalence_date)
    ) %>%
    filter(!is.na(dx_date) & dx_date <= prevalence_date_calc)
  
  # Determine who is alive at prevalence date
  cancer_df <- cancer_df %>%
    mutate(
      alive_at_prev_date = case_when(
        !is.na(death_date) & death_date <= prevalence_date_calc ~ FALSE,
        !is.na(last_contact) & last_contact >= prevalence_date_calc ~ TRUE,
        !is.na(last_contact) & last_contact >= (prevalence_date_calc - 365) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Count survivors
  survivors <- cancer_df %>% 
    filter(alive_at_prev_date == TRUE) %>%
    nrow()
  
  # Calculate prevalence rate (as percentage)
  prevalence_year <- as.numeric(format(as.Date(prevalence_date), "%Y"))
  pop_filtered <- pop_data %>%
    filter(year == prevalence_year)
  
  # Calculate pop_total with validation
  if (nrow(pop_filtered) == 0) {
    warning("No population data found for year ", prevalence_year)
    pop_total <- 0
  } else {
    pop_total <- if (sex_group == "Both") {
      sum(pop_filtered$pop_wpp, na.rm = TRUE)
    } else {
      sum(pop_filtered$pop_wpp[pop_filtered$sex == tolower(sex_group)], na.rm = TRUE)
    }
  }
  
  # Ensure pop_total is not NA or NULL
  pop_total <- if (is.na(pop_total) || is.null(pop_total)) 0 else pop_total
  
  prevalence_rate <- if (pop_total > 0) survivors / pop_total * 100 else 0
  
  # Calculate age group data for survivors
  age_data <- cancer_df %>%
    filter(alive_at_prev_date == TRUE) %>%
    mutate(age_group = cut(age, 
                           breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
                           labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                      "80-84", "85+"),
                           right = FALSE)) %>%
    filter(!is.na(age_group)) %>%
    group_by(age_group, sex) %>%
    summarise(survivors = n(), .groups = 'drop')
  
  return(list(
    survivors = survivors,
    prevalence_rate = prevalence_rate,
    pop_total = pop_total,
    age_data = age_data
  ))
}

# Function to standardize parish names across datasets
standardize_parish_names <- function(df, parish_col = "parish") {
  if (!(parish_col %in% names(df))) {
    return(df)
  }
  
  df %>%
    mutate(!!parish_col := case_when(
      # Fix typo in Christ Church
      !!sym(parish_col) %in% c("Christ Churc") ~ "Christ Church",
      # Standardize St. abbreviations to Saint
      !!sym(parish_col) %in% c("St. Andrew", "St Andrew", "St.Andrew") ~ "Saint Andrew",
      !!sym(parish_col) %in% c("St. George", "St George", "St.George") ~ "Saint George",
      !!sym(parish_col) %in% c("St. James", "St James", "St.James") ~ "Saint James",
      !!sym(parish_col) %in% c("St. John", "St John", "St.John") ~ "Saint John",
      !!sym(parish_col) %in% c("St. Joseph", "St Joseph", "St.Joseph") ~ "Saint Joseph",
      !!sym(parish_col) %in% c("St. Lucy", "St Lucy", "St.Lucy") ~ "Saint Lucy",
      !!sym(parish_col) %in% c("St. Michael", "St Michael", "St.Michael") ~ "Saint Michael",
      !!sym(parish_col) %in% c("St. Peter", "St Peter", "St.Peter") ~ "Saint Peter",
      !!sym(parish_col) %in% c("St. Philip", "St Philip", "St.Philip") ~ "Saint Philip",
      !!sym(parish_col) %in% c("St. Thomas", "St Thomas", "St.Thomas") ~ "Saint Thomas",
      TRUE ~ !!sym(parish_col)
    ))
}

# Also standardize the parish population data names
parish_pop_long <- standardize_parish_names(parish_pop_long, "parish")

# Function to compute cancer cases by parish for top 5 cancers
compute_parish_top5_cases <- function(cancer_data) {
  # Get top 5 cancer sites by total frequency (excluding O&U)
  top5_sites <- cancer_data %>%
    filter(siteiarc != "Other and unspecified (O&U)") %>%
    count(siteiarc) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    pull(siteiarc)
  
  # Count cases by parish for each top 5 cancer
  parish_data <- cancer_data %>%
    filter(siteiarc %in% top5_sites) %>%
    filter(!is.na(parish), parish != "")
  
  # Standardize parish names
  parish_data <- standardize_parish_names(parish_data, "parish")
  
  # Group and count
  parish_data <- parish_data %>%
    group_by(parish, siteiarc) %>%
    summarise(cases = n(), .groups = 'drop')
  
  return(list(data = parish_data, top5 = top5_sites))
}

# Function to compute ASIR by parish using actual parish population data
compute_parish_asir <- function(cancer_data, parish_pop_data, who_weights, site, sex_group) {
  # Filter for site and sex
  if (site == "All cancers") {
    cancer_df <- cancer_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    cancer_df <- cancer_data %>% filter(siteiarc == site)
  }
  
  if (sex_group != "Both") {
    cancer_df <- cancer_df %>% filter(sex == tolower(sex_group))
  }
  
  if (nrow(cancer_df) == 0 || !("parish" %in% names(cancer_df))) {
    return(data.frame(parish = character(), asir = numeric()))
  }
  
  # Standardize parish names in cancer data
  cancer_df <- standardize_parish_names(cancer_df, "parish")
  
  # Create age groups (1-18 for WHO standard)
  cancer_df <- cancer_df %>%
    mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), 
                                      labels = 1:18, right = FALSE))) %>%
    filter(!is.na(age_group), !is.na(parish), parish != "")
  
  if (nrow(cancer_df) == 0) {
    return(data.frame(parish = character(), asir = numeric()))
  }
  
  # Calculate cancer counts by parish and age group
  parish_cancer <- cancer_df %>%
    group_by(parish, age_group) %>%
    summarise(counts = n(), .groups = 'drop')
  
  # Get all unique parishes from the data
  all_parishes <- unique(cancer_df$parish)
  
  # Create complete grid of parish x age_group combinations
  parish_age_grid <- expand_grid(
    parish = all_parishes,
    age_group = 1:18
  )
  
  # Merge cancer counts with the grid
  parish_cancer_full <- parish_age_grid %>%
    left_join(parish_cancer, by = c("parish", "age_group")) %>%
    mutate(counts = coalesce(counts, 0))
  
  # For sex-specific calculations, we need to adjust population
  # Since we don't have sex-specific parish populations, we'll use total and assume 50/50 split
  # This is a limitation but reasonable for parish-level analysis
  pop_multiplier <- if (sex_group == "Both") 1.0 else 0.5
  
  # Merge with parish population data
  parish_cancer_pop <- parish_cancer_full %>%
    left_join(parish_pop_data, by = c("parish", "age_group")) %>%
    mutate(
      population = coalesce(population * pop_multiplier, 0),
      age_rate = ifelse(population > 0, counts / population * 100000, 0)
    )
  
  # Calculate ASIR by parish using WHO weights
  parish_asir <- parish_cancer_pop %>%
    group_by(parish) %>%
    summarise(
      asir = sum(age_rate * who_weights[age_group]),
      total_cases = sum(counts),
      .groups = 'drop'
    ) %>%
    arrange(desc(asir))
  
  return(parish_asir)
}

# Function to compute mortality rate by parish using actual parish population data
compute_parish_mortality <- function(mortality_data, parish_pop_data, who_weights, site, sex_group) {
  # Filter for site and sex
  if (site == "All cancers") {
    mort_df <- mortality_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    mort_df <- mortality_data %>% filter(siteiarc == site)
  }
  
  if (sex_group != "Both") {
    mort_df <- mort_df %>% filter(sex == tolower(sex_group))
  }
  
  if (nrow(mort_df) == 0 || !("parish" %in% names(mort_df))) {
    return(data.frame(parish = character(), mortality_rate = numeric()))
  }
  
  # Standardize parish names in mortality data
  mort_df <- standardize_parish_names(mort_df, "parish")
  
  # Create age groups (1-18 for WHO standard)
  mort_df <- mort_df %>%
    mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), 
                                      labels = 1:18, right = FALSE))) %>%
    filter(!is.na(age_group), !is.na(parish), parish != "")
  
  if (nrow(mort_df) == 0) {
    return(data.frame(parish = character(), mortality_rate = numeric()))
  }
  
  # Calculate death counts by parish and age group
  parish_deaths <- mort_df %>%
    group_by(parish, age_group) %>%
    summarise(deaths = n(), .groups = 'drop')
  
  # Get all unique parishes from the data
  all_parishes <- unique(mort_df$parish)
  
  # Create complete grid of parish x age_group combinations
  parish_age_grid <- expand_grid(
    parish = all_parishes,
    age_group = 1:18
  )
  
  # Merge death counts with the grid
  parish_deaths_full <- parish_age_grid %>%
    left_join(parish_deaths, by = c("parish", "age_group")) %>%
    mutate(deaths = coalesce(deaths, 0))
  
  # For sex-specific calculations, adjust population (assume 50/50 split)
  pop_multiplier <- if (sex_group == "Both") 1.0 else 0.5
  
  # Merge with parish population data
  parish_deaths_pop <- parish_deaths_full %>%
    left_join(parish_pop_data, by = c("parish", "age_group")) %>%
    mutate(
      population = coalesce(population * pop_multiplier, 0),
      age_rate = ifelse(population > 0, deaths / population * 100000, 0)
    )
  
  # Calculate age-standardized mortality rate by parish using WHO weights
  parish_mort <- parish_deaths_pop %>%
    group_by(parish) %>%
    summarise(
      mortality_rate = sum(age_rate * who_weights[age_group]),
      total_deaths = sum(deaths),
      .groups = 'drop'
    ) %>%
    arrange(desc(mortality_rate))
  
  return(parish_mort)
}

# Function to create leaflet map
create_parish_map <- function(shapefile, data_df, value_col, legend_title, map_values) {
  
  # Standardize parish names in data if parish column exists
  if ("parish" %in% names(data_df)) {
    data_df <- standardize_parish_names(data_df, "parish")
  }
  
  # Merge shapefile with data
  # Note: shapefile uses NAME_1, not PARISH_NAME
  map_data <- shapefile %>%
    left_join(data_df, by = c("NAME_1" = "parish"))
  
  # Check if we have any non-NA values
  has_data <- any(!is.na(map_data[[value_col]]))
  
  if (!has_data) {
    # Return a simple gray map with a message
    return(
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = "#CCCCCC",
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7
        ) %>%
        addControl("No data available for selected criteria", position = "topright")
    )
  }
  
  # Create color palette
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = map_data[[value_col]],
    na.color = "#808080"
  )
  
  # Create leaflet map
  leaflet(map_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~pal(get(value_col)),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 3,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = ~paste0(NAME_1, ": ", round(get(value_col), 2)),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~get(value_col),
      opacity = 0.7,
      title = legend_title,
      position = "bottomright"
    )
}

# Function to get top prevalent cancers
get_top_prevalent_cancers <- function(data, sex_group = "Both", prevalence_date = "2022-12-31") {
  # Filter for sex if specified
  df <- data
  if (sex_group != "Both") {
    df <- df %>% filter(sex == tolower(sex_group))
  }
  
  # Parse dates and calculate survival status
  df <- df %>%
    mutate(
      dx_date = as.Date(parse_incidence(IncidenceDate)),
      death_date = if_else(deceased == "dead", as.Date(dmy(dod), quiet = TRUE), as.Date(NA)),
      last_contact = as.Date(dmy(dlc), quiet = TRUE),
      prevalence_date_calc = as.Date(prevalence_date)
    ) %>%
    filter(!is.na(dx_date) & dx_date <= prevalence_date_calc)
  
  # Determine who is alive at prevalence date
  df <- df %>%
    mutate(
      alive_at_prev_date = case_when(
        !is.na(death_date) & death_date <= prevalence_date_calc ~ FALSE,
        !is.na(last_contact) & last_contact >= prevalence_date_calc ~ TRUE,
        !is.na(last_contact) & last_contact >= (prevalence_date_calc - 365) ~ TRUE,
        TRUE ~ FALSE
      )
    )
  
  # Count survivors by cancer site
  counts <- df %>%
    filter(alive_at_prev_date == TRUE, siteiarc != "Other and unspecified (O&U)") %>%
    count(siteiarc)
  total_survivors <- sum(counts$n)
  counts %>%
    mutate(`Prevalence (%)` = round(n / total_survivors * 100, 1)) %>%
    select(-n) %>%
    arrange(desc(`Prevalence (%)`)) %>%
    head(10) %>%
    rename(`Cancer Site` = siteiarc)
}

# Function to parse incidence dates
parse_incidence <- function(x) {
  x <- as.character(x)
  sapply(x, function(y) {
    if (is.na(y) || y == "") return(NA)
    tryCatch({
      if (grepl("^\\d{8}$", y)) {  # YYYYMMDD
        return(format(ymd(y), "%Y-%m-%d"))
      } else if (grepl("^\\d{1,2} \\w{3} \\d{4}$", y)) {  # DD MMM YYYY
        return(format(dmy(y), "%Y-%m-%d"))
      } else {
        return(NA)
      }
    }, warning = function(w) {
      return(NA)
    }, error = function(e) {
      return(NA)
    })
  }, USE.NAMES = FALSE)
}

# Function to compute Age Standardized Mortality Rate (ASMR)
compute_asmr <- function(mortality_data, pop_data, who_weights, site, sex_group) {
  if (site == "All cancers") {
    mort_df <- mortality_data %>% filter(siteiarc != "Other and unspecified (O&U)")
  } else {
    mort_df <- mortality_data %>% filter(siteiarc == site)
  }
  if (sex_group != "Both") {
    mort_df <- mort_df %>% filter(sex == sex_group)
  }
  if (nrow(mort_df) == 0) {
    return(data.frame(year = integer(), asmr = numeric()))
  }
  
  mort_df <- mort_df %>%
    mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), labels = 1:18, right = FALSE))) %>%
    filter(!is.na(age_group)) %>%
    group_by(year = dodyear, age_group) %>%
    summarise(counts = n(), .groups = 'drop')
  
  years <- unique(mortality_data$dodyear)
  age_groups <- 1:18
  full_df <- expand_grid(year = years, age_group = age_groups) %>%
    left_join(mort_df, by = c("year", "age_group")) %>%
    mutate(counts = coalesce(counts, 0))
  
  if (sex_group == "Both") {
    pop_df <- pop_data %>%
      group_by(year, age5) %>%
      summarise(pop = sum(pop_wpp), .groups = 'drop') %>%
      rename(age_group = age5)
  } else {
    pop_df <- pop_data %>%
      filter(sex == tolower(sex_group)) %>%
      dplyr::select(year, age_group = age5, pop = pop_wpp)
  }
  
  full_df <- full_df %>%
    left_join(pop_df, by = c("year", "age_group")) %>%
    mutate(pop = coalesce(pop, 0),
           age_rate = ifelse(pop > 0, counts / pop * 100000, 0)) %>%
    group_by(year) %>%
    summarise(asmr = sum(age_rate * who_weights[age_group]), .groups = 'drop')
  
  full_df
}

# Function to compute ASMR trends for top 5 fatal cancer sites
compute_top5_asmr_trends <- function(mortality_data, pop_data, who_weights) {
  # Get top 5 fatal cancer sites by total frequency (excluding O&U)
  top5_fatal_sites <- mortality_data %>%
    filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
    count(siteiarc) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    pull(siteiarc)
  
  # Compute ASMR for each of the top 5 fatal sites
  asmr_trends <- map_dfr(top5_fatal_sites, ~{
    site_data <- compute_asmr(mortality_data, pop_data, who_weights, .x, "Both")
    if(nrow(site_data) > 0) {
      site_data$cancer_site <- .x
      return(site_data)
    } else {
      return(NULL)
    }
  })
  
  return(asmr_trends)
}


# Function to create PowerPoint report with specified slide order
create_powerpoint_report <- function(data, mortality_data, pop_data, who_weights) {
  # Create new PowerPoint presentation
  ppt <- read_pptx()
  
  tryCatch({
    # SLIDE 1: Title slide
    ppt <- ppt %>%
      add_slide(layout = "Title Slide", master = "Office Theme") %>%
      ph_with(value = "Barbados National Cancer Registry", location = ph_location_type(type = "ctrTitle")) %>%
      ph_with(value = paste("Dashboard Report -", Sys.Date()), location = ph_location_type(type = "subTitle"))
    
    # SLIDE 2: Data Quality Indicators
    data_quality <- data %>%
      summarise(
        n = n(),
        mv_count = sum(grepl("Hx|Cytology|Lab|Haem", basis, ignore.case = TRUE), na.rm = TRUE),
        dco_count = sum(basis == "DCO", na.rm = TRUE),
        ill_def_count = sum(grepl("C76|C80|UNKNOWN", primarysite, ignore.case = TRUE) |
                              grepl("C76|C80", top, ignore.case = TRUE), na.rm = TRUE)
      ) %>%
      mutate(
        mv_prop = round(mv_count / n * 100, 1),
        dco_prop = round(dco_count / n * 100, 1),
        ill_def_prop = round(ill_def_count / n * 100, 1)
      )
    
    quality_text <- paste0(
      "• Microscopic Verification (MV%): ", data_quality$mv_prop, "%\n",
      "• Death Certificate Only (DCO%): ", data_quality$dco_prop, "%\n",
      "• Ill-Defined Sites%: ", data_quality$ill_def_prop, "%\n\n",
      "Data quality indicators show the reliability and completeness of cancer registry data."
    )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Data Quality Indicators", location = ph_location_type(type = "title")) %>%
      ph_with(value = quality_text, location = ph_location_type(type = "body"))
    
    # SLIDE 3: Key Statistics
    total_cases <- nrow(data)
    total_deaths <- nrow(mortality_data)
    avg_age <- round(mean(data$age, na.rm = TRUE), 1)
    avg_age_death <- round(mean(mortality_data$age, na.rm = TRUE), 1)
    
    summary_text <- paste0(
      "• Total Incidental Cases: ", format(total_cases, big.mark = ","), "\n",
      "• Total Deaths (2008-2024): ", format(total_deaths, big.mark = ","), "\n",
      "• Average Age at Diagnosis: ", avg_age, " years\n",
      "• Average Age at Death: ", avg_age_death, " years"
    )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Key Statistics", location = ph_location_type(type = "title")) %>%
      ph_with(value = summary_text, location = ph_location_type(type = "body"))
    
    # SLIDE 4: Age Distribution
    age_dist <- data %>%
      mutate(age_group = cut(age, 
                             breaks = c(0, 18, 65, Inf), 
                             labels = c("Pediatric (0-17)", "Adult (18-64)", "Elderly (65+)"), 
                             right = FALSE)) %>%
      filter(!is.na(age_group)) %>%
      count(age_group) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    
    age_plot <- ggplot(age_dist, aes(x = age_group, y = n, fill = age_group)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(n, " (", percentage, "%)")), vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("lightblue", "steelblue", "darkblue")) +
      theme_minimal() +
      labs(title = "Cases by Age Group", x = "Age Group", y = "Number of Cases") +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 10)
      )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Age Distribution", location = ph_location_type(type = "title")) %>%
      ph_with(value = dml(ggobj = age_plot), location = ph_location_type(type = "body"))
    
    # SLIDE 5: Trend in incident cancer cases
    cases_by_year <- data %>%
      group_by(dxyr) %>%
      summarise(cases = n(), .groups = 'drop')
    
    cases_trend_plot <- ggplot(cases_by_year, aes(x = dxyr, y = cases)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = cases), vjust = -0.5, size = 3) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      labs(title = "Trend in Cancer Cases by Year (2013-2022)", 
           x = "Year", y = "Number of Cases") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Trend in Incident Cancer Cases", location = ph_location_type(type = "title")) %>%
      ph_with(value = dml(ggobj = cases_trend_plot), location = ph_location_type(type = "body"))
    
    # SLIDE 6: Top 10 Cancer Sites
    top_sites <- data %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10)
    
    top_sites_plot <- ggplot(top_sites, aes(x = reorder(siteiarc, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = n), hjust = -0.2, size = 3) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 10 Cancer Sites (2013-2022)", 
           x = "Cancer Site", y = "Number of Cases") +
      theme(
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 8)
      )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Top 10 Cancer Sites", location = ph_location_type(type = "title")) %>%
      ph_with(value = dml(ggobj = top_sites_plot), location = ph_location_type(type = "body"))
    
    # SLIDE 7: Age Standardised Incidence Rate Trends by Sex
    tryCatch({
      # Compute ASIR for both sexes
      asr_male <- compute_asir(data, pop_data, who_weights, "All cancers", "Male")
      asr_female <- compute_asir(data, pop_data, who_weights, "All cancers", "Female")
      
      if(nrow(asr_male) > 0 && nrow(asr_female) > 0) {
        # Combine data for plotting
        asr_combined <- bind_rows(
          asr_male %>% mutate(sex = "Male"),
          asr_female %>% mutate(sex = "Female")
        )
        
        # Create ASR trend plot by sex
        asr_sex_plot <- ggplot(asr_combined, aes(x = year, y = asir, color = sex)) +
          geom_line(size = 1.5, alpha = 0.8) +
          geom_point(size = 3, alpha = 0.9) +
          scale_color_manual(values = c("Male" = "#3182BD", "Female" = "#DD1C77")) +
          scale_x_continuous(breaks = seq(min(asr_combined$year), max(asr_combined$year), by = 1)) +
          theme_minimal() +
          labs(
            title = "Age Standardised Incidence Rate Trends by Sex\n(All Cancers, 2013-2022)",
            x = "Year", 
            y = "ASIR per 100,000",
            color = "Sex"
          ) +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          )
        
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "Age Standardised Incidence Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
          ph_with(value = dml(ggobj = asr_sex_plot), location = ph_location_type(type = "body"))
        
      } else {
        # Fallback if no ASR data available
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "Age Standardised Incidence Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
          ph_with(value = "Insufficient data available for ASIR trend analysis by sex.", location = ph_location_type(type = "body"))
      }
      
    }, error = function(e) {
      warning(paste("Error creating ASIR by sex slide:", e$message))
      # Add error slide
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "Age Standardised Incidence Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
        ph_with(value = "Error generating ASIR trends by sex.", location = ph_location_type(type = "body"))
    })
    
    # SLIDE 8: Age Standardised Incidence Rate Trends for Top 5 Cancer Sites
    tryCatch({
      # Compute ASR trends for top 5 cancer sites
      asr_top5_trends <- compute_top5_asr_trends(data, pop_data, who_weights)
      
      if(nrow(asr_top5_trends) > 0) {
        # Create ASR trend plot for top 5 sites
        asr_top5_plot <- ggplot(asr_top5_trends, aes(x = year, y = asir, color = cancer_site)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 2.5, alpha = 0.9) +
          scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")[1:length(unique(asr_top5_trends$cancer_site))]) +
          scale_x_continuous(breaks = seq(min(asr_top5_trends$year), max(asr_top5_trends$year), by = 1)) +
          theme_minimal() +
          labs(
            title = "Age Standardised Incidence Rate Trends\nTop 5 Cancer Sites (2013-2022)",
            x = "Year", 
            y = "ASIR per 100,000",
            color = "Cancer Site"
          ) +
          theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 9),
            axis.title = element_text(size = 10),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 8),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          ) +
          guides(color = guide_legend(
            title = "Cancer Site",
            override.aes = list(size = 3),
            ncol = 1
          ))
        
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "ASIR Trends - Top 5 Cancer Sites", location = ph_location_type(type = "title")) %>%
          ph_with(value = dml(ggobj = asr_top5_plot), location = ph_location_type(type = "body"))
        
      } else {
        # Fallback if no data available
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "ASIR Trends - Top 5 Cancer Sites", location = ph_location_type(type = "title")) %>%
          ph_with(value = "Insufficient data available for top 5 cancer sites ASIR trend analysis.", location = ph_location_type(type = "body"))
      }
      
    }, error = function(e) {
      warning(paste("Error creating top 5 ASIR trends slide:", e$message))
      # Add error slide
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "ASIR Trends - Top 5 Cancer Sites", location = ph_location_type(type = "title")) %>%
        ph_with(value = "Error generating ASIR trends for top 5 cancer sites.", location = ph_location_type(type = "body"))
    })
    
    # SLIDE 9: Trend in cancer deaths
    deaths_by_year <- mortality_data %>%
      group_by(dodyear) %>%
      summarise(deaths = n(), .groups = 'drop')
    
    deaths_trend_plot <- ggplot(deaths_by_year, aes(x = dodyear, y = deaths)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = deaths), vjust = -0.5, size = 3) +
      scale_x_continuous(breaks = seq(min(mortality_data$dodyear), max(mortality_data$dodyear), by = 2)) +
      theme_minimal() +
      labs(title = "Trend in Cancer Deaths by Year (2008-2024)", 
           x = "Year", y = "Number of Deaths") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(size = 12, face = "bold")
      )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Trend in Cancer Deaths", location = ph_location_type(type = "title")) %>%
      ph_with(value = dml(ggobj = deaths_trend_plot), location = ph_location_type(type = "body"))
    
    
    # NEW SLIDE 10: Age Standardised Mortality Rate Trends by Sex
    tryCatch({
      # Compute ASMR for both sexes
      asmr_male <- compute_asmr(mortality_data, pop_data, who_weights, "All cancers", "Male")
      asmr_female <- compute_asmr(mortality_data, pop_data, who_weights, "All cancers", "Female")
      
      if(nrow(asmr_male) > 0 && nrow(asmr_female) > 0) {
        # Combine data for plotting
        asmr_combined <- bind_rows(
          asmr_male %>% mutate(sex = "Male"),
          asmr_female %>% mutate(sex = "Female")
        )
        
        # Create ASMR trend plot by sex
        asmr_sex_plot <- ggplot(asmr_combined, aes(x = year, y = asmr, color = sex)) +
          geom_line(size = 1.5, alpha = 0.8) +
          geom_point(size = 3, alpha = 0.9) +
          scale_color_manual(values = c("Male" = "#3182BD", "Female" = "#DD1C77")) +
          scale_x_continuous(breaks = seq(min(asmr_combined$year), max(asmr_combined$year), by = 2)) +
          theme_minimal() +
          labs(
            title = "Age Standardised Mortality Rate Trends by Sex\n(All Cancers, 2008-2024)",
            x = "Year", 
            y = "ASMR per 100,000",
            color = "Sex"
          ) +
          theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 11),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          )
        
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "Age Standardised Mortality Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
          ph_with(value = dml(ggobj = asmr_sex_plot), location = ph_location_type(type = "body"))
        
      } else {
        # Fallback if no ASMR data available
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "Age Standardised Mortality Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
          ph_with(value = "Insufficient data available for ASMR trend analysis by sex.", location = ph_location_type(type = "body"))
      }
      
    }, error = function(e) {
      warning(paste("Error creating ASMR by sex slide:", e$message))
      # Add error slide
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "Age Standardised Mortality Rate Trends by Sex", location = ph_location_type(type = "title")) %>%
        ph_with(value = "Error generating ASMR trends by sex.", location = ph_location_type(type = "body"))
    })
    
    # SLIDE 11: Top 10 Mortality Sites
    top_deaths <- mortality_data %>%
      filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10)
    
    if(nrow(top_deaths) > 0) {
      death_plot <- ggplot(top_deaths, aes(x = reorder(siteiarc, n), y = n)) +
        geom_bar(stat = "identity", fill = "darkred") +
        geom_text(aes(label = n), hjust = -0.2, size = 3) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Top 10 Cancer Deaths (2008-2024)", 
             x = "Cancer Site", y = "Number of Deaths") +
        theme(
          axis.text = element_text(size = 9),
          plot.title = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 8)
        )
      
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "Top 10 Mortality Sites", location = ph_location_type(type = "title")) %>%
        ph_with(value = dml(ggobj = death_plot), location = ph_location_type(type = "body"))
    }
    
    # SLIDE 12: Age Standardised Mortality Rate Trends for Top 5 Fatal Cancer Sites
    tryCatch({
      # Compute ASMR trends for top 5 fatal cancer sites
      asmr_top5_trends <- compute_top5_asmr_trends(mortality_data, pop_data, who_weights)
      
      if(nrow(asmr_top5_trends) > 0) {
        # Create ASMR trend plot for top 5 fatal sites
        asmr_top5_plot <- ggplot(asmr_top5_trends, aes(x = year, y = asmr, color = cancer_site)) +
          geom_line(size = 1.2, alpha = 0.8) +
          geom_point(size = 2.5, alpha = 0.9) +
          scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")[1:length(unique(asmr_top5_trends$cancer_site))]) +
          scale_x_continuous(breaks = seq(min(asmr_top5_trends$year), max(asmr_top5_trends$year), by = 2)) +
          theme_minimal() +
          labs(
            title = "Age Standardised Mortality Rate Trends\nTop 5 Fatal Cancer Sites (2008-2024)",
            x = "Year", 
            y = "ASMR per 100,000",
            color = "Cancer Site"
          ) +
          theme(
            plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
            axis.text = element_text(size = 9),
            axis.title = element_text(size = 10),
            legend.title = element_text(size = 10, face = "bold"),
            legend.text = element_text(size = 8),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid.minor = element_blank()
          ) +
          guides(color = guide_legend(
            title = "Cancer Site",
            override.aes = list(size = 3),
            ncol = 1
          ))
        
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "ASMR Trends - Top 5 Fatal Cancer Sites", location = ph_location_type(type = "title")) %>%
          ph_with(value = dml(ggobj = asmr_top5_plot), location = ph_location_type(type = "body"))
        
      } else {
        # Fallback if no data available
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "ASMR Trends - Top 5 Fatal Cancer Sites", location = ph_location_type(type = "title")) %>%
          ph_with(value = "Insufficient data available for top 5 fatal cancer sites ASMR trend analysis.", location = ph_location_type(type = "body"))
      }
      
    }, error = function(e) {
      warning(paste("Error creating top 5 ASMR trends slide:", e$message))
      # Add error slide
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "ASMR Trends - Top 5 Fatal Cancer Sites", location = ph_location_type(type = "title")) %>%
        ph_with(value = "Error generating ASMR trends for top 5 fatal cancer sites.", location = ph_location_type(type = "body"))
    })
    
    # SLIDE 13: Kaplan-Meier Survival Curves for Top 5 Cancer Sites
    tryCatch({
      # Get top 5 cancer sites by frequency (excluding O&U)
      top5_sites <- data %>%
        filter(siteiarc != "Other and unspecified (O&U)") %>%
        count(siteiarc) %>%
        arrange(desc(n)) %>%
        head(5) %>%
        pull(siteiarc)
      
      # Prepare survival data for top 5 sites
      surv_data <- data %>%
        filter(siteiarc %in% top5_sites) %>%
        mutate(
          dx_date = as.Date(parse_incidence(IncidenceDate)),
          end_date = if_else(deceased == "dead", as.Date(dmy(dod), quiet = TRUE), as.Date(dmy(dlc), quiet = TRUE)),
          event = if_else(deceased == "dead", 1, 0),
          time_days = as.numeric(difftime(end_date, dx_date, units = "days"))
        ) %>%
        filter(!is.na(dx_date) & !is.na(end_date) & !is.na(time_days) & time_days >= 0) %>%
        # Convert time to years for better readability
        mutate(time_years = time_days / 365.25)
      
      if(nrow(surv_data) > 0) {
        # Create survival curves for each cancer site
        surv_curves_list <- list()
        
        for(site in top5_sites) {
          site_data <- surv_data %>% filter(siteiarc == site)
          if(nrow(site_data) >= 5) {  # Need at least 5 cases for meaningful survival analysis
            surv_fit_site <- survfit(Surv(time_years, event) ~ 1, data = site_data)
            
            # Extract survival data for plotting
            surv_df <- data.frame(
              time = c(0, surv_fit_site$time),
              surv = c(1, surv_fit_site$surv),
              siteiarc = site
            ) %>%
              filter(time <= 10)  # Limit to 10 years
            
            surv_curves_list[[site]] <- surv_df
          }
        }
        
        # Combine all survival curves
        if(length(surv_curves_list) > 0) {
          all_surv_data <- do.call(rbind, surv_curves_list)
          
          # Create Kaplan-Meier plot
          km_plot <- ggplot(all_surv_data, aes(x = time, y = surv, color = siteiarc)) +
            geom_step(size = 1.2, alpha = 0.8) +
            scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")[1:length(unique(all_surv_data$siteiarc))]) +
            scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
            scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10)) +
            theme_minimal() +
            labs(
              title = "Kaplan-Meier Survival Curves\nTop 5 Cancer Sites (2013-2022)",
              x = "Time (Years)", 
              y = "Survival Probability",
              color = "Cancer Site"
            ) +
            theme(
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 9),
              axis.title = element_text(size = 10),
              legend.title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 8),
              legend.position = "bottom",
              legend.key.width = unit(1.5, "cm"),
              panel.grid.minor = element_blank()
            ) +
            guides(color = guide_legend(
              title = "Cancer Site",
              override.aes = list(size = 3),
              ncol = 1
            ))
          
          ppt <- ppt %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with(value = "Kaplan-Meier Survival Curves", location = ph_location_type(type = "title")) %>%
            ph_with(value = dml(ggobj = km_plot), location = ph_location_type(type = "body"))
          
        } else {
          # No sites have enough data for survival analysis
          ppt <- ppt %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with(value = "Kaplan-Meier Survival Curves", location = ph_location_type(type = "title")) %>%
            ph_with(value = "Insufficient cases (minimum 5 per site) for reliable Kaplan-Meier analysis.", location = ph_location_type(type = "body"))
        }
        
      } else {
        # Fallback if no survival data available
        ppt <- ppt %>%
          add_slide(layout = "Title and Content", master = "Office Theme") %>%
          ph_with(value = "Kaplan-Meier Survival Curves", location = ph_location_type(type = "title")) %>%
          ph_with(value = "Insufficient survival data available for Kaplan-Meier analysis.", location = ph_location_type(type = "body"))
      }
      
    }, error = function(e) {
      warning(paste("Error creating Kaplan-Meier slide:", e$message))
      # Add error slide
      ppt <- ppt %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = "Kaplan-Meier Survival Curves", location = ph_location_type(type = "title")) %>%
        ph_with(value = "Error generating Kaplan-Meier survival curves.", location = ph_location_type(type = "body"))
    })
    
    # SLIDE 14: Contact Information
    contact_text <- paste0(
      "The Barbados National Registry (BNR)\n",
      "The George Alleyne Chronic Disease Research Centre\n",
      "UWI Avalon, Jemmotts Lane\n",
      "Bridgetown, Barbados, W.I.\n\n",
      "Tel: 246-426-6416\n",
      "Fax: 246-426-8406\n",
      "Email: bnr.uwi.edu"
    )
    
    ppt <- ppt %>%
      add_slide(layout = "Title and Content", master = "Office Theme") %>%
      ph_with(value = "Contact Information", location = ph_location_type(type = "title")) %>%
      ph_with(value = contact_text, location = ph_location_type(type = "body"))
    
  }, error = function(e) {
    warning(paste("Error creating PowerPoint:", e$message))
    # Create a minimal presentation if there are errors
    ppt <- read_pptx() %>%
      add_slide(layout = "Title Slide", master = "Office Theme") %>%
      ph_with(value = "BNR Cancer Registry Report", location = ph_location_type(type = "ctrTitle")) %>%
      ph_with(value = "Report generation encountered an error", location = ph_location_type(type = "subTitle"))
  })
  
  return(ppt)
}

# Preprocess data if needed (e.g., convert dates, etc.)
# Assuming dxyr is integer year, siteiarc is character

# --- FULL CORRECTED UI SECTION ---

ui <- dashboardPage(
  title = "BNR Cancer Registry Dashboard",
  dashboardHeader(
    title = div(
      img(src = "bnr_logo.png", height = 40, style = "margin-right: 10px; vertical-align: middle;"),
      span(style = "font-size: 30px; font-weight: bold;", "BNR Cancer Registry Dashboard")
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    id = "",
    div(
      id = "sidebar_content",
      sidebarMenu(id = "tabs",
                  menuItem("Home", tabName = "home_landing", icon = icon("home")),
                  menuItem("About", tabName = "about", icon = icon("question-circle")),
                  menuItem("Modules", icon = icon("th"), startExpanded = FALSE,
                           menuSubItem("Overview",     tabName = "home",         icon = icon("tachometer-alt")),
                           menuSubItem("Incidence",    tabName = "incidence",    icon = icon("chart-bar")),
                           menuSubItem("Mortality",    tabName = "mortality",    icon = icon("skull-crossbones")),
                           menuSubItem("Survival",     tabName = "survival",     icon = icon("heartbeat")),
                           menuSubItem("Prevalence",   tabName = "prevalence",   icon = icon("user-check")),
                           menuSubItem("Projection",   tabName = "projection",   icon = icon("chart-line")),
                           menuSubItem("Data Quality", tabName = "data_quality", icon = icon("check-circle")),
                           menuSubItem("Reports",      tabName = "reports",      icon = icon("file-alt"))
                  ),
                  menuItem("Additional Information", tabName = "additional", icon = icon("info-circle")),
                  menuItem("Contact Us",             tabName = "contact",    icon = icon("envelope"))
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "bnr_logo.png"),
      tags$style(HTML("
  .female-table .dataTable th { background-color: #FFC1CC !important; }
  .male-table .dataTable th { background-color: #ADD8E6 !important; }
  .both-table .dataTable th { background-color: #90EE90 !important; }
  .male-surv-table .dataTable th { background-color: #ADD8E6 !important; }
  .female-surv-table .dataTable th { background-color: #FFC1CC !important; }
  .main-header .logo { width: 100% !important; text-align: center; padding-left: 10px; background-color: #253494 !important; color: #FFFFFF !important; }
  .collaborator-logo { display: block; margin: 10px auto; height: 100px; width: auto; object-fit: contain; }
  .collaborator-text { text-align: left; font-size: 16px; font-weight: bold; }
  .login-container { max-width: 900px; margin: 50px auto; padding: 20px; border: 1px solid #ccc; border-radius: 5px; }
  
  /* Hide sidebar initially */
  body.not-authenticated .main-sidebar { display: none !important; }
  body.not-authenticated .content-wrapper { margin-left: 0 !important; }
  body.not-authenticated .main-header { margin-left: 0 !important; }

  /* Increase font size of Modules dropdown sub-items */
  .sidebar-menu .treeview-menu > li > a {
    font-size: 15px !important;
    padding-top: 8px !important;
    padding-bottom: 8px !important;
  }
  .sidebar-menu .treeview-menu > li > a > .fa,
  .sidebar-menu .treeview-menu > li > a > .glyphicon,
  .sidebar-menu .treeview-menu > li > a > svg {
    font-size: 15px !important;
  }
"))
    ),
    div(id = "loginpage",
        class = "login-container",
        div(id = "login_text", 
            style = "font-size: 1.3em;", 
            div(style = "text-align: center;", h1(tags$strong("Welcome to the Barbados National Cancer Registry Data Dashboard."))),
            p("This secure portal provides authorized users with access to comprehensive cancer registry data for the nation of Barbados. The dashboard is a vital tool for public health professionals, researchers, and policymakers, enabling data-driven insights to improve cancer prevention, treatment, and control efforts."),
            p("Please log in using your credentials to access the full range of data, reports, and analytical tools."),
            p(tags$strong("Forgot your password?"), " Please contact your system administrator for assistance.")
        ),
        shinyauthr::loginUI(
          id = "login_form",
          title = "BNR Cancer Registry Dashboard Login",
          user_title = "Username",
          pass_title = "Password",
          login_title = "Log in",
          error_message = "Invalid username or password"
        )
    ),
    shinyjs::hidden(
      div(id = "dashboard_content",
          tabItems(
            
            # --- HOME / LANDING PAGE ---
            tabItem(tabName = "home_landing",
                    tags$head(tags$style(HTML("
                      .module-card {
                        background: #ffffff;
                        border-radius: 10px;
                        padding: 24px 20px;
                        text-align: center;
                        cursor: pointer;
                        transition: transform 0.2s, box-shadow 0.2s;
                        border-top: 5px solid #253494;
                        height: 100%;
                        box-shadow: 0 2px 8px rgba(0,0,0,0.10);
                      }
                      .module-card:hover {
                        transform: translateY(-4px);
                        box-shadow: 0 8px 24px rgba(37,52,148,0.18);
                      }
                      .module-card .card-icon {
                        font-size: 38px;
                        margin-bottom: 12px;
                      }
                      .module-card h4 {
                        font-weight: bold;
                        color: #253494;
                        margin-bottom: 8px;
                        font-size: 17px;
                      }
                      .module-card p {
                        font-size: 13px;
                        color: #555;
                        margin: 0;
                      }
                      .bnr-hero {
                        background: linear-gradient(135deg, #253494 0%, #1a6fc4 100%);
                        border-radius: 12px;
                        padding: 36px 30px 28px 30px;
                        color: #ffffff;
                        margin-bottom: 28px;
                      }
                      .bnr-hero h1 { font-size: 28px; font-weight: bold; margin-bottom: 10px; }
                      .bnr-hero p  { font-size: 15px; opacity: 0.92; margin-bottom: 0; }
                      .module-row { margin-bottom: 20px; }
                    "))),
                    fluidRow(
                      column(12,
                             div(class = "bnr-hero",
                                 h1(icon("ribbon"), "  Barbados National Cancer Registry"),
                                 p("The BNR Cancer Registry Dashboard is a secure, web-based platform providing authorised users with
                            comprehensive cancer surveillance data for Barbados. It supports evidence-based decision-making
                            for public health professionals, researchers, and policymakers."),
                                 br(),
                                 p(tags$strong("The dashboard contains 8 modules:"),
                                   " Overview · Incidence · Mortality · Survival · Prevalence · Projections · Data Quality · Reports."),
                                 p("Select any module below to navigate directly to it.")
                             )
                      )
                    ),
                    # Row 1: Overview, Incidence, Mortality, Survival
                    fluidRow(
                      class = "module-row",
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'home', {priority: 'event'})",
                                 div(class = "card-icon", icon("tachometer-alt", style = "color:#253494;")),
                                 h4("Overview"),
                                 p("Key statistics, case trends, top cancer sites and geographic distribution for Barbados.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'incidence', {priority: 'event'})",
                                 div(class = "card-icon", icon("chart-bar", style = "color:#1a6fc4;")),
                                 h4("Incidence"),
                                 p("New cancer cases by year, site and sex. Includes crude rates, ASIR and cumulative incidence.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'mortality', {priority: 'event'})",
                                 div(class = "card-icon", icon("skull-crossbones", style = "color:#c0392b;")),
                                 h4("Mortality"),
                                 p("Cancer deaths from 2008–2024. Explore crude and age-standardised mortality rates by parish.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'survival', {priority: 'event'})",
                                 div(class = "card-icon", icon("heartbeat", style = "color:#27ae60;")),
                                 h4("Survival"),
                                 p("Kaplan-Meier survival curves and 1-, 3-, and 5-year survival probabilities by site and sex.")
                             )
                      )
                    ),
                    # Row 2: Prevalence, Projection, Data Quality, Reports
                    fluidRow(
                      class = "module-row",
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'prevalence', {priority: 'event'})",
                                 div(class = "card-icon", icon("user-check", style = "color:#e67e22;")),
                                 h4("Prevalence"),
                                 p("Number and proportion of cancer survivors living in Barbados as of 31 December 2022.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'projection', {priority: 'event'})",
                                 div(class = "card-icon", icon("chart-line", style = "color:#8e44ad;")),
                                 h4("Projections"),
                                 p("Statistical forecasts of future cancer incidence and mortality based on historical trends.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'data_quality', {priority: 'event'})",
                                 div(class = "card-icon", icon("check-circle", style = "color:#16a085;")),
                                 h4("Data Quality"),
                                 p("Microscopic verification, death certificate only, and ill-defined site indicators.")
                             )
                      ),
                      column(3,
                             div(class = "module-card", onclick = "Shiny.setInputValue('home_nav', 'reports', {priority: 'event'})",
                                 div(class = "card-icon", icon("file-alt", style = "color:#2980b9;")),
                                 h4("Reports"),
                                 p("Generate and download a comprehensive PowerPoint report summarising all key findings.")
                             )
                      )
                    ),
                    fluidRow(
                      column(12,
                             hr(),
                             p(style = "font-size: 12px; color: #888; text-align: center;",
                               icon("info-circle"),
                               " Data covers cancer incidence (2013–2022) and mortality (2008–2024) for Barbados. ",
                               "Population estimates are sourced from the UN World Population Prospects (WPP). ",
                               "For access or data enquiries, please contact the BNR via the Contact Us section."
                             )
                      )
                    )
            ),
            
            # --- OVERVIEW PAGE (formerly Home) ---
            tabItem(tabName = "home",
                    fluidRow(
                      valueBoxOutput("total_cases", width = 6),
                      valueBoxOutput("home_total_deaths", width = 6)
                    ),
                    fluidRow(
                      valueBoxOutput("avg_age", width = 6),
                      valueBoxOutput("avg_age_death", width = 6)
                    ),
                    fluidRow(
                      valueBoxOutput("pediatric_cases", width = 3),
                      valueBoxOutput("elderly_cases", width = 3),
                      valueBoxOutput("pediatric_deaths", width = 3),
                      valueBoxOutput("elderly_deaths", width = 3)
                    ),
                    fluidRow(
                      box(title = "Cases Over Years (2013-2022)", plotOutput("cases_over_years"), width = 6),
                      box(title = "Top Cancer Sites (2013-2022)", DT::dataTableOutput("top_sites"), width = 6)
                    ),
                    fluidRow(
                      box(title = "Top 5 Pediatric Cancer Sites", DT::dataTableOutput("top5_pediatric_sites"), width = 6),
                      box(title = "Top 5 Elderly Cancer Sites", DT::dataTableOutput("top5_elderly_sites"), width = 6)
                    ),
                    fluidRow(
                      box(title = "Top 10 Cancer Deaths", DT::dataTableOutput("top10_deaths_both_home"), width = 6),
                      box(title = "Top 10 Elderly Deaths", DT::dataTableOutput("top10_deaths_elderly_home"), width = 6)
                    ),
                    fluidRow(
                      box(title = "Cases by Parish (2013-2022)", plotOutput("cases_by_parish"), width = 12)
                    ),
                    fluidRow(
                      box(
                        title = "Top 5 Cancers by Parish - Number of Cases",
                        width = 12, status = "primary", solidHeader = TRUE,
                        selectInput("home_parish_cancer", "Select Cancer:", choices = NULL),
                        leafletOutput("home_parish_map", height = 500)
                      )
                    )
            ),
            
            # --- ABOUT PAGE ---
            tabItem(tabName = "about",
                    fluidRow(
                      column(12,
                             h2(icon("question-circle"), " About This Dashboard",
                                style = "color: #253494; font-weight: bold; margin-bottom: 20px;"),
                             p(style = "font-size: 16px; color: #555;",
                               "Welcome to the ", tags$strong("Barbados National Cancer Registry (BNR) Data Dashboard."),
                               " This guide explains each module and how to use them effectively.")
                      )
                    ),
                    fluidRow(
                      # Incidence
                      box(
                        title = tagList(icon("chart-bar"), " Incidence"),
                        status = "primary", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " The number of new cancer cases diagnosed in Barbados over time."),
                        tags$ul(
                          tags$li(tags$strong("Frequency:"), " View raw counts of new cases by year and cancer site. Use the dropdowns to filter by year or specific site."),
                          tags$li(tags$strong("Crude Incidence Rate:"), " The rate of new cases per 100,000 population without age adjustment."),
                          tags$li(tags$strong("ASIR (Age-Standardised Incidence Rate):"), " Incidence rates adjusted for age using the WHO 2000 standard population, allowing fair comparisons across years and populations."),
                          tags$li(tags$strong("Cumulative Incidence:"), " The probability (%) of developing cancer between birth and age 74.")
                        ),
                        p(tags$em("Tip:"), " Use the ", tags$strong("Select Metric"), " radio buttons at the top to switch between views.")
                      ),
                      # Mortality
                      box(
                        title = tagList(icon("skull-crossbones"), " Mortality"),
                        status = "danger", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " Cancer-related deaths recorded in Barbados from 2008 to 2024."),
                        tags$ul(
                          tags$li(tags$strong("Frequency:"), " Counts of cancer deaths by year, sex, and site."),
                          tags$li(tags$strong("Crude Mortality Rate:"), " Deaths per 100,000 population without age adjustment."),
                          tags$li(tags$strong("ASMR (Age-Standardised Mortality Rate):"), " Mortality rates adjusted for age using the WHO standard, enabling trend comparisons over time."),
                          tags$li(tags$strong("Geographic View:"), " An interactive map showing mortality rates by parish.")
                        ),
                        p(tags$em("Tip:"), " Filter by sex and cancer site using the controls provided in each sub-section.")
                      )
                    ),
                    fluidRow(
                      # Survival
                      box(
                        title = tagList(icon("heartbeat"), " Survival"),
                        status = "success", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " The probability that a patient survives after a cancer diagnosis, over time."),
                        tags$ul(
                          tags$li(tags$strong("Kaplan-Meier Curves:"), " Visual survival curves showing the proportion of patients alive at each point in time after diagnosis."),
                          tags$li(tags$strong("1-, 3-, and 5-Year Survival Gauges:"), " At-a-glance survival probability indicators at key time points."),
                          tags$li(tags$strong("Age Band Analysis:"), " Survival broken down by age group to identify high-risk populations.")
                        ),
                        p(tags$em("Tip:"), " Select a cancer site and sex using the filters to explore survival for specific subgroups.")
                      ),
                      # Prevalence
                      box(
                        title = tagList(icon("user-check"), " Prevalence"),
                        status = "warning", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " The number and proportion of people living with cancer (survivors) as of December 31, 2022."),
                        tags$ul(
                          tags$li(tags$strong("Survivors Count:"), " Total number of cancer survivors alive at the prevalence date."),
                          tags$li(tags$strong("Prevalence Rate (%):"), " The percentage of the population that are cancer survivors."),
                          tags$li(tags$strong("Age & Sex Breakdown:"), " A chart showing the distribution of survivors by 5-year age groups and sex."),
                          tags$li(tags$strong("Top Prevalent Cancers:"), " Tables listing the most common cancers among survivors, by sex.")
                        ),
                        p(tags$em("Tip:"), " Use the cancer site selector to focus on a specific cancer type.")
                      )
                    ),
                    fluidRow(
                      # Projection
                      box(
                        title = tagList(icon("chart-line"), " Projection"),
                        status = "info", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " Statistical forecasts of future cancer incidence and mortality trends."),
                        tags$ul(
                          tags$li("Projections are generated using historical registry data and population growth estimates."),
                          tags$li("Useful for planning healthcare resources and public health interventions."),
                          tags$li("Forecasts are presented with confidence intervals to communicate uncertainty.")
                        ),
                        p(tags$em("Tip:"), " Projections are most reliable for common cancer sites with longer data histories.")
                      ),
                      # Data Quality
                      box(
                        title = tagList(icon("check-circle"), " Data Quality"),
                        status = "primary", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " Indicators that reflect the completeness and reliability of the registry data."),
                        tags$ul(
                          tags$li(tags$strong("MV% (Microscopic Verification):"), " Percentage of cases confirmed by histology, cytology, or laboratory tests. Higher values indicate more reliable diagnoses."),
                          tags$li(tags$strong("DCO% (Death Certificate Only):"), " Cases identified only from a death certificate, with no prior diagnosis record. Lower values are preferred."),
                          tags$li(tags$strong("Ill-Defined Sites (%):"), " Cases recorded with vague or unspecified primary sites. Lower values indicate better coding quality.")
                        ),
                        p(tags$em("Tip:"), " Use this tab routinely to monitor data quality trends over time and identify areas needing improvement.")
                      )
                    ),
                    fluidRow(
                      # Reports
                      box(
                        title = tagList(icon("file-alt"), " Reports"),
                        status = "success", solidHeader = TRUE, width = 6,
                        p(tags$strong("What it shows:"), " Automated report generation for sharing or offline analysis."),
                        tags$ul(
                          tags$li("Generate a comprehensive ", tags$strong("PowerPoint report"), " summarising key statistics, charts, and data quality indicators."),
                          tags$li("The report covers incidence trends, top cancer sites, age-standardised rates, survival curves, and contact information."),
                          tags$li("Reports are generated dynamically using the latest data loaded into the dashboard.")
                        ),
                        p(tags$em("Tip:"), " Click the download button to save the report directly to your device.")
                      ),
                      # General Tips
                      box(
                        title = tagList(icon("lightbulb"), " General Tips"),
                        status = "warning", solidHeader = TRUE, width = 6,
                        tags$ul(
                          tags$li("All modules require you to be logged in. Contact your system administrator if you need access credentials."),
                          tags$li("Filters and selectors within each module update charts and tables in real time."),
                          tags$li("Hover over charts for interactive tooltips showing exact values."),
                          tags$li("Tables support sorting by clicking on column headers."),
                          tags$li("For questions about the data or methodology, refer to the ", tags$strong("Contact Us"), " section.")
                        ),
                        hr(),
                        p(style = "font-size: 13px; color: #777;",
                          icon("info-circle"), " Data covers cancer incidence (2013–2022) and mortality (2008–2024) for Barbados. Population estimates are sourced from the UN World Population Prospects (WPP).")
                      )
                    )
            ),
            
            # --- INCIDENCE PAGE ---
            tabItem(tabName = "incidence",
                    fluidRow(
                      column(12, radioButtons("metric", "Select Metric:", choices = c("Frequency", "Crude Incidence", "ASIR", "Cumulative Incidence"), inline = TRUE))
                    ),
                    conditionalPanel(
                      condition = "input.metric == 'Frequency'",
                      fluidRow(
                        column(4, selectInput("year_select", "Select Year:", choices = c("All", sort(unique(data$dxyr))), selected = "All")),
                        column(4, selectInput("site_select", "Select Cancer Site:", choices = c("All", sort(unique(data$siteiarc))), selected = "All"))
                      ),
                      fluidRow(valueBoxOutput("num_cases", 4), valueBoxOutput("num_female_cases", 4), valueBoxOutput("num_male_cases", 4)),
                      fluidRow(box(title = "Cases by Year", plotOutput("bar_graph"), 6), box(title = "Cases by Sex", plotOutput("sex_bar_graph"), 6)),
                      fluidRow(
                        box(title = "Top 10 Incidental Cancers", DT::dataTableOutput("top10_table"), 6),
                        box(title = "Top 5 Incidental Cancers by Sex", div(class = "female-table", h4("FEMALES"), DT::dataTableOutput("top5_female_table")), div(class = "male-table", h4("MALES"), DT::dataTableOutput("top5_male_table")), 6)
                      ),
                      fluidRow(box(title = "Cases by 5-Year Age Bands", plotOutput("cases_by_age_bands"), width = 12))
                    ),
                    conditionalPanel(
                      condition = "input.metric == 'Crude Incidence'",
                      fluidRow(column(6, selectInput("crude_site_select", "Select Cancer Site:", choices = NULL)), column(6, checkboxGroupInput("crude_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_crude_both", 4), valueBoxOutput("avg_crude_female", 4), valueBoxOutput("avg_crude_male", 4)),
                      fluidRow(box(title = "Crude Incidence Rate Trend", plotOutput("crude_line_graph"), 12))
                    ),
                    conditionalPanel(
                      condition = "input.metric == 'ASIR'",
                      fluidRow(column(6, selectInput("asir_site_select", "Select Cancer Site:", NULL)), column(6, checkboxGroupInput("asir_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_asir_both", 4), valueBoxOutput("avg_asir_female", 4), valueBoxOutput("avg_asir_male", 4)),
                      fluidRow(box(title = "ASIR Trend", plotOutput("asir_line_graph"), 12))
                    ),
                    conditionalPanel(
                      condition = "input.metric == 'Cumulative Incidence'",
                      fluidRow(column(6, selectInput("cum_site_select", "Select Cancer Site:", NULL)), column(6, checkboxGroupInput("cum_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_cum_both", 4), valueBoxOutput("avg_cum_female", 4), valueBoxOutput("avg_cum_male", 4)),
                      fluidRow(box(title = "Cumulative Incidence Trend", plotOutput("cum_line_graph"), 12))
                    ),
                    tabBox(
                      title = "Parish-Level Incidence Maps", width = 12,
                      tabPanel("Both Sexes", fluidRow(column(6, h4("All Cancers - ASIR"), leafletOutput("inc_parish_map_both_all")), column(6, h4("Top 5 Cancers - ASIR"), selectInput("inc_parish_cancer_both", "Select Cancer:", NULL), leafletOutput("inc_parish_map_both_top5")))),
                      tabPanel("Males", fluidRow(column(6, h4("All Cancers - ASIR (Males)"), leafletOutput("inc_parish_map_male_all")), column(6, h4("Top 5 Cancers - ASIR (Males)"), selectInput("inc_parish_cancer_male", "Select Cancer:", NULL), leafletOutput("inc_parish_map_male_top5")))),
                      tabPanel("Females", fluidRow(column(6, h4("All Cancers - ASIR (Females)"), leafletOutput("inc_parish_map_female_all")), column(6, h4("Top 5 Cancers - ASIR (Females)"), selectInput("inc_parish_cancer_female", "Select Cancer:", NULL), leafletOutput("inc_parish_map_female_top5"))))
                    )
            ),
            
            # --- MORTALITY PAGE ---
            tabItem(tabName = "mortality",
                    fluidRow(
                      column(12, radioButtons("mort_metric", "Select Metric:", choices = c("Frequency", "Crude Mortality", "ASMR", "Cumulative Mortality"), inline = TRUE))
                    ),
                    
                    # 1. Cumulative Mortality
                    conditionalPanel(
                      condition = "input.mort_metric == 'Cumulative Mortality'",
                      fluidRow(column(6, selectInput("cum_mort_site_select", "Select Cancer Site:", NULL)), column(6, checkboxGroupInput("cum_mort_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_cum_mort_both", 4), valueBoxOutput("avg_cum_mort_female", 4), valueBoxOutput("avg_cum_mort_male", 4)),
                      fluidRow(box(title = "Cumulative Mortality Trend", plotOutput("cum_mort_line_graph"), 12))
                    ),
                    
                    # 2. Frequency
                    conditionalPanel(
                      condition = "input.mort_metric == 'Frequency'",
                      fluidRow(column(4, selectInput("mort_year_select", "Select Year:", choices = c("All", sort(unique(mortality_data$dodyear))), selected = "All")), 
                               column(4, selectInput("mort_site_select", "Select Cancer Site:", choices = c("All", sort(unique(mortality_data$siteiarc))), selected = "All"))),
                      fluidRow(valueBoxOutput("num_deaths", 4), valueBoxOutput("mort_female_deaths", 4), valueBoxOutput("mort_male_deaths", 4)),
                      fluidRow(box(title = "Deaths by Year", plotOutput("deaths_by_year"), 6), box(title = "Deaths by Sex", plotOutput("deaths_by_sex"), 6)),
                      fluidRow(
                        box(title = "Top 10 Cancer Deaths", DT::dataTableOutput("top10_deaths_table"), 6),
                        box(title = "Top 5 Cancer Deaths by Sex", div(class = "female-table", h4("FEMALES"), DT::dataTableOutput("top5_female_deaths_table")), div(class = "male-table", h4("MALES"), DT::dataTableOutput("top5_male_deaths_table")), 6)
                      ),
                      fluidRow(box(title = "Deaths by 5-Year Age Bands", plotOutput("deaths_by_age_bands"), width = 12))
                    ),
                    
                    # 3. Crude Mortality (FIXED: Now inside tabItem)
                    conditionalPanel(
                      condition = "input.mort_metric == 'Crude Mortality'",
                      fluidRow(column(6, selectInput("crude_mort_site_select", "Select Cancer Site:", NULL)), column(6, checkboxGroupInput("crude_mort_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_crude_mort_both", 4), valueBoxOutput("avg_crude_mort_female", 4), valueBoxOutput("avg_crude_mort_male", 4)),
                      fluidRow(box(title = "Crude Mortality Rate Trend", plotOutput("crude_mort_line_graph"), 12))
                    ),
                    
                    # 4. ASMR (FIXED: Now inside tabItem)
                    conditionalPanel(
                      condition = "input.mort_metric == 'ASMR'",
                      fluidRow(column(6, selectInput("asmr_site_select", "Select Cancer Site:", NULL)), column(6, checkboxGroupInput("asmr_sex_select", "Select Sex:", choices = c("Both", "Female", "Male"), selected = c("Both", "Female", "Male"), inline = TRUE))),
                      fluidRow(valueBoxOutput("avg_asmr_both", 4), valueBoxOutput("avg_asmr_female", 4), valueBoxOutput("avg_asmr_male", 4)),
                      fluidRow(box(title = "ASMR Trend", plotOutput("asmr_line_graph"), 12))
                    ),
                    
                    # Parish-Level Mortality Maps (FIXED: Now inside tabItem)
                    tabBox(
                      title = "Parish-Level Mortality Maps", width = 12,
                      tabPanel("Both Sexes", fluidRow(column(6, h4("All Cancers - Mortality Rate"), leafletOutput("mort_parish_map_both_all")), column(6, h4("Top 5 Cancers - Mortality Rate"), selectInput("mort_parish_cancer_both", "Select Cancer:", NULL), leafletOutput("mort_parish_map_both_top5")))),
                      tabPanel("Males", fluidRow(column(6, h4("All Cancers - Mortality Rate (Males)"), leafletOutput("mort_parish_map_male_all")), column(6, h4("Top 5 Cancers - Mortality Rate (Males)"), selectInput("mort_parish_cancer_male", "Select Cancer:", NULL), leafletOutput("mort_parish_map_male_top5")))),
                      tabPanel("Females", fluidRow(column(6, h4("All Cancers - Mortality Rate (Females)"), leafletOutput("mort_parish_map_female_all")), column(6, h4("Top 5 Cancers - Mortality Rate (Females)"), selectInput("mort_parish_cancer_female", "Select Cancer:", NULL), leafletOutput("mort_parish_map_female_top5"))))
                    )
            ), # Correctly closes the Mortality Tab
            
            # --- SURVIVAL PAGE ---
            tabItem(tabName = "survival",
                    h2("Survival Page"),
                    fluidRow(column(6, selectInput("surv_year_select", "Select Year:", choices = "All")), column(6, selectInput("surv_site_select", "Select Cancer Site:", choices = "All"))),
                    fluidRow(column(4, plotlyOutput("gauge_1yr")), column(4, plotlyOutput("gauge_3yr")), column(4, plotlyOutput("gauge_5yr"))),
                    fluidRow(box(title = "1-Year Survival by Age Band", plotOutput("surv_1yr_age"), width = 12)),
                    fluidRow(box(title = "3-Year Survival by Age Band", plotOutput("surv_3yr_age"), width = 12)),
                    fluidRow(box(title = "5-Year Survival by Age Band", plotOutput("surv_5yr_age"), width = 12)),
                    fluidRow(
                      column(4, box(title = "Highest 5-Year Survival (Both)", div(class = "both-table", DT::dataTableOutput("top_survival_both")), width = NULL)),
                      column(4, box(title = "Highest 5-Year Survival (Males)", div(class = "male-surv-table", DT::dataTableOutput("top_survival_male")), width = NULL)),
                      column(4, box(title = "Highest 5-Year Survival (Females)", div(class = "female-surv-table", DT::dataTableOutput("top_survival_female")), width = NULL))
                    )
            ),
            
            # --- PREVALENCE PAGE ---
            tabItem(tabName = "prevalence",
                    h2("Cancer Prevalence"),
                    fluidRow(column(6, selectInput("prev_site_select", "Select Cancer Site:", choices = NULL))),
                    fluidRow(valueBoxOutput("num_survivors", 4), valueBoxOutput("prevalence_rate", 4), valueBoxOutput("total_population", 4)),
                    fluidRow(box(title = "Prevalence by Age Groups and Sex", plotOutput("prevalence_by_age_sex"), width = 12)),
                    fluidRow(
                      column(4, box(title = "Top 10 Most Prevalent (Both)", div(class = "both-table", DT::dataTableOutput("top_prevalence_both")), width = NULL)),
                      column(4, box(title = "Top 10 Most Prevalent (Females)", div(class = "female-table", DT::dataTableOutput("top_prevalence_female")), width = NULL)),
                      column(4, box(title = "Top 10 Most Prevalent (Males)", div(class = "male-table", DT::dataTableOutput("top_prevalence_male")), width = NULL))
                    )
            ),
            
            # Data Quality page
            tabItem(tabName = "data_quality",
                    h2("Data Quality Indicators (All Years)"),
                    tabsetPanel(
                      tabPanel("MV%",
                               fluidRow(
                                 box(
                                   title = "MV% by Year (Bar Graph)",
                                   plotOutput("mv_bar"),
                                   width = 6
                                 ),
                                 box(
                                   title = "Statistical Process Control (SPC) Chart for MV%",
                                   plotOutput("mv_spc"),
                                   width = 6
                                 )
                               ),
                               fluidRow(
                                 box(
                                   title = "Interpretation of SPC Chart",
                                   htmlOutput("spc_interpretation_mv"),
                                   width = 12
                                 )
                               )
                      ),
                      tabPanel("DCO%",
                               fluidRow(
                                 box(
                                   title = "DCO% by Year (Bar Graph)",
                                   plotOutput("dco_bar"),
                                   width = 6
                                 ),
                                 box(
                                   title = "Statistical Process Control (SPC) Chart for DCO%",
                                   plotOutput("dco_spc"),
                                   width = 6
                                 )
                               ),
                               fluidRow(
                                 box(
                                   title = "Interpretation of SPC Chart",
                                   htmlOutput("spc_interpretation_dco"),
                                   width = 12
                                 )
                               )
                      ),
                      tabPanel("Ill-Defined Sites%",
                               fluidRow(
                                 box(
                                   title = "Ill-Defined Sites% by Year (Bar Graph)",
                                   plotOutput("ill_def_bar"),
                                   width = 6
                                 ),
                                 box(
                                   title = "Statistical Process Control (SPC) Chart for Ill-Defined Sites%",
                                   plotOutput("ill_def_spc"),
                                   width = 6
                                 )
                               ),
                               fluidRow(
                                 box(
                                   title = "Interpretation of SPC Chart",
                                   htmlOutput("spc_interpretation_ill_def"),
                                   width = 12
                                 )
                               )
                      ),
                      tabPanel("Topo-Morph Consistency%",
                               fluidRow(
                                 box(
                                   title = "Topo-Morph Consistency % by Year (Bar Graph)",
                                   plotOutput("topo_morph_bar"),
                                   width = 6
                                 ),
                                 box(
                                   title = "Statistical Process Control (SPC) Chart for Topo-Morph Consistency%",
                                   plotOutput("topo_morph_spc"),
                                   width = 6
                                 )
                               ),
                               fluidRow(
                                 box(
                                   title = "Interpretation of SPC Chart",
                                   htmlOutput("spc_interpretation_topo_morph"),
                                   width = 12
                                 )
                               )
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Definitions of Indicators",
                        htmlOutput("indicator_definitions"),
                        width = 12
                      )
                    )
            ),
            
            tabItem(tabName = "reports",
                    h2("Reports Page"),
                    fluidRow(
                      box(
                        title = "Available Cancer Reports",
                        DT::dataTableOutput("reports_table"),
                        width = 12
                      )
                    ),
                    fluidRow(
                      box(
                        title = "Generate Custom Reports",
                        width = 12,
                        icon = icon("file-powerpoint"),
                        p("Generate automated PowerPoint presentations with key statistics, 
              visualizations, and data quality indicators from the dashboard."),
                        p("The generated report includes:"),
                        tags$ul(
                          tags$li("Key statistics and demographics"),
                          tags$li("Top cancer sites by incidence and mortality"),
                          tags$li("Temporal trends in cancer cases"),
                          tags$li("Age distribution analysis"),
                          tags$li("Survival analysis"),
                          tags$li("Data quality indicators"),
                          tags$li("Contact information")
                        ),
                        uiOutput("generate_ppt_button"),
                        hr(),
                        p(em("Note: Report generation may take 1-2 minutes depending on data size."), 
                          style = "color: #888; font-size: 0.9em;")
                      )
                    )
            ),
            
            tabItem(tabName = "additional",
                    h2(tags$strong("Additional Information")),
                    h3(tags$strong("BNR Cancer Registry Online Dashboard Release Notes")),
                    p("The data presented in the BNR Cancer Registry Online Dashboard can be used to examine the current landscape of cancer in Barbados, estimate disease burden, follow trends over time, and make comparisons across different cancer types, demographic groups, and geographic areas."),
                    h3(tags$strong("Table of Contents:")),
                    tags$ol(
                      tags$li("Data Availability"),
                      tags$li("Definitions"),
                      tags$li("Data Quality")
                    ),
                    h4(tags$strong("Data Availability")),
                    p("The BNR Cancer Registry Dashboard is updated on a periodic basis. The current release (September 2025) of this dashboard includes data up to the end of diagnosis year 2023. Due to standard delays in the capture and coding of cancer cases, the BNR Cancer Registry data are currently considered complete for cases up to the end of 2023."),
                    p("Dashboard reports for outcomes (survival, lifetime risk) are updated periodically (last updated using 2022 incidence data)."),
                    p("Average Annual Percent Change (AAPC) reported in age-standardized cancer incidence are reported using data up to the end of 2022."),
                    h4(tags$strong("Definitions")),
                    p("An incidence rate is the number of new disease events occurring in a specified population during a year, usually expressed as the number of events per 100,000 population at risk. That is,"),
                    p(tags$strong("Incidence rate = (new events / population) × 100,000")),
                    p("The numerator of the incidence rate is the number of new disease events; the denominator is the size of the population. The number of new events may include multiple events occurring in one patient. In general, the incidence rate does not include recurrences (where recurrence is defined as a presentation to the healthcare system within a certain period of the initiating event)."),
                    p("The age standardised rate is the proportion of cases (or deaths) in a given population (and year) weighted by the age structure of the population. For incidence (ASIR) and mortality (ASMR) calculations, cases and deaths were weighted by the WHO World Standard population."),
                    p("A mortality rate is the number of deaths, in which the disease (cancer) was the underlying cause of death, occurring in a specified population during a year. Mortality is usually expressed as the number of deaths due to the disease per 100,000 population. That is,"),
                    p(tags$strong("Mortality rate = (disease deaths/population) × 100,000")),
                    p("The numerator of the mortality rate is the number of deaths; the denominator is the size of the population."),
                    h5("Case Definitions"),
                    p("Case definition for 2008 diagnoses: “All in-situ and malignant neoplasms with a behaviour code of 2 or 3 according to the International Classification of Diseases for Oncology, 3rd Edition (ICD-O-3) as well as benign tumours of the brain & other parts of CNS, pituitary gland, craniopharyngeal duct and the pineal gland (behaviour code of 0 or 1).”"),
                    p("Case definition for 2013 onwards diagnoses: “All malignant neoplasms with a behaviour code of 3 according to the ICD-O-3 and in-situ neoplasms of the cervix only (CIN3). Exclude all other in-situ neoplasms and basal cell and squamous cell carcinoma of skin, non-genital areas”."),
                    p("The case definition for 2014 onwards remains the same as 2013 but was reworded to: Data were collected on all malignant neoplasms with a behaviour code of 3, according to the International Classification of Diseases for Oncology, 3rd Edition 1st Revision (ICD-O-3.1), as well as in situ neoplasms of the cervix only (CIN 3) diagnosed in 2014."),
                    h5(tags$strong("Residency")),
                    p("‘Usual Residence’ used in the Population and Housing Census is as follows:"),
                    p("Usual Residence – This is defined as the place where a person being enumerated lives and sleeps most of the time."),
                    tags$ol(
                      tags$li("For persons with more than one home, usual residence will be the one at which the person spends the greater part of the year. Thus, for an individual who has more than one place of residence because his workplace or school is away from home, the usual residence should be that place in which he/she spends at least four nights of the week."),
                      tags$li("Fishermen at sea are considered to have their place of usual residence where they dwell when on shore."),
                      tags$li("Barbadians in the farm labour programme were enumerated in their usual households; seamen or crewmembers on vessels plying foreign ports should record as their usual residence the place where they stay when on shore."),
                      tags$li("Aircraft pilots are considered to have their usual residence in the households in which they dwell."),
                      tags$li("Foreign diplomats are the usual residents of the countries they represent and were not enumerated.")
                    ),
                    h4(tags$strong("Data Quality")),
                    p("In order to share data and make it comparable to other countries and year-to-year, the BNR must maintain quality. We engage several tools for standardising and formatting variables, checking for accuracy, duplicates and missing data as well as performing preliminary analysis. Data Management and Analysis were performed using the International Association for Research in Cancer software: IARCcrgTools version 2.12 (by J. Ferlay, Section of Cancer Surveillance, International Agency for Research on Cancer, Lyon, France), Stata version 17.1 (StataCorp., College Station, TX, USA), CanReg5 database version 5.43 (International Agency for Research in Cancer, Lyon, France), Research electronic data capture (REDCap), Version 12.3.3, the SEER Hematopoietic database (Surveillance, Epidemiology and End Results (SEER) Program [www.seer.cancer.gov] Hematopoietic and Lymphoid Database, Version 2.1 data released 05/23/2012. National Cancer Institute, DCCPS, Surveillance Research Program).")
            ),
            # --- OTHER TABS ---
            tabItem(tabName = "projection",
                    
                    # --- Page header ---
                    fluidRow(
                      column(12,
                             h2(icon("chart-line"), " Cancer Incidence Projections",
                                style = "color:#253494; font-weight:bold; margin-bottom:6px;"),
                             p(style = "font-size:15px; color:#555;",
                               "Five-year projections (2023–2027) of cancer incidence for all cancers combined and the top 5 most frequent cancer sites in Barbados, based on registry data from 2013–2022.")
                      )
                    ),
                    
                    hr(),
                    
                    # --- All Cancers projection ---
                    fluidRow(
                      box(
                        title = tagList(icon("chart-area"), " All Cancers — Projected Incidence (2023–2027)"),
                        status = "primary", solidHeader = TRUE, width = 12,
                        plotlyOutput("proj_all_cancers", height = 380)
                      )
                    ),
                    
                    # --- Top 5 individual site projections ---
                    fluidRow(
                      box(
                        title = tagList(icon("chart-bar"), " Top 5 Cancer Sites — Projected Incidence (2023–2027)"),
                        status = "info", solidHeader = TRUE, width = 12,
                        plotlyOutput("proj_top5_sites", height = 420)
                      )
                    ),
                    
                    # --- Projection summary table ---
                    fluidRow(
                      box(
                        title = tagList(icon("table"), " Projection Summary Table"),
                        status = "success", solidHeader = TRUE, width = 12,
                        p(style = "font-size:13px; color:#555; margin-bottom:10px;",
                          "Projected annual case counts with 95% prediction intervals for each cancer site."),
                        DT::dataTableOutput("proj_summary_table")
                      )
                    ),
                    
                    hr(),
                    
                    # --- Methods section ---
                    fluidRow(
                      column(12,
                             h3(icon("flask"), " Methods",
                                style = "color:#253494; font-weight:bold; margin-bottom:14px;"),
                             div(style = "background:#f8f9fa; border-left:5px solid #253494; border-radius:6px; padding:20px 24px;",
                                 
                                 h4("Data Source", style = "margin-top:0; color:#253494;"),
                                 p("Projections are derived from cancer incidence data recorded by the Barbados National Cancer Registry (BNR) covering the period ",
                                   tags$strong("2013 to 2022"), ". Cases classified as 'Other and unspecified (O&U)' are excluded from all analyses. ",
                                   "The five cancer sites with the highest total case counts over the observation period are selected as the top 5 sites."),
                                 
                                 h4("Statistical Model", style = "color:#253494;"),
                                 p("A ", tags$strong("Negative Binomial regression model"), " is fitted to the annual case counts for each cancer group. ",
                                   "Negative Binomial regression is preferred over standard Poisson regression when overdispersion is present — ",
                                   "that is, when the observed variance in annual counts exceeds the mean. This is a common feature of small-registry cancer data, ",
                                   "where year-to-year variation reflects both underlying trends and random fluctuation."),
                                 p("The model takes the form:"),
                                 tags$blockquote(
                                   style = "font-family:monospace; background:#fff; border:1px solid #ddd; padding:10px 16px; border-radius:4px;",
                                   "Cases(t) ~ NegBin(μ(t), θ)",
                                   br(),
                                   "log(μ(t)) = β₀ + β₁ × Year"
                                 ),
                                 p("where ", tags$em("μ(t)"), " is the expected number of cases in year ", tags$em("t"),
                                   ", ", tags$em("θ"), " is the dispersion parameter, and ",
                                   tags$em("β₁"), " is the estimated annual trend. ",
                                   "If model fitting fails for a given site (e.g., due to sparse data), a fallback ",
                                   tags$strong("linear regression"), " on raw counts is applied."),
                                 
                                 h4("Projection Horizon", style = "color:#253494;"),
                                 p("Projections are generated for the ", tags$strong("five-year period 2023–2027"),
                                   " by extrapolating the fitted trend beyond the observation window. ",
                                   "The last observed year in the registry data is 2022."),
                                 
                                 h4("Uncertainty Quantification", style = "color:#253494;"),
                                 p("Each projected point estimate is accompanied by a ",
                                   tags$strong("95% prediction interval (PI)"), " derived from the standard error of the linear predictor on the log scale, ",
                                   "back-transformed to the count scale. Prediction intervals account for both parameter uncertainty and ",
                                   "the inherent variability of future observations, and should be interpreted as the range within which the ",
                                   "true number of cases is expected to fall in 95% of hypothetical future realisations of the same process."),
                                 
                                 h4("Limitations", style = "color:#253494;"),
                                 tags$ul(
                                   tags$li("Projections assume that historical trends observed from 2013–2022 continue linearly into the future. Structural breaks — such as changes in screening policy, population demographics, or registry completeness — are not modelled."),
                                   tags$li("The relatively short observation window (10 years) and small annual case counts for some sites mean that projection uncertainty is substantial, particularly for rarer cancers."),
                                   tags$li("Population growth and ageing are not explicitly incorporated; projections reflect trend extrapolation on crude counts rather than age-standardised rates."),
                                   tags$li("These projections are intended for planning and surveillance purposes and should not be used as clinical or policy mandates without supplementary analysis.")
                                 ),
                                 
                                 h4("Software", style = "color:#253494;"),
                                 p("All analyses are performed in ", tags$strong("R"), " using the ",
                                   tags$code("MASS"), " package (", tags$code("glm.nb"), " function) for Negative Binomial regression, ",
                                   "and the ", tags$code("plotly"), " package for interactive visualisations.")
                             )
                      )
                    )
                    
            ),       
            tabItem(tabName = "contact",
                    h2(tags$strong("Contact Us")),
                    p("Contact information for inquiries."),
                    p(" "),
                    p(" "),
                    h2(tags$strong("The Barbados National Registry (BNR)")),
                    p("The George Alleyne Chronic Disease Research Centre"),
                    p("UWI Avalon"),
                    p("Jemmotts Lane"),
                    p("Bridgetown"),
                    p("Barbados, W.I."),
                    p("Tel: 246-426-6416"),
                    p("Fax: 246-426-8406"),
                    p("Email: bnr.uwi.edu"),
                    h2(tags$strong("Collaborators")),
                    fluidRow(
                      column(1,
                             img(src = "moh_logo.png", class = "collaborator-logo"),
                             p(class = "collaborator-text", "Ministry of Health and Wellness")
                      ),
                      column(2,
                             img(src = "cdcc_logo.png", class = "collaborator-logo"),
                             p(class = "collaborator-text", "The George Alleyne Chronic Disease Research Centre")
                      ),
                      column(3,
                             img(src = "cahir_logo.png", class = "collaborator-logo"),
                             p(class = "collaborator-text", "The Caribbean Institute for Health Research")
                      ),
                      column(2,
                             img(src = "uwi_logo.png", class = "collaborator-logo"),
                             p(class = "collaborator-text", "The University of the West Indies, Cave Hill Campus")
                      ),
                    ) # End of tabItems
            ) # End of dashboard_content div
          ) # End of shinyjs::hidden
      ) # End of dashboardBody
    ) # End of dashboardPage
  )
)


server <- function(input, output, session) {
  
  # --- Home landing page: navigate to module on card click ---
  observeEvent(input$home_nav, {
    shinydashboard::updateTabItems(session, "tabs", selected = input$home_nav)
  })
  
  # Authentication
  credentials <- shinyauthr::loginServer(
    id = "login_form",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  # ADD THE INITIALIZATION HERE:
  observe({
    # Initialize with not-authenticated class
    shinyjs::addClass(selector = "body", class = "not-authenticated")
  })
  
  observe({
    if (credentials()$user_auth) {
      # User is authenticated
      shinyjs::hide(id = "loginpage")
      shinyjs::show(id = "dashboard_content")
      shinyjs::removeClass(selector = "body", class = "not-authenticated")
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      # User is not authenticated
      shinyjs::show(id = "loginpage")
      shinyjs::hide(id = "dashboard_content")
      shinyjs::addClass(selector = "body", class = "not-authenticated")
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  # Create a reactive for the Top 5 Parish Data
  parish_top5_data <- reactive({
    req(credentials()$user_auth)
    compute_parish_top5_cases(data)
  })
  
  # Home infographic calculations
  output$total_cases <- renderValueBox({
    req(credentials()$user_auth)
    valueBox(
      nrow(data),
      "Total Incidental Cases (2013-2022)",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$home_total_deaths <- renderValueBox({
    req(credentials()$user_auth)
    valueBox(
      nrow(mortality_data),
      "Total Deaths (2008-2024)",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$avg_age <- renderValueBox({
    req(credentials()$user_auth)
    avg_age <- round(mean(data$age, na.rm = TRUE), 1)
    valueBox(
      avg_age,
      "Average Age at Diagnosis",
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$avg_age_death <- renderValueBox({
    req(credentials()$user_auth)
    avg_age <- round(mean(mortality_data$age, na.rm = TRUE), 1)
    valueBox(
      avg_age,
      "Average Age at Death",
      icon = icon("user-times"),
      color = "olive"
    )
  })
  
  output$pediatric_cases <- renderValueBox({
    req(credentials()$user_auth)
    pediatric_pct <- round(100 * sum(data$age < 15, na.rm = TRUE) / nrow(data), 1)
    valueBox(
      paste0(pediatric_pct, "%"),
      "Pediatric Incidental Cases (Age < 15)",
      icon = icon("child"),
      color = "purple"
    )
  })
  
  output$elderly_cases <- renderValueBox({
    req(credentials()$user_auth)
    elderly_pct <- round(100 * sum(data$age >= 65, na.rm = TRUE) / nrow(data), 1)
    valueBox(
      paste0(elderly_pct, "%"),
      "Elderly Incidental Cases (Age ≥ 65)",
      icon = icon("user-plus"),
      color = "orange"
    )
  })
  
  output$pediatric_deaths <- renderValueBox({
    req(credentials()$user_auth)
    pediatric_deaths_pct <- round(100 * sum(mortality_data$age < 15, na.rm = TRUE) / nrow(mortality_data), 1)
    valueBox(
      paste0(pediatric_deaths_pct, "%"),
      "Pediatric Deaths (Age < 15)",
      icon = icon("child"),
      color = "maroon"
    )
  })
  
  output$elderly_deaths <- renderValueBox({
    req(credentials()$user_auth)
    elderly_deaths_pct <- round(100 * sum(mortality_data$age >= 65, na.rm = TRUE) / nrow(mortality_data), 1)
    valueBox(
      paste0(elderly_deaths_pct, "%"),
      "Elderly Deaths (Age ≥ 65)",
      icon = icon("user-plus"),
      color = "teal"
    )
  })
  
  output$cases_over_years <- renderPlot({
    req(credentials()$user_auth)
    data %>%
      group_by(dxyr) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = dxyr, y = cases)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_text(aes(label = round(cases, 1), y = cases * 1.01), vjust = -0.5, size = 5) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cancer Cases Over Years", x = "Year", y = "Number of Cases")
  })
  
  output$top_sites <- DT::renderDataTable({
    req(credentials()$user_auth)
    data %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  })
  
  output$top5_pediatric_sites <- DT::renderDataTable({
    req(credentials()$user_auth)
    data %>%
      filter(age < 15, siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$top5_elderly_sites <- DT::renderDataTable({
    req(credentials()$user_auth)
    data %>%
      filter(age >= 65, siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$top10_deaths_both_home <- DT::renderDataTable({
    req(credentials()$user_auth)
    mortality_data %>%
      filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 10, searching = FALSE, dom = 't'))
  
  output$top10_deaths_elderly_home <- DT::renderDataTable({
    req(credentials()$user_auth)
    mortality_data %>%
      filter(age >= 65, !is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 10, searching = FALSE, dom = 't'))
  
  output$cases_by_parish <- renderPlot({
    req(credentials()$user_auth)
    data %>%
      filter(!is.na(parish), parish != "") %>%
      group_by(parish) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = reorder(parish, -cases), y = cases)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = cases, y = cases * 1.01), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cancer Cases by Parish", x = "Parish", y = "Number of Cases")
  })
  
  # Top 25 sites
  top25_sites <- data %>%
    filter(siteiarc != "Other and unspecified (O&U)") %>%
    count(siteiarc) %>%
    arrange(desc(n)) %>%
    head(25) %>%
    pull(siteiarc)
  
  # Update select inputs for ASIR and Cumulative
  observe({
    updateSelectInput(session, "asir_site_select", choices = c("All cancers", top25_sites), selected = "All cancers")
    updateSelectInput(session, "cum_site_select", choices = c("All cancers", top25_sites), selected = "All cancers")
    updateSelectInput(session, "crude_site_select", choices = c("All cancers", top25_sites), selected = "All cancers")
  })
  
  # Incidence page - Frequency
  filtered_data <- reactive({
    req(credentials()$user_auth)
    df <- data
    if (input$year_select != "All") {
      df <- df %>% filter(dxyr == as.integer(input$year_select))
    }
    if (input$site_select != "All") {
      df <- df %>% filter(siteiarc == input$site_select)
    }
    df
  })
  
  output$num_cases <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Frequency")
    valueBox(
      nrow(filtered_data()),
      "Number of Cases (2013-2022)",
      icon = icon("users"),
      color = "green"
    )
  })
  
  output$num_female_cases <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Frequency")
    female_cases <- nrow(filtered_data() %>% filter(sex == "female"))
    valueBox(
      female_cases,
      "Female Cases",
      icon = icon("venus"),
      color = "maroon"
    )
  })
  
  output$num_male_cases <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Frequency")
    male_cases <- nrow(filtered_data() %>% filter(sex == "male"))
    valueBox(
      male_cases,
      "Male Cases",
      icon = icon("mars"),
      color = "blue"
    )
  })
  
  output$bar_graph <- renderPlot({
    req(credentials()$user_auth, input$metric == "Frequency")
    df <- data
    if (input$site_select != "All") {
      df <- df %>% filter(siteiarc == input$site_select)
    }
    df %>%
      group_by(dxyr) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = dxyr, y = cases)) +
      geom_bar(stat = "identity", fill = "#005a32") +
      geom_text(aes(label = cases, y = cases * 1.01), vjust = -0.5, size = 5) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(x = "Year", y = "Number of Cases")
  })
  
  output$sex_bar_graph <- renderPlot({
    req(credentials()$user_auth)
    df <- data
    if (input$site_select != "All") {
      df <- df %>% filter(siteiarc == input$site_select)
    }
    df %>%
      group_by(dxyr, sex) %>%
      summarise(cases = n(), .groups = 'drop') %>%
      ggplot(aes(x = dxyr, y = cases, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("female" = "#DD1C77", "male" = "#3182BD")) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cases by Sex", x = "Year", y = "Number of Cases")
  })
  
  output$cases_by_age_bands <- renderPlot({
    req(credentials()$user_auth, input$metric == "Frequency")
    filtered_data() %>%
      mutate(age_band = cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85+"),
                            right = FALSE)) %>%
      filter(!is.na(age_band)) %>%
      group_by(age_band) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = age_band, y = cases)) +
      geom_bar(stat = "identity", fill = "maroon4") +
      geom_text(aes(label = cases, y = cases * 1.01), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cases by 5-Year Age Bands", x = "Age Band", y = "Number of Cases")
  })
  
  output$top10_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$metric == "Frequency")
    year_df <- data
    if (input$year_select != "All") {
      year_df <- year_df %>% filter(dxyr == as.integer(input$year_select))
    }
    year_df %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top5_female_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$metric == "Frequency")
    year_df <- data
    if (input$year_select != "All") {
      year_df <- year_df %>% filter(dxyr == as.integer(input$year_select))
    }
    year_df %>%
      filter(sex == "female", siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$top5_male_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$metric == "Frequency")
    year_df <- data
    if (input$year_select != "All") {
      year_df <- year_df %>% filter(dxyr == as.integer(input$year_select))
    }
    year_df %>%
      filter(sex == "male", siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  # Update selectInputs for top 5 cancers
  observe({
    req(credentials()$user_auth)
    
    # 1. Define top5_sites here so it's available below
    top5_sites <- data %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(siteiarc)
    
    # 2. Update the home page selector
    updateSelectInput(session, "home_parish_cancer", choices = top5_sites, selected = top5_sites[1])
    
    # 3. Update the incidence page selectors
    updateSelectInput(session, "inc_parish_cancer_both", choices = top5_sites, selected = top5_sites[1])
    updateSelectInput(session, "inc_parish_cancer_male", choices = top5_sites, selected = top5_sites[1])
    updateSelectInput(session, "inc_parish_cancer_female", choices = top5_sites, selected = top5_sites[1])
  })
  
  # Render home parish map
  output$home_parish_map <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$home_parish_cancer)
    
    # This will now work because parish_top5_data() is defined above
    parish_data <- parish_top5_data()$data %>%
      filter(siteiarc == input$home_parish_cancer)
    
    create_parish_map(
      parish_shapefile, 
      parish_data, 
      "cases", 
      paste("Number of Cases -", input$home_parish_cancer)
    )
  })
  
  # Incidence page - ASIR
  asir_data <- reactive({
    req(credentials()$user_auth, input$metric == "ASIR")
    site <- input$asir_site_select
    asir_both <- NULL
    asir_female <- NULL
    asir_male <- NULL
    if ("Both" %in% input$asir_sex_select) {
      asir_both <- compute_asir(data, pop_data, who_weights, site, "Both")
    }
    if ("Female" %in% input$asir_sex_select) {
      asir_female <- compute_asir(data, pop_data, who_weights, site, "Female")
    }
    if ("Male" %in% input$asir_sex_select) {
      asir_male <- compute_asir(data, pop_data, who_weights, site, "Male")
    }
    list(both = asir_both, female = asir_female, male = asir_male)
  })
  
  output$avg_asir_both <- renderValueBox({
    req(credentials()$user_auth, input$metric == "ASIR")
    df <- asir_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASIR (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$asir, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASIR (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_asir_female <- renderValueBox({
    req(credentials()$user_auth, input$metric == "ASIR")
    df <- asir_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASIR (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$asir, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASIR (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_asir_male <- renderValueBox({
    req(credentials()$user_auth, input$metric == "ASIR")
    df <- asir_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASIR (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$asir, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASIR (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$asir_line_graph <- renderPlot({
    req(credentials()$user_auth, input$metric == "ASIR")
    dflist <- asir_data()
    
    # Initialize an empty ggplot object
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "ASIR per 100,000", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    # Add layers only if data exists and has rows
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = asir, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = asir, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = asir, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = asir, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = asir, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = asir, color = "Male"))
    }
    
    # If no data is plotted, add a message
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Incidence page - Crude Incidence
  crude_data <- reactive({
    req(credentials()$user_auth, input$metric == "Crude Incidence")
    site <- input$crude_site_select
    crude_both <- NULL
    crude_female <- NULL
    crude_male <- NULL
    if ("Both" %in% input$crude_sex_select) {
      crude_both <- compute_crude_incidence(data, pop_data, site, "Both")
    }
    if ("Female" %in% input$crude_sex_select) {
      crude_female <- compute_crude_incidence(data, pop_data, site, "Female")
    }
    if ("Male" %in% input$crude_sex_select) {
      crude_male <- compute_crude_incidence(data, pop_data, site, "Male")
    }
    list(both = crude_both, female = crude_female, male = crude_male)
  })
  
  output$avg_crude_both <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Crude Incidence")
    df <- crude_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Rate (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$crude_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Rate (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_crude_female <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Crude Incidence")
    df <- crude_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Rate (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$crude_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Rate (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_crude_male <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Crude Incidence")
    df <- crude_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Rate (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$crude_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Rate (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$crude_line_graph <- renderPlot({
    req(credentials()$user_auth, input$metric == "Crude Incidence")
    dflist <- crude_data()
    
    # Initialize an empty ggplot object
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "Crude Incidence Rate per 100,000", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    # Add layers only if data exists and has rows
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = crude_rate, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = crude_rate, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = crude_rate, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = crude_rate, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = crude_rate, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = crude_rate, color = "Male"))
    }
    
    # If no data is plotted, add a message
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Incidence page - Cumulative Incidence
  cum_data <- reactive({
    req(credentials()$user_auth, input$metric == "Cumulative Incidence")
    site <- input$cum_site_select
    cum_both <- NULL
    cum_female <- NULL
    cum_male <- NULL
    if ("Both" %in% input$cum_sex_select) {
      cum_both <- compute_cuminc(data, pop_data, site, "Both")
    }
    if ("Female" %in% input$cum_sex_select) {
      cum_female <- compute_cuminc(data, pop_data, site, "Female")
    }
    if ("Male" %in% input$cum_sex_select) {
      cum_male <- compute_cuminc(data, pop_data, site, "Male")
    }
    list(both = cum_both, female = cum_female, male = cum_male)
  })
  
  output$avg_cum_both <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Cumulative Incidence")
    df <- cum_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Risk % (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$cuminc, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Risk % (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_cum_female <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Cumulative Incidence")
    df <- cum_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Risk % (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$cuminc, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Risk % (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_cum_male <- renderValueBox({
    req(credentials()$user_auth, input$metric == "Cumulative Incidence")
    df <- cum_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Risk % (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$cuminc, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Risk % (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$cum_line_graph <- renderPlot({
    req(credentials()$user_auth, input$metric == "Cumulative Incidence")
    dflist <- cum_data()
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "Cumulative Incidence % (0-74 years)", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = cuminc, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = cuminc, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = cuminc, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = cuminc, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = cuminc, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = cuminc, color = "Male"))
    }
    
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Update selectInputs for top 5 cancers (Incidence & Home)
  observe({ 
    req(credentials()$user_auth) 
    
    # Get the sites from our new reactive
    sites <- parish_top5_data()$top5
    
    # Update all relevant dropdowns
    updateSelectInput(session, "home_parish_cancer", choices = sites, selected = sites[1])
    updateSelectInput(session, "inc_parish_cancer_both", choices = sites, selected = sites[1])
    updateSelectInput(session, "inc_parish_cancer_male", choices = sites, selected = sites[1])
    updateSelectInput(session, "inc_parish_cancer_female", choices = sites, selected = sites[1])
  })
  
  # BOTH SEXES - All Cancers
  output$inc_parish_map_both_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, "All cancers", "Both")
    create_parish_map(parish_shapefile, parish_asir, "asir", "ASIR - All Cancers (Both Sexes)")
  })
  
  # BOTH SEXES - Top 5
  output$inc_parish_map_both_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$inc_parish_cancer_both)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, input$inc_parish_cancer_both, "Both")
    create_parish_map(parish_shapefile, parish_asir, "asir", 
                      paste("ASIR -", input$inc_parish_cancer_both, "(Both Sexes)"))
  })
  
  # MALES - All Cancers
  output$inc_parish_map_male_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, "All cancers", "Male")
    create_parish_map(parish_shapefile, parish_asir, "asir", "ASIR - All Cancers (Males)")
  })
  
  # MALES - Top 5
  output$inc_parish_map_male_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$inc_parish_cancer_male)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, input$inc_parish_cancer_male, "Male")
    create_parish_map(parish_shapefile, parish_asir, "asir", 
                      paste("ASIR -", input$inc_parish_cancer_male, "(Males)"))
  })
  
  # FEMALES - All Cancers
  output$inc_parish_map_female_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, "All cancers", "Female")
    create_parish_map(parish_shapefile, parish_asir, "asir", "ASIR - All Cancers (Females)")
  })
  
  # FEMALES - Top 5
  output$inc_parish_map_female_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$inc_parish_cancer_female)
    parish_asir <- compute_parish_asir(data, parish_pop_long, who_weights, input$inc_parish_cancer_female, "Female")
    create_parish_map(parish_shapefile, parish_asir, "asir", 
                      paste("ASIR -", input$inc_parish_cancer_female, "(Females)"))
  })
  
  observe({
    # Get top 25 mortality sites
    top25_mort_sites <- mortality_data %>%
      filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(25) %>%
      pull(siteiarc)
    
    updateSelectInput(session, "crude_mort_site_select", choices = c("All cancers", top25_mort_sites), selected = "All cancers")
    updateSelectInput(session, "asmr_site_select", choices = c("All cancers", top25_mort_sites), selected = "All cancers")
    updateSelectInput(session, "cum_mort_site_select", choices = c("All cancers", top25_mort_sites), selected = "All cancers")
  })
  
  # Function to compute Cumulative Mortality (0-74 years)
  compute_cumulative_mortality <- function(mortality_data, pop_data, site, sex_group) {
    if (site == "All cancers") {
      mort_df <- mortality_data %>% filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)")
    } else {
      mort_df <- mortality_data %>% filter(siteiarc == site)
    }
    if (sex_group != "Both") {
      mort_df <- mort_df %>% filter(sex == sex_group)
    }
    if (nrow(mort_df) == 0) {
      return(data.frame(year = integer(), cum_mort = numeric()))
    }
    mort_df <- mort_df %>%
      mutate(age_group = as.numeric(cut(age, breaks = c(seq(0, 85, 5), Inf), labels = 1:18, right = FALSE))) %>%
      filter(!is.na(age_group)) %>%
      group_by(year = dodyear, age_group) %>%
      summarise(counts = n(), .groups = 'drop')
    
    years <- unique(mortality_data$dodyear)
    age_groups <- 1:18
    full_df <- expand_grid(year = years, age_group = age_groups) %>%
      left_join(mort_df, by = c("year", "age_group")) %>%
      mutate(counts = coalesce(counts, 0))
    
    if (sex_group == "Both") {
      pop_df <- pop_data %>%
        group_by(year, age5) %>%
        summarise(pop = sum(pop_wpp), .groups = 'drop') %>%
        rename(age_group = age5)
    } else {
      pop_df <- pop_data %>%
        filter(sex == tolower(sex_group)) %>%
        dplyr::select(year, age_group = age5, pop = pop_wpp)
    }
    
    full_df <- full_df %>%
      left_join(pop_df, by = c("year", "age_group")) %>%
      mutate(pop = coalesce(pop, 0),
             age_rate = ifelse(pop > 0, counts / pop * 100000, 0)) %>%
      group_by(year) %>%
      summarise(cum_mort = sum(age_rate[age_group %in% 1:15] * 5) / 100000 * 100, .groups = 'drop')
    
    full_df
  }
  
  # Function to compute Crude Mortality Rate
  compute_crude_mortality <- function(mortality_data, pop_data, site, sex_group) {
    if (site == "All cancers") {
      mort_df <- mortality_data %>% filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)")
    } else {
      mort_df <- mortality_data %>% filter(siteiarc == site)
    }
    if (sex_group != "Both") {
      mort_df <- mort_df %>% filter(sex == sex_group)
    }
    if (nrow(mort_df) == 0) {
      return(data.frame(year = integer(), crude_mort_rate = numeric()))
    }
    
    # Count deaths by year
    mort_counts <- mort_df %>%
      group_by(year = dodyear) %>%
      summarise(counts = n(), .groups = 'drop')
    
    # Get population data
    if (sex_group == "Both") {
      pop_df <- pop_data %>%
        group_by(year) %>%
        summarise(pop = sum(pop_wpp), .groups = 'drop')
    } else {
      pop_df <- pop_data %>%
        filter(sex == tolower(sex_group)) %>%
        group_by(year) %>%
        summarise(pop = sum(pop_wpp), .groups = 'drop')
    }
    
    # Merge mortality counts with population data
    crude_df <- mort_counts %>%
      left_join(pop_df, by = "year") %>%
      mutate(
        pop = coalesce(pop, 0),
        crude_mort_rate = ifelse(pop > 0, counts / pop * 100000, 0)
      ) %>%
      dplyr::select(year, crude_mort_rate)
    
    crude_df
  }
  
  # Mortality page - Frequency (existing with updates)
  filtered_mort_data <- reactive({
    req(credentials()$user_auth)
    df <- mortality_data
    if (input$mort_year_select != "All") {
      df <- df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df
  })
  
  # Update existing mortality outputs to be conditional on Frequency metric
  output$num_deaths <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    valueBox(
      nrow(filtered_mort_data()),
      "Number of Deaths (2008-2024)",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$mort_female_deaths <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    female_deaths <- nrow(filtered_mort_data() %>% filter(sex == "Female"))
    valueBox(
      female_deaths,
      "Female Deaths",
      icon = icon("venus"),
      color = "maroon"
    )
  })
  
  output$mort_male_deaths <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    male_deaths <- nrow(filtered_mort_data() %>% filter(sex == "Male"))
    valueBox(
      male_deaths,
      "Male Deaths",
      icon = icon("mars"),
      color = "blue"
    )
  })
  
  output$deaths_by_year <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    df <- mortality_data
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df %>%
      group_by(dodyear) %>%
      summarise(deaths = n()) %>%
      ggplot(aes(x = dodyear, y = deaths)) +
      geom_bar(stat = "identity", fill = "red") +
      geom_text(aes(label = deaths, y = deaths * 1.01), vjust = -0.5, size = 5) +
      scale_x_continuous(breaks = seq(min(mortality_data$dodyear), max(mortality_data$dodyear), by = 2)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(x = "Year", y = "Number of Deaths")
  })
  
  output$deaths_by_sex <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    df <- mortality_data
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df %>%
      group_by(dodyear, sex) %>%
      summarise(deaths = n(), .groups = 'drop') %>%
      ggplot(aes(x = dodyear, y = deaths, fill = sex)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Female" = "#DD1C77", "Male" = "#3182BD")) +
      scale_x_continuous(breaks = seq(min(mortality_data$dodyear), max(mortality_data$dodyear), by = 2)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Deaths by Sex", x = "Year", y = "Number of Deaths")
  })
  
  output$top10_deaths_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    year_df <- mortality_data
    if (input$mort_year_select != "All") {
      year_df <- year_df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    year_df %>%
      filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top5_female_deaths_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    year_df <- mortality_data
    if (input$mort_year_select != "All") {
      year_df <- year_df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    year_df %>%
      filter(sex == "Female", !is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$top5_male_deaths_table <- DT::renderDataTable({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    year_df <- mortality_data
    if (input$mort_year_select != "All") {
      year_df <- year_df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    year_df %>%
      filter(sex == "Male", !is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$deaths_by_age_bands <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "Frequency")
    filtered_mort_data() %>%
      mutate(age_band = cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85+"),
                            right = FALSE)) %>%
      filter(!is.na(age_band)) %>%
      group_by(age_band) %>%
      summarise(deaths = n()) %>%
      ggplot(aes(x = age_band, y = deaths)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = deaths, y = deaths * 1.01), vjust = -0.5, size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Deaths by 5-Year Age Bands", x = "Age Band", y = "Number of Deaths")
  })
  
  # Mortality page - Crude Mortality
  crude_mort_data <- reactive({
    req(credentials()$user_auth, input$mort_metric == "Crude Mortality")
    site <- input$crude_mort_site_select
    crude_mort_both <- NULL
    crude_mort_female <- NULL
    crude_mort_male <- NULL
    if ("Both" %in% input$crude_mort_sex_select) {
      crude_mort_both <- compute_crude_mortality(mortality_data, pop_data, site, "Both")
    }
    if ("Female" %in% input$crude_mort_sex_select) {
      crude_mort_female <- compute_crude_mortality(mortality_data, pop_data, site, "Female")
    }
    if ("Male" %in% input$crude_mort_sex_select) {
      crude_mort_male <- compute_crude_mortality(mortality_data, pop_data, site, "Male")
    }
    list(both = crude_mort_both, female = crude_mort_female, male = crude_mort_male)
  })
  
  output$avg_crude_mort_both <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Crude Mortality")
    df <- crude_mort_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Mortality Rate (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$crude_mort_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Mortality Rate (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_crude_mort_female <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Crude Mortality")
    df <- crude_mort_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Mortality Rate (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$crude_mort_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Mortality Rate (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_crude_mort_male <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Crude Mortality")
    df <- crude_mort_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Crude Mortality Rate (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$crude_mort_rate, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Crude Mortality Rate (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$crude_mort_line_graph <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "Crude Mortality")
    dflist <- crude_mort_data()
    
    # Initialize an empty ggplot object
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "Crude Mortality Rate per 100,000", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    # Add layers only if data exists and has rows
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = crude_mort_rate, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = crude_mort_rate, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = crude_mort_rate, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = crude_mort_rate, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = crude_mort_rate, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = crude_mort_rate, color = "Male"))
    }
    
    # If no data is plotted, add a message
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Mortality page - Cumulative Mortality
  cum_mort_data <- reactive({
    req(credentials()$user_auth, input$mort_metric == "Cumulative Mortality")
    site <- input$cum_mort_site_select
    cum_mort_both <- NULL
    cum_mort_female <- NULL
    cum_mort_male <- NULL
    if ("Both" %in% input$cum_mort_sex_select) {
      cum_mort_both <- compute_cumulative_mortality(mortality_data, pop_data, site, "Both")
    }
    if ("Female" %in% input$cum_mort_sex_select) {
      cum_mort_female <- compute_cumulative_mortality(mortality_data, pop_data, site, "Female")
    }
    if ("Male" %in% input$cum_mort_sex_select) {
      cum_mort_male <- compute_cumulative_mortality(mortality_data, pop_data, site, "Male")
    }
    list(both = cum_mort_both, female = cum_mort_female, male = cum_mort_male)
  })
  
  output$avg_cum_mort_both <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Cumulative Mortality")
    df <- cum_mort_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Mortality % (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$cum_mort, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Mortality % (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_cum_mort_female <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Cumulative Mortality")
    df <- cum_mort_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Mortality % (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$cum_mort, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Mortality % (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_cum_mort_male <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "Cumulative Mortality")
    df <- cum_mort_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average Cumulative Mortality % (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$cum_mort, na.rm = TRUE)
      valueBox(round(avg, 1), "Average Cumulative Mortality % (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$cum_mort_line_graph <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "Cumulative Mortality")
    dflist <- cum_mort_data()
    
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "Cumulative Mortality % (0-74 years)", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = cum_mort, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = cum_mort, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = cum_mort, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = cum_mort, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = cum_mort, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = cum_mort, color = "Male"))
    }
    
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Mortality page - ASMR
  asmr_data <- reactive({
    req(credentials()$user_auth, input$mort_metric == "ASMR")
    site <- input$asmr_site_select
    asmr_both <- NULL
    asmr_female <- NULL
    asmr_male <- NULL
    if ("Both" %in% input$asmr_sex_select) {
      asmr_both <- compute_asmr(mortality_data, pop_data, who_weights, site, "Both")
    }
    if ("Female" %in% input$asmr_sex_select) {
      asmr_female <- compute_asmr(mortality_data, pop_data, who_weights, site, "Female")
    }
    if ("Male" %in% input$asmr_sex_select) {
      asmr_male <- compute_asmr(mortality_data, pop_data, who_weights, site, "Male")
    }
    list(both = asmr_both, female = asmr_female, male = asmr_male)
  })
  
  output$avg_asmr_both <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "ASMR")
    df <- asmr_data()$both
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASMR (Both)", icon = icon("users"), color = "green")
    } else {
      avg <- mean(df$asmr, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASMR (Both)", icon = icon("users"), color = "green")
    }
  })
  
  output$avg_asmr_female <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "ASMR")
    df <- asmr_data()$female
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASMR (Female)", icon = icon("venus"), color = "maroon")
    } else {
      avg <- mean(df$asmr, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASMR (Female)", icon = icon("venus"), color = "maroon")
    }
  })
  
  output$avg_asmr_male <- renderValueBox({
    req(credentials()$user_auth, input$mort_metric == "ASMR")
    df <- asmr_data()$male
    if (is.null(df) || nrow(df) == 0) {
      valueBox("N/A", "Average ASMR (Male)", icon = icon("mars"), color = "blue")
    } else {
      avg <- mean(df$asmr, na.rm = TRUE)
      valueBox(round(avg, 1), "Average ASMR (Male)", icon = icon("mars"), color = "blue")
    }
  })
  
  output$asmr_line_graph <- renderPlot({
    req(credentials()$user_auth, input$mort_metric == "ASMR")
    dflist <- asmr_data()
    
    # Initialize an empty ggplot object
    p <- ggplot() +
      theme_minimal() +
      labs(x = "Year", y = "ASMR per 100,000", color = "Sex") +
      scale_color_manual(values = c("Both" = "black", "Female" = "#DD1C77", "Male" = "#3182BD")) +
      theme(
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        legend.title = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 12, color = "black")
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty(n = 10), labels = scales::label_number(accuracy = 1))
    
    # Add layers only if data exists and has rows
    if (!is.null(dflist$both) && nrow(dflist$both) > 0) {
      p <- p + geom_line(data = dflist$both, aes(x = year, y = asmr, color = "Both"), size = 1) +
        geom_point(data = dflist$both, aes(x = year, y = asmr, color = "Both"))
    }
    if (!is.null(dflist$female) && nrow(dflist$female) > 0) {
      p <- p + geom_line(data = dflist$female, aes(x = year, y = asmr, color = "Female"), size = 1) +
        geom_point(data = dflist$female, aes(x = year, y = asmr, color = "Female"))
    }
    if (!is.null(dflist$male) && nrow(dflist$male) > 0) {
      p <- p + geom_line(data = dflist$male, aes(x = year, y = asmr, color = "Male"), size = 1) +
        geom_point(data = dflist$male, aes(x = year, y = asmr, color = "Male"))
    }
    
    # If no data is plotted, add a message
    if (is.null(dflist$both) && is.null(dflist$female) && is.null(dflist$male)) {
      p <- p + annotate("text", x = 0.5, y = 0.5, label = "No Data Available", size = 5)
    }
    
    p
  })
  
  # Update selectInputs for top 5 cancers
  observe({
    req(credentials()$user_auth)
    # Get top 5 from mortality data
    top5_mort <- mortality_data %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(siteiarc)
    
    updateSelectInput(session, "mort_parish_cancer_both", choices = top5_mort, selected = top5_mort[1])
    updateSelectInput(session, "mort_parish_cancer_male", choices = top5_mort, selected = top5_mort[1])
    updateSelectInput(session, "mort_parish_cancer_female", choices = top5_mort, selected = top5_mort[1])
  })
  
  # BOTH SEXES - All Cancers
  output$mort_parish_map_both_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, "All cancers", "Both")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      "Mortality Rate - All Cancers (Both Sexes)")
  })
  
  # BOTH SEXES - Top 5
  output$mort_parish_map_both_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$mort_parish_cancer_both)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, 
                                            input$mort_parish_cancer_both, "Both")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      paste("Mortality Rate -", input$mort_parish_cancer_both, "(Both Sexes)"))
  })
  
  # MALES - All Cancers
  output$mort_parish_map_male_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, "All cancers", "Male")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      "Mortality Rate - All Cancers (Males)")
  })
  
  # MALES - Top 5
  output$mort_parish_map_male_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$mort_parish_cancer_male)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, 
                                            input$mort_parish_cancer_male, "Male")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      paste("Mortality Rate -", input$mort_parish_cancer_male, "(Males)"))
  })
  
  # FEMALES - All Cancers
  output$mort_parish_map_female_all <- renderLeaflet({
    req(credentials()$user_auth)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, "All cancers", "Female")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      "Mortality Rate - All Cancers (Females)")
  })
  
  # FEMALES - Top 5
  output$mort_parish_map_female_top5 <- renderLeaflet({
    req(credentials()$user_auth)
    req(input$mort_parish_cancer_female)
    parish_mort <- compute_parish_mortality(mortality_data, parish_pop_long, who_weights, 
                                            input$mort_parish_cancer_female, "Female")
    create_parish_map(parish_shapefile, parish_mort, "mortality_rate", 
                      paste("Mortality Rate -", input$mort_parish_cancer_female, "(Females)"))
  })
  
  # Data quality calculations (for all years)
  data_quality_by_year <- reactive({
    req(credentials()$user_auth)
    df <- data %>%
      group_by(dxyr) %>%
      summarise(
        n = n(),
        mv_count = sum(grepl("Hx|Cytology|Lab|Haem", basis, ignore.case = TRUE), na.rm = TRUE),
        dco_count = sum(basis == "DCO", na.rm = TRUE),
        ill_def_count = sum(grepl("C76|C80|UNKNOWN", primarysite, ignore.case = TRUE) |
                              grepl("C76|C80", top, ignore.case = TRUE), na.rm = TRUE),
        topo_morph_count = sum(mapply(function(top, icd10, morph) {
          !is.na(top) && !is.na(icd10) && !is.na(morph) && morph != "" &&
            grepl(top, icd10, ignore.case = TRUE) &&
            !grepl("Neoplasm, malignant|NOS", morph, ignore.case = TRUE)
        }, top, icd10, morph), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        mv_prop = mv_count / n * 100,
        dco_prop = dco_count / n * 100,
        ill_def_prop = ill_def_count / n * 100,
        topo_morph_prop = topo_morph_count / n * 100
      ) %>%
      arrange(dxyr)
  })
  
  # MV% Bar Graph
  output$mv_bar <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    ggplot(by_year, aes(x = factor(dxyr), y = mv_prop)) +
      geom_bar(stat = "identity", fill = "#b2df8a") +
      geom_text(aes(label = sprintf("%.1f", mv_prop)), vjust = -0.5, size = 4) +
      labs(title = "Microscopic Verification (MV%) by Year", x = "Year", y = "MV%") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  })
  
  # MV% SPC Chart
  output$mv_spc <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    qcc(by_year$mv_prop / 100, sizes = by_year$n, type = "p",
        xlab = "Year", ylab = "MV Proportion", title = "SPC p-Chart for MV% Over Years",
        labels = by_year$dxyr)
  })
  
  # DCO% Bar Graph
  output$dco_bar <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    ggplot(by_year, aes(x = factor(dxyr), y = dco_prop)) +
      geom_bar(stat = "identity", fill = "#fb9a99") +
      geom_text(aes(label = sprintf("%.1f", dco_prop)), vjust = -0.5, size = 4) +
      labs(title = "Death Certificate Only (DCO%) by Year", x = "Year", y = "DCO%") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  })
  
  # DCO% SPC Chart
  output$dco_spc <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    qcc(by_year$dco_prop / 100, sizes = by_year$n, type = "p",
        xlab = "Year", ylab = "DCO Proportion", title = "SPC p-Chart for DCO% Over Years",
        labels = by_year$dxyr)
  })
  
  # Ill-Defined Sites% Bar Graph
  output$ill_def_bar <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    ggplot(by_year, aes(x = factor(dxyr), y = ill_def_prop)) +
      geom_bar(stat = "identity", fill = "#fdbf6f") +
      geom_text(aes(label = sprintf("%.1f", ill_def_prop)), vjust = -0.5, size = 4) +
      labs(title = "Ill-Defined Sites% by Year", x = "Year", y = "Ill-Defined Sites%") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  })
  
  # Ill-Defined Sites% SPC Chart
  output$ill_def_spc <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    qcc(by_year$ill_def_prop / 100, sizes = by_year$n, type = "p",
        xlab = "Year", ylab = "Ill-Defined Sites Proportion", title = "SPC p-Chart for Ill-Defined Sites% Over Years",
        labels = by_year$dxyr)
  })
  
  # Topo-Morph Consistency% Bar Graph
  output$topo_morph_bar <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    ggplot(by_year, aes(x = factor(dxyr), y = topo_morph_prop)) +
      geom_bar(stat = "identity", fill = "#cab2d6") +
      geom_text(aes(label = sprintf("%.1f", topo_morph_prop)), vjust = -0.5, size = 4) +
      labs(title = "Topography-Morphology Consistency% by Year", x = "Year", y = "Topo-Morph Consistency%") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  })
  
  # Topo-Morph Consistency% SPC Chart
  output$topo_morph_spc <- renderPlot({
    req(credentials()$user_auth)
    by_year <- data_quality_by_year()
    qcc(by_year$topo_morph_prop / 100, sizes = by_year$n, type = "p",
        xlab = "Year", ylab = "Topo-Morph Consistency Proportion", title = "SPC p-Chart for Topo-Morph Consistency% Over Years",
        labels = by_year$dxyr)
  })
  
  # SPC Interpretations
  output$spc_interpretation_mv <- renderUI({
    req(credentials()$user_auth)
    HTML("
      <p>The Statistical Process Control (SPC) p-chart monitors the proportion of cases with microscopic verification (MV%) over time to assess the stability and quality of cancer registry data. Key elements include:</p>
      <ul>
        <li><strong>Centerline (CL):</strong> Represents the average MV% across all years, indicating the expected proportion under stable conditions.</li>
        <li><strong>Control Limits (UCL/LCL):</strong> The Upper Control Limit (UCL) and Lower Control Limit (LCL) define the range of expected variation based on statistical norms (typically ±3 standard deviations). Points within these limits suggest normal variation.</li>
        <li><strong>Data Points:</strong> Each point represents the MV% for a given year, calculated as the number of microscopically verified cases divided by total cases.</li>
      </ul>
      <p>How to interpret the chart:</p>
      <ul>
        <li><strong>Points within control limits:</strong> Indicate that MV% is stable and within expected variation, suggesting consistent data quality processes.</li>
        <li><strong>Points outside control limits:</strong> Suggest unusual variation, potentially due to changes in diagnostic practices, data collection issues, or errors. For example, a point above the UCL may indicate improved verification processes, while a point below the LCL may signal under-reporting of verified cases.</li>
        <li><strong>Patterns or trends:</strong> Consistent trends (e.g., several consecutive points increasing or decreasing) or shifts (e.g., multiple points above/below the centerline) may indicate systematic changes in registry processes, such as new diagnostic technologies or coding errors.</li>
      </ul>
      <p>Practical implications: A stable MV% (e.g., 80–90% as per IARC standards) with most points within control limits suggests reliable data quality. Out-of-control points or trends warrant investigation into data collection, coding practices, or external factors (e.g., changes in pathology services).</p>
    ")
  })
  
  output$spc_interpretation_dco <- renderUI({
    req(credentials()$user_auth)
    HTML("
      <p>The Statistical Process Control (SPC) p-chart monitors the proportion of Death Certificate Only (DCO%) cases over time to assess the completeness of cancer registry data. Key elements include:</p>
      <ul>
        <li><strong>Centerline (CL):</strong> Represents the average DCO% across all years, indicating the expected proportion under stable conditions.</li>
        <li><strong>Control Limits (UCL/LCL):</strong> The Upper Control Limit (UCL) and Lower Control Limit (LCL) define the range of expected variation based on statistical norms (typically ±3 standard deviations). Points within these limits suggest normal variation.</li>
        <li><strong>Data Points:</strong> Each point represents the DCO% for a given year, calculated as the number of DCO cases divided by total cases.</li>
      </ul>
      <p>How to interpret the chart:</p>
      <ul>
        <li><strong>Points within control limits:</strong> Indicate that DCO% is stable, suggesting consistent case ascertainment processes.</li>
        <li><strong>Points outside control limits:</strong> Suggest unusual variation, potentially due to incomplete case reporting or changes in death certificate integration. A point above the UCL may indicate reliance on death certificates, while a point below the LCL may reflect improved case capture.</li>
        <li><strong>Patterns or trends:</strong> Consistent trends or shifts may indicate systematic changes, such as improved hospital reporting or issues with data linkage.</li>
      </ul>
      <p>Practical implications: A low and stable DCO% (e.g., <5% as per IARC standards) suggests high-quality data ascertainment. High or increasing DCO% warrants investigation into case-finding procedures.</p>
    ")
  })
  
  output$spc_interpretation_ill_def <- renderUI({
    req(credentials()$user_auth)
    HTML("
      <p>The Statistical Process Control (SPC) p-chart monitors the proportion of cases with ill-defined primary sites (Ill-Defined Sites%) over time to assess the specificity of cancer registry data. Key elements include:</p>
      <ul>
        <li><strong>Centerline (CL):</strong> Represents the average Ill-Defined Sites% across all years, indicating the expected proportion under stable conditions.</li>
        <li><strong>Control Limits (UCL/LCL):</strong> The Upper Control Limit (UCL) and Lower Control Limit (LCL) define the range of expected variation based on statistical norms (typically ±3 standard deviations). Points within these limits suggest normal variation.</li>
        <li><strong>Data Points:</strong> Each point represents the Ill-Defined Sites% for a given year, calculated as the number of cases with ill-defined sites divided by total cases.</li>
      </ul>
      <p>How to interpret the chart:</p>
      <ul>
        <li><strong>Points within control limits:</strong> Indicate that Ill-Defined Sites% is stable, suggesting consistent coding practices.</li>
        <li><strong>Points outside control limits:</strong> Suggest unusual variation, potentially due to poor diagnostic precision or coding errors. A point above the UCL may indicate increased use of vague codes, while a point below the LCL may reflect improved site specification.</li>
        <li><strong>Patterns or trends:</strong> Consistent trends or shifts may indicate systematic issues, such as changes in diagnostic technology or coder training.</li>
      </ul>
      <p>Practical implications: A low and stable Ill-Defined Sites% (e.g., <5% as per IARC standards) suggests high-quality data. High or increasing percentages warrant review of diagnostic and coding processes.</p>
    ")
  })
  
  output$spc_interpretation_topo_morph <- renderUI({
    req(credentials()$user_auth)
    HTML("
      <p>The Statistical Process Control (SPC) p-chart monitors the proportion of cases with consistent topography and morphology (Topo-Morph Consistency%) over time to assess the accuracy of cancer registry data. Key elements include:</p>
      <ul>
        <li><strong>Centerline (CL):</strong> Represents the average Topo-Morph Consistency% across all years, indicating the expected proportion under stable conditions.</li>
        <li><strong>Control Limits (UCL/LCL):</strong> The Upper Control Limit (UCL) and Lower Control Limit (LCL) define the range of expected variation based on statistical norms (typically ±3 standard deviations). Points within these limits suggest normal variation.</li>
        <li><strong>Data Points:</strong> Each point represents the Topo-Morph Consistency% for a given year, calculated as the number of cases with consistent topography and morphology divided by total cases.</li>
      </ul>
      <p>How to interpret the chart:</p>
      <ul>
        <li><strong>Points within control limits:</strong> Indicate that Topo-Morph Consistency% is stable, suggesting reliable coding practices.</li>
        <li><strong>Points outside control limits:</strong> Suggest unusual variation, potentially due to errors in topography or morphology coding. A point above the UCL may indicate improved coding accuracy, while a point below the LCL may reflect inconsistencies.</li>
        <li><strong>Patterns or trends:</strong> Consistent trends or shifts may indicate systematic changes, such as updates to coding standards or training issues.</li>
      </ul>
      <p>Practical implications: A high and stable Topo-Morph Consistency% (e.g., >90%) suggests accurate data coding. Low or decreasing percentages warrant investigation into coding practices or data validation processes.</p>
    ")
  })
  
  # Indicator Definitions
  output$indicator_definitions <- renderUI({
    req(credentials()$user_auth)
    HTML("
      <h4>Definitions of Data Quality Indicators:</h4>
      <ul>
        <li><strong>MV% (Microscopic Verification):</strong> Percentage of cases with microscopic verification, where the diagnosis is confirmed by histology, cytology, laboratory tests, or hematology (basis containing 'Hx', 'Cytology', 'Lab', or 'Haem'). A high MV% (e.g., 80–90% per IARC standards) indicates reliable diagnostic confirmation.</li>
        <li><strong>DCO% (Death Certificate Only):</strong> Percentage of cases identified solely through death certificates, indicating incomplete clinical data. A low DCO% (e.g., <5%) suggests effective case ascertainment.</li>
        <li><strong>Ill-Defined Sites%:</strong> Percentage of cases with ill-defined primary sites (e.g., ICD-O codes C76, C80, or 'UNKNOWN'). A low percentage (e.g., <5%) indicates precise site coding.</li>
        <li><strong>Topo-Morph Consistency%:</strong> Percentage of cases where the topography code (top) matches the ICD-10 code (icd10) and the morphology code (morph) is specific (excludes vague terms like 'Neoplasm, malignant' or 'NOS'). A high percentage (e.g., >90%) indicates accurate and consistent coding.</li>
      </ul>
    ")
  })
  
  # Reports page
  reports_data <- reactive({
    req(credentials()$user_auth)
    data.frame(
      Report_Name = c(
        "Cancer in Barbados 2008: Annual Report of the BNR-Cancer",
        "Cancer in Barbados 2013: Annual Report of the BNR-Cancer",
        "Cancer in Barbados 2014",
        "Cancer in Barbados 2015",
        "Cancer in Barbados: Report 2022",
        "Cancer in Barbados: Report 2024",
        "Cancer in Barbados: Report 2025"
      ),
      Cancer_Reporting_Period = c(
        "2008",
        "2013",
        "2014",
        "2015",
        "2016, 2017, 2018",
        "2019, 2020",
        "2021, 2022"
      ),
      Cancer_Reporting_Period = c(
        "2008",
        "2013",
        "2014",
        "2015",
        "2016, 2017, 2018",
        "2019, 2020",
        "2021, 2022"
      ),
      File_Name = c(
        "BNR-C_ann_rpt_2008_final.pdf",
        "BNR-C_ann_rpt_2013_Final.pdf",
        "Cancer Report 2014- Final Draft_20190905.pdf",
        "20220506_BNRAnnualReport2015.pdf",
        "BNR Cancer Annual Report 2022.pdf",
        "BNR Cancer Annual Report 2024_2019 and 2020_Final Draft.pdf",
        "BNR Cancer Annual Report 2025_Years 2021-2022_(FINAL).pdf"
      ),
      stringsAsFactors = FALSE
    )
  })  
  output$reports_table <- DT::renderDataTable({
    req(credentials()$user_auth)
    datatable(
      reports_data(),
      options = list(
        pageLength = 10,
        searching = TRUE,
        dom = 't',
        columnDefs = list(
          list(
            targets = 3,  # Hide File_Name column (index 2, 0-based)
            visible = FALSE
          ),
          list(
            targets = 4,  # Add Download column (index 3, 0-based)
            render = JS(
              "function(data, type, row, meta) {",
              "  return '<a class=\"btn btn-primary btn-sm\" href=\"' + encodeURI(row[4]) + '\" download>Download</a>';",
              "}"
            )
          )
        )
      ),
      escape = FALSE,
      colnames = c("Report Name", "Cancer Reporting Period", "", "Download")
    )
  })  
  
  # PowerPoint Report Generation
  output$generate_ppt_button <- renderUI({
    req(credentials()$user_auth)
    div(
      downloadButton("download_ppt", "Generate PowerPoint Report", 
                     class = "btn btn-primary btn-lg", 
                     style = "margin: 10px;"),
      br(),
      p("This will generate a comprehensive PowerPoint presentation with key statistics, 
        visualizations, and data quality indicators from the dashboard.", 
        style = "margin-top: 10px; color: #666;")
    )
  })
  
  output$download_ppt <- downloadHandler(
    filename = function() {
      paste0("BNR_Cancer_Registry_Report_", Sys.Date(), ".pptx")
    },
    content = function(file) {
      # Show progress
      withProgress(message = 'Generating PowerPoint Report...', value = 0, {
        incProgress(0.1, detail = "Initializing...")
        
        # Check if required packages are loaded
        if (!requireNamespace("officer", quietly = TRUE) || !requireNamespace("rvg", quietly = TRUE)) {
          stop("Required packages 'officer' and 'rvg' are not installed.")
        }
        
        incProgress(0.3, detail = "Processing data...")
        
        # Generate the PowerPoint
        tryCatch({
          ppt <- create_powerpoint_report(data, mortality_data, pop_data, who_weights)
          incProgress(0.8, detail = "Creating file...")
          
          # Save the presentation
          print(ppt, target = file)
          incProgress(1, detail = "Complete!")
          
        }, error = function(e) {
          # Create error file
          cat("Error generating PowerPoint report:", e$message, file = file)
          stop("Failed to generate PowerPoint report: ", e$message)
        })
      })
    },
    contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  )
  
  
  # Survival page
  year_filtered <- reactive({
    req(credentials()$user_auth)
    df <- data
    if (input$surv_year_select != "All") {
      df <- df %>% filter(dxyr == as.integer(input$surv_year_select))
    }
    df <- df %>%
      mutate(
        dx_date = as.Date(parse_incidence(IncidenceDate)),
        end_date = if_else(deceased == "dead", as.Date(dmy(dod), quiet = TRUE), as.Date(dmy(dlc), quiet = TRUE)),
        event = if_else(deceased == "dead", 1, 0),
        time_days = as.numeric(difftime(end_date, dx_date, units = "days"))
      ) %>%
      filter(!is.na(dx_date) & !is.na(end_date) & !is.na(time_days) & time_days >= 0)
    df
  })
  
  surv_per_site_both <- reactive({
    req(credentials()$user_auth)
    df <- year_filtered()
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow)) %>%
      filter(cases >= 5)  # Only include sites with 5 or more cases
    
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      dplyr::select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  surv_per_site_male <- reactive({
    req(credentials()$user_auth)
    df <- year_filtered() %>% filter(sex == "male")
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow)) %>%
      filter(cases >= 5)  # Only include sites with 5 or more cases
    
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      dplyr::select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  surv_per_site_female <- reactive({
    req(credentials()$user_auth)
    df <- year_filtered() %>% filter(sex == "female")
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow)) %>%
      filter(cases >= 5)  # Only include sites with 5 or more cases
    
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      dplyr::select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  output$top_survival_both <- DT::renderDataTable({
    req(credentials()$user_auth)
    surv_per_site_both()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_survival_male <- DT::renderDataTable({
    req(credentials()$user_auth)
    surv_per_site_male()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_survival_female <- DT::renderDataTable({
    req(credentials()$user_auth)
    surv_per_site_female()
  }, options = list(pageLength = 10, searching = FALSE))
  
  surv_filtered_data <- reactive({
    req(credentials()$user_auth)
    df <- data
    if (input$surv_year_select != "All") {
      df <- df %>% filter(dxyr == as.integer(input$surv_year_select))
    }
    if (input$surv_site_select != "All") {
      df <- df %>% filter(siteiarc == input$surv_site_select)
    }
    df <- df %>%
      mutate(
        dx_date = as.Date(parse_incidence(IncidenceDate)),
        end_date = if_else(deceased == "dead", as.Date(dmy(dod), quiet = TRUE), as.Date(dmy(dlc), quiet = TRUE)),
        event = if_else(deceased == "dead", 1, 0),
        time_days = as.numeric(difftime(end_date, dx_date, units = "days"))
      ) %>%
      filter(!is.na(dx_date) & !is.na(end_date) & !is.na(time_days) & time_days >= 0)
    df
  })
  
  surv_data_with_age <- reactive({
    req(credentials()$user_auth)
    df <- surv_filtered_data()
    df %>%
      mutate(age_band = cut(age, 
                            breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf),
                            labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                       "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                                       "80-84", "85+"),
                            right = FALSE)) %>%
      filter(!is.na(age_band))
  })
  
  surv_by_age <- function(df, time_years) {
    if (nrow(df) == 0) return(data.frame(age_band = character(), surv_prob = numeric()))
    df_nested <- df %>%
      group_by(age_band) %>%
      nest() %>%
      mutate(surv_prob = map_dbl(data, ~{
        if (nrow(.x) < 2) return(NA)  # Require at least 2 cases for survival analysis
        fit <- tryCatch({
          survfit(Surv(time_days, event) ~ 1, data = .x)
        }, error = function(e) {
          return(NULL)
        })
        if (is.null(fit)) return(NA)
        summ <- summary(fit, times = 365.25 * time_years, extend = TRUE)
        if (length(summ$surv) == 0 || is.na(summ$surv[length(summ$surv)])) return(NA)
        summ$surv[length(summ$surv)] * 100
      }))
    df_nested %>%
      filter(!is.na(surv_prob)) %>%
      dplyr::select(age_band, surv_prob)
  }
  
  output$surv_1yr_age <- renderPlot({
    req(credentials()$user_auth)
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 1)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "royalblue4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)) +
        labs(x = "Age Band", y = "1-Year Survival (%)") +
        ylim(0, 100)
    }
  })
  
  output$surv_3yr_age <- renderPlot({
    req(credentials()$user_auth)
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 3)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "springgreen4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)) +
        labs(x = "Age Band", y = "3-Year Survival (%)") +
        ylim(0, 100)
    }
  })
  
  output$surv_5yr_age <- renderPlot({
    req(credentials()$user_auth)
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 5)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "goldenrod4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14)) +
        labs(x = "Age Band", y = "5-Year Survival (%)") +
        ylim(0, 100)
    }
  })
  
  surv_probs <- reactive({
    req(credentials()$user_auth)
    surv_data <- surv_filtered_data()
    if (nrow(surv_data) < 2) return(rep(NA, 3))  # Require at least 2 cases
    fit <- tryCatch({
      survfit(Surv(time_days, event) ~ 1, data = surv_data)
    }, error = function(e) {
      return(NULL)
    })
    if (is.null(fit)) return(rep(NA, 3))
    summ <- summary(fit)
    if (length(summ$time) == 0) return(rep(NA, 3))
    times_days <- c(365.25 * 1, 365.25 * 3, 365.25 * 5)
    probs <- numeric(3)
    for (i in 1:3) {
      t <- times_days[i]
      if (any(summ$time <= t)) {
        idx <- max(which(summ$time <= t))
        probs[i] <- summ$surv[idx] * 100
      } else {
        probs[i] <- summ$surv[length(summ$surv)] * 100
      }
    }
    probs
  })
  
  output$gauge_1yr <- renderPlotly({
    req(credentials()$user_auth)
    percent <- surv_probs()[1]
    if (is.na(percent)) {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "No Data Available", showlegend = FALSE) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE))
    } else {
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = percent,
        title = list(text = "1-Year Survival (%)", font = list(size = 16)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 50), color = "red"),
            list(range = c(50, 75), color = "yellow"),
            list(range = c(75, 100), color = "green")
          )
        ),
        width = 300,
        height = 250
      ) %>%
        layout(margin = list(l = 20, r = 30))
    }
  })
  
  output$gauge_3yr <- renderPlotly({
    req(credentials()$user_auth)
    percent <- surv_probs()[2]
    if (is.na(percent)) {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "No Data Available", showlegend = FALSE) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE))
    } else {
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = percent,
        title = list(text = "3-Year Survival (%)", font = list(size = 16)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 50), color = "red"),
            list(range = c(50, 75), color = "yellow"),
            list(range = c(75, 100), color = "green")
          )
        ),
        width = 300,
        height = 250
      ) %>%
        layout(margin = list(l = 20, r = 30))
    }
  })
  
  output$gauge_5yr <- renderPlotly({
    req(credentials()$user_auth)
    percent <- surv_probs()[3]
    if (is.na(percent)) {
      plot_ly(type = "scatter", mode = "text") %>%
        add_text(x = 0.5, y = 0.5, text = "No Data Available", showlegend = FALSE) %>%
        layout(xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
               yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE))
    } else {
      plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = percent,
        title = list(text = "5-Year Survival (%)", font = list(size = 16)),
        gauge = list(
          axis = list(range = list(0, 100), tickwidth = 1, tickcolor = "darkblue"),
          bar = list(color = "darkblue"),
          bgcolor = "white",
          borderwidth = 2,
          bordercolor = "gray",
          steps = list(
            list(range = c(0, 50), color = "red"),
            list(range = c(50, 75), color = "yellow"),
            list(range = c(75, 100), color = "green")
          )
        ),
        width = 300,
        height = 250
      ) %>%
        layout(margin = list(l = 20, r = 30))
    }
  })
  
  # Update prevalence site selector
  observe({
    updateSelectInput(session, "prev_site_select", choices = c("All cancers", top25_sites), selected = "All cancers")
  })
  
  # Prevalence calculations
  prevalence_data <- reactive({
    req(credentials()$user_auth)
    site <- input$prev_site_select
    compute_prevalence(data, pop_data, site, "Both")
  })
  
  output$num_survivors <- renderValueBox({
    req(credentials()$user_auth)
    survivors <- prevalence_data()$survivors
    valueBox(
      survivors,
      "Cancer Survivors (as of Dec 31, 2022)",
      icon = icon("user-check"),
      color = "green"
    )
  })
  
  output$prevalence_rate <- renderValueBox({
    req(credentials()$user_auth)
    rate <- round(prevalence_data()$prevalence_rate, 2)
    valueBox(
      rate,
      "Prevalence (%)",
      icon = icon("chart-line"),
      color = "blue"
    )
  })
  
  output$total_population <- renderValueBox({
    req(credentials()$user_auth)
    prev_data <- compute_prevalence(data, pop_data, input$prev_site_select, "Both")
    pop_value <- if (is.na(prev_data$pop_total) || prev_data$pop_total == 0) {
      "No Data"
    } else {
      format(prev_data$pop_total, big.mark = ",")
    }
    valueBox(
      pop_value,
      "Total Population",
      icon = icon("users"),
      color = if (pop_value == "No Data") "red" else "purple"
    )
  })
  
  output$prevalence_by_age_sex <- renderPlot({
    req(credentials()$user_auth)
    age_data <- prevalence_data()$age_data
    
    if (nrow(age_data) == 0) {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No Data Available", size = 6) +
        theme_minimal() +
        theme(axis.text = element_blank(), axis.ticks = element_blank())
    } else {
      total_survivors <- sum(age_data$survivors)
      age_data <- age_data %>%
        mutate(percentage = survivors / total_survivors * 100)
      ggplot(age_data, aes(x = age_group, y = percentage, fill = sex)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("female" = "#DD1C77", "male" = "#3182BD")) +
        geom_text(aes(label = round(percentage, 1)), position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 5) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
              axis.text.y = element_text(size = 12),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 11)) +
        labs(title = "Cancer Prevalence by 5-Year Age Groups and Sex", 
             x = "Age Group", y = "Percentage (%)", fill = "Sex")
    }
  })
  
  # Top prevalence tables
  output$top_prevalence_both <- DT::renderDataTable({
    req(credentials()$user_auth)
    get_top_prevalent_cancers(data, "Both")
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_prevalence_female <- DT::renderDataTable({
    req(credentials()$user_auth)
    get_top_prevalent_cancers(data, "Female")
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_prevalence_male <- DT::renderDataTable({
    req(credentials()$user_auth)
    get_top_prevalent_cancers(data, "Male")
  }, options = list(pageLength = 10, searching = FALSE))
  
  # ---------------------------------------------------------------------------
  # PROJECTION outputs
  # ---------------------------------------------------------------------------
  
  # Helper: fit Negative Binomial (with linear fallback) and return a
  # data frame of observed + projected counts with 95% prediction intervals.
  project_site <- function(df, site_label, proj_years = 2023:2027) {
    # Summarise annual counts
    counts_df <- df %>%
      group_by(year = dxyr) %>%
      summarise(cases = n(), .groups = "drop") %>%
      arrange(year)
    
    if (nrow(counts_df) < 3) return(NULL)
    
    fit_df  <- data.frame(year = counts_df$year, cases = counts_df$cases)
    new_df  <- data.frame(year = proj_years)
    all_yrs <- data.frame(year = c(counts_df$year, proj_years))
    
    pred <- tryCatch({
      mod <- MASS::glm.nb(cases ~ year, data = fit_df,
                          control = glm.control(maxit = 200))
      p   <- predict(mod, newdata = all_yrs, type = "link", se.fit = TRUE)
      list(
        fit  = exp(p$fit),
        lo   = exp(p$fit - 1.96 * p$se.fit),
        hi   = exp(p$fit + 1.96 * p$se.fit)
      )
    }, error = function(e) {
      mod <- lm(cases ~ year, data = fit_df)
      p   <- predict(mod, newdata = all_yrs, interval = "prediction", level = 0.95)
      list(
        fit = pmax(p[, "fit"], 0),
        lo  = pmax(p[, "lwr"], 0),
        hi  = pmax(p[, "upr"], 0)
      )
    })
    
    data.frame(
      site   = site_label,
      year   = all_yrs$year,
      cases  = c(counts_df$cases, rep(NA, length(proj_years))),
      fit    = pred$fit,
      lo     = pred$lo,
      hi     = pred$hi,
      period = c(rep("Observed", nrow(counts_df)), rep("Projected", length(proj_years)))
    )
  }
  
  # Reactive: compute projections for all cancers + top 5
  proj_data <- reactive({
    req(credentials()$user_auth)
    
    obs_data <- data %>% filter(siteiarc != "Other and unspecified (O&U)")
    
    # All cancers
    all_proj <- project_site(obs_data, "All Cancers")
    
    # Top 5 sites
    top5 <- obs_data %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      pull(siteiarc)
    
    site_projs <- map_dfr(top5, function(s) {
      project_site(obs_data %>% filter(siteiarc == s), s)
    })
    
    list(all = all_proj, top5 = site_projs, top5_names = top5)
  })
  
  # --- Plot: All Cancers ---
  output$proj_all_cancers <- renderPlotly({
    req(credentials()$user_auth)
    d <- proj_data()$all
    if (is.null(d)) return(plotly_empty())
    
    obs  <- d %>% filter(period == "Observed")
    proj <- d %>% filter(period == "Projected")
    full <- d  # fitted line spans all years
    
    plot_ly() %>%
      # 95% PI ribbon (projected only)
      add_ribbons(data = proj, x = ~year, ymin = ~lo, ymax = ~hi,
                  fillcolor = "rgba(37,52,148,0.15)", line = list(color = "transparent"),
                  name = "95% Prediction Interval", showlegend = TRUE) %>%
      # Fitted trend line (full span)
      add_lines(data = full, x = ~year, y = ~fit,
                line = list(color = "#253494", dash = "dot", width = 2),
                name = "Fitted trend") %>%
      # Observed points
      add_markers(data = obs, x = ~year, y = ~cases,
                  marker = list(color = "#253494", size = 8),
                  name = "Observed") %>%
      # Projected points
      add_markers(data = proj, x = ~year, y = ~fit,
                  marker = list(color = "#e74c3c", size = 8, symbol = "diamond"),
                  name = "Projected") %>%
      layout(
        xaxis = list(title = "Year", tickformat = "d",
                     tickvals = as.list(d$year)),
        yaxis = list(title = "Number of Cases"),
        shapes = list(list(
          type = "line", x0 = 2022.5, x1 = 2022.5,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "grey", dash = "dash", width = 1.5)
        )),
        legend = list(orientation = "h", x = 0, y = -0.2),
        hovermode = "x unified"
      )
  })
  
  # --- Plot: Top 5 Sites ---
  output$proj_top5_sites <- renderPlotly({
    req(credentials()$user_auth)
    d <- proj_data()$top5
    if (is.null(d) || nrow(d) == 0) return(plotly_empty())
    
    palette <- c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd")
    sites   <- unique(d$site)
    
    p <- plot_ly()
    for (i in seq_along(sites)) {
      s     <- sites[i]
      col   <- palette[i]
      obs   <- d %>% filter(site == s, period == "Observed")
      proj  <- d %>% filter(site == s, period == "Projected")
      full  <- d %>% filter(site == s)
      
      p <- p %>%
        add_ribbons(data = proj, x = ~year, ymin = ~lo, ymax = ~hi,
                    fillcolor = paste0(substr(col, 1, 7), "22"),
                    line = list(color = "transparent"),
                    showlegend = FALSE, legendgroup = s) %>%
        add_lines(data = full, x = ~year, y = ~fit,
                  line = list(color = col, dash = "dot", width = 1.8),
                  showlegend = FALSE, legendgroup = s) %>%
        add_markers(data = obs, x = ~year, y = ~cases,
                    marker = list(color = col, size = 7),
                    name = s, legendgroup = s) %>%
        add_markers(data = proj, x = ~year, y = ~fit,
                    marker = list(color = col, size = 7, symbol = "diamond"),
                    showlegend = FALSE, legendgroup = s)
    }
    
    p %>% layout(
      xaxis = list(title = "Year", tickformat = "d"),
      yaxis = list(title = "Number of Cases"),
      shapes = list(list(
        type = "line", x0 = 2022.5, x1 = 2022.5,
        y0 = 0, y1 = 1, yref = "paper",
        line = list(color = "grey", dash = "dash", width = 1.5)
      )),
      legend = list(orientation = "h", x = 0, y = -0.2),
      hovermode = "x unified"
    )
  })
  
  # --- Summary table ---
  output$proj_summary_table <- DT::renderDataTable({
    req(credentials()$user_auth)
    d_all  <- proj_data()$all
    d_top5 <- proj_data()$top5
    
    combined <- bind_rows(d_all, d_top5) %>%
      filter(period == "Projected") %>%
      mutate(
        `Projected Cases` = round(fit),
        `95% PI Lower`    = round(lo),
        `95% PI Upper`    = round(hi)
      ) %>%
      dplyr::select(`Cancer Site` = site, Year = year,
                    `Projected Cases`, `95% PI Lower`, `95% PI Upper`)
    
    DT::datatable(combined,
                  options = list(pageLength = 15, searching = FALSE, dom = "tip"),
                  rownames = FALSE
    ) %>%
      DT::formatStyle("Cancer Site",
                      fontWeight = "bold"
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)