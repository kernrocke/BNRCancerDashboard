# app.R
# Required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(lubridate)
library(survival)
library(plotly)
library(tidyr)
library(purrr)

# Read the CSV files
data <- read.csv("data/cancer_2013_2022_bnr.csv", stringsAsFactors = FALSE)
mortality_data <- read.csv("data/cancer_death_2008_2024.csv", stringsAsFactors = FALSE)

# Preprocess data if needed (e.g., convert dates, etc.)
# Assuming dxyr is integer year, siteiarc is character

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
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Incidence", tabName = "incidence", icon = icon("chart-bar")),
      menuItem("Mortality", tabName = "mortality", icon = icon("skull-crossbones")),
      menuItem("Survival", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Projection", tabName = "projection", icon = icon("chart-line")),
      menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
      menuItem("Additional Information", tabName = "additional", icon = icon("info-circle")),
      menuItem("Contact Us", tabName = "contact", icon = icon("envelope"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "bnr_logo.png"),
      tags$style(HTML("
        .female-table .dataTable th { background-color: #FFC1CC !important; }
        .male-table .dataTable th { background-color: #ADD8E6 !important; }
        .both-table .dataTable th { background-color: #90EE90 !important; }
        .male-surv-table .dataTable th { background-color: #ADD8E6 !important; }
        .female-surv-table .dataTable th { background-color: #FFC1CC !important; }
        .main-header .logo { width: 100% !important; text-align: centre; padding-left: 10px; background-color: #253494 !important; color: #FFFFFF !important; }
        .collaborator-logo { display: block; margin: 9 auto 10px; height: 100px; width: auto; object-fit: contain; }
        .collaborator-text { text-align: left; font-size: 16px; font-weight: bold; }
      "))
    ),
    tabItems(
      # Home page with infographic
      tabItem(tabName = "home",
              fluidRow(
                valueBoxOutput("total_cases"),
                valueBoxOutput("home_total_deaths"),
                valueBoxOutput("avg_age")
              ),
              fluidRow(
                valueBoxOutput("pediatric_cases", width = 6),
                valueBoxOutput("elderly_cases", width = 6)
              ),
              fluidRow(
                box(
                  title = "Cases Over Years (2013-2022)",
                  plotOutput("cases_over_years"),
                  width = 6
                ),
                box(
                  title = "Top Cancer Sites (2013-2022)",
                  DT::dataTableOutput("top_sites"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Top 5 Pediatric Cancer Sites (Age < 15) - 2013-2022",
                  DT::dataTableOutput("top5_pediatric_sites"),
                  width = 6
                ),
                box(
                  title = "Top 5 Elderly Cancer Sites (Age ≥ 65) - 2013-2022",
                  DT::dataTableOutput("top5_elderly_sites"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Cases by Parish (2013-2022)",
                  plotOutput("cases_by_parish"),
                  width = 12
                )
              )
      ),
      
      # Incidence page
      tabItem(tabName = "incidence",
              fluidRow(
                column(4,
                       selectInput("year_select", "Select Year:",
                                   choices = c("All", sort(unique(data$dxyr))),
                                   selected = "All")
                ),
                column(4,
                       selectInput("site_select", "Select Cancer Site:",
                                   choices = c("All", sort(unique(data$siteiarc))),
                                   selected = "All")
                )
              ),
              fluidRow(
                valueBoxOutput("num_cases", width = 4),
                valueBoxOutput("num_female_cases", width = 4),
                valueBoxOutput("num_male_cases", width = 4)
              ),
              fluidRow(
                box(
                  title = "Cases by Year",
                  plotOutput("bar_graph"),
                  width = 6
                ),
                box(
                  title = "Cases by Sex",
                  plotOutput("sex_bar_graph"),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Top 10 Incidental Cancers",
                  DT::dataTableOutput("top10_table"),
                  width = 6
                ),
                box(
                  title = "Top 5 Incidental Cancers by Sex",
                  div(class = "female-table",
                      h4("FEMALES"),
                      DT::dataTableOutput("top5_female_table")
                  ),
                  div(class = "male-table",
                      h4("MALES"),
                      DT::dataTableOutput("top5_male_table")
                  ),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Cases by 5-Year Age Bands",
                  plotOutput("cases_by_age_bands"),
                  width = 12
                )
              )
      ),
      
      # Mortality page
      tabItem(tabName = "mortality",
              fluidRow(
                column(4,
                       selectInput("mort_year_select", "Select Year:",
                                   choices = c("All", sort(unique(mortality_data$dodyear))),
                                   selected = "All")
                ),
                column(4,
                       selectInput("mort_site_select", "Select Cancer Site:",
                                   choices = c("All", sort(unique(mortality_data$siteiarc))),
                                   selected = "All")
                )
              ),
              fluidRow(
                valueBoxOutput("num_deaths", width = 6),
                valueBoxOutput("avg_age_death", width = 6)
              ),
              fluidRow(
                box(
                  title = "Deaths by Year",
                  plotOutput("deaths_by_year"),
                  width = 6
                ),
                box(
                  title = "Deaths by Year and Sex",
                  plotOutput("deaths_by_sex"),
                  width = 6
                )
              ),
              fluidRow(
                column(4, 
                       box(title = "Top 10 Cancer Sites (Both Sexes)", 
                           div(class = "both-table", DT::dataTableOutput("top_deaths_both")),
                           width = NULL)
                ),
                column(4, 
                       box(title = "Top 10 Cancer Sites (Females)", 
                           div(class = "female-table", DT::dataTableOutput("top_deaths_female")),
                           width = NULL)
                ),
                column(4, 
                       box(title = "Top 10 Cancer Sites (Males)", 
                           div(class = "male-table", DT::dataTableOutput("top_deaths_male")),
                           width = NULL)
                )
              ),
              fluidRow(
                box(
                  title = "Deaths by 5-Year Age Bands",
                  plotOutput("deaths_by_age_bands"),
                  width = 12
                )
              )
      ),
      
      # Survival page
      tabItem(tabName = "survival",
              h2("Survival Page"),
              fluidRow(
                column(6,
                       selectInput("surv_year_select", "Select Year:",
                                   choices = c("All", sort(unique(data$dxyr))),
                                   selected = "All")
                ),
                column(6,
                       selectInput("surv_site_select", "Select Cancer Site:",
                                   choices = c("All", sort(unique(data$siteiarc))),
                                   selected = "All")
                )
              ),
              fluidRow(
                column(4, plotlyOutput("gauge_1yr")),
                column(4, plotlyOutput("gauge_3yr")),
                column(4, plotlyOutput("gauge_5yr"))
              ),
              fluidRow(
                box(
                  title = "1-Year Survival by Age Band",
                  plotOutput("surv_1yr_age"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "3-Year Survival by Age Band",
                  plotOutput("surv_3yr_age"),
                  width = 12
                )
              ),
              fluidRow(
                box(
                  title = "5-Year Survival by Age Band",
                  plotOutput("surv_5yr_age"),
                  width = 12
                )
              ),
              fluidRow(
                column(4, 
                       box(title = "Top 10 Cancer Sites with Highest 5-Year Survival (Both Sexes)", 
                           div(class = "both-table", DT::dataTableOutput("top_survival_both")),
                           width = NULL)
                ),
                column(4, 
                       box(title = "Top 10 Cancer Sites with Highest 5-Year Survival (Males)", 
                           div(class = "male-surv-table", DT::dataTableOutput("top_survival_male")),
                           width = NULL)
                ),
                column(4, 
                       box(title = "Top 10 Cancer Sites with Highest 5-Year Survival (Females)", 
                           div(class = "female-surv-table", DT::dataTableOutput("top_survival_female")),
                           width = NULL)
                )
              )
      ),
      
      # Placeholder for other pages
      tabItem(tabName = "projection",
              h2("Projection Page"),
              p("Content for projections will be added here.")
      ),
      tabItem(tabName = "reports",
              h2("Reports Page"),
              fluidRow(
                box(
                  title = "Cancer Reports",
                  DT::dataTableOutput("reports_table"),
                  width = 12
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
              h5("Data Collection Methodology"),
              p("Cases were ascertained by trained data abstractors via review of pathological and laboratory data, as well as data from key departments at the Queen Elizabeth Hospital: haematology clinic, the Clara Brathwaite Centre for Oncology & Nuclear Medicine, colposcopy, and death records."),
              p("Following case ascertainment, data were abstracted directly onto encrypted laptops, using the International Agency for Research on Cancer (IARC)’s CanReg software, version 5. For complete information on each tumour, further retrieval from additional sources (e.g., private physicians and clinics) was performed as required. This is necessary as patients may take several pathways to diagnosis, whether accessing initial care through: the general practitioner, a non-governmental organisation (NGO) through breast or prostate screening programs, a specialist physician, or a surgeon. By collecting data from all sources, the most representative incidence date for the tumour can be determined (the first date of definitive diagnosis)."),
              p("Mortality data was entered into a Research electronic data capture (REDCap) database from paper records existing within the Barbados National Registration Department. This allowed the team to conduct death clearance and provides death clearance data to other departments within the Ministry of Health and Wellness."),
              p("The Barbados National Registry continues to make every effort to ensure cancer data is comparable with other registries internationally, as such, we have outlined below the definitions and assumptions used for reporting and the changes made over time:"),
              tags$ol(
                tags$li("The Registry switched from The International Agency for Research on Cancer (IARC) definition of incidence, for 2008 data collection year, to the European Network of Cancer Registries (ENCR) definition which better matched data we had collected for 2013 onward (see Appendix for definitions)."),
                tags$li("Residency is categorised as:", 
                        tags$ol(type = "i",
                                tags$li("Persons living on the island for six months or more"),
                                tags$li("‘Usual residence’ as per the Barbados Statistical Services definition (See Appendix)"),
                                tags$li("All persons registered with the Electoral and Boundaries Commission"),
                                tags$li("The address listed on the death certificate if no other information available")
                        )
                ),
                tags$li("Only malignant tumours are for ASIRs are included in this report, per international standards. The summary tables include both malignant tumours and cervical carcinoma in situ. Notes accompanying the tables will guide readers accordingly."),
                tags$li("Nationally reported annual numbers of cancer deaths, presented by the Ministry of Health and Wellness, may differ from numbers of deaths and age-standardised mortality rates (ASMRs) reported by the BNR. MHW reports based on underlying cause of death and BNR reports all cases with cancer listed on the death certificate. All cases with cancer listed as a cause-of-death are treated as a death certificate notification and are investigated to determine the year of incidence.")
              ),
              h4(tags$strong("Data Analysis")),
              p("In order to share data and make it comparable to other countries and year-to-year, the BNR must maintain quality. We engage several tools for standardising and formatting variables, checking for accuracy, duplicates and missing data as well as performing preliminary analysis. Data Management and Analysis were performed using the International Association for Research in Cancer software: IARCcrgTools version 2.12 (by J. Ferlay, Section of Cancer Surveillance, International Agency for Research on Cancer, Lyon, France), Stata version 17.1 (StataCorp., College Station, TX, USA), CanReg5 database version 5.43 (International Agency for Research in Cancer, Lyon, France), Research electronic data capture (REDCap), Version 12.3.3, the SEER Hematopoietic database (Surveillance, Epidemiology and End Results (SEER) Program [www.seer.cancer.gov] Hematopoietic and Lymphoid Database, Version 2.1 data released 05/23/2012. National Cancer Institute, DCCPS, Surveillance Research Program).")
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
                column(8,
                       img(src = "moh_logo.png", class = "collaborator-logo"),
                       p(class = "collaborator-text", "Ministry of Health and Wellness")
                ),
                column(8,
                       img(src = "cdcc_logo.png", class = "collaborator-logo"),
                       p(class = "collaborator-text", "The George Alleyne Chronic Disease Research Centre")
                ),
                column(8,
                       img(src = "cahir_logo.png", class = "collaborator-logo"),
                       p(class = "collaborator-text", "The Caribbean Institute for Health Research")
                ),
                column(8,
                       img(src = "uwi_logo.png", class = "collaborator-logo"),
                       p(class = "collaborator-text", "The University of the West Indies, Cave Hill Campus")
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Home infographic calculations
  output$total_cases <- renderValueBox({
    valueBox(
      nrow(data),
      "Total Cases (2013-2022)",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # New value box for total deaths on the home page
  output$home_total_deaths <- renderValueBox({
    valueBox(
      nrow(mortality_data),
      "Total Deaths (2008-2024)",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$avg_age <- renderValueBox({
    avg_age <- round(mean(data$age, na.rm = TRUE), 1)
    valueBox(
      avg_age,
      "Average Age at Diagnosis",
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$pediatric_cases <- renderValueBox({
    pediatric_pct <- round(100 * sum(data$age < 15, na.rm = TRUE) / nrow(data), 1)
    valueBox(
      paste0(pediatric_pct, "%"),
      "Pediatric Cases (Age < 15)",
      icon = icon("child"),
      color = "purple"
    )
  })
  
  output$elderly_cases <- renderValueBox({
    elderly_pct <- round(100 * sum(data$age >= 65, na.rm = TRUE) / nrow(data), 1)
    valueBox(
      paste0(elderly_pct, "%"),
      "Elderly Cases (Age ≥ 65)",
      icon = icon("user-plus"),
      color = "orange"
    )
  })
  
  output$cases_over_years <- renderPlot({
    data %>%
      group_by(dxyr) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = dxyr, y = cases)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_text(aes(label = round(cases, 1), y = cases * 1.01), vjust = -0.5, size = 4) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cancer Cases Over Years", x = "Year", y = "Number of Cases")
  })
  
  output$top_sites <- DT::renderDataTable({
    data %>%
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  })
  
  output$top5_pediatric_sites <- DT::renderDataTable({
    data %>%
      filter(age < 15, siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$top5_elderly_sites <- DT::renderDataTable({
    data %>%
      filter(age >= 65, siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(5) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  }, options = list(pageLength = 5, searching = FALSE, dom = 't'))
  
  output$cases_by_parish <- renderPlot({
    data %>%
      filter(!is.na(parish), parish != "") %>%
      group_by(parish) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = reorder(parish, -cases), y = cases)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = round(cases, 1), y = cases * 1.01), vjust = -0.5, size = 4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cancer Cases by Parish", x = "Parish", y = "Number of Cases")
  })
  
  # Incidence page
  filtered_data <- reactive({
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
    valueBox(
      nrow(filtered_data()),
      "Number of Cases",
      icon = icon("chart-bar"),
      color = "purple"
    )
  })
  
  output$num_female_cases <- renderValueBox({
    female_cases <- nrow(filtered_data() %>% filter(sex == "female"))
    valueBox(
      female_cases,
      "Female Cases",
      icon = icon("venus"),
      color = "red"
    )
  })
  
  output$num_male_cases <- renderValueBox({
    male_cases <- nrow(filtered_data() %>% filter(sex == "male"))
    valueBox(
      male_cases,
      "Male Cases",
      icon = icon("mars"),
      color = "blue"
    )
  })
  
  output$bar_graph <- renderPlot({
    df <- data
    if (input$site_select != "All") {
      df <- df %>% filter(siteiarc == input$site_select)
    }
    df %>%
      group_by(dxyr) %>%
      summarise(cases = n()) %>%
      ggplot(aes(x = dxyr, y = cases)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_text(aes(label = round(cases, 1), y = cases * 1.01), vjust = -0.5, size = 4) +
      scale_x_continuous(breaks = seq(min(data$dxyr), max(data$dxyr), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cases by Year", x = "Year", y = "Number of Cases")
  })
  
  output$sex_bar_graph <- renderPlot({
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
    df <- filtered_data()
    df %>%
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
      geom_text(aes(label = cases, y = cases * 1.01), vjust = -0.5, size = 4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(title = "Cases by 5-Year Age Bands", x = "Age Band", y = "Number of Cases")
  })
  
  output$top10_table <- DT::renderDataTable({
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
  
  # Mortality page
  filtered_mort_data <- reactive({
    df <- mortality_data
    if (input$mort_year_select != "All") {
      df <- df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df
  })
  
  output$num_deaths <- renderValueBox({
    valueBox(
      nrow(filtered_mort_data()),
      "Number of Deaths (2008-2024)",
      icon = icon("skull"),
      color = "red"
    )
  })
  
  output$avg_age_death <- renderValueBox({
    avg_age <- round(mean(filtered_mort_data()$age, na.rm = TRUE), 1)
    valueBox(
      avg_age,
      "Average Age at Death",
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$deaths_by_year <- renderPlot({
    df <- mortality_data
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df %>%
      group_by(dodyear) %>%
      summarise(deaths = n()) %>%
      ggplot(aes(x = dodyear, y = deaths)) +
      geom_bar(stat = "identity", fill = "red") +
      geom_text(aes(label = deaths, y = deaths * 1.01), vjust = -0.5, size = 4) +
      scale_x_continuous(breaks = seq(min(mortality_data$dodyear), max(mortality_data$dodyear), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(x = "Year", y = "Number of Deaths")
  })
  
  output$deaths_by_sex <- renderPlot({
    df <- mortality_data
    if (input$mort_site_select != "All") {
      df <- df %>% filter(siteiarc == input$mort_site_select)
    }
    df %>%
      group_by(dodyear, sex) %>%
      summarise(deaths = n(), .groups = 'drop') %>%
      ggplot(aes(x = dodyear, y = deaths, color = sex, group = sex)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Female" = "#DD1C77", "Male" = "#3182BD")) +
      scale_x_continuous(breaks = seq(min(mortality_data$dodyear), max(mortality_data$dodyear), by = 1)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(x = "Year", y = "Number of Deaths", color = "Sex")
  })
  
  top_deaths_both <- reactive({
    df <- mortality_data
    if (input$mort_year_select != "All") {
      df <- df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    df %>%
      filter(!is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  })
  
  output$top_deaths_both <- DT::renderDataTable({
    top_deaths_both()
  }, options = list(pageLength = 10, searching = FALSE))
  
  top_deaths_female <- reactive({
    df <- mortality_data
    if (input$mort_year_select != "All") {
      df <- df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    df %>%
      filter(sex == "Female" & !is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  })
  
  output$top_deaths_female <- DT::renderDataTable({
    top_deaths_female()
  }, options = list(pageLength = 10, searching = FALSE))
  
  top_deaths_male <- reactive({
    df <- mortality_data
    if (input$mort_year_select != "All") {
      df <- df %>% filter(dodyear == as.integer(input$mort_year_select))
    }
    df %>%
      filter(sex == "Male" & !is.na(siteiarc) & siteiarc != "" & siteiarc != "Other and unspecified (O&U)") %>%
      count(siteiarc) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, Frequency = n)
  })
  
  output$top_deaths_male <- DT::renderDataTable({
    top_deaths_male()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$deaths_by_age_bands <- renderPlot({
    df <- filtered_mort_data()
    df %>%
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
      geom_text(aes(label = deaths, y = deaths * 1.01), vjust = -0.5, size = 4) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      labs(x = "Age Band", y = "Number of Deaths")
  })
  
  # Reports page
  reports_data <- reactive({
    data.frame(
      Report_Name = c(
        "Cancer in Barbados 2008: Annual Report of the BNR-Cancer",
        "Cancer in Barbados 2013: Annual Report of the BNR-Cancer",
        "Cancer in Barbados 2014",
        "Cancer in Barbados 2015",
        "Cancer in Barbados: Report 2022",
        "Cancer in Barbados: Report 2024"
      ),
      Cancer_Reporting_Period = c(
        "2008",
        "2013",
        "2014",
        "2015",
        "2016, 2017, 2018",
        "2019, 2020"
      ),
      Cancer_Reporting_Period = c(
        "2008",
        "2013",
        "2014",
        "2015",
        "2016, 2017, 2018",
        "2019, 2020"
      ),
      File_Name = c(
        "BNR-C_ann_rpt_2008_final.pdf",
        "BNR-C_ann_rpt_2013_Final.pdf",
        "Cancer Report 2014- Final Draft_20190905.pdf",
        "20220506_BNRAnnualReport2015.pdf",
        "BNR Cancer Annual Report 2022.pdf",
        "BNR Cancer Annual Report 2024_2019 and 2020_Final Draft.pdf"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$reports_table <- DT::renderDataTable({
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
      colnames = c("Repor Name", "Cancer Reporting Period", "", "Download")
    )
  })
  
  # Survival page
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
  
  year_filtered <- reactive({
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
    df <- year_filtered()
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow))
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  surv_per_site_male <- reactive({
    df <- year_filtered() %>% filter(sex == "male")
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow))
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  surv_per_site_female <- reactive({
    df <- year_filtered() %>% filter(sex == "female")
    if (nrow(df) == 0) return(data.frame(`Cancer Site` = character(), `5-Year Survival (%)` = numeric(), Cases = numeric()))
    df_nested <- df %>% 
      filter(siteiarc != "Other and unspecified (O&U)") %>%
      group_by(siteiarc) %>% 
      nest() %>%
      mutate(cases = map_dbl(data, nrow))
    df_nested$surv5 <- map_dbl(df_nested$data, ~{
      if (nrow(.x) < 1) return(NA)
      fit <- survfit(Surv(time_days, event) ~ 1, data = .x)
      summ <- summary(fit, times = 365.25 * 5, extend = TRUE)
      if (length(summ$surv) == 0) return(NA)
      summ$surv[length(summ$surv)] * 100
    })
    df_nested %>%
      filter(!is.na(surv5)) %>%
      select(siteiarc, surv5, cases) %>%
      arrange(desc(surv5)) %>%
      head(10) %>%
      rename(`Cancer Site` = siteiarc, `5-Year Survival (%)` = surv5, Cases = cases) %>%
      mutate(`5-Year Survival (%)` = round(`5-Year Survival (%)`, 2))
  })
  
  output$top_survival_both <- DT::renderDataTable({
    surv_per_site_both()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_survival_male <- DT::renderDataTable({
    surv_per_site_male()
  }, options = list(pageLength = 10, searching = FALSE))
  
  output$top_survival_female <- DT::renderDataTable({
    surv_per_site_female()
  }, options = list(pageLength = 10, searching = FALSE))
  
  surv_filtered_data <- reactive({
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
      select(age_band, surv_prob)
  }
  
  output$surv_1yr_age <- renderPlot({
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 1)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "royalblue4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 4) +
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
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 3)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "springgreen4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 4) +
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
    df <- surv_data_with_age()
    surv_df <- surv_by_age(df, 5)
    if (nrow(surv_df) == 0) {
      ggplot() + annotate("text", x=1, y=1, label="No Data Available") + theme_minimal()
    } else {
      ggplot(surv_df, aes(x = age_band, y = surv_prob)) +
        geom_bar(stat = "identity", fill = "goldenrod4") +
        geom_text(aes(label = round(surv_prob, 1), y = surv_prob + 2), vjust = -0.5, size = 4) +
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
}

# Run the application
shinyApp(ui = ui, server = server)
