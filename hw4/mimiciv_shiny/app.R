library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(stringr)
library(shiny)

satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"
# BigQuery authentication using service account
bq_auth(path = satoken)

con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

transfer_tbl <- tbl(con_bq, "transfers")
procedures_tbl <- tbl(con_bq, "procedures_icd")
dprocedures_tbl <- tbl(con_bq, "d_icd_procedures")
diagnoses_tbl <- tbl(con_bq, "diagnoses_icd")
ddiagnoses_tbl <- tbl(con_bq, "d_icd_diagnoses")
patients_tbl <- tbl(con_bq, "patients")
labevents_tbl <- tbl(con_bq, "labevents")
admissions_tbl <- tbl(con_bq, "admissions")
ditems_tbl <- tbl(con_bq, "d_items")
chartevents_tbl <- tbl(con_bq, "chartevents")

# Load your dataset
mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")

# Define the UI
ui <- fluidPage(
  tabsetPanel(
    # First tab for "Patient characteristics"
    tabPanel("Patient characteristics",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Variable of interest:",
                             choices = c("Last care unit", "Lab events")),
                 checkboxInput(inputId = "removeOutliers",
                               label = "Remove outliers in IQR method for measurements?",
                               value = FALSE)
               ),
               mainPanel(
                 plotOutput("plot1")
               )
             )
    ),
    
    # Second tab for "Patient's ADT and ICU stay information"
    tabPanel("Patient's ADT and ICU stay information",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select a patient"),
                 selectizeInput("PatientID", "Patient ID", choices = NULL, options = list(maxItems = 1)),
                 selectInput("type", "which kind of information", choices = c("ADT", "ICU stay"), selected = "ADT"),
                 actionButton("submit", "Submit")
               ),
               mainPanel(
                 plotOutput("plot2")
               )
             )  
      )
    )
  )


# Define server logic
server <- function(input, output, session) {
  observe({
    req(mimic_icu_cohort)
    updateSelectizeInput(session, "PatientID", choices = mimic_icu_cohort$subject_id, server = TRUE)
  })
  
  output$plot1 <- renderPlot({
    req(input$variable)
    if (input$variable == "Last care unit") {
      # Plot the last care unit with x and y swapped
      mimic_icu_cohort %>%
        ggplot( aes(y = last_careunit)) +
        geom_bar() +
        labs(title = "Last care unit",
             y = "Care unit",
             x = "Count")  
    } else if (input$variable == "Lab events") {
      # Plot the lab events with x and y swapped
      plot_data <- mimic_icu_cohort %>%
        select(sodium, potassium, glucose, creatinine, chloride, bicarbonate) %>%
        pivot_longer(cols = everything(), names_to = "lab", values_to = "value") %>%
        na.omit()
      
      if (input$removeOutliers) {
        # Remove outliers using IQR method
        plot_data <- plot_data %>%
          group_by(lab) %>%
          mutate(outlier = abs(value - median(value)) > 1.5 * IQR(value)) %>%
          filter(!outlier)
      }
      
      ggplot(plot_data, aes(x = value, y = lab)) +  
        geom_boxplot() +
        labs(title = "Lab events",
             x = "Count",
             y = "Value") 
    }
  })
  
  
  output$plot2 <- renderPlot({
    req(input$submit)
    req(input$PatientID)
    if (input$type == "ADT") { 
    sid <- as.integer(input$PatientID)
    
    sid_lab <- labevents_tbl %>%
      filter(subject_id == sid) %>%
      collect()
    
    sid_adt <- transfer_tbl %>%
      filter(subject_id == sid & eventtype != "discharge") %>%
      collect() 
    
    sid_pat <- patients_tbl %>%
      filter(subject_id == sid) %>%
      collect()
    
    sid_hpcd <- procedures_tbl %>%
      filter(subject_id == sid) %>%
      mutate(chartdate = as.POSIXct(chartdate)) %>%
      collect()
    
    sid_ipcd <- dprocedures_tbl %>%
      collect()
    
    sid_pcd <- left_join(sid_hpcd , sid_ipcd, by = "icd_code") %>%
      collect() 
    
    sid_dgn <- diagnoses_tbl %>%
      filter(subject_id == sid) %>%
      collect()
    
    sid_ddgn <- ddiagnoses_tbl %>%
      collect()
    
    top_sid_ddgn <- sid_dgn %>%
      slice_head(n = 3) %>%  
      ungroup() %>%  
      inner_join(sid_ddgn, by = "icd_code")
    
    sid_ad <- admissions_tbl %>%
      filter(subject_id == sid) %>%
      collect()
    
    ggplot() +
      geom_segment(data = sid_adt, aes(
        x = intime, 
        xend = outtime,
        y = "ADT",
        yend = "ADT",
        color = careunit,
        linewidth = str_detect(careunit, "(ICU|CCU)"))) +
      geom_point(data = sid_lab,
                 aes(
                   x = charttime,
                   y = "Lab"),
                 shape = 3) +
      geom_jitter(data = sid_pcd,
                  aes(
                    x = chartdate,
                    y = "Procedure",
                    shape = long_title)) +
      scale_shape_manual(values = c(1:10),
                         labels = unique(sid_pcd$long_title)) +
      guides(shape = guide_legend(ncol = 2)) +
      labs(
        x = "Calendar Time",
        y = "",
        shape = "Procedure", 
        color = "Care Unit",
        title = paste(
          "Patient", sid, ",",
          sid_pat$gender, ",",
          sid_pat$anchor_age, "years old,",
          tolower(sid_ad$race), "\n",
          paste(top_sid_ddgn$long_title[1:3], collapse = "\n")
        )
      ) +
      scale_y_discrete(
        limits = c("Procedure", "Lab", "ADT"),
      ) +
      theme(
        legend.position = "bottom",
        legend.box = "vertical"
      )
  }else if (input$type == "ICU stay") {
    sid <- as.integer(input$PatientID)
    
    sid_ite <- ditems_tbl |>
      filter(abbreviation %in% c("HR", "NBPd", "NBPs", "RR", "Temperature F")) |>
      collect()
    
    sid_cet <- chartevents_tbl |>
      filter(subject_id == sid) |>
      collect() |>
      inner_join(sid_ite, by = "itemid") |>
      mutate(value = as.numeric(value)) 
    
    ggplot(sid_cet) +
      geom_line(aes(x = charttime, y =value,group= itemid, 
                    color = abbreviation), na.rm = T) +
      geom_point(aes(x = charttime, y =value,group= itemid, 
                     color = abbreviation)) +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      labs(x = "Calendar Time", title = paste("Patient", sid, "ICU stays - Vitals")) +
      theme_light() +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1)
      ) }
} )
}

# Create Shiny app
shinyApp(ui, server)
