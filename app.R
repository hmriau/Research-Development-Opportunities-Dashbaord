#install.packages("auth0", repos = "https://cloud.r-project.org" )
library(auth0)
library(shiny)
library(bslib)
library(REDCapR)
library(dplyr)
library(DT)
library(plotly)
library(lubridate)
library(bsicons)
library(tidyr)

ui <-   page_navbar(
  title = "Research Development Opportunities",
  theme = bslib::bs_theme(version = 5, bootswatch = "yeti"),
  
  nav_panel(
    "Dashboard",
    div(  # Main container div
      style = "width: 100%; max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      # Value boxes row (stays fixed at top)
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box(
          title = "Opportunities (YTD)",
          value = textOutput("n_opp_ytd"),
          showcase = bs_icon("activity"),
          theme = "primary",
          height = "120px",
          full_screen = FALSE
        ),
        value_box(
          title = "Pursued",
          value = textOutput("n_pursued"),
          showcase = bs_icon("pencil"),
          theme = "secondary",
          height = "120px",
          full_screen = FALSE
        ),
        value_box(
          title = "Successes",
          value = textOutput("n_success"),
          showcase = bs_icon("speedometer2"),
          theme = "success",
          height = "120px",
          full_screen = FALSE
        ),
        gap = "10px",
        class = "mb-4 sticky-top",  # Makes this row stick when scrolling
        style = "background-color: white; z-index: 1000; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
      ),
      
      # Gauge plot
      card(
        card_header("Mean days to decision"),
        div(
          style = "height: 400px;",  # Fixed height container
          plotlyOutput("gauge_plot", height = "100%")
        ),
        class = "mb-4"
      ),
      
      # Monthly plot
      card(
        card_header("Opportunities per Month (2025)"),
        div(
          style = "height: 500px;",  # Fixed height container
          plotlyOutput("monthly_plot", height = "100%")
        ),
        class = "mb-4"
      ),
      
      # Breakdown tabs
      card(
        card_header("Opportunity Breakdown"),
        navset_card_tab(
          nav_panel(
            "By Type",
            div(
              style = "height: 500px;",
              plotlyOutput("type_plot", height = "100%")
            )
          ),
          nav_panel(
            "By Funding Source",
            div(
              style = "height: 500px;",
              plotlyOutput("source_plot", height = "100%")
            )
          )
        ),
        class = "mb-4"
      ),
      
      # Data table
      card(
        card_header("Most Recent Opportunities"),
        div(
          style = "height: 600px; overflow-y: auto;",  # Scrollable container
          DTOutput("data_table")
        )
      )
    )
  ),
  # Tab 2: REDCap Data Editor (placeholder)
  nav_panel(
    "REDCap Data Editor",
    card(
      card_header("REDCap Data Editor", class = "bg-primary text-white"),
      layout_sidebar(
        # Sidebar Panel (controls only)
        sidebar = sidebar(
          width = 350,
          position = "left",
          selectInput("record_id", "Select Record:", choices = NULL),
          uiOutput("field_editor"),
          actionButton("save", "Save Changes", class = "btn-primary mt-3")
        ),
        
        # Main Panel (data display with export buttons)
        card(
          card_header(
            "Record Data",
            div(
              class = "float-end",  # Aligns buttons to right
              downloadButton("export_csv", "CSV", class = "btn-sm btn-success me-1"),
              downloadButton("export_excel", "Excel", class = "btn-sm btn-info")
            ),
            class = "bg-light"
          ),
          card_body(
            DTOutput("record_table"),
            height = "600px",
            max_height = "100%",
            fillable = TRUE
          ),
          full_screen = TRUE
        )
      )
    )
  ),
  
  # Tab 3: Opportunity Scoring (placeholder)
  nav_panel(
    "Opportunity Scoring",
    card(
      card_header("Opportunity Scoring - Coming Soon"),
      p("This tab will help evaluate and score new opportunities in future versions."),
      tags$div(
        class = "text-center",
        tags$img(
          src = "https://cdn-icons-png.flaticon.com/512/3281/3281289.png",
          style = "height: 100px; opacity: 0.5;"
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Reactive values to store both raw and labeled data
  data_store <- reactiveValues(
    labeled = NULL,
    raw = NULL,
    meta = NULL
  )
  
  # Fetch all required data
  observe({
    req(session$userData$auth0_credentials)
    
    # Get labeled data for display
    labeled <- redcap_read_oneshot(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN"),
      raw_or_label = "label"
    )$data
    
    # Get raw data for editing
    raw <- redcap_read_oneshot(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN"),
      raw_or_label = "raw"
    )$data
    
    # Get metadata
    meta <- redcap_metadata_read(
      redcap_uri = Sys.getenv("REDCAP_URL"),
      token = Sys.getenv("REDCAP_TOKEN")
    )$data
    
    data_store$labeled <- labeled
    data_store$raw <- raw
    data_store$meta <- meta
  })
  # Reactive REDCap data connection
  redcap_data <- reactive({
    redcap_uri <- Sys.getenv("REDCAP_URL")
    redcap_token <- Sys.getenv("REDCAP_TOKEN")
    
    if (redcap_uri == "" || redcap_token == "") {
      showNotification("REDCap API credentials not configured", type = "error")
      return(NULL)
    }
    
    result <- tryCatch({
      redcap_read_oneshot(
        redcap_uri = redcap_uri,
        token = redcap_token,
        raw_or_label = "label"
      )
    }, error = function(e) {
      showNotification(paste("REDCap connection failed:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(result) || !result$success) {
      showNotification("Failed to load REDCap data", type = "error")
      return(NULL)
    }
    
    result$data %>%
      filter(opportunity_name != "test") %>%
      mutate(
        decision_date = ymd(decision_date),
        days_opp_decision = as.numeric(decision_date - opportunity_date) + 1,
        opportunity_date = ymd(opportunity_date)  # Ensure proper date format
      )
  })
  
  # Dashboard calculations
  output$n_opp_ytd <- renderText({
    req(redcap_data())
    nrow(redcap_data() %>% 
           filter(opportunity_date >= as.Date("2025-01-01")))
  })
  
  output$n_pursued <- renderText({
    req(redcap_data())
    nrow(redcap_data() %>% 
           filter(pursue_decision == "Yes",
                  opportunity_date >= as.Date("2025-01-01")))
  })
  
  output$n_success <- renderText({
    req(redcap_data())
    nrow(redcap_data() %>% 
           filter(status == "Awarded",
                  opportunity_date >= as.Date("2025-01-01")))
  })
  
  # Gauge plot
  output$gauge_plot <- renderPlotly({
    req(redcap_data())
    current_value <- mean(redcap_data()$days_opp_decision, na.rm = TRUE)
    target_value <- 14
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = current_value,
      delta = list(reference = target_value),
      gauge = list(
        axis = list(range = c(100, 0)),
        bar = list(color = "blue"),
        steps = list(
          list(range = c(100, 60), color = "red"),
          list(range = c(60, 15), color = "yellow"),
          list(range = c(15, 0), color = "green")
        )
      )
    )
  })
  
  # Monthly plot - starting Jan 2025
  output$monthly_plot <- renderPlotly({
    req(redcap_data())
    
    # Create complete month sequence from Jan 2025
    all_months <- seq.Date(
      from = as.Date("2025-01-01"),
      to = max(redcap_data()$opportunity_date, na.rm = TRUE),
      by = "month"
    )
    
    monthly_counts <- redcap_data() %>%
      filter(opportunity_date >= as.Date("2025-01-01")) %>%
      mutate(month = floor_date(opportunity_date, unit = "month")) %>%
      group_by(month) %>%
      summarise(count = n()) %>%
      complete(month = all_months, fill = list(count = 0))  # Fill missing months with 0
    
    plot_ly(
      data = monthly_counts,
      x = ~month,
      y = ~count,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#4E79A7', width = 2),
      marker = list(color = '#4E79A7', size = 8)
    ) %>%
      layout(
        xaxis = list(
          title = "Month",
          range = c(as.Date("2025-01-01"), max(monthly_counts$month, na.rm = TRUE)),
          tickformat = "%b %Y"
        ),
        yaxis = list(title = "Count", rangemode = "tozero"),
        hovermode = "x unified"
      )
  })
  
  
  # Type plot
  output$type_plot <- renderPlotly({
    req(redcap_data())
    counts_by_type <- redcap_data() %>%
      group_by(opportunity_type) %>%
      summarise(count = n())
    
    plot_ly(
      data = counts_by_type,
      x = ~opportunity_type,
      y = ~count,
      type = 'bar'
    ) %>%
      layout(
        xaxis = list(title = "Opportunity Type"),
        yaxis = list(title = "Count")
      )
  })
  
  # Source plot
  output$source_plot <- renderPlotly({
    req(redcap_data())
    counts_by_source <- redcap_data() %>%
      group_by(funding_source) %>%
      summarise(count = n())
    
    plot_ly(
      data = counts_by_source,
      x = ~funding_source,
      y = ~count,
      type = 'bar'
    ) %>%
      layout(
        xaxis = list(title = "Funding Source"),
        yaxis = list(title = "Count")
      )
  })
  # Export handlers
  output$export_csv <- downloadHandler(
    filename = function() paste("opportunities-", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data_store$labeled, file, row.names = FALSE)
  )
  
  output$export_excel <- downloadHandler(
    filename = function() paste("opportunities-", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) writexl::write_xlsx(data_store$labeled, file)
  )
  
  # Data table
  output$data_table <- renderDT({
    req(redcap_data())
    datatable(
      redcap_data(),
      filter = 'top',
      options = list(pageLength = 5, order = list(list(0, 'desc'))),
      rownames = FALSE
    )
  })
  
  # REDCap preview table
  output$redcap_preview <- renderDT({
    req(redcap_data())
    datatable(
      redcap_data() %>% head(10),
      options = list(dom = 't', pageLength = 5),
      rownames = FALSE
    )
  })
}

shinyAppAuth0(ui, server)