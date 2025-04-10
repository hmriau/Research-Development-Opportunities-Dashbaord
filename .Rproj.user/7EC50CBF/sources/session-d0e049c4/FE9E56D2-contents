library(auth0)
library(shiny)
library(bslib)
library(REDCapR)
library(dplyr)
library(DT)
library(plotly)
library(lubridate)
library(echarts4r) 
library(purrr)
library(tidyr)

# UI ----------------------------------------------------------------------
ui <- page_navbar(
  id = "tabs",
  title = div(
    "Research Development Opportunities",
    style = "font-family: 'Roboto Slab', serif; font-weight: 700; color: white;"  # Added white text
  ),
 
  theme = bslib::bs_theme(
    version = 5,
    "primary" = "#522E91",
    "secondary" = "#F26649",
    "success" = "#60C2AC",
    "info" = "#73CDE1",
    "warning" = "#FFDF5D",
    "bg" = "#FFFFFF",
    "fg" = "#000000",
    "navbar-bg" = "#522E91",
    "body-color" = "#000000",  # Explicit text color
    "card-cap-bg" = "#FFFFFF", # Match your custom card header
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto Slab"),
    "card-border-width" = "1px",  # More consistent with your design
    "card-border-color" = "#E9ECEF"
  ),
  tags$head(
    tags$script(HTML("
    // Prevent accidental double-clicks on delete button
    $(document).on('click', '#delete', function() {
      $(this).prop('disabled', true);
      setTimeout(function() {
        $('#delete').prop('disabled', false);
      }, 2000);
    });
  ")),
    # Badge handler
    tags$script(HTML("
    Shiny.addCustomMessageHandler('updateScoringBadge', function(count) {
      // ... badge handling code ...
    });
  ")),
    tags$style(HTML("
  /* Base layout */
  html {
    scroll-padding-top: 60px;
  }
  
  /* Navbar styling */
  header.navbar {
    background: linear-gradient(40deg, #F26649 10%, #522E91 90%);
    color: white;
    border-bottom: none;
  }
  
  .navbar-title, 
  .nav-item a {
    color: rgba(255,255,255,0.8);
  }
  
  .nav-item a:hover {
    color: white;
  }
  
  /* Card styling */
  .metrics-card {
    background-color: #F8F9FA;
    border: 1px solid #E9ECEF;
  }
  
  .metrics-card .card-header {
    background-color: white;
    color: #522E91;
    border-bottom: 1px solid #E9ECEF;
    font-family: 'Roboto Slab', serif;
  }
  
  /* Tab styling */
  .nav-tabs {
    border-bottom: 1px solid #dee2e6;
    
    .nav-link {
      color: #522E91;
      
      &.active {
        color: #F26649;
        font-weight: bold;
      }
    }
  }
  
  /* Alert styling */
  .alert {
    padding: 15px;
    border-radius: 4px;
    margin: 15px 0;
    font-family: 'Roboto', sans-serif;
    
    i {
      margin-right: 10px;
    }
    
    &-success {
      background-color: #60C2AC20;
      border-left: 4px solid #60C2AC;
      color: #0d362f;
    }
    
    &-warning {
      background-color: #FFDF5D20;
      border-left: 4px solid #FFDF5D;
      color: #5c4a00;
    }
  }
  
  /* Plot containers */
  .plot-container {
    width: 100%;
    height: 100%;
    min-height: 400px;
    
    .js-plotly-plot {
      width: 100% !important;
      height: 100% !important;
    }
  }
  
  /* Badges */
  .badge {
    font-size: 0.75em;
    padding: 3px 6px;
    border-radius: 12px;
    vertical-align: middle;
  }
  
  /* Responsive adjustments */
  @media (max-width: 768px) {
    .plot-container {
      min-height: 300px;
    }
  }
"))
  ),
  nav_panel(
    "Dashboard",
    card(
      class = "metrics-card",  # Add this class
      card_header(
        "Metrics",
        actionButton("refresh_data", "Refresh Data", 
                     icon = icon("sync"), 
                     class = "btn-sm float-end")
      ),
      layout_columns(
        # Your value boxes here
        value_box(
          title = "Opportunities",
          value = textOutput("n_opp"),
          showcase = bsicons::bs_icon("activity"),
          theme = "primary",
          class = "h-100"
        ),
        value_box(
          title = "Pursued",
          value = textOutput("n_pursued"),
          showcase = bsicons::bs_icon("pencil"),
          theme_color = "secondary",  # Uses your defined secondary color (#F26649)
          class = "h-100"
        ),
        value_box(
          title = "Successes",
          value = textOutput("n_success"), 
          showcase = bsicons::bs_icon("speedometer2"),
          theme_color = "success",  # Uses your defined success color (#60C2AC)
          class = "h-100"
        ),
        col_widths = c(4, 4, 4)
      ),
      # Visualization cards with auto-height
      layout_columns(
        card(
          card_header("Average Decision Time"),
          div(class = "plot-container",
              plotlyOutput("gauge_plot")
          )
        ),
        card(
          card_header("Opportunity Breakdown"),
          div(class = "plot-container",
              navset_card_tab(
                nav_panel("By Type", plotlyOutput("type_plot")),
                nav_panel("By Source", plotlyOutput("source_plot"))
              )
          )
        ),
        col_widths = c(6, 6)
      ),
      # Time series card with more height
      card(
        card_header("Monthly Trends"),
        div(style = "height: auto; min-height: 250px;",  # Taller minimum height
            plotlyOutput("monthly_plot")
        )
      )
    )
  ),
  nav_panel(
    "REDCap Data Editor",
    card(
      card_header("Data Editor"),  # Now minimal
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          position = "left",
          selectInput("record_id", "Select Record:", choices = NULL),
          uiOutput("field_editor"),
          actionButton("save", "Save Changes", class = "btn-secondary mt-3"),
          actionButton("delete", "Delete Record", 
                       class = "btn-danger mt-3", 
                       icon = icon("trash-can"))
        ),
        card(
          card_header("Record Data"),
          DTOutput("record_table"),
          height = "600px"
        )
      )
    )
  ),
  nav_panel(
    span(
      "Opportunity Scoring",
      span(class = "badge bg-danger", 
           id = "scoring-badge",
           style = "display: none; margin-left: 8px;")  # Badge element
    ),
    card(
      card_header("Opportunity Scoring"),
      uiOutput("scoring_alert"),
      layout_sidebar(
        sidebar = sidebar(
          width = 350,
          position = "left",
          selectInput("reviewer", "Select Your Name:", 
                      choices = c("Penny", "Janice", "Steve", "Anna", "Chris")),
          selectInput("selected_opportunity", "Select an Opportunity:", choices = NULL),
          
          sliderInput("feasibility", "Feasibility", min = 1, max = 5, value = 3),
          sliderInput("impact", "Impact", min = 1, max = 5, value = 3),
          sliderInput("alignment", "Strategic Fit", min = 1, max = 5, value = 3),
          
          radioButtons("decision", "Pursue This Opportunity?", 
                       choices = c("Yes", "No"), inline = TRUE),
          
          actionButton("save_score", "Submit Score", class = "btn-primary"),
          actionButton("clear_scores", "Clear My Scores", 
                       class = "btn-outline-danger mt-2"),
          selectInput("status_update", "Set Opportunity Status:",
                      choices = NULL, selected = NULL),
          actionButton("submit_status", "Submit Status", class = "btn-success mt-2")
        ),
        navset_card_tab(
          nav_panel("Opportunities", DTOutput("scoring_table")),
          nav_panel("Aggregated Scores", DTOutput("summary_table")),
          nav_panel("Radar Chart", echarts4rOutput("radar_plot"))
        )
      )
    )
  )
)
# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Initialize data stores
  data_store <- reactiveValues(
    labeled = data.frame(),  # Labeled data for display
    raw = data.frame(),      # Raw data for saving back to REDCap
    meta = data.frame(),     # Metadata for field types/choices
    last_update = NULL
  )
  
  # Load data from REDCap
  load_data <- function() {
    tryCatch({
      # Get labeled data for display
      labeled <- redcap_read_oneshot(
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN"),
        raw_or_label = "label"
      )$data
      
      # Get raw data for saving back
      raw <- redcap_read_oneshot(
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN"),
        raw_or_label = "raw"
      )$data
      
      # Get metadata for field types
      meta <- redcap_metadata_read(
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN")
      )$data
      
      if (nrow(labeled) == 0) stop("No data returned from REDCap")
      
      data_store$labeled <- labeled
      data_store$raw <- raw
      data_store$meta <- meta
      data_store$last_update <- Sys.time()
      
      # Update record selector
      updateSelectInput(session, "record_id", choices = raw$record_id)
      
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  }
  
  # Initial data load
  observeEvent(TRUE, once = TRUE, {
    load_data()
  })
  
  # Dashboard outputs
  output$n_opp <- renderText({
    req(data_store$labeled)
    nrow(data_store$labeled)
  })
  
  output$n_pursued <- renderText({
    req(data_store$labeled)
    sum(data_store$labeled$pursue_decision == "Yes", na.rm = TRUE)
  })
  
  output$n_success <- renderText({
    req(data_store$labeled)
    sum(data_store$labeled$status == "Awarded", na.rm = TRUE)
  })
  
  output$data_table <- renderDT({
    req(data_store$labeled)
    datatable(data_store$labeled, options = list(pageLength = 5))
  })
  
  output$scoring_alert <- renderUI({
    req(data_store$labeled)
    
    # Count opportunities waiting for scoring
    scorable_count <- data_store$labeled %>%
      filter(status == "Waiting support decision") %>%
      nrow()
    
    if (scorable_count > 0) {
      div(class = "alert alert-success",
          icon("circle-exclamation"),
          paste("There are", scorable_count, "opportunities waiting for scoring."),
          style = "margin-bottom: 20px;")
    } else {
      div(class = "alert alert-warning",
          icon("triangle-exclamation"),
          "No opportunities currently require scoring.",
          style = "margin-bottom: 20px;")
    }
  })
  
  output$gauge_plot <- renderPlotly({
    req(data_store$labeled)
    
    data <- data_store$labeled %>%
      mutate(
        decision_date = ymd(decision_date),
        opportunity_date = ymd(opportunity_date),
        days_to_decision = as.numeric(decision_date - opportunity_date)
      ) %>%
      filter(!is.na(days_to_decision))
    
    current_value <- mean(data$days_to_decision, na.rm = TRUE)
    target_value <- 14
    
    plot_ly(
      type = "indicator",
      mode = "gauge+number+delta",
      value = current_value,
      height = 250,  # Fixed height in pixels
      width = "100%", # Full width of container
      number = list(suffix = " days"),
      delta = list(
        reference = target_value,
        increasing = list(color = "#F26649"),
        decreasing = list(color = "#60C2AC")
      ),
      gauge = list(
        axis = list(range = c(0, 30), tickwidth = 1, tickcolor = "#522E91"),
        bar = list(color = "#522E91"),
        bgcolor = "white",
        steps = list(
          list(range = c(25, 30), color = "#F26649"),
          list(range = c(15, 25), color = "#FFDF5D"),
          list(range = c(0, 15), color = "#60C2AC")
        ),
        threshold = list(
          line = list(color = "black", width = 4),
          thickness = 0.75,
          value = target_value
        )
      )
    ) %>%
      layout(
        margin = list(l = 30, r = 30, b = 30, t = 30, pad = 4),
        autosize = FALSE  # Disable autosize when using fixed height
      )
  })
  # Breakdown by type
  # Breakdown by type - corrected version
  output$type_plot <- renderPlotly({
    req(data_store$labeled)
    
    plot_ly(
      data = data_store$labeled,
      x = ~opportunity_type,
      type = "histogram",
      height = 250,  # Slightly taller for better fit
      width = "100%",
      marker = list(
        color = "#522E91",  # Your primary color
        line = list(color = "#FFFFFF", width = 1)  # White borders
      )
    ) %>%
      layout(
        margin = list(l = 60, r = 30, b = 120, t = 30, pad = 4),
        xaxis = list(
          title = "",
          tickangle = -45,
          categoryorder = "total descending"
        ),
        yaxis = list(title = "Count"),
        bargap = 0.1  # Space between bars
      )
  })
  output$source_plot <- renderPlotly({
    req(data_store$labeled)
    
    plot_ly(
      data = data_store$labeled,
      x = ~funding_source,
      type = "histogram",
      height = 250,  # Same height as type plot
      width = "100%",
      marker = list(
        color = "#F26649",  # Your secondary color
        line = list(color = "#FFFFFF", width = 1)  # White borders
      )
    ) %>%
      layout(
        margin = list(l = 60, r = 30, b = 120, t = 30, pad = 4),
        xaxis = list(
          title = "Funding Source",
          tickangle = -45,
          categoryorder = "total descending"
        ),
        yaxis = list(title = "Count"),
        bargap = 0.1
      )
  })
  # Monthly time series
  output$monthly_plot <- renderPlotly({
    req(data_store$labeled)
    
    # Create a safe sequence function
    safe_date_seq <- function(min_date, max_date) {
      if (is.finite(min_date) && is.finite(max_date)) {
        seq(min_date, max_date, by = "month")
      } else {
        as.Date(character())  # Return empty date vector if invalid
      }
    }
    
    # Process data with error handling
    monthly_data <- tryCatch({
      processed <- data_store$labeled %>%
        mutate(
          opportunity_date = ymd(opportunity_date),
          month = floor_date(opportunity_date, "month")
        ) %>%
        filter(!is.na(month))
      
      if (nrow(processed) > 0) {
        min_date <- min(processed$month, na.rm = TRUE)
        max_date <- max(processed$month, na.rm = TRUE)
        
        processed %>%
          count(month) %>%
          complete(
            month = safe_date_seq(min_date, max_date),
            fill = list(n = 0)
          )
      } else {
        data.frame(month = as.Date(character()), n = integer())
      }
    }, error = function(e) {
      data.frame(month = as.Date(character()), n = integer())
    })
    
    # Handle empty data case
    if (nrow(monthly_data) == 0) {
      return(plotly_empty(type = "scatter") %>%
               layout(
                 title = list(text = "No valid date data available"),
                 plot_bgcolor = "#FFFFFF",
                 paper_bgcolor = "#FFFFFF"
               ))
    }
    
    # Create the plot
    plot_ly(
      monthly_data,
      x = ~month,
      y = ~n,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "#522E91", width = 3),
      marker = list(color = "#F26649", size = 8),
      hoverinfo = "text",
      text = ~paste0(
        "Month: ", format(month, "%B %Y"), "\n",
        "Opportunities: ", n
      )
    ) %>%
      layout(
        xaxis = list(
          title = "",
          gridcolor = "#e1e1e1",
          type = "date",
          tickformat = "%b %Y"
        ),
        yaxis = list(
          title = "Number of Opportunities",
          gridcolor = "#e1e1e1",
          zeroline = FALSE
        ),
        hovermode = "x unified",
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(t = 40)
      )
  })
  
  # Data editor outputs
  # TODO dropdown select boxes
  output$record_table <- renderDT({
    req(data_store$labeled)
    datatable(data_store$labeled, 
              filter = 'top',  # Adds filter inputs above each column
              options = list(scrollX = TRUE))
  })
  
  # Dynamic field editor UI
  output$field_editor <- renderUI({
    req(input$record_id, data_store$raw, data_store$meta)
    
    record <- data_store$raw[data_store$raw$record_id == input$record_id, ]
    fields <- setdiff(names(record), "record_id")
    
    lapply(fields, function(field) {
      meta <- data_store$meta[data_store$meta$field_name == field, ]
      
      if (nrow(meta) == 0) {
        return(textInput(field, label = field, value = record[[field]]))
      }
      
      # Handle different field types
      if (meta$field_type %in% c("dropdown", "radio")) {
        choices <- parse_choices(meta$select_choices_or_calculations)
        current_value <- record[[field]]
        
        # Get labeled value for display
        labeled_value <- if (current_value %in% sapply(choices, `[[`, "value")) {
          choices[[which(sapply(choices, `[[`, "value") == current_value)]]$label
        } else {
          current_value
        }
        
        selectInput(
          inputId = field,
          label = field,
          choices = c("", setNames(sapply(choices, `[[`, "value"), 
                                   sapply(choices, `[[`, "label"))),
          selected = current_value
        )
        
      } else if (meta$field_type == "checkbox") {
        choices <- parse_choices(meta$select_choices_or_calculations)
        checkboxGroupInput(
          inputId = field,
          label = field,
          choices = setNames(sapply(choices, `[[`, "value"), 
                             sapply(choices, `[[`, "label")),
          selected = {
            selected <- character(0)
            for (choice in choices) {
              checkbox_name <- paste0(field, "___", choice$value)
              if (record[[checkbox_name]] == "1") {
                selected <- c(selected, choice$value)
              }
            }
            selected
          }
        )
      } else {
        textInput(field, label = field, value = record[[field]])
      }
    })
  })
  
  ## For the scorer
  
  user_scores <- reactiveVal(
    tibble::tibble(
      reviewer = character(),
      opportunity_name = character(),
      feasibility = numeric(),
      impact = numeric(),
      alignment = numeric(),
      decision = character()
    )
  )
  # Update opportunity dropdown choices
  observe({
    req(data_store$labeled)
    
    # Filter for only "Waiting support decision" opportunities
    scorable_count <- data_store$labeled %>%
      filter(status == "Waiting support decision") %>%
      nrow()
    
    # Update both the alert and the tab badge
    session$sendCustomMessage("updateScoringBadge", scorable_count)
    
    filtered_opps <- data_store$labeled %>%
      filter(status == "Waiting support decision") %>%
      pull(opportunity_name) %>%
      unique()
    
    updateSelectInput(session, "selected_opportunity", 
                      choices = filtered_opps)
  })
  
  observe({
    req(data_store$meta)
    
    status_meta <- data_store$meta %>%
      filter(field_name == "status")  # Adjust this if your field name is different
    
    if (nrow(status_meta) == 1 && status_meta$field_type == "dropdown") {
      choices <- parse_choices(status_meta$select_choices_or_calculations)
      
      updateSelectInput(
        session, "status_update",
        choices = setNames(
          sapply(choices, `[[`, "value"),
          sapply(choices, `[[`, "label")
        ),
        selected = choices[[which(sapply(choices, `[[`, "label") == "Development")]]$value  # default
      )
    }
  })
  
  # Save user scores locally
  observeEvent(input$save_score, {
    req(input$selected_opportunity, input$reviewer, input$decision,
        input$feasibility, input$impact, input$alignment)
    
    # Validate inputs
    if (input$selected_opportunity == "") {
      showNotification("Please select an opportunity", type = "error")
      return()
    }
    
    # Create new score entry with explicit types
    new_entry <- tibble::tibble(
      reviewer = as.character(input$reviewer),
      opportunity_name = as.character(input$selected_opportunity),
      feasibility = as.numeric(input$feasibility),
      impact = as.numeric(input$impact),
      alignment = as.numeric(input$alignment),
      decision = as.character(input$decision)
    )
    
    # Get current scores
    current <- isolate(user_scores())
    
    # Update scores
    updated <- current %>%
      filter(!(reviewer == input$reviewer & 
                 opportunity_name == input$selected_opportunity)) %>%
      bind_rows(new_entry)
    
    user_scores(updated)
    
    # Visual feedback
    showNotification(
      paste("Score submitted for", input$selected_opportunity), 
      type = "message"
    )
    
    # Debug output in UI
    output$debug_output <- renderPrint({
      user_scores()
    })
  })
  # Clear scores for current user
  observeEvent(input$clear_scores, {
    current_scores <- user_scores() %>%
      mutate(
        reviewer = as.character(reviewer),
        opportunity_name = as.character(opportunity_name),
        feasibility = as.numeric(feasibility),
        impact = as.numeric(impact),
        alignment = as.numeric(alignment),
        decision = as.character(decision)
      )
    
    updated_scores <- current_scores %>%
      filter(reviewer != input$reviewer)
    
    user_scores(updated_scores)
    showNotification("Your scores have been cleared", type = "message")
  })
  
  # Save status
  
  observeEvent(input$submit_status, {
    req(input$selected_opportunity, input$status_update, data_store$raw)
    
    # Get record_id for the selected opportunity
    record <- data_store$raw %>%
      filter(opportunity_name == input$selected_opportunity)
    
    if (nrow(record) != 1) {
      showNotification("Could not uniquely identify the record", type = "error")
      return()
    }
    
    record_id <- record$record_id
    
    # Create REDCap update
    updated_record <- record
    updated_record$status <- input$status_update
    
    tryCatch({
      result <- redcap_write(
        ds_to_write = updated_record,
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN")
      )
      
      if (result$success) {
        showNotification("Status updated successfully!", type = "message")
        load_data()  # Refresh data
      } else {
        showNotification("Failed to update status", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Display opportunities table
  output$scoring_table <- renderDT({
    req(data_store$labeled)
    
    filtered_data <- data_store$labeled %>%
      filter(status == "Waiting support decision") %>%
      select(record_id, opportunity_name, dev_lead, lead_contact, status)
    
    datatable(filtered_data, 
              options = list(
                scrollX = TRUE,
                pageLength = 10
              ))
  })
  
  # Display aggregated scores
  output$summary_table <- renderDT({
    req(nrow(user_scores()) > 0)
    
    df <- user_scores() %>%
      mutate(across(c(feasibility, impact, alignment), as.numeric))
    
    # Calculate averages
    avg_scores <- df %>%
      group_by(opportunity_name) %>%
      summarise(
        feasibility = round(mean(feasibility, na.rm = TRUE), 2),
        impact = round(mean(impact, na.rm = TRUE), 2),
        alignment = round(mean(alignment, na.rm = TRUE), 2),
        decision = paste(decision[decision != ""], collapse = ", "),
        .groups = "drop"
      ) %>%
      mutate(reviewer = "Average")
    
    # Combine with individual scores
    combined <- df %>%
      select(reviewer, opportunity_name, feasibility, impact, alignment, decision) %>%
      bind_rows(avg_scores) %>%
      arrange(opportunity_name, reviewer)
    
    datatable(combined, options = list(scrollX = TRUE))
  })
  
  # Radar plot using echarts4r (exact implementation from your scoring app)
  plot_data <- reactive({
    req(nrow(user_scores()) > 0)
    
    # Convert all columns to proper types first
    df <- user_scores() %>%
      mutate(
        reviewer = as.character(reviewer),
        opportunity_name = as.character(opportunity_name),
        feasibility = as.numeric(feasibility),
        impact = as.numeric(impact),
        alignment = as.numeric(alignment),
        decision = as.character(decision)
      ) %>%
      # Remove any rows with NA values in critical columns
      filter(
        !is.na(feasibility),
        !is.na(impact), 
        !is.na(alignment),
        !is.na(opportunity_name),
        !is.na(reviewer)
      )
    
    # Calculate averages per reviewer per opportunity
    df <- df %>%
      group_by(opportunity_name, reviewer) %>%
      summarise(
        feasibility = mean(feasibility, na.rm = TRUE),
        impact = mean(impact, na.rm = TRUE),
        alignment = mean(alignment, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Add "Average" row for each opportunity
    avg_data <- df %>%
      group_by(opportunity_name) %>%
      summarise(
        feasibility = mean(feasibility, na.rm = TRUE),
        impact = mean(impact, na.rm = TRUE),
        alignment = mean(alignment, na.rm = TRUE),
        reviewer = "Average",
        .groups = "drop"
      )
    
    # Combine individual and average scores
    combined <- bind_rows(df, avg_data) %>%
      mutate(
        feasibility = round(as.numeric(feasibility), 2),
        impact = round(as.numeric(impact), 2),
        alignment = round(as.numeric(alignment), 2)
      )
    
    # Pivot to long then wide format while preserving opportunity_name
    plot_data_list <- combined %>%
      pivot_longer(
        cols = c(feasibility, impact, alignment),
        names_to = "domain",
        values_to = "score"
      ) %>%
      mutate(
        domain = factor(
          domain,
          levels = c("feasibility", "impact", "alignment"),
          labels = c("Feasibility", "Impact", "Alignment")
        )
      ) %>%
      group_by(opportunity_name) %>%
      group_split() %>%
      map(~ {
        .x %>%
          select(-opportunity_name) %>%
          pivot_wider(
            names_from = reviewer,
            values_from = score
          ) %>%
          mutate(across(where(is.numeric), ~ round(as.numeric(.x), 2)))
      })
    
    names(plot_data_list) <- unique(combined$opportunity_name)
    plot_data_list
  })
  # Render the radar chart with echarts4r (exact working version)
  output$radar_plot <- renderEcharts4r({
    req(input$selected_opportunity, plot_data())
    
    df <- plot_data()[[input$selected_opportunity]]
    req(nrow(df) > 0)
    
    # Extract reviewer names (columns except "domain")
    reviewers <- setdiff(names(df), "domain")
    
    # Define radar axes
    radar_axes <- list(
      list(name = "Feasibility", max = 5, min = 1),
      list(name = "Impact", max = 5, min = 1),
      list(name = "Alignment", max = 5, min = 1)
    )
    
    # Build radar chart with all reviewers
    chart <- df %>%
      e_charts(domain) 
    
    # Add each reviewer's scores as a separate radar series
    for (reviewer in reviewers) {
      chart <- chart %>% 
        e_radar_(reviewer, name = reviewer)
    }
    
    # Final chart formatting
    chart %>%
      e_radar_opts(indicator = radar_axes) %>%
      e_tooltip(trigger = "item") %>%
      e_legend(show = TRUE) %>%
      e_color(color = c("#522E91", "#F26649", "#60C2AC", "#FFDF5D", "#73CDE1")) %>% # Your app's color scheme
      e_title(text = "Opportunity Scores by Reviewer", 
              subtext = "Hover to view individual scores")
  })
 
  ## Observers
  
  observeEvent(input$selected_opportunity, {
    updateSliderInput(session, "feasibility", value = 3)
    updateSliderInput(session, "impact", value = 3)
    updateSliderInput(session, "alignment", value = 3)
    updateRadioButtons(session, "decision", selected = character(0))
  })
  
  
  observeEvent(input$delete, {
    req(input$record_id)
    
    showModal(modalDialog(
      title = "Confirm Deletion",
      paste("Are you sure you want to permanently delete record", input$record_id, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    tryCatch({
      # Delete record from REDCap
      redcap_delete(
        records = input$record_id,
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN")
      )
      
      # Remove modal and refresh data
      removeModal()
      showNotification("Record deleted successfully", type = "message")
      load_data()  # Refresh the data
      
    }, error = function(e) {
      showNotification(paste("Error deleting record:", e$message), type = "error")
    })
  })
  
  # Save changes back to REDCap
  observeEvent(input$save, {
    req(input$record_id, data_store$raw)
    
    updated_record <- data_store$raw[data_store$raw$record_id == input$record_id, ]
    meta <- data_store$meta
    
    for (field in names(updated_record)) {
      if (field == "record_id") next
      
      field_meta <- meta[meta$field_name == sub("___.*", "", field), ]
      
      if (nrow(field_meta) == 0) next
      
      if (field_meta$field_type == "checkbox") {
        # Handle checkbox fields
        choices <- parse_choices(field_meta$select_choices_or_calculations)
        for (choice in choices) {
          checkbox_name <- paste0(field_meta$field_name, "___", choice$value)
          updated_record[[checkbox_name]] <- ifelse(choice$value %in% input[[field_meta$field_name]], "1", "0")
        }
      } else if (field_meta$field_name == field) {
        # Handle regular fields
        updated_record[[field]] <- input[[field]]
      }
    }
    
    tryCatch({
      result <- redcap_write(
        ds_to_write = updated_record,
        redcap_uri = Sys.getenv("REDCAP_URL"),
        token = Sys.getenv("REDCAP_TOKEN")
      )
      
      if (result$success) {
        showNotification("Record saved successfully!", type = "message")
        load_data() # Refresh data
      } else {
        showNotification("Failed to save record", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Error saving record:", e$message), type = "error")
    })
  })
  
  # Helper function to parse REDCap choices
  parse_choices <- function(choices_str) {
    if (is.null(choices_str) || is.na(choices_str) || choices_str == "") {
      return(list())
    }
    
    choices <- strsplit(choices_str, "\\s*\\|\\s*")[[1]]
    lapply(choices, function(choice) {
      parts <- strsplit(trimws(choice), "\\s*,\\s*")[[1]]
      if (length(parts) >= 2) {
        list(value = parts[1], label = paste(parts[-1], collapse = ", "))
      } else {
        list(value = parts[1], label = parts[1])
      }
    })
  }
}

# Run the app
options(shiny.port = 8080)
shinyAppAuth0(ui, server)