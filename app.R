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
    style = "font-family: 'Roboto Slab', serif; font-weight: 700;"
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
    base_font = bslib::font_google("Roboto"),
    heading_font = bslib::font_google("Roboto Slab")
  ),
  tags$head(
    tags$style(HTML("
    /* Original gradient and card styles (unchanged) */
    .bslib-navbar {
      background: linear-gradient(40deg, #F26649 10%, #522E91 90%) !important;
    }
    .card-header.gradient-bg {
      background: linear-gradient(40deg, #F26649 10%, #522E91 90%);
      color: white;
      border: none;
    }
    .card:not(.gradient-card) {
      border: 1px solid #e1e1e1;
      box-shadow: none;
    }
    .card:not(.gradient-card) .card-header {
      background-color: white;
      color: #522E91;
      border-bottom: 1px solid #e1e1e1;
      font-family: 'Roboto Slab', serif;
    }
    
    /* Refined value boxes */
    .value-box {
      border: 1px solid #e1e1e1;
      border-radius: 4px;
      height: 120px; /* Fixed height for consistency */
      margin-bottom: 15px; /* Space between value boxes */
    }
    
    /* Adjusted plot containers */
    .html-widget {
      flex-grow: 1;
      min-height: 250px; /* Reduced from 300px */
      max-height: 350px; /* Added maximum height */
    }
    
    /* Card layout adjustments */
    .card-body {
      display: flex;
      flex-direction: column;
      padding: 15px; /* Consistent padding */
    }
    
    /* Specific plot height controls */
    #gauge_plot, #type_plot, #source_plot {
      height: 280px !important; /* Fixed height for main plots */
    }
    
    #monthly_plot {
      height: 320px !important; /* Slightly taller for time series */
    }
   
    .radar-plot {
    height: 400px !important;
    width: 100%;
  }
  .dt-buttons .btn {
    margin-right: 5px;
  }
    
    /* Responsive adjustments */
    @media (max-width: 992px) {
      .value-box {
        height: auto;
        min-height: 100px;
      }
      .html-widget {
        min-height: 200px;
      }
    }
  ")),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@400;700&family=Roboto&display=swap",
      rel = "stylesheet"
    )
  ),
  nav_panel(
    "Dashboard",
    card(
      card_header("Metrics", class = "gradient-bg"),
      layout_columns(
        value_box(
          title = "Opportunities",
          value = textOutput("n_opp"),
          showcase = bsicons::bs_icon("activity"),
          theme = "primary",
          class = "h-100",  # Maintain full height
          style = "min-height: 150px;"  # Set minimum height
        ),
        value_box(
          title = "Pursued",
          value = textOutput("n_pursued"),
          showcase = bsicons::bs_icon("pencil"),
          theme = "secondary", 
          class = "h-100",
          style = "min-height: 150px;"
        ),
        value_box(
          title = "Successes",
          value = textOutput("n_success"), 
          showcase = bsicons::bs_icon("speedometer2"),
          theme = "success",
          class = "h-100",
          style = "min-height: 150px;"
        ),
        col_widths = c(4, 4, 4)
      ),
      
      # Visualization cards with auto-height
      layout_columns(
        card(
          card_header("Average Decision Time"),
          div(style = "height: auto; min-height: 300px;",  # Auto-adjusting height
              plotlyOutput("gauge_plot")
          )
        ),
        card(
          card_header("Opportunity Breakdown"),
          div(style = "height: auto; min-height: 300px;",
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
        div(style = "height: auto; min-height: 400px;",  # Taller minimum height
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
          actionButton("save", "Save Changes", class = "btn-secondary mt-3")
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
    "Opportunity Scoring",
    card(
      card_header("Opportunity Scoring"),
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
                       class = "btn-outline-danger mt-2")
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
  
 
  
  output$gauge_plot <- renderPlotly({
    req(data_store$labeled)
    
    # Calculate days to decision
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
      layout(margin = list(l=20, r=20))
  })
  
  # Breakdown by type
  # Breakdown by type - corrected version
  output$type_plot <- renderPlotly({
    req(data_store$labeled)
    
    plot_ly(
      data = data_store$labeled,
      x = ~opportunity_type,
      type = "histogram",  # This automatically bins/counts categorical data
      marker = list(color = "#522E91"),
      hoverinfo = "y"  # Show only count on hover
    ) %>%
      layout(
        xaxis = list(title = "Opportunity Type", tickangle = -45),
        yaxis = list(title = "Count", rangemode = "tozero"),
        margin = list(b = 100)
      )
  })
  
  output$source_plot <- renderPlotly({
    req(data_store$labeled)
    
    plot_ly(
      data = data_store$labeled,
      x = ~funding_source,
      type = "histogram",  # Automatic counting
      marker = list(color = "#F26649"),
      hoverinfo = "y"
    ) %>%
      layout(
        xaxis = list(title = "Funding Source", tickangle = -45),
        yaxis = list(title = "Count", rangemode = "tozero"),
        margin = list(b = 100)
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
    data.frame(
      reviewer = character(),
      opportunity_name = character(),
      feasibility = numeric(),
      impact = numeric(),
      alignment = numeric(),
      decision = character(),
      stringsAsFactors = FALSE
    )
  )
  
  # Update opportunity dropdown choices
  observe({
    req(data_store$labeled)
    
    # Filter for only "Waiting support decision" opportunities
    filtered_opps <- data_store$labeled %>%
      filter(status == "Waiting support decision") %>%
      pull(opportunity_name) %>%
      unique()
    
    updateSelectInput(session, "selected_opportunity", 
                      choices = filtered_opps)
  })
  
  # Save user scores locally
  observeEvent(input$save_score, {
    req(input$selected_opportunity)
    
    new_score <- data.frame(
      reviewer = as.character(input$reviewer),
      opportunity_name = as.character(input$selected_opportunity),
      feasibility = as.numeric(input$feasibility),
      impact = as.numeric(input$impact),
      alignment = as.numeric(input$alignment),
      decision = as.character(input$decision),
      stringsAsFactors = FALSE
    )
    
    # Convert existing scores to proper types before binding
    current_scores <- user_scores() %>%
      mutate(
        reviewer = as.character(reviewer),
        opportunity_name = as.character(opportunity_name),
        feasibility = as.numeric(feasibility),
        impact = as.numeric(impact),
        alignment = as.numeric(alignment),
        decision = as.character(decision)
      )
    
    # Remove existing score from same user for same opportunity
    updated_scores <- current_scores %>%
      filter(!(reviewer == input$reviewer & 
                 opportunity_name == input$selected_opportunity)) %>%
      bind_rows(new_score)
    
    user_scores(updated_scores)
    showNotification("Score submitted successfully!", type = "message")
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
  
  observe({
    updateSliderInput(session, "feasibility", value = as.numeric(input$feasibility))
    updateSliderInput(session, "impact", value = as.numeric(input$impact))
    updateSliderInput(session, "alignment", value = as.numeric(input$alignment))
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
#options(shiny.port = 8080)
shinyAppAuth0(ui, server)