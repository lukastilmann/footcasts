# Load required libraries
library(shiny)
library(readr)
library(dplyr) # For filter and select

source("../config.R")

# --- Load static data ---
# Attempt to load leagues data, handle potential errors
leagues_df <- NULL
tryCatch({
  # Loading leagues file
  leagues_df <- read_csv(CONFIG$paths$leagues_data, show_col_types = FALSE)
  if (!all(c("id", "name", "country", "level") %in% names(leagues_df))) {
    stop("Leagues CSV is missing required columns: id, name, country, level.")
  }
}, error = function(e) {
  warning(paste("Error loading ./data/leagues.csv:", e$message))
  # Provide a fallback or stop the app if leagues_df is critical for startup
  leagues_df <<- data.frame( # Use <<- to assign to global leagues_df in this fallback
    id = c(4, 9),
    name = c("Bundesliga (Fallback)", "2. Bundesliga (Fallback)"),
    country = c("Germany", "Germany"), # Assuming Germany for fallbacks
    level = c(1, 2), # Assuming levels for fallbacks
    stringsAsFactors = FALSE
  )
  shiny::showNotification(
    "Could not load '../data/leagues.csv'. Using fallback league definitions. Please check the file path and content.",
    type = "warning",
    duration = NULL
  )
})

# Filter for selectable leagues
selectable_leagues <- leagues_df %>%
  filter(id %in% CONFIG$leagues$enabled_ids)

if (nrow(selectable_leagues) == 0) {
  stop(paste("Configured league IDs", paste(CONFIG$leagues$enabled_ids, collapse = ", "),
             "not found in leagues.csv or fallback. App cannot start."))
}

# Create choices for the selectInput (names are league names, values are league ids)
league_choices <- setNames(selectable_leagues$id, selectable_leagues$name)

# Source helper functions
source("get_forecast.R")     # Contains get_forecast()
source("format_forecast.R")  # Contains format_forecast_table()
source("format_results.R")
source("load_matchday.R")



# --- UI Definition ---
ui <- fluidPage(
  titlePanel("Football League Forecast - 2024/2025"),
  sidebarLayout(
    sidebarPanel(
      selectInput("league", "Select League:",
                  choices = league_choices), # Dynamically set choices
      selectInput("matchday", "Select Matchday:",
                  choices = c("Pre-Season")) # Choices will be updated in server
    ),
    mainPanel(
      h3(textOutput("forecast_header")),
      tableOutput("forecast_table"),
      hr(),
      uiOutput("results_header_ui"),
      tableOutput("matchday_results_table")
    )
  )
)

# --- Server Definition ---
server <- function(input, output, session) {

  # For now, just return current season from config file
  # at some point, seasons could be chosen
  selected_season <- reactive(({
    return(CONFIG$seasons$current)
  }))

  # Helper to get league details from the loaded selectable_leagues
  get_selected_league_details <- reactive({
    req(input$league) # input$league is the ID
    league_info <- selectable_leagues[selectable_leagues$id == as.numeric(input$league), ]
    if (nrow(league_info) == 1) {
      return(as.list(league_info))
    }
    shiny::showNotification(paste("Details for league ID", input$league, "not found."), type = "error")
    return(NULL)
  })

  # Update matchday choices reactively (since number of matchdays differs between leagues)
  observe({
    req(get_selected_league_details())
    n_matchdays <- get_selected_league_details()$n_matchdays
    updateSelectInput(session, "matchday",
                      choices = c("Pre-Season", 1:n_matchdays),
                      selected = input$matchday)
  })

  output$forecast_header <- renderText({
    league_details <- get_selected_league_details()
    req(league_details)
    paste("Forecast for", league_details$name)
  })

  # Reactive expression to load ALL matchday forecast data for the selected league
  loaded_league_forecast_data <- reactive({
    selected_league_id <- input$league # This is now the ID
    req(selected_league_id)

    league_details <- get_selected_league_details()
    req(league_details)

    # Forecast indices: 0 (Pre-Season) to n_matchdays for that league
    matchday_forecast_indices_to_load <- 0:(league_details$n_matchdays)

    loading_notification_id <- showNotification(
      paste("Loading all forecast data for", league_details$name, "..."),
      type = "message", duration = NULL, id = "forecastLoadingNotification"
    )
    on.exit(removeNotification(loading_notification_id), add = TRUE)

    all_matchdays_for_league <- list()
    files_found_count <- 0

    for (md_forecast_idx in matchday_forecast_indices_to_load) {
      forecast_table <- get_forecast(
        season_end_year = as.numeric(selected_season()),
        league_id = as.numeric(selected_league_id),
        matchday_value = md_forecast_idx
      )

      if (!is.null(forecast_table) && nrow(forecast_table) > 0 && !("Message" %in% names(forecast_table))) {
        all_matchdays_for_league[[as.character(md_forecast_idx)]] <- forecast_table
        files_found_count <- files_found_count + 1
      } else {
        # Determine context for message (Pre-Season or after which matchday)
        forecast_context <- if (md_forecast_idx == 0) {
          "Pre-Season"
        } else {
          paste("after Matchday", md_forecast_idx)
        }
        all_matchdays_for_league[[as.character(md_forecast_idx)]] <-
          data.frame(Message = paste0("Forecast data not available for state ", md_forecast_idx,
                                      " (", forecast_context, ")."))
      }
    }

    if (files_found_count == 0 && length(matchday_forecast_indices_to_load) > 0) {
      showNotification(
        paste("No forecast data files found for any matchday state in", league_details$name, "for the", FORECAST_SEASON_END_YEAR, "season."),
        type = "warning", duration = 10
      )
      return(list("error" = data.frame(Message = paste("No forecast data found for", league_details$name))))
    }
    return(all_matchdays_for_league)
  })

  # Reactive expression to select the specific matchday's forecast table
  current_forecast_table_to_display <- reactive({
    req(input$matchday)
    league_forecast_data <- loaded_league_forecast_data()

    if (is.null(league_forecast_data)) return(data.frame(Message = "Error loading league forecast data."))
    if ("error" %in% names(league_forecast_data)) return(league_forecast_data[["error"]])

    forecast_key <- if (input$matchday == "Pre-Season") {
      "0"
    } else {
      as.character(input$matchday)
    }

    selected_df <- league_forecast_data[[forecast_key]]

    if (is.null(selected_df)) {
      league_details <- get_selected_league_details()
      league_name_display <- if(!is.null(league_details)) league_details$name else "the selected league"
      return(data.frame(Message = paste0("Forecast data for ", league_name_display, ", selection '", input$matchday, "' (key ", forecast_key, ") could not be retrieved.")))
    }
    return(selected_df)
  })

  output$forecast_table <- renderTable({
    raw_data <- current_forecast_table_to_display()
    league_details <- get_selected_league_details()
    req(league_details)

    # num_teams can be dynamically set if available in leagues_df or known.
    # e.g. num_teams <- league_details$teams_count if such a column exists.
    # For Bundesliga (ID 4) and 2. Bundesliga (ID 9), it's 18.
    # format_forecast_table tries to infer this, so explicit passing is optional.
    num_teams <- length(raw_data)

    formatted_table <- format_forecast_table(
      raw_forecast_df = raw_data,
      league_id = league_details$id,
      season = selected_season()
    )
    return(formatted_table)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 'm', width = 'auto', align = 'l')


  # --- Matchday Results Section ---

  # Reactive expression to FILTER results for the selected actual matchday FROM the loaded season data
  loaded_matchday_results <- reactive({
    req(input$league, input$matchday)

    if (input$matchday == "Pre-Season") {
      return(NULL) # No results to display for Pre-Season
    }

    league_details <- get_selected_league_details() # For context in messages
    req(league_details)

    actual_matchday_number <- as.numeric(input$matchday)

    # Filter the data for the selected week/matchday
    # Ensure Wk column is numeric if it's being compared to a numeric value
    # If Wk can be non-numeric, adjust comparison or ensure all_season_results$Wk is coerced correctly

    specific_matchday_results <- load_matchday_results(league_details$country,
                                                       league_details$level,
                                                       selected_season(),
                                                       actual_matchday_number)

    if (nrow(specific_matchday_results) == 0) {
      return(data.frame(Message = paste("No results found for Matchday", actual_matchday_number, "of", league_details$name, "in the loaded data.")))
    }

    # Format the results using our new function
    formatted_results <- format_match_results(specific_matchday_results)
    return(formatted_results)
  })

  # UI for the results header (conditionally displayed)
  output$results_header_ui <- renderUI({
    req(input$league, input$matchday)
    if (input$matchday == "Pre-Season") {
      return(NULL)
    }
    league_details <- get_selected_league_details()
    req(league_details)
    h3(paste("Results for", league_details$name, "- Matchday", input$matchday))
  })

  # Render the matchday results table
  output$matchday_results_table <- renderTable({
    results_to_display <- loaded_matchday_results()
    if (is.null(results_to_display)) { # Handles Pre-Season explicitly returning NULL
      return(NULL)
    }
    return(results_to_display)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = 's', width = 'auto', align = 'c')

} # End server

shinyApp(ui = ui, server = server)
