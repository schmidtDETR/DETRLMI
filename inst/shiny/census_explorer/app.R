library(shiny)
library(tidycensus)
library(tidyverse)
library(data.table)
library(DT)
library(collapsibleTree)

# --- 1. DATA PREP FUNCTION ---
parsed_variables <- function(year, dataset){

  out <- tidycensus::load_variables(year = year, dataset = dataset)

  out <- out |>
    mutate(
      table = str_remove(name, "_.*"),
      year = year,
      dataset = dataset,
      label_clean = trimws(label, which = "both"),
      label_clean = str_remove_all(label_clean, ":"),
      label_clean = str_remove_all(label_clean, "--"),
      label_tokens = stringr::str_split(label_clean, "!!"),
      label_tokens = purrr::map(label_tokens, \(x) {
        x |> stringr::str_trim() |> purrr::discard(\(y) y == "")
      }),
      label_depth = purrr::map_int(label_tokens, length)
    ) |>
    rename("variable" = "name")

  max_depth <- max(out$label_depth, na.rm = TRUE)

  if (is.finite(max_depth) && max_depth > 0) {
    for (i in seq_len(max_depth)) {
      out[[paste0("label_level_", i-1)]] <- purrr::map_chr(
        out$label_tokens,
        \(x) if (length(x) >= i) x[[i]] else NA_character_
      )
    }
  }

  out <- out |>
    dplyr::select(-label_tokens, -label_depth, -label_clean, -label_level_0)

  return(out)
}

# --- 2. SHINY UI ---
ui <- fluidPage(
  titlePanel("Census ACS Variable Explorer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("1. API Parameters"),
      helpText("Select the survey year and dataset type. (Data may take a moment to load)"),

      selectInput("year", "Survey Year:",
                  choices = seq(2024, 2010, by = -1),
                  selected = 2022),

      selectInput("dataset", "Dataset:",
                  choices = c("ACS 1-Year Subject" = "acs1/subject",
                              "ACS 5-Year Subject" = "acs5/subject",
                              "ACS 1-Year Detailed" = "acs1",
                              "ACS 5-Year Detailed" = "acs5",
                              "ACS 1-Year Data Profile" = "acs1/profile",
                              "ACS 5-Year Data Profile" = "acs5/profile"),
                  selected = "acs1/subject"),

      hr(),
      h4("2. Filters"),
      helpText("Narrow down the Concept and Table within the selected dataset."),

      selectizeInput("concept", "Select Concept:",
                     choices = c("All"),
                     selected = "All"),

      selectizeInput("table", "Select Table:",
                     choices = c("All"),
                     selected = "All"),

      hr(),
      # --- NEW: Code Snippet UI ---
      h4("3. Code Snippet"),
      helpText("Copy this code to pull the selected table in your script:"),
      verbatimTextOutput("code_snippet"),

      hr(),
      helpText("Click on nodes in the tree to expand details. To search for specific string values, switch to the Data Table tab.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Hierarchy Tree",
                 br(),
                 uiOutput("tree_ui")),

        tabPanel("Data Table",
                 br(),
                 downloadButton("download_data", "Download as CSV", class = "btn-primary"),
                 br(), br(),
                 DTOutput("var_table"))
      )
    )
  )
)

# --- 3. SHINY SERVER ---
server <- function(input, output, session) {

  # Reactive block to load and parse data
  base_data <- reactive({
    req(input$year, input$dataset)

    id <- showNotification(
      paste("Fetching and parsing", input$dataset, "data for", input$year, "... Please wait."),
      duration = NULL,
      type = "message"
    )
    on.exit(removeNotification(id), add = TRUE)

    parsed_variables(year = as.numeric(input$year), dataset = input$dataset)
  })

  # Dynamically calculate level columns based on the current dataset
  level_cols <- reactive({
    cols <- grep("label_level_", names(base_data()), value = TRUE)
    cols[order(as.numeric(gsub("label_level_", "", cols)))]
  })

  # Update Concept dropdown
  observe({
    df <- base_data()
    concept_choices <- sort(na.omit(unique(df$concept)))
    updateSelectizeInput(session, "concept", choices = c("All", concept_choices), selected = "All")
  })

  # Update Table dropdown
  observe({
    df <- base_data()
    if (input$concept == "All") {
      table_choices <- sort(na.omit(unique(df$table)))
    } else {
      table_choices <- df |>
        filter(concept == input$concept) |>
        pull(table) |>
        unique() |>
        na.omit() |>
        sort()
    }
    updateSelectizeInput(session, "table", choices = c("All", table_choices), selected = "All")
  })

  # Reactive dataset for tables/trees
  filtered_data <- reactive({
    df <- base_data()
    if (input$concept != "All") {
      df <- df |> filter(concept == input$concept)
    }
    if (input$table != "All") {
      df <- df |> filter(table == input$table)
    }
    return(df)
  })

  # --- NEW: Generate Code Snippet ---
  output$code_snippet <- renderText({
    req(input$year, input$dataset)

    if (input$table == "All") {
      return("# Please select a specific Table\n# from the dropdown above.")
    }

    # Extract just "acs1" or "acs5" from the dataset string using regex
    survey_type <- stringr::str_extract(input$dataset, "^acs[15]")

    # Construct the R code string
    paste0(
      "my_data <- get_acs(\n",
      "  geography = \"state\", # Update as needed\n",
      "  table = \"", input$table, "\",\n",
      "  year = ", input$year, ",\n",
      "  survey = \"", survey_type, "\"\n",
      ")"
    )
  })

  # Conditionally render the tree
  output$tree_ui <- renderUI({
    if (input$table == "All") {
      h4("Please select a specific Table from the sidebar to view the hierarchy tree.", style = "color: gray;")
    } else {
      collapsibleTreeOutput("tree_plot", height = "700px")
    }
  })

  # Render the Hierarchy Tree
  output$tree_plot <- renderCollapsibleTree({
    req(input$table != "All")

    table_data <- filtered_data() |> select(any_of(level_cols()))
    table_data <- table_data |> select(where(~ !all(is.na(.x))))
    active_levels <- names(table_data)

    tree_df <- table_data |>
      distinct() |>
      mutate(across(everything(), ~replace_na(.x, "(End)")))

    collapsibleTree(
      tree_df,
      hierarchy = active_levels,
      root = input$table,
      collapsed = TRUE,
      zoomable = TRUE,
      fontSize = 12
    )
  })

  # Render the Interactive Data Table
  output$var_table <- renderDT({
    datatable(
      filtered_data() |> select(variable, concept, table, any_of(level_cols())),
      filter = "top",
      options = list(pageLength = 15, autoWidth = TRUE, scrollX = TRUE, searchHighlight = TRUE),
      rownames = FALSE
    )
  })

  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() {
      safe_dataset <- gsub("/", "_", input$dataset)
      paste0("census_vars_", input$year, "_", safe_dataset, ".csv")
    },
    content = function(file) {
      out_data <- filtered_data() |> select(variable, concept, table, any_of(level_cols()))
      if (!is.null(input$var_table_rows_all)) {
        out_data <- out_data[input$var_table_rows_all, ]
      }
      write.csv(out_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
