library(shiny)
library(bslib)
library(expressyouRcell)

data(cell_dt, package = "expressyouRcell")
data(neuron_dt, package = "expressyouRcell")
data(microglia_dt, package = "expressyouRcell")
data(macrophage_dt, package = "expressyouRcell")
data(lymphocyte_dt, package = "expressyouRcell")
data(fibroblast_dt, package = "expressyouRcell")
data(gene_loc_table_mm22, package = "expressyouRcell")

pictograph_data <- list(
  cell = cell_dt,
  neuron = neuron_dt,
  microglia = microglia_dt,
  macrophage = macrophage_dt,
  lymphocyte = lymphocyte_dt,
  fibroblast = fibroblast_dt
)

ui <- fluidPage(
  titlePanel("expressyouRcell Demo"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "demo_dataset",
        "Demo dataset",
        choices = c("example_list" = "example_list"),
        selected = "example_list"
      ),
      selectInput(
        "cell_type",
        "Dataset",
        choices = names(pictograph_data),
        selected = "cell"
      ),
      radioButtons(
        "coloring_method",
        "Coloring method",
        choices = c(
          "Enrichment (FDR-based)" = "enrichment",
          "Mean of selected column" = "mean"
        ),
        selected = "enrichment"
      ),
      conditionalPanel(
        condition = "input.coloring_method == 'mean'",
        uiOutput("value_column_ui")
      ),
      checkboxInput("show_legend", "Show legend", value = TRUE),
      
      tags$div(
        style = "margin-top: 15px;",
        
        actionButton("run", "Visualize results"),
        tags$div(
          style = "margin-top: 5px; font-size: 12px; color: #888;",
          "Note: The visualization may take a few seconds to generate.",
          textOutput("run_status")
        )
      ),      
      
      downloadButton("download_results", "Export results"),
      tags$hr(),
      verbatimTextOutput("status"),
      width=3
    ),
    mainPanel(
      wellPanel(
        textOutput("timepoint_label"),
        plotOutput("cell_plot", height = "650px", width = "100%"),
        uiOutput("timepoint_controls")        
      ), width=9
    )
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#2C3E50",
    secondary = "#18BC9C"
  )
)

server <- function(input, output, session) {
  current_timepoint <- reactiveVal(1)
  run_status <- reactiveVal("")

  output$run_status <- renderText({
    run_status()
  })
  
  demo_column_choices <- reactive({
    selected_data <- expressyouRcell::example_list
    common_columns <- Reduce(intersect, lapply(selected_data, names))
    numeric_columns <- common_columns[
      vapply(
        common_columns,
        function(column) {
          all(vapply(selected_data, function(timepoint) {
            is.numeric(timepoint[[column]])
          }, logical(1)))
        },
        logical(1)
      )
    ]

    if (length(numeric_columns) > 0) {
      numeric_columns
    } else {
      common_columns
    }
  })

  output$value_column_ui <- renderUI({
    choices <- demo_column_choices()
    req(length(choices) > 0)

    selected_column <- if ("demo_value" %in% choices) {
      "demo_value"
    } else if ("logFC" %in% choices) {
      "logFC"
    } else {
      choices[[1]]
    }

    selectInput(
      "value_column",
      "Column for mean coloring",
      choices = choices,
      selected = selected_column
    )
  })

  observeEvent(input$run, {    
    current_timepoint(1)
    run_status("Running...")
  })

  pipeline_result <- eventReactive(input$run, {
    req(input$demo_dataset)
   
    selected_data <- expressyouRcell::example_list
    coloring_mode <- input$coloring_method

    req(coloring_mode %in% c("enrichment", "mean"))

    color_cell_args <- list(
      timepoint_list = selected_data,
      pictograph = input$cell_type,
      gene_loc_table = gene_loc_table_mm22,
      coloring_mode = coloring_mode,
      data_type = "diffanalysis",
      pval_col = "p_value",
      legend = input$show_legend
    )

    if (identical(coloring_mode, "mean")) {
      req(input$value_column)
      color_cell_args$col_name <- input$value_column
    }

    cell_output <- withProgress(message = "Running...", {
      do.call(color_cell, color_cell_args)
    })

    list(
      cell = cell_output,
      cell_type = input$cell_type,
      demo_dataset = input$demo_dataset,
      timepoints = names(selected_data),
      coloring_mode = coloring_mode,
      value_column = if (identical(coloring_mode, "mean")) input$value_column else NULL
    )
  })

  timepoint_plots <- reactive({
    result <- pipeline_result()
    plots <- if (!is.null(result$cell$plot)) {
      result$cell$plot
    } else {
      result$cell
    }

    req(length(plots) > 0)
    plots
  })

  observeEvent(input$previous_timepoint, {
    current_timepoint(max(1, current_timepoint() - 1))
  })

  observeEvent(input$next_timepoint, {
    plots <- timepoint_plots()
    current_timepoint(min(length(plots), current_timepoint() + 1))
  })

  output$timepoint_label <- renderText({
    result <- pipeline_result()
    plots <- timepoint_plots()
    index <- min(current_timepoint(), length(plots))
    plot_names <- names(plots)
    has_plot_name <- !is.null(plot_names) &&
      length(plot_names) >= index &&
      nzchar(plot_names[[index]])
    if (!has_plot_name) {
      plot_names <- result$timepoints
    }

    if (!is.null(plot_names) &&
        length(plot_names) >= index &&
        nzchar(plot_names[[index]])) {
      sprintf("%s (%d/%d)", plot_names[[index]], index, length(plots))
    } else {
      sprintf("Timepoint %d of %d", index, length(plots))
    }
  })

  output$cell_plot <- renderPlot({    plots <- timepoint_plots()
    index <- min(current_timepoint(), length(plots))
    plots[[index]]
  })

  output$timepoint_controls <- renderUI({
    plots <- timepoint_plots()
    index <- min(current_timepoint(), length(plots))

    tagList(
      actionButton(
        "previous_timepoint",
        "Previous",
        disabled = if (index <= 1) "disabled" else NULL
      ),
      actionButton(
        "next_timepoint",
        "Next",
        disabled = if (index >= length(plots)) "disabled" else NULL
      )
    )
  })

  output$status <- renderText({
    result <- pipeline_result()
    status_lines <- c(
      "Backend package: expressyouRcell",
      paste("Demo dataset:", result$demo_dataset),
      paste("Cell dataset:", result$cell_type),
      paste("Coloring mode:", result$coloring_mode)
    )

    if (!is.null(result$value_column)) {
      status_lines <- c(status_lines, paste("Mean column:", result$value_column))
    }

    paste(
      status_lines,
      paste("Timepoints:", paste(result$timepoints, collapse = ", ")),
      paste("color_cell output:", paste(names(result$cell), collapse = ", ")),
      sep = "\n"
    )
  })

  output$download_results <- downloadHandler(
    filename = function() {
      sprintf("expressyouRcell_results_%s.zip", format(Sys.Date(), "%Y%m%d"))
    },
    content = function(file) {
      result <- pipeline_result()
      plots <- timepoint_plots()

      if (!result$cell_type %in% c("cell", "neuron", "fibroblast", "microglia")) {
        stop(
          "animate() in the installed backend supports cell, neuron, fibroblast, and microglia in this build."
        )
      }

      export_dir <- file.path(
        tempdir(),
        paste0("expressyouRcell_export_", session$token, "_", as.integer(Sys.time()))
      )
      results_dir <- file.path(export_dir, "results")
      dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

      for (index in seq_along(plots)) {
        local({
          png(
            filename = file.path(results_dir, sprintf("timepoint_%d.png", index)),
            width = 1600,
            height = 1200,
            res = 150
          )
          on.exit(dev.off(), add = TRUE)
          print(plots[[index]])
        })
      }

      animate(
        data = result$cell,
        timepoints = result$timepoints,
        seconds = 1,
        fps = 4,
        input_dir = results_dir,
        names = result$timepoints,
        filename = "animation",
        format = "gif"
      )

      zip::zip(
        zipfile = file,
        files = "results",
        root = export_dir,
        mode = "mirror"
      )
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
