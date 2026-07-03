library(shiny)
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
        choices = c("FDR" = "fdr", "VALUE" = "value"),
        selected = "fdr"
      ),
      actionButton("run", "Visualize results"),
      actionButton("animate", "Create Rtion"),
      tags$hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      wellPanel(
        textOutput("timepoint_label"),
        plotOutput("cell_plot", height = "650px"),
        uiOutput("timepoint_controls")
      ),
      uiOutput("animation_preview")
    )
  )
)

server <- function(input, output, session) {
  current_timepoint <- reactiveVal(1)

  pipeline_result <- eventReactive(input$run, {
    req(input$demo_dataset)

    selected_data <- expressyouRcell::example_list
    coloring_mode <- if (identical(input$coloring_method, "fdr")) {
      "enrichment"
    } else {
      "mean"
    }

    cell_output <- color_cell(
      timepoint_list = selected_data,
      pictograph = input$cell_type,
      gene_loc_table = gene_loc_table_mm22,
      coloring_mode = coloring_mode,
      data_type = "diffanalysis",
      col_name = "demo_value",
      pval_col = "p_value",
      legend = TRUE
    )

    list(
      cell = cell_output,
      demo_dataset = input$demo_dataset,
      timepoints = names(selected_data),
      method = input$coloring_method
    )
  })

  observeEvent(input$run, {
    current_timepoint(1)
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

  output$cell_plot <- renderPlot({
    plots <- timepoint_plots()
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
    paste(
      "Backend package: expressyouRcell",
      paste("Demo dataset:", result$demo_dataset),
      paste("Cell dataset:", input$cell_type),
      paste("Coloring method:", toupper(result$method)),
      paste("Timepoints:", paste(result$timepoints, collapse = ", ")),
      paste("color_cell output:", paste(names(result$cell), collapse = ", ")),
      sep = "\n"
    )
  })

  animation_file <- eventReactive(input$animate, {
    result <- pipeline_result()

    if (!input$cell_type %in% c("cell", "neuron", "fibroblast", "microglia")) {
      showNotification(
        "animate() in the installed backend supports cell, neuron, fibroblast, and microglia in this build.",
        type = "warning"
      )
      return(NULL)
    }

    output_dir <- file.path(tempdir(), paste0("expressyouRcell_", session$token))
    animate(
      data = result$cell,
      timepoints = result$timepoints,
      seconds = 1,
      fps = 4,
      input_dir = output_dir,
      names = result$timepoints,
      filename = "demo_animation",
      format = "gif"
    )

    addResourcePath("expressyouRcell_demo", output_dir)
    file.path("expressyouRcell_demo", "demo_animation.gif")
  })

  output$animation_preview <- renderUI({
    src <- animation_file()
    req(src)
    tags$img(src = src, style = "max-width: 100%; height: auto;")
  })
}

shinyApp(ui, server)
