library(shiny)
library(data.table)
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

demo_timepoints <- function(n = 250) {
  genes <- unique(gene_loc_table_mm22$gene_symbol)
  set.seed(42)

  list(
    baseline = data.table(
      gene_symbol = sample(genes, n),
      demo_value = rnorm(n),
      p_value = runif(n)
    ),
    followup = data.table(
      gene_symbol = sample(genes, n),
      demo_value = rnorm(n, mean = 0.25),
      p_value = runif(n)
    )
  )
}

ui <- fluidPage(
  titlePanel("expressyouRcell Demo"),
  sidebarLayout(
    sidebarPanel(
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
      actionButton("run", "Run pipeline"),
      actionButton("animate", "Create animation"),
      tags$hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      plotOutput("cell_plot", height = "650px"),
      uiOutput("animation_preview")
    )
  )
)

server <- function(input, output, session) {
  pipeline_result <- eventReactive(input$run, {
    timepoints <- demo_timepoints()
    coloring_mode <- if (identical(input$coloring_method, "fdr")) {
      "enrichment"
    } else {
      "mean"
    }

    cell_output <- color_cell(
      timepoint_list = timepoints,
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
      timepoints = names(timepoints),
      method = input$coloring_method
    )
  }, ignoreInit = FALSE)

  output$cell_plot <- renderPlot({
    result <- pipeline_result()
    result$cell$plot[[1]]
  })

  output$status <- renderText({
    result <- pipeline_result()
    paste(
      "Backend package: expressyouRcell",
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
