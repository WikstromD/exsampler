#' Server logic
#'
#' This defines the server-side logic for the exsampler Shiny app.
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#'
#' @import shiny
#' @importFrom purrr map set_names
#' @importFrom glue glue
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 .data
app_server <- function(input, output, session) {
  
  #  keep  all_band_choices
  all_band_choices <- list(
    "Pointwise"            = "pointwise",
    "Bootstrap"            = "boot",
    "Tail-Sensitive (TS)"  = "ts",
    "Kolmogorov-Smirnov (KS)" = "ks"
  )
  
  output$band_ui <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "pp_plot") {
      # only the Pointwise checkbox, none selected by default
      checkboxGroupInput(
        "band_types",
        "Confidence Bands:",
        choices  = list("Pointwise" = "pointwise"),
        selected = character(0)
      )
    } else if (input$plot_type %in% c("qq_normal", "qq_detrended")) {
      # all four checkboxes, none selected by default
      checkboxGroupInput(
        "band_types",
        "Confidence Bands:",
        choices  = all_band_choices,
        selected = character(0)
      )
    } else {
      NULL
    }
  })
  
  
  list_global_data_frames <- function() {
    objs <- ls(envir = .GlobalEnv, all.names = TRUE)
    keep <- vapply(
      objs,
      function(nm) {
        obj <- try(get(nm, envir = .GlobalEnv), silent = TRUE)
        if (inherits(obj, "try-error")) return(FALSE)
        is.data.frame(obj) || inherits(obj, "tbl_df")
      },
      logical(1)
    )
    objs[keep]
  }
  
  rt <- shiny::reactiveTimer(2000)
  
  observe({
    rt()  
    
    global_choices <- list_global_data_frames()
    pkg_choices    <- c("exSample_Distributions") 
    
    choices <- unique(c(pkg_choices, global_choices))
    if (length(choices) == 0) choices <- pkg_choices
    
    updateSelectInput(
      session, "dataset_name",
      choices  = choices,
      selected = isolate(if (isTruthy(input$dataset_name) && input$dataset_name %in% choices)
        input$dataset_name else choices[[1]])
    )
  })
  
  
  selected_data <- reactive({
  req(input$dataset_name)
  pkg <- "exsampler"
  # 1) Internal/namespace (for internal data)
  if (exists(input$dataset_name, envir = asNamespace(pkg), inherits = FALSE)) {
    get(input$dataset_name, envir = asNamespace(pkg), inherits = FALSE)
  } else if (exists(input$dataset_name, envir = as.environment(paste0("package:", pkg)), inherits = FALSE)) {
    # 2) Attached package environment (external data loaded on attach)
    get(input$dataset_name, envir = as.environment(paste0("package:", pkg)), inherits = FALSE)
  } else {
    # 3) Try to load from installed external data (data/) into a temp env
    env <- new.env(parent = emptyenv())
    utils::data(list = input$dataset_name, package = pkg, envir = env)
    if (exists(input$dataset_name, envir = env, inherits = FALSE)) {
      get(input$dataset_name, envir = env, inherits = FALSE)
    } else if (exists(input$dataset_name, envir = .GlobalEnv, inherits = FALSE)) {
      # 4) Fallback: global environment (useful during development)
      get(input$dataset_name, envir = .GlobalEnv, inherits = FALSE)
    } else {
      stop(sprintf("Dataset '%s' not found in %s or global environment.", input$dataset_name, pkg))
    }
  }
})
observeEvent(selected_data(), {
    vars <- names(selected_data())[sapply(selected_data(), is.numeric)]
    updateSelectInput(session, "variable_name", choices = vars)
  }, ignoreNULL = FALSE)
  
  data_and_position_info <- eventReactive(input$generate_plots, {
    req(selected_data(), input$variable_name)
    generate_random_data_for_plot_grid(
      selected_data(),
      input$variable_name,
      seed_modifier   = input$generate_plots,
      match_increment = input$match_increment
    )
  })
  
  individual_plots <- reactive({
    info <- data_and_position_info()
    req(info)
    
    # grab vector of selected bands, or NULL if none
    bands <- input$band_types
    if (is.null(bands) || length(bands) == 0) bands <- NULL
    
    purrr::map(
      info$shuffled_data_frames,
      ~ create_single_plot(.x, info$variable_name, input$plot_type, bands)
    )
  })
  
  
  output$plot_grid <- renderPlot({
    patchwork::wrap_plots(
      individual_plots(),
      ncol = data_and_position_info()$grid_cols
    )
  }, res = 300)
  
  observeEvent(list(data_and_position_info(), input$variable_name), {
    output$guess_result <- renderUI(HTML(""))
  })
  
  observeEvent(input$plot_click, {
    info <- data_and_position_info()
    req(input$plot_click, info)
    cols <- info$grid_cols
    x <- input$plot_click$x
    y <- input$plot_click$y
    col_i <- ceiling(x * cols)
    row_i <- ceiling((1 - y) * cols)
    pos   <- (row_i - 1) * cols + col_i
    
    msg <- if (pos == info$real_position) {
      glue::glue("<p style='color:green; font-weight:bold;'>Correct! You found the real data at position {info$real_position}.</p>")
    } else {
      glue::glue("
        <p style='color:red; font-weight:bold;'>Not quite.</p>
        <p>You clicked position {pos} (simulated). The real data was at position <b>{info$real_position}</b>.</p>
      ")
    }
    output$guess_result <- renderUI(HTML(msg))
  })
  
  output$plot_definition <- renderUI({
    switch(
      input$plot_type,
      
      # QQ Normal
      qq_normal = HTML("
<div style='font-size:120%;'>
  <p></p>
  <p><b>QQ plots</b> compare your data's sample quantiles (dots) to the theoretical normal quantiles (reference line).</p>
  <p>When the data are normal, points lie close to a straight line with small random deviations.</p>
</div>
"),
      
      # QQ Detrended
      qq_detrended = HTML("
<div style='font-size:120%;'>
  <p></p>
  <p><b>Detrended QQ plots</b> display the distance of observed quantiles (dots) to normal quantiles by plotting their vertical differences (observed - theoretical) against a reference line of 0.</p>
  <p>The x-axis shows the theoretical quantiles, and the y-axis shows the value of observed - theoretical quantiles.</p>
  <p>When the data are normally distributed, points fall close to the horizontal reference line at zero, with small random variation.</p>
</div>
"),
      
      # PP Plot
      pp_plot = HTML("
<div style='font-size:120%;'>
  <p><b>PP plots</b> compare your empirical CDF (proportion <= x) with the normal CDF.</p>
  <p>If the data are approximately normal, points hug the diagonal reference line.</p>
"),
      
      
      # Histogram
      histogram = HTML("
<div style='font-size:120%;'>
  <p></p>
  <p><b>Histograms</b> shows how often values fall into bins, with two overlays:</p>
  <p>- The dotted curve (kernel density estimate) tracing your data's shape.</p>
  <p>- The solid bell curve (normal density) showing how a normal distribution would look.</p>
</div>
"),
      
      # Default (when no plot_type matches)
      HTML("")
    )
  })
  
}
