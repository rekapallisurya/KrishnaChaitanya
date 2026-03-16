suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(DT)
})

resolve_project_root <- function() {
  candidates <- unique(c(
    normalizePath(getwd(), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(getwd(), "insurance-pricing-case-study-R"), winslash = "/", mustWork = FALSE)
  ))

  for (root in candidates) {
    if (file.exists(file.path(root, "scripts", "utils", "data_loader.R"))) {
      return(root)
    }
  }

  stop("Could not locate project root containing scripts/utils/data_loader.R")
}

PROJECT_ROOT <- resolve_project_root()
source(file.path(PROJECT_ROOT, "scripts", "utils", "data_loader.R"))

server <- function(input, output, session) {
  core <- load_core_data(PROJECT_ROOT)

  predictions <- reactive({
    files <- list.files(file.path(PROJECT_ROOT, "results"), pattern = "^predictions_.*\\.csv$", full.names = TRUE)
    if (length(files) == 0) return(data.table())

    pred <- rbindlist(lapply(files, fread), fill = TRUE)
    pred <- pred %>%
      left_join(core$policies %>% select(policy_id, naics_description, state), by = "policy_id") %>%
      as.data.table()

    pred
  })

  observe({
    updateSelectInput(session, "industry", choices = c("All", sort(unique(core$policies$naics_description))))
    updateSelectInput(session, "state", choices = c("All", sort(unique(core$policies$state))))
  })

  output$kpi_policies <- renderText({
    format(n_distinct(core$policies$policy_id), big.mark = ",")
  })

  output$kpi_exposure <- renderText({
    format(round(sum(core$exposures$exposure, na.rm = TRUE), 2), nsmall = 2)
  })

  output$kpi_losses <- renderText({
    format(round(sum(core$claims$ultimate_loss, na.rm = TRUE), 0), big.mark = ",")
  })

  output$freq_plot <- renderPlotly({
    freq <- core$claims %>% count(policy_id, name = "claim_count")
    p <- ggplot(freq, aes(claim_count)) +
      geom_histogram(binwidth = 1, fill = "#355070", color = "white") +
      labs(title = "Claim Frequency Distribution", x = "Claims per policy", y = "Count")
    ggplotly(p)
  })

  output$sev_plot <- renderPlotly({
    p <- ggplot(core$claims, aes(ultimate_loss)) +
      geom_histogram(bins = 60, fill = "#b56576", color = "white") +
      scale_x_log10() +
      labs(title = "Claim Severity Distribution", x = "Ultimate loss (log scale)", y = "Count")
    ggplotly(p)
  })

  output$industry_plot <- renderPlotly({
    industry <- core$policies %>% count(naics_description)
    p <- ggplot(industry, aes(reorder(naics_description, n), n)) +
      geom_col(fill = "#6d597a") +
      coord_flip() +
      labs(title = "NAICS Industry Breakdown", x = "Industry", y = "Policies")
    ggplotly(p)
  })

  output$triangle_heatmap <- renderPlotly({
    tri <- as.data.table(core$claim_development)
    p <- ggplot(tri, aes(x = factor(development_month), y = factor(accident_year), fill = cumulative_paid)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c() +
      labs(title = "Loss Development Triangle Heatmap", x = "Development Month", y = "Accident Year")
    ggplotly(p)
  })

  output$dev_factor_table <- renderDT({
    tri <- as.data.table(core$claim_development)[order(accident_year, development_month)]
    tri[, prior_cum := shift(cumulative_paid), by = accident_year]
    tri[, dev_factor := ifelse(!is.na(prior_cum) & prior_cum > 0, cumulative_paid / prior_cum, NA_real_)]
    datatable(tri[, .(accident_year, development_month, cumulative_paid, dev_factor)], options = list(pageLength = 8))
  })

  output$metrics_table <- renderDT({
    path <- file.path(PROJECT_ROOT, "results", "model_metrics.csv")
    if (!file.exists(path)) return(datatable(data.table(message = "Run modelling scripts first")))
    datatable(fread(path), options = list(pageLength = 10))
  })

  output$lift_plot <- renderPlotly({
    path <- file.path(PROJECT_ROOT, "results", "lift_charts.csv")
    if (!file.exists(path)) return(NULL)
    lift <- fread(path)
    if (input$model != "All") lift <- lift[model == input$model]

    p <- ggplot(lift, aes(risk_decile, actual_loss_cost, color = model)) +
      geom_line(size = 1) +
      geom_point() +
      labs(title = "Lift Chart: Actual Loss Cost by Predicted Decile", x = "Risk Decile", y = "Actual loss cost")
    ggplotly(p)
  })

  output$calibration_plot <- renderPlotly({
    path <- file.path(PROJECT_ROOT, "results", "calibration_results.csv")
    if (!file.exists(path)) return(NULL)
    cal <- fread(path)
    if (input$model != "All") cal <- cal[model == input$model]

    p <- ggplot(cal, aes(avg_predicted, avg_actual, color = model)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(title = "Calibration: Predicted vs Actual", x = "Average predicted", y = "Average actual")
    ggplotly(p)
  })

  output$shap_plot <- renderPlotly({
    path <- file.path(PROJECT_ROOT, "results", "shap_importance.csv")
    if (!file.exists(path)) return(NULL)
    shp <- fread(path)
    shp <- shp[order(-gain)][1:min(15, .N)]

    p <- ggplot(shp, aes(reorder(feature, gain), gain)) +
      geom_col(fill = "#355070") +
      coord_flip() +
      labs(title = "SHAP-style Feature Importance (XGBoost Gain)", x = "Feature", y = "Gain")
    ggplotly(p)
  })

  output$shap_table <- renderDT({
    path <- file.path(PROJECT_ROOT, "results", "shap_importance.csv")
    if (!file.exists(path)) return(datatable(data.table(message = "Run shap_analysis.R first")))
    datatable(fread(path), options = list(pageLength = 10))
  })
}
