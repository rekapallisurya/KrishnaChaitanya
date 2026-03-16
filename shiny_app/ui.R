suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  library(DT)
})

build_ui <- function() {
  fluidPage(
    titlePanel("General Liability Pricing Analytics Dashboard"),
    sidebarLayout(
      sidebarPanel(
        selectInput("model", "Select Model", choices = c("All", "GLM", "GAM", "XGBoost", "TweedieGBM"), selected = "All"),
        selectInput("industry", "Filter Industry", choices = "All", selected = "All"),
        selectInput("state", "Filter State", choices = "All", selected = "All")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Portfolio Overview", value = "overview",
                   fluidRow(
                     column(4, h4("Policies"), textOutput("kpi_policies")),
                     column(4, h4("Exposure"), textOutput("kpi_exposure")),
                     column(4, h4("Total Losses"), textOutput("kpi_losses"))
                   )),
          tabPanel("EDA", value = "eda",
                   plotlyOutput("freq_plot"),
                   plotlyOutput("sev_plot"),
                   plotlyOutput("industry_plot")),
          tabPanel("Claims Development", value = "development",
                   plotlyOutput("triangle_heatmap"),
                   DTOutput("dev_factor_table")),
          tabPanel("Model Comparison", value = "model_comp",
                   DTOutput("metrics_table"),
                   plotlyOutput("lift_plot"),
                   plotlyOutput("calibration_plot")),
          tabPanel("Explainability", value = "explain",
                   plotlyOutput("shap_plot"),
                   DTOutput("shap_table"))
        )
      )
    )
  )
}

ui <- build_ui()
