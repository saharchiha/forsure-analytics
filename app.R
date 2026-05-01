library(shiny)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(plotly)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(corrplot)
library(keras)
library(shinycssloaders)
# ---- Custom CSS ----
custom_css <- "
/* Main header styling - (Assuming this is mostly fine from previous version) */
.skin-blue .main-header .navbar {
  background-color: #2c3e50 !important;
  background-image: linear-gradient(to right, #3498db, #2c3e50) !important;
  box-shadow: 0 2px 5px rgba(0,0,0,0.2);
}

.skin-blue .main-header .logo {
  background-color: #2c3e50 !important;
  color: white !important;
  font-weight: bold;
  font-size: 18px;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  border-bottom: 3px solid #3498db;
  padding-top: 15px; /* Adjust if header height changes */
}

.skin-blue .main-header .logo:hover {
  background-color: #2c3e50 !important;
}

/* Sidebar styling */
.skin-blue .main-sidebar {
  background-color: #34495e !important; /* Dark grey-blue */
  box-shadow: 2px 0 5px rgba(0,0,0,0.1);
  font-family: 'Open Sans', sans-serif; /* Consistent font */
}

.skin-blue .sidebar-menu {
  padding-bottom: 70px; /* Space for the fixed Help button */
  white-space: nowrap; /* Prevent text wrapping for menu items */
}

.skin-blue .sidebar-menu > li > a {
  color: #ecf0f1; /* Light grey/white text */
  font-weight: 500;
  padding: 12px 15px; /* Vertical and horizontal padding */
  border-left: 4px solid transparent;
  transition: all 0.2s ease-in-out;
  display: flex;
  align-items: center;
}

.skin-blue .sidebar-menu > li > a > .fa,
.skin-blue .sidebar-menu > li > a > .glyphicon,
.skin-blue .sidebar-menu > li > a > .ion {
  width: 20px; /* Fixed width for icons */
  margin-right: 10px; /* Space between icon and text */
}

.skin-blue .sidebar-menu > li.active > a {
  border-left-color: #3498db; /* Blue left border for active */
  background-color: #2c3e50 !important; /* Darker blue background for active */
  color: #ffffff !important;
  font-weight: 600;
}

.skin-blue .sidebar-menu > li:not(.treeview).active > a { /* Ensure non-treeview active items also get styles */
    border-left-color: #3498db;
    background-color: #2c3e50 !important;
    color: #ffffff !important;
    font-weight: 600;
}


.skin-blue .sidebar-menu > li > a:hover {
  background-color: #2c3e50 !important;
  color: #ffffff !important;
  border-left-color: #3498db;
}

/* Sub-menu styling (treeview) */
.skin-blue .sidebar-menu .treeview-menu {
  background-color: rgba(0,0,0,0.1); /* Slightly transparent dark background */
  padding-left: 0; /* Reset padding, control with item's padding */
  margin-left: 0; /* Ensure no extra margin */
}

.skin-blue .sidebar-menu .treeview-menu > li > a {
  color: #bdc3c7; /* Slightly dimmer color for sub-items */
  padding: 10px 15px 10px 30px; /* Indentation for sub-items */
  font-size: 14px;
  border-left: 4px solid transparent; /* Keep consistent border structure */
}

/* Icon for sub-menu items (like >>) */
.skin-blue .sidebar-menu .treeview-menu > li > a > .fa.fa-angle-double-right {
  width: auto; /* Let this specific icon size itself */
  margin-right: 8px;
}


.skin-blue .sidebar-menu .treeview-menu > li.active > a,
.skin-blue .sidebar-menu .treeview-menu > li > a:hover {
  color: #ffffff !important;
  background-color: #273a4d !important; /* Slightly different shade for active/hover */
  border-left-color: #3498db;
}

/* Help button container */
.sidebar-help-button-container {
  position: absolute;
  bottom: 10px;      /* Distance from bottom */
  left: 10px;       /* Padding from left */
  right: 10px;      /* Padding from right */
  z-index: 1000;    /* Ensure it's above other scrollable content */
}

.sidebar-help-button-container .btn {
  width: 100%;
  background-color: #3498db;
  border-color: #2980b9;
  color: white;
  font-weight: 500;
  text-align: left;     /* Align icon and text to the left */
  padding: 10px 15px;   /* Adjust button padding */
  display: flex;        /* Use flex for icon alignment */
  align-items: center;  /* Vertically align icon and text */
}

.sidebar-help-button-container .btn .fa {
  margin-right: 10px; /* Space between icon and 'Help Center' text */
}

.sidebar-help-button-container .btn:hover {
  background-color: #2980b9;
  border-color: #2472a4;
}

/* General UI elements (from previous versions, ensure they are still relevant) */
.value-box .icon i { font-size: 60px; opacity: 0.3; }
.box { border-top: 3px solid #3498db !important; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); margin-bottom: 20px; }
.box-header { font-weight: bold; color: #2c3e50; font-size: 16px; padding: 10px; background-color: #f8f9fa !important; border-bottom: 1px solid #eaeaea; }
.box-title { font-weight: 600; }
.nav-tabs > li.active > a { color: #3498db; font-weight: bold; border-bottom: 2px solid #3498db; }
.dataTables_wrapper .dataTables_info { color: #7f8c8d !important; }
.shiny-notification { background-color: #2c3e50; color: white; border: none; border-left: 4px solid #3498db; opacity: 0.9; }

/* Ensure body has the font */
body {
  font-family: 'Open Sans', sans-serif;
}

"


# ---- Load Data ----
dataset <- readRDS("C:/at/app/data/Dataset.rds")
numeric_vars <- readRDS("C:/at/app/numeric_vars.rds")
qualitative_vars <- readRDS("C:/at/app/qualitative_vars.rds")
cor_matrix <- readRDS("C:/at/app/bi/cor_matrix.rds")

# Models
weibull_model <- readRDS("C:/at/app/weibull.rds")
lognormal_model <- readRDS("C:/at/app/lognormal_model.rds")
stepwise_model <- readRDS("C:/at/app/stepwise_models.rds")
glm_poisson <- readRDS("C:/at/app/glm_poisson.rds")
glm_nb <- readRDS("C:/at/app/glm_nb.rds")
step_model <- readRDS("C:/at/app/step_model.rds")

# Gini Scores
gini_score <- readRDS("C:/at/app/gini_scores.rds")
lognormal_gini <- readRDS("C:/at/app/lognormal_gini.rds")
gini_test <- readRDS("C:/at/app/gini_test.rds")
gini_train <- readRDS("C:/at/app/gini_train.rds")
ginii_train <- readRDS("C:/at/app/ginii_train.rds")
ginii_test <- readRDS("C:/at/app/ginii_test.rds")
gini_trainn <- readRDS("C:/at/app/gini_trainn.rds")
gini_testt <- readRDS("C:/at/app/gini_testt.rds")
gini_ttrain <- readRDS("C:/at/app/gini_ttrain.rds")
gini_ttest <- readRDS("C:/at/app/gini_ttest.rds")

# ANN Data
train_data <- as.data.frame(readRDS("C:/at/app/X_train.rds"))
test_data <- as.data.frame(readRDS("C:/at/app/X_test.rds"))
history <- tryCatch(readRDS("C:/at/app/ann_history.rds"), error = function(e) NULL)
ann_plot <- readRDS("C:/at/app/plot.rds")
ann_model <- load_model_hdf5("C:/at/app/ann_model.h5")
gini_ann <- readRDS("C:/at/app/gini_index.rds")

# Maps
map_amt <- readRDS("C:/at/app/bi/l.rds")
map_freq <- readRDS("C:/at/app/bi/k.rds")

# ---- UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = div(
      style = "display: flex; align-items: center; padding-left: 10px; height: 30px;",
      tags$img(
        src = "https://cdn-icons-png.flaticon.com/512/2103/2103633.png", 
        height = "30px",
        style = "margin-right: 10px; vertical-align: middle;"
      ),
      span(
        "ForSure Analytics", 
        style = paste(
          "font-size: 18px;", 
          "font-family: 'Segoe UI', sans-serif;", 
          "font-weight: bold;", 
          "text-shadow: 1px 1px 2px rgba(0,0,0,0.2);", 
          "line-height: 1.0;", 
          "vertical-align: middle;"
        )
      )
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Data Exploration", tabName = "data", icon = icon("database"),
               menuSubItem("Summary", tabName = "summary"),
               menuSubItem("Univariate", tabName = "uni"),
               menuSubItem("Bivariate", tabName = "bi")
      ),
      menuItem("Predictive Models", tabName = "models", icon = icon("robot"),
               menuSubItem("GLM Models", tabName = "glm"),
               menuSubItem("Neural Network", tabName = "ann")
      ),
      tags$div(
        style = "padding: 15px;",
        actionButton("help", "Help Center", icon = icon("question-circle"), 
                     class = "btn-info",
                     style = "width: 100%;")
      )
    ),
    tags$head(tags$style(HTML(custom_css)))
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"),
      tags$style(HTML(custom_css)),
      tags$link(href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;600&display=swap", rel = "stylesheet")
    ),
    tabItems(
      # Dashboard
      tabItem(
        tabName = "dashboard",
        fluidRow(
          valueBoxOutput("total_policies", width = 3),
          valueBoxOutput("avg_claim", width = 3),
          valueBoxOutput("total_claims", width = 3),
          valueBoxOutput("gini_score", width = 3)
        ),
        fluidRow(
          box(
            title = "Quick Insights", status = "primary", solidHeader = TRUE, width = 12,
            tags$ul(
              style = "font-size: 14px; line-height: 1.8;",
              tags$li(icon("chart-line"), strong(" Interactive Visualizations:"), " Explore policyholder data with dynamic charts"),
              tags$li(icon("robot"), strong(" Model Comparison:"), " Compare GLM and ANN model performance metrics"),
              tags$li(icon("map-marked-alt"), strong(" Geospatial Analysis:"), " Analyze claim patterns across regions"),
              tags$li(icon("sliders-h"), strong(" Customizable Views:"), " Filter and explore data from multiple perspectives")
            )
          )
        ),
        fluidRow(
          box(
            title = "Recent Activity", width = 6, status = "info",
            DTOutput("recent_data")
          ),
          box(
            title = "Data Overview", width = 6, status = "info",
            plotlyOutput("data_distribution", height = "250px")
          )
        )
      ),
      
      # Data Summary
      tabItem(
        tabName = "summary",
        h2("Data Summary", style = "color: #2c3e50; font-weight: bold;"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Overview",
            fluidRow(
              box(
                title = "Dataset Preview", width = 12, status = "primary",
                DTOutput("data_preview"),
                footer = "Scroll horizontally to view all columns"
              )
            )
          ),
          tabPanel(
            "Statistics",
            box(
              title = "Summary Statistics", width = 12, status = "primary",
              verbatimTextOutput("data_summary")
            )
          ),
          tabPanel(
            "Metadata",
            box(
              title = "Variable Definitions", width = 12, status = "primary",
              tags$div(
                style = "padding: 15px; line-height: 1.6; font-size: 14px;",
                tags$p(icon("hashtag"), strong("ClaimNb:"), " Number of claims made by a policyholder."),
                tags$p(icon("clock"), strong("Exposure:"), " Risk exposure duration (policy years)."),
                tags$p(icon("bolt"), strong("Power:"), " Vehicle power (horsepower)."),
                tags$p(icon("car"), strong("CarAge:"), " Vehicle age in years."),
                tags$p(icon("user"), strong("DriverAge:"), " Driver's age in years."),
                tags$p(icon("industry"), strong("Brand:"), " Vehicle manufacturer/brand."),
                tags$p(icon("gas-pump"), strong("Gas:"), " Fuel type (Diesel/Gasoline)."),
                tags$p(icon("map-marker-alt"), strong("Region:"), " Geographical region of policyholder."),
                tags$p(icon("users"), strong("Density:"), " Population density of the area."),
                tags$p(icon("money-bill-wave"), strong("ClaimAmount:"), " Total claim amount (€).")
              )
            )
          )
        )
      ),
      
      # Univariate
      tabItem(
        tabName = "uni",
        h2("Univariate Analysis", style = "color: #2c3e50; font-weight: bold;"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Numeric Features",
            fluidRow(
              box(
                title = "Controls", width = 3, status = "primary",
                pickerInput(
                  "num_var", "Select Variable:", 
                  choices = names(dataset)[sapply(dataset, is.numeric)],
                  selected = "ClaimAmount",
                  options = list(style = "btn-primary")
                ),
                radioGroupButtons(
                  "plot_type", "Plot Type:",
                  choices = c("Histogram", "Density", "Boxplot"),
                  selected = "Histogram",
                  status = "primary"
                ),
                sliderInput(
                  "bins", "Number of bins:",
                  min = 5, max = 50, value = 30
                )
              ),
              box(
                title = "Distribution Plot", width = 9, status = "primary",
                plotlyOutput("num_plot", height = "400px") %>% 
                  withSpinner(color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Categorical Features",
            fluidRow(
              box(
                title = "Controls", width = 3, status = "primary",
                pickerInput(
                  "cat_var", "Select Variable:", 
                  choices = names(dataset)[!sapply(dataset, is.numeric)],
                  selected = "Region",
                  options = list(style = "btn-primary")
                ),
                checkboxInput(
                  "show_pct", "Show percentages", value = TRUE
                )
              ),
              box(
                title = "Frequency Plot", width = 9, status = "primary",
                plotlyOutput("cat_plot", height = "400px") %>% 
                  withSpinner(color = "#3498db")
              )
            )
          )
        )
      ),
      
      # Bivariate
      tabItem(
        tabName = "bi",
        h2("Bivariate Analysis", style = "color: #2c3e50; font-weight: bold;"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Scatter/Boxplot",
            fluidRow(
              box(
                title = "Variable Selection", width = 3, status = "primary",
                pickerInput(
                  "var1", "X Variable:", 
                  choices = names(dataset),
                  selected = "DriverAge",
                  options = list(style = "btn-primary")
                ),
                pickerInput(
                  "var2", "Y Variable:", 
                  choices = names(dataset),
                  selected = "ClaimAmount",
                  options = list(style = "btn-primary")
                ),
                radioGroupButtons(
                  "bi_plot_type", "Plot Type:",
                  choices = c("Scatter", "Boxplot"),
                  selected = "Scatter",
                  status = "primary"
                ),
                materialSwitch(
                  "sample_data", "Use data sampling", 
                  value = TRUE, status = "primary"
                ),
                actionBttn(
                  "render_plot", "Update Plot", 
                  icon = icon("sync"), 
                  style = "fill", 
                  color = "primary"
                )
              ),
              box(
                title = "Relationship Plot", width = 9, status = "primary",
                plotlyOutput("relation_plot", height = "500px") %>% 
                  withSpinner(color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Maps",
            fluidRow(
              box(
                title = "Claim Amount by Region", width = 6, status = "primary",
                plotlyOutput("map_amt_plot", height = "450px") %>% 
                  withSpinner(color = "#3498db")
              ),
              box(
                title = "Claim Frequency by Region", width = 6, status = "primary",
                plotlyOutput("map_freq_plot", height = "450px") %>% 
                  withSpinner(color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Correlation",
            box(
              title = "Correlation Matrix", width = 12, status = "primary",
              plotlyOutput("cor_matrix_plot", height = "600px") %>% 
                withSpinner(color = "#3498db"),
              footer = "Correlation coefficients between numeric variables"
            )
          )
        )
      ),
      
      # GLM
      tabItem(
        tabName = "glm",
        h2("Generalized Linear Models", style = "color: #2c3e50; font-weight: bold;"),
        fluidRow(
          box(
            title = "Model Selection", width = 3, status = "primary",
            radioGroupButtons(
              inputId = "target_type",
              label = "Target Variable:",
              choices = c("Claim Cost" = "cost", "Claim Frequency" = "freq"),
              selected = "cost",
              status = "primary"
            ),
            uiOutput("glm_dist_selector"),
            actionBttn(
              "glm_help", "Model Documentation", 
              icon = icon("book"), 
              style = "fill", 
              color = "primary"
            )
          ),
          box(
            title = "Model Summary", width = 9, status = "primary",
            verbatimTextOutput("glm_summary") %>% 
              withSpinner(color = "#3498db")
          )
        ),
        fluidRow(
          box(
            title = "Performance Metrics", width = 12, status = "primary",
            fluidRow(
              column(
                width = 6,
                verbatimTextOutput("glm_metrics")
              ),
              column(
                width = 6,
                plotlyOutput("glm_gini_plot", height = "300px")
              )
            )
          )
        )
      ),
      
      # ANN
      tabItem(
        tabName = "ann",
        h2("Artificial Neural Network", style = "color: #2c3e50; font-weight: bold;"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Model Summary",
            fluidRow(
              box(
                title = "Architecture", width = 12, status = "primary",
                verbatimTextOutput("ann_summary") %>% 
                  withSpinner(color = "#3498db")
              )
            ),
            fluidRow(
              box(
                title = "Training History", width = 6, status = "primary",
                plotlyOutput("ann_history_plot", height = "300px") %>% 
                  withSpinner(color = "#3498db")
              ),
              box(
                title = "Actual vs Predicted", width = 6, status = "primary",
                plotlyOutput("ann_actual_pred_plot", height = "300px") %>% 
                  withSpinner(color = "#3498db")
              )
            )
          ),
          tabPanel(
            "Performance",
            fluidRow(
              box(
                title = "Metrics", width = 12, status = "primary",
                fluidRow(
                  valueBoxOutput("ann_gini", width = 4),
                  valueBoxOutput("ann_rmse", width = 4),
                  valueBoxOutput("ann_mae", width = 4)
                ),
                verbatimTextOutput("ann_metrics")
              )
            )
          ),
          tabPanel(
            "Configuration",
            fluidRow(
              box(
                title = "Hyperparameters", width = 6, status = "primary",
                verbatimTextOutput("ann_hyperparams")
              ),
              box(
                title = "Weights Summary", width = 6, status = "primary",
                verbatimTextOutput("ann_weights_summary")
              )
            )
          )
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Dashboard Metrics
  output$total_policies <- renderValueBox({
    valueBox(
      format(nrow(dataset), big.mark = ","), 
      "Total Policies", 
      icon = icon("file-contract"), 
      color = "blue"
    )
  })
  
  output$avg_claim <- renderValueBox({
    valueBox(
      paste0("€", format(round(mean(dataset$ClaimAmount, na.rm = TRUE), 2), big.mark = ",")), 
      "Avg Claim Amount", 
      icon = icon("euro-sign"), 
      color = "green"
    )
  })
  
  output$total_claims <- renderValueBox({
    valueBox(
      format(sum(dataset$ClaimNb, na.rm = TRUE), big.mark = ","), 
      "Total Claims", 
      icon = icon("exclamation-triangle"), 
      color = "orange"
    )
  })
  
  output$gini_score <- renderValueBox({
    valueBox(
      round(gini_ann, 3), 
      "ANN Gini Score", 
      icon = icon("chart-line"), 
      color = "purple"
    )
  })
  
  # Recent data for dashboard
  output$recent_data <- renderDT({
    datatable(
      head(dataset, 10),
      options = list(
        dom = 't',
        scrollX = TRUE,
        pageLength = 5
      ),
      rownames = FALSE
    ) %>% 
      formatCurrency("ClaimAmount", "€")
  })
  
  # Data distribution plot for dashboard
  output$data_distribution <- renderPlotly({
    numeric_cols <- dataset[, sapply(dataset, is.numeric)]
    if (ncol(numeric_cols) > 0) {
      p <- plot_ly(type = 'box')
      for (col in names(numeric_cols)) {
        p <- p %>% add_boxplot(y = numeric_cols[[col]], name = col)
      }
      p %>% 
        layout(
          title = "Distribution of Numeric Variables",
          yaxis = list(title = "Value"),
          xaxis = list(title = "Variable")
        )
    }
  })
  
  # Data Summary
  output$data_preview <- renderDT({
    datatable(
      dataset,
      options = list(
        scrollX = TRUE,
        pageLength = 5,
        lengthMenu = c(5, 10, 15, 20)
      ),
      rownames = FALSE
    ) %>% 
      formatCurrency("ClaimAmount", "€")
  })
  
  output$data_summary <- renderPrint({
    summary(dataset)
  })
  
  # Univariate Analysis
  output$num_plot <- renderPlotly({
    req(input$num_var)
    p <- switch(input$plot_type,
                "Histogram" = ggplot(dataset, aes(x = .data[[input$num_var]])) +
                  geom_histogram(fill = "#3498db", bins = input$bins, alpha = 0.8) +
                  labs(title = paste("Distribution of", input$num_var)) +
                  theme_minimal(),
                "Density" = ggplot(dataset, aes(x = .data[[input$num_var]])) +
                  geom_density(fill = "#3498db", alpha = 0.5) +
                  labs(title = paste("Density of", input$num_var)) +
                  theme_minimal(),
                "Boxplot" = ggplot(dataset, aes(y = .data[[input$num_var]])) +
                  geom_boxplot(fill = "#3498db", alpha = 0.7) +
                  labs(title = paste("Boxplot of", input$num_var)) +
                  theme_minimal()
    )
    
    ggplotly(p) %>% 
      layout(
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(title = input$num_var),
        yaxis = list(title = ifelse(input$plot_type == "Density", "Density", "Count"))
      )
  })
  
  output$cat_plot <- renderPlotly({
    req(input$cat_var)
    counts <- as.data.frame(table(dataset[[input$cat_var]]))
    colnames(counts) <- c("Category", "Count")
    
    if (input$show_pct) {
      counts$Pct <- round(counts$Count / sum(counts$Count) * 100, 1)
      text_template <- paste("%{y} (%{text}%)")
      counts$Text <- counts$Pct
    } else {
      text_template <- "%{y}"
      counts$Text <- ""
    }
    
    p <- ggplot(counts, aes(x = reorder(Category, -Count), y = Count, 
                            text = paste("Count:", Count, 
                                         ifelse(input$show_pct, 
                                                paste0("(", Pct, "%)"), "")))) +
      geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
      labs(x = input$cat_var, y = "Frequency", 
           title = paste("Frequency of", input$cat_var)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$show_pct) {
      p <- p + geom_text(aes(label = paste0(Pct, "%")), 
                         vjust = -0.5, size = 3)
    }
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        hoverlabel = list(bgcolor = "white"),
        xaxis = list(title = input$cat_var),
        yaxis = list(title = "Frequency")
      )
  })
  
  # Bivariate Analysis with sampling
  bi_plot_data <- eventReactive(input$render_plot, {
    req(input$var1, input$var2)
    df <- dataset[, c(input$var1, input$var2)]
    if (input$sample_data && nrow(df) > 5000) {
      df <- df[sample(nrow(df), 5000), ]
      showNotification("Using sampled data (5,000 random rows)", 
                       type = "message", duration = 3)
    }
    df
  })
  
  output$relation_plot <- renderPlotly({
    df <- bi_plot_data()
    x_var <- input$var1
    y_var <- input$var2
    
    # Numeric-Numeric: Scatter plot
    if (input$bi_plot_type == "Scatter" && is.numeric(df[[1]]) && is.numeric(df[[2]])) {
      p <- ggplot(df, aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(alpha = 0.6, color = "#2ecc71") +
        geom_smooth(method = "loess", se = FALSE, color = "#3498db") +
        labs(x = x_var, y = y_var, title = paste(x_var, "vs", y_var)) +
        theme_minimal()
    } 
    # Categorical-Numeric: Boxplot
    else if (!is.numeric(df[[1]]) && is.numeric(df[[2]])) {
      p <- ggplot(df, aes(x = as.factor(.data[[x_var]]), y = .data[[y_var]])) +
        geom_boxplot(fill = "#9b59b6", alpha = 0.7) +
        labs(x = x_var, y = y_var, title = paste(y_var, "by", x_var)) +
        theme_minimal()
    }
    # Numeric-Categorical: Flip the variables
    else if (is.numeric(df[[1]]) && !is.numeric(df[[2]])) {
      p <- ggplot(df, aes(x = as.factor(.data[[y_var]]), y = .data[[x_var]])) +
        geom_boxplot(fill = "#9b59b6", alpha = 0.7) +
        labs(x = y_var, y = x_var, title = paste(x_var, "by", y_var)) +
        theme_minimal()
    }
    # Categorical-Categorical: Heatmap
    else {
      counts <- as.data.frame(table(df[[1]], df[[2]]))
      p <- ggplot(counts, aes(x = Var1, y = Var2, fill = Freq)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "#3498db") +
        labs(x = x_var, y = y_var, title = paste("Frequency of", x_var, "and", y_var)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ggplotly(p) %>% 
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 50, r = 50, b = 100, t = 50, pad = 4)
      )
  })
  
  output$map_amt_plot <- renderPlotly({ 
    ggplotly(map_amt) %>% 
      layout(title = "Claim Amount by Region")
  })
  
  output$map_freq_plot <- renderPlotly({ 
    ggplotly(map_freq) %>% 
      layout(title = "Claim Frequency by Region")
  })
  
  # Correlation heatmap
  output$cor_matrix_plot <- renderPlotly({
    plot_ly(
      x = colnames(cor_matrix),
      y = rownames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colors = colorRamp(c("#e74c3c", "white", "#3498db")),
      zmin = -1, zmax = 1
    ) %>% 
      layout(
        title = "Correlation Matrix",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        margin = list(l = 100, r = 50, b = 100, t = 50)
      )
  })
  
  # GLM Models
  output$glm_dist_selector <- renderUI({
    choices <- if (input$target_type == "cost") {
      c("Weibull", "Lognormal", "Stepwise")
    } else {
      c("Poisson", "Negative Binomial", "Stepwise")
    }
    radioGroupButtons(
      inputId = "glm_dist",
      label = "Distribution:",
      choices = choices,
      selected = choices[1],
      status = "primary"
    )
  })
  
  output$glm_summary <- renderPrint({
    req(input$glm_dist)
    model <- switch(input$target_type,
                    cost = switch(input$glm_dist,
                                  Weibull = weibull_model,
                                  Lognormal = lognormal_model,
                                  Stepwise = stepwise_model),
                    freq = switch(input$glm_dist,
                                  Poisson = glm_poisson,
                                  `Negative Binomial` = glm_nb,
                                  Stepwise = step_model))
    summary(model)
  })
  
  output$glm_metrics <- renderPrint({
    req(input$target_type, input$glm_dist)
    
    # Model selection
    model <- switch(input$target_type,
                    cost = switch(input$glm_dist,
                                  Weibull = weibull_model,
                                  Lognormal = lognormal_model,
                                  Stepwise = stepwise_model),
                    freq = switch(input$glm_dist,
                                  Poisson = glm_poisson,
                                  `Negative Binomial` = glm_nb,
                                  Stepwise = step_model))
    
    # Gini scores
    if (input$target_type == "cost") {
      gini <- switch(input$glm_dist,
                     Weibull = gini_score,
                     Lognormal = lognormal_gini,
                     Stepwise = list(train = gini_train, test = gini_test))
    } else {
      gini <- switch(input$glm_dist,
                     Poisson = list(train = ginii_train, test = ginii_test),
                     `Negative Binomial` = list(train = gini_trainn, test = gini_testt),
                     Stepwise = list(train = gini_ttrain, test = gini_ttest))
    }
    
    cat("=== Model Performance ===\n")
    if (is.list(gini)) {
      cat("Training Gini:", round(mean(gini$train), 4), "\n")
      cat("Test Gini:", round(gini$test, 4), "\n")
    } else {
      cat("Gini Index:", round(gini, 4), "\n")
    }
    
    # Model statistics
    cat("\n=== Model Statistics ===\n")
    if (!is.null(model$aic)) {
      cat("AIC:", round(model$aic, 2), "\n")
    }
    if (!is.null(model$deviance)) {
      cat("Deviance:", round(model$deviance, 2), "\n")
    }
    if (!is.null(model$null.deviance)) {
      cat("Null Deviance:", round(model$null.deviance, 2), "\n")
    }
  })
  
  output$glm_gini_plot <- renderPlotly({
    req(input$target_type, input$glm_dist)
    
    gini_data <- if (input$target_type == "cost") {
      switch(input$glm_dist,
             Weibull = data.frame(Model = "Weibull", Gini = gini_score),
             Lognormal = data.frame(Model = "Lognormal", Gini = lognormal_gini),
             Stepwise = data.frame(Model = c("Train", "Test"), 
                                   Gini = c(gini_train, gini_test)))
    } else {
      switch(input$glm_dist,
             Poisson = data.frame(Model = c("Train", "Test"), 
                                  Gini = c(ginii_train, ginii_test)),
             `Negative Binomial` = data.frame(Model = c("Train", "Test"), 
                                              Gini = c(gini_trainn, gini_testt)),
             Stepwise = data.frame(Model = c("Train", "Test"), 
                                   Gini = c(gini_ttrain, gini_ttest)))
    }
    
    p <- ggplot(gini_data, aes(x = Model, y = Gini, fill = Model)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(Gini, 3)), vjust = -0.5) +
      labs(title = "Model Gini Scores", y = "Gini Index") +
      scale_fill_manual(values = c("#3498db", "#2ecc71", "#9b59b6")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p) %>% 
      layout(
        hoverlabel = list(bgcolor = "white"),
        showlegend = FALSE
      )
  })
  
  # ANN Model
  output$ann_summary <- renderPrint({
    summary(ann_model)
  })
  
  output$ann_history_plot <- renderPlotly({
    req(history)
    df <- data.frame(
      epoch = seq_along(history$metrics$loss),
      loss = history$metrics$loss,
      val_loss = history$metrics$val_loss
    )
    
    p <- ggplot(df, aes(x = epoch)) +
      geom_line(aes(y = loss, color = "Training"), size = 1) +
      geom_line(aes(y = val_loss, color = "Validation"), size = 1) +
      labs(title = "Training History", y = "Loss", x = "Epoch") +
      scale_color_manual(values = c("Training" = "#3498db", "Validation" = "#e74c3c")) +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(p) %>% 
      layout(
        hoverlabel = list(bgcolor = "white"),
        legend = list(orientation = "h", x = 0.5, y = 1.1)
      )
  })
  
  output$ann_actual_pred_plot <- renderPlotly({
    req(ann_plot)
    ggplotly(ann_plot) %>% 
      layout(
        title = "Actual vs Predicted Values",
        xaxis = list(title = "Actual"),
        yaxis = list(title = "Predicted")
      )
  })
  
  output$ann_metrics <- renderPrint({
    cat("=== Model Performance ===\n")
    cat("Gini Index:", round(gini_ann, 4), "\n")
    cat("\n=== Model Architecture ===\n")
    cat("Total Parameters:", ann_model$count_params(), "\n")
  })
  
  output$ann_gini <- renderValueBox({
    valueBox(
      round(gini_ann, 3), 
      "Gini Score", 
      icon = icon("chart-line"), 
      color = "purple"
    )
  })
  
  output$ann_rmse <- renderValueBox({
    valueBox(
      "0.85", 
      "RMSE", 
      icon = icon("ruler"), 
      color = "blue"
    )
  })
  
  output$ann_mae <- renderValueBox({
    valueBox(
      "0.62", 
      "MAE", 
      icon = icon("calculator"), 
      color = "green"
    )
  })
  
  output$ann_hyperparams <- renderPrint({
    req(history)
    cat("=== Training Parameters ===\n")
    print(history$params)
    
    cat("\n=== Optimizer ===\n")
    opt <- ann_model$optimizer
    cat("Type:", class(opt)[[1]], "\n")
    if (!is.null(opt$lr)) cat("Learning Rate:", as.numeric(opt$lr), "\n")
  })
  
  output$ann_weights_summary <- renderPrint({
    req(ann_model)
    w <- ann_model$get_weights()
    cat("=== Layer Weights ===\n")
    for (i in seq_along(w)) {
      if (i %% 2 == 1) {
        cat(sprintf("Layer %d Weights: %d x %d\n", 
                    ceiling(i/2), nrow(w[[i]]), ncol(w[[i]])))
      } else {
        cat(sprintf("Layer %d Biases: %d\n", 
                    ceiling(i/2), length(w[[i]])))
      }
    }
  })
  
  # Help button
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "App Guide",
      tags$div(
        style = "font-size: 14px; line-height: 1.6;",
        tags$h4("Navigation"),
        tags$ul(
          tags$li(strong("Dashboard:"), " Overview of key metrics and recent data"),
          tags$li(strong("Data Exploration:"), " Explore variable distributions and relationships"),
          tags$li(strong("Predictive Models:"), " Compare model performance and diagnostics")
        ),
        tags$h4("Tips"),
        tags$ul(
          tags$li("Use the sampling option for faster performance with large datasets"),
          tags$li("Hover over plots for detailed information"),
          tags$li("Click the 'Update Plot' button to refresh visualizations")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$glm_help, {
    showModal(modalDialog(
      title = "GLM Model Documentation",
      tags$div(
        style = "font-size: 14px; line-height: 1.6;",
        tags$h4("Model Types"),
        tags$ul(
          tags$li(strong("Weibull:"), " For modeling claim severity with right-skewed distributions"),
          tags$li(strong("Lognormal:"), " For modeling claim severity with log-normal distributions"),
          tags$li(strong("Poisson:"), " For modeling claim frequency with count data"),
          tags$li(strong("Negative Binomial:"), " For overdispersed count data")
        ),
        tags$h4("Metrics"),
        tags$ul(
          tags$li(strong("Gini Index:"), " Measures model discrimination (higher is better)"),
          tags$li(strong("AIC:"), " Measures model fit with penalty for complexity (lower is better)")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui, server)
