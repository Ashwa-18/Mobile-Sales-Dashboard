library(shiny)
library(shinydashboard)
library(plotly)
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(tidyr)

# DB connection
localuserpassword <- "ashwa770"
storiesDb <- dbConnect(
  RMariaDB::MariaDB(),
  user = 'root',
  password = localuserpassword,
  dbname = 'Mobile',
  host = 'localhost'
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mobile Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introductory Report", tabName = "intro", icon = icon("info-circle")),
      selectInput("city_filter", "Select City:", choices = NULL, selected = NULL, multiple = TRUE, selectize = TRUE),
      menuItem("Brand Analysis", tabName = "brandanalysis", icon = icon("chart-bar")),
      menuItem("Mobile Model Analysis", tabName = "bubble" , icon = icon("chart-bar")),
      selectInput("year_filter", "Select Year:", choices = NULL, selected = NULL, multiple = TRUE, selectize = TRUE),
      selectInput("month_filter", "Select Month:", choices = NULL, selected = NULL, multiple = TRUE, selectize = TRUE),
      sliderInput("age_filter", "Customer Age:", min = 10, max = 70, value = c(10, 70)),
      checkboxGroupInput(
        inputId = "payment_filter",
        label = "Select Payment Method(s):",
        choices = c("Cash", "Credit Card", "Online", "Debit Card", "UPI"),
        selected = NULL
      ),
      
      menuItem("Trend Analysis", tabName = "Trend ", icon = icon("chart-line")),
      menuItem("Customer Rating Anaysis", tabName = "barchartmodel", icon = icon("circle")),
      selectInput("theme_filter", "Select Theme:", choices = c("Light", "Dark", "Blue"), selected = "Light")
      
      
    )
  ),
  dashboardBody(
    tags$head(
      # Import Google Font
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Poppins:wght@500&display=swap"),
      
      # CSS Styles
      tags$style(HTML("
    /* Theme-Specific Backgrounds */
    body.light-theme .content-wrapper,
    body.light-theme .main-footer,
    body.light-theme .main-header {
      background-color: white !important;
      color: black !important;
    }

    body.dark-theme .content-wrapper,
    body.dark-theme .main-footer,
    body.dark-theme .main-header {
      background-color: #1e1e1e !important;
      color: white !important;
    }

    body.blue-theme .content-wrapper,
    body.blue-theme .main-footer,
    body.blue-theme .main-header {
      background-color: #25B6D2 !important;
      color: #003366 !important;
    }

    /* Sidebar Styling */
    .main-sidebar {
      background-color: #1F2E3D !important;
      box-shadow: 2px 0px 10px rgba(0,0,0,0.2);
      border-right: 1px solid #333;
    }

    .sidebar-menu > li > a {
      color: #ffffff !important;
      font-weight: 500;
      border-radius: 10px;
      margin: 5px 0px;
      padding: 10px 9px;
      letter-spacing: 1px;
      font-family: 'Poppins', sans-serif;
    }

    .sidebar-menu > li:hover > a,
    .sidebar-menu > li.active > a {
      background-color: #25B6D2 !important;
      color: #fff !important;
      font-weight: bold;
    }
    
   #shiny-tab-intro h3 {
    color: #25B6D2 !important;
    font-weight: bold;
    border-bottom: 2px solid #25B6D2;
    padding-bottom: 5px;
    text-transform: uppercase;
  }
    /* Heading Styling */
    h3 {
      color: white !important;
      font-weight: bold;
      border-bottom: 2px solid #25B6D2;
      padding-bottom: 5px;
      text-transform: uppercase;
    }

    /* Body Text Styling */
    p {
      font-size: 20px;
    }

    /* Title Styling with Animation */
    .intro-title {
      font-size: 30px;
      font-weight: bold;
      color: #25B6D2;
      text-align: center;
      margin-top: 20px;
      font-family: 'Poppins', sans-serif;
      animation: fadeSlideUp 1.2s ease-out;
    }

    @keyframes fadeSlideUp {
      0% { opacity: 0; transform: translateY(20px); }
      100% { opacity: 1; transform: translateY(0); }
    }

    .intro-content {
      font-size: 16px;
      margin: 30px auto;
      width: 85%;
      line-height: 1.6;
      color: #333333;
    }

    .insight-block {
      display: flex;
      align-items: center;
      margin-bottom: 20px;
    }

    .insight-block img {
      height: 30px;
      margin-right: 15px;
      transition: transform 0.3s ease;
    }

    .insight-block img:hover {
      transform: scale(1.3);
    }

    .insight-text {
      font-size: 20px;
      font-family: 'Poppins', sans-serif;
    }
  ")),
      
      # Theme Toggle Script
      tags$script(HTML("
    Shiny.addCustomMessageHandler('setTheme', function(theme) {
      document.body.classList.remove('light-theme', 'dark-theme', 'blue-theme');
      document.body.classList.add(theme + '-theme');
    });
  "))
    ),
    tabItems(
      tabItem(tabName = "intro",
              
              # Title
              div(class = "intro-title", "📱 Mobile Sales Analysis Dashboard"),
              
              # Banner Image
              div(style = "text-align: center; margin-top: 20px;",
                  tags$img(src = "money.gif", height = "200px")
              ),
              
              # Objective
              div(class = "intro-content",
                  HTML("
        <h3>Objective of the Project:</h3>
        <p>
        The primary aim of this dashboard is to analyze <b>Sales</b> as the <b>Target Variable</b>, providing a comprehensive view of how it varies by city, brand, time, and quantity. This dataset includes critical business variables such as:
        </p>
        <ul>
          <li>City</li>
          <li>Brand</li>
          <li>Day</li>
          <li>Quantity Sold</li>
          <li>Customer Rating</li>
          <li>Payment Method</li>
          <li>Sales Value</li>
        </ul>
        <h3>Key Insights to Be Explored:</h3>
      ")
              ),
              
              # Key Insights with Image Bullets
              div(class = "intro-content",
                  div(class = "insight-block",
                      tags$img(src = "right-arrow.gif"),
                      div(class = "insight-text", "Which cities and brands generate the highest sales?")
                  ),
                  div(class = "insight-block",
                      tags$img(src = "right-arrow.gif"),
                      div(class = "insight-text", "Are there specific days or weeks where sales peak or drop?")
                  ),
                  div(class = "insight-block",
                      tags$img(src = "right-arrow.gif"),
                      div(class = "insight-text", "Understand customer buying patterns and preferences.")
                  )
              ),
              
              # 👉 Scroll Button to Dataset Section
              div(style = "text-align: center; margin-top: 30px;",
                  actionButton("goto_data", "Go to Dataset 📊", class = "btn-info", style = "font-size: 16px; padding: 10px 20px;")
              ),
              
              br(), br(),
              tags$hr(),
              
              # 👉 Dataset Preview Section
              # 👉 Dataset Preview Section
              div(id = "data_section",
                  h3("📄 Complete Mobile Dataset"),
                  p("Here is a preview of the mobile sales data used in the dashboard."),
                  div(
                    style = "overflow-x: auto; width: 100%;",
                    DTOutput("introDataTable")
                  )
                  
              )
              
      ),
      
      
      
      
      tabItem(tabName = "brandanalysis",
              fluidRow(
                valueBoxOutput("totalBrands", width = 4),
                valueBoxOutput("topBrandSales", width = 4),
                valueBoxOutput("totalSalesAllBrands", width = 4)
              ),
              
              
              fluidRow(
                box(
                  title = "Are there any brands with outliers (very high or low sales) ?",
                  plotlyOutput("boxplotChart"),
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  br(),
                  
                  # 👇 Interpretation and Advantages Block
                  tags$div(
                    style = "background-color:#7FD6E4; padding:12px; border-radius:10px; font-size:14px;",
                    HTML("
      <b>Interpretation:</b>
      <ul>
        <li>This chart shows how much the sales go up or down for each mobile brand.</li>
        <li>The middle line in the box shows the average (middle) sales.</li>
        <li>A tall box means the brand’s sales change a lot between models.</li>
        <li>Dots above the boxes are outliers — models that sold much more than others.</li>
        <li>Apple and Samsung show bigger changes, while Vivo and Xiaomi have more steady sales.</li>
      </ul>

      <b>Advantage for Owner:</b>
      <ul>
        <li>Owners can see which brands have models that perform better than usual (outliers).</li>
        <li>Helps decide which brands are safe for regular sales, and which may bring surprise profits or losses.</li>
      </ul>

      <b>Advantage for Customer:</b>
      <ul>
        <li>Customers can trust brands with steady sales (e.g., Vivo, Xiaomi) as they may be more reliable.</li>
        <li>Brands with too much up and down (e.g., Apple) might have high or low quality depending on the model.</li>
      </ul>
    ")
                  )
                ),
                
                box(title = "Which Brand earned the highest total sales ? ", plotlyOutput("lollipopChart"), width = 6, solidHeader = TRUE, status = "primary"),
                box(title = "Which brand sold the most units each year ?", plotlyOutput("brandUnitBarChart"), width = 6, solidHeader = TRUE, status = "primary"),
                box(title = " Sales and Transactions by Brand", DTOutput("brandSummaryTable"), width = 12, solidHeader = TRUE, status = "primary")
              )
      ),
      
      tabItem(tabName = "Trend", 
              fluidRow(
                valueBoxOutput("totalTransactions",width = 6),
                valueBoxOutput("avgPricePerUnit",width = 6)
                
              ),
              
              
              fluidRow(
                box(
                  title = "Filters",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  fluidRow(
                    column(6,
                           selectInput("brand_filter", "Select Brand:", choices = NULL, selected = NULL, multiple = TRUE, selectize = TRUE)
                    ),
                    column(6,
                           selectInput("model_filter", "Select Mobile Model:", choices = NULL, selected = NULL, multiple = TRUE, selectize = TRUE)
                    )
                  )
                )
              ),
              fluidRow(
                box(title = "In which months are more phones sold?", plotlyOutput("lineSales"), width = 12, solidHeader = TRUE, status = "primary"),
                box(title = "Which month generated the highest total sales revenue?", plotlyOutput("areaSales"), width = 12, solidHeader = TRUE, status = "primary")
              )
      ),
      
      
      
      tabItem(tabName = "bubble",
              fluidRow(
                valueBoxOutput("totalMobileModels", width = 3),
                valueBoxOutput("topSellingModel", width = 3),
                valueBoxOutput("topModelSales",width = 6)
                
              ),
              
              fluidRow(
                box(
                  title = "Is the most expensive mobile also the most sold?",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("bubbleChart", height = "400px"),
                  br(),
                  
                  # 🔍 Interpretation Lines (Your Insights)
                  tags$div(
                    style = "background-color:#7FD6E4; padding:15px; border-radius:10px; font-size:15px;",
                    HTML("
        <b>Interpretation:</b>
        <ul>
          <li>We can see which mobile models are both expensive and popular — these appear as big bubbles on the top-right.</li>
          <li>Some models are less expensive but still have high unit sales, meaning they are affordable and widely sold.</li>
          <li>Some expensive models may have fewer units sold, so despite a high price, they might not generate high total sales.</li>
          <li>This chart helps us check if the most expensive mobile is also the best-selling one.</li>
        </ul>
      ")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "In which city does a mobile model have the highest contribution ?", plotlyOutput("salesHeatmap"), width = 12, solidHeader = TRUE, status = "primary",
                  br(),
                  
                  # 🔍 Interpretation Lines (Your Insights)
                  tags$div(
                    style = "background-color:#7FD6E4; padding:15px; border-radius:10px; font-size:15px;",
                    HTML("
        <b>Business Insights We Can Draw :</b>
        <ul>
          <li><b>Some mobile models are very popular in certain cities (shown by dark blue boxes).</b></li>
          <li><b>Run ads in cities where a model is already doing well to further increase sales.</b></li>
          <li><b>If a model is not selling well in a city, consider applying discounts or promotional offers.</b></li>
        </ul>
      ")
                  )
                )
              ),
              
              
              fluidRow(
                box(
                  title = "Do Expensive Phones Always Get Higher Ratings?",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  
                  # Chart
                  plotlyOutput("ribbonChart"),
                  br(),
                  
                  # Interpretation block
                  tags$div(
                    style = "background-color:#7FD6E4; padding:15px; border-radius:10px; font-size:15px;",
                    HTML("
        <b>Key Insights:</b>
        <ul>
          <li><b>iPhone 11</b> and <b>iPhone 12</b> are ranked high in price but drop in rating — they are expensive but not top-rated.</li>
          <li><b>Vivo Y51</b> ranks lowest in price but highest in rating — it's the most liked despite being low-cost.</li>
          <li><b>Redmi 9</b> ranks in the middle for both — it offers balanced value.</li>
          <li><b>iPhone SE</b> is affordable but not strongly liked — low price doesn’t guarantee high ratings.</li>
        </ul>
        <b>Conclusion:</b><br>
        This chart helps identify mobile models that are <b>low in cost but highly rated</b>, and others that are <b>pricey but not preferred by users</b>.
      ")
                  )
                )
              )
              
      ),
      
      # ✅ MOVE THIS OUTSIDE THE ABOVE
      tabItem(tabName = "barchartmodel",
              fluidRow(
                box(title = "Which model appears to be the customer favorite overall ?", plotlyOutput("barRatingChart"), width = 12, solidHeader = TRUE, status = "primary")
              ),
              fluidRow(
                box(title = "Which brand has the biggest gap between Best and Poor ratings ?", plotlyOutput("dumbbellChart"), width = 12, solidHeader = TRUE, status = "primary")),
              fluidRow(
                box(
                  title = " Rating Distribution",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  plotlyOutput("funnelChart")
                ),
                box(title = "Payment Method Pie Chart", width = 6,
                    status = "primary",
                    solidHeader = TRUE,
                    plotlyOutput("paymentPie"))
              )
              
              
              
              
      )
      
      
    ),
    tags$script(HTML("
  $(document).on('click', '#goto_data', function() {
    $('html, body').animate({
      scrollTop: $('#data_section').offset().top
    }, 500);
  });
"))
    
  )   
  
  
)

# SERVER
server <- function(input, output, session) {
  brand_colors <- c(
    "#ADD8E6",  # lightblue
    "#87CEEB",  # skyblue
    "#4682B4",  # steelblue
    "#1E90FF",  # dodgerblue
    "#00008B"   # darkblue
  )
  
  # Load mobile info data
  df_info <- reactive({
    dbGetQuery(storiesDb, "SELECT *, ROW_NUMBER() OVER () AS RowID FROM mobileinfo")
  })
  
  # Load date info from sec_mobsale and extract Month Name
  df_day <- reactive({
    df <- dbGetQuery(storiesDb, "SELECT `Day Name`, `Date` FROM sec_mobsale")
    df$Date <- as.Date(df$Date, format = "%d-%b-%y")  # e.g., 1-Jan-21
    df$Month <- as.numeric(format(df$Date, "%m"))
    df$MonthName <- format(df$Date, "%B")  # Full month name
    df$Year <- as.numeric(format(df$Date, "%Y"))
    df
  })
  
  # Join both datasets using RowID
  combined_data <- reactive({
    df1 <- df_info()
    df2 <- df_day()
    
    df1 <- df1 %>% mutate(RowID = row_number())
    df2 <- df2 %>% mutate(RowID = row_number())
    
    df <- inner_join(df1, df2, by = "RowID")
    df$Sales <- df$`Units Sold` * df$`Price Per Unit`
    
    # Use correct year column if duplicated
    if ("Year.y" %in% colnames(df)) {
      df$Year <- df$Year.y
    }
    
    df
  })
  
  # Update filters dynamically
  observe({
    data <- combined_data()
    
    if (!is.null(data) && nrow(data) > 0) {
      if ("City" %in% colnames(data)) {
        updateSelectInput(session, "city_filter", choices = sort(unique(data$City)))
      }
      if ("Year" %in% colnames(data)) {
        updateSelectInput(session, "year_filter", choices = sort(unique(data$Year)))
      }
      if ("MonthName" %in% colnames(data)) {
        updateSelectInput(session, "month_filter", choices = month.name)  # Jan to Dec
      }
      if ("Brand" %in% colnames(data)) {
        updateSelectInput(session, "brand_filter", choices = sort(unique(data$Brand)))
      }
      if ("Mobile Model" %in% colnames(data)) {
        updateSelectInput(session, "model_filter", choices = sort(unique(data$`Mobile Model`)))
      }
      
    }
    
  })
  
  # Apply all filters
  filtered_data <- reactive({
    df <- combined_data()
    
    if (!is.null(input$city_filter) && length(input$city_filter) > 0) {
      df <- df %>% filter(City %in% input$city_filter)
    }
    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- df %>% filter(Year %in% input$year_filter)
    }
    if (!is.null(input$month_filter) && length(input$month_filter) > 0) {
      df <- df %>% filter(MonthName %in% input$month_filter)
    }
    # Apply brand filter
    if (!is.null(input$brand_filter) && length(input$brand_filter) > 0) {
      df <- df %>% filter(Brand %in% input$brand_filter)
    }
    
    # Apply model filter
    if (!is.null(input$model_filter) && length(input$model_filter) > 0) {
      df <- df %>% filter(`Mobile Model` %in% input$model_filter)
    }
    # Apply payment method filter
    if (!is.null(input$payment_filter) && length(input$payment_filter) > 0) {
      df <- df %>% filter(`Payment Method` %in% input$payment_filter)
    }
    
    
    
    df <- df %>% filter(`Customer Age` >= input$age_filter[1], `Customer Age` <= input$age_filter[2])
    
    df
  })
  
  # Total number of unique brands
  output$totalBrands <- renderValueBox({
    df <- filtered_data()
    total <- length(unique(df$Brand))
    valueBox(value = total,
             subtitle = "Total Brands",
             icon = icon("tags"),
             color = "blue")
  })
  
  # Top Brand by Total Sales
  output$topBrandSales <- renderValueBox({
    df <- filtered_data()
    top_brand <- df %>%
      group_by(Brand) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1) %>%
      pull(Brand)
    
    valueBox(value = top_brand,
             subtitle = "Top Brand by Sales",
             icon = icon("chart-line"),
             color = "navy")
  })
  
  output$totalSalesAllBrands <- renderValueBox({
    df <- filtered_data()
    total_sales <- sum(df$Sales, na.rm = TRUE)
    total_sales_million <- round(total_sales / 1e6, 0)  # Round to 2 decimal places
    
    valueBox(
      value = paste0("$", total_sales_million, "M"),
      subtitle = "Total Sales by All Brands",
      icon = icon("dollar-sign"),
      color = "aqua"
    )
  })
  
  output$totalMobileModels <- renderValueBox({
    df <- filtered_data()
    total_models <- df %>% distinct(`Mobile Model`) %>% nrow()
    
    valueBox(
      value = total_models,
      subtitle = "Total Mobile Models",
      icon = icon("mobile-alt"),
      color = "light-blue"
    )
  })
  
  
  output$topSellingModel <- renderValueBox({
    df <- filtered_data()
    
    top_model <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1)
    
    valueBox(
      value = top_model$`Mobile Model`,
      subtitle = "Top-Selling Mobile Model",
      icon = icon("star"),
      color = "blue"
    )
  })
  
  output$topModelSales <- renderValueBox({
    df <- filtered_data()
    top_model <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(TotalSales)) %>%
      slice(1)
    
    valueBox(
      value = paste0(top_model$`Mobile Model`, " ($", round(top_model$TotalSales / 1e6, 2), "M)"),
      subtitle = "Top Mobile Model (Sales)",
      icon = icon("money-bill-wave"),
      color = "purple"
    )
  })
  
  output$totalTransactions <- renderValueBox({
    df <- filtered_data()
    total_txn <- nrow(df)
    
    valueBox(
      value = formatC(total_txn, format = "d", big.mark = ","),
      subtitle = "Total Transactions",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$totalTransactions <- renderValueBox({
    df <- filtered_data()
    total_txn <- nrow(df)
    
    valueBox(
      value = formatC(total_txn, format = "d", big.mark = ","),
      subtitle = "Total Transactions",
      icon = icon("shopping-cart"),
      color = "blue"
    )
  })
  
  output$avgPricePerUnit <- renderValueBox({
    df <- filtered_data()
    avg_price <- mean(df$`Price Per Unit`, na.rm = TRUE)
    
    valueBox(
      value = paste0("Rs ", round(avg_price, 0)),
      subtitle = "Average Price Per Unit",
      icon = icon("tags"),
      color = "teal"
    )
  })
  
  
  
  
  output$boxplotChart <- renderPlotly({
    df <- filtered_data()
    
    # Create brand-color mapping
    brand_levels <- unique(df$Brand)
    color_palette <- rep(brand_colors, length.out = length(brand_levels))
    names(color_palette) <- brand_levels
    
    p <- ggplot(df, aes(x = Brand, y = Sales, fill = Brand)) +
      geom_boxplot(alpha = 0.85, outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      labs(
        title = "Sales Distribution by Brand",
        x = "Brand",
        y = "Sales"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, color = "#2c3e50", size = 15),
        axis.title = element_text(color = "#34495e"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#555555"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "#F8F9FA", color = NA),
        panel.background = element_rect(fill = "#F8F9FA", color = NA)
      )
    
    ggplotly(p)
  })
  
  output$lollipopChart <- renderPlotly({
    df <- filtered_data()
    
    # Extract Year
    df$Year <- format(as.Date(df$Date), "%Y")
    
    # Group by Brand and Year
    data_sum <- df %>%
      group_by(Year, Brand) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop")
    
    # Sort brands by total sales
    brand_levels <- data_sum %>%
      group_by(Brand) %>%
      summarise(AllTimeSales = sum(TotalSales)) %>%
      arrange(AllTimeSales) %>%
      pull(Brand)
    
    data_sum$Brand <- factor(data_sum$Brand, levels = brand_levels)
    
    # Color gradient
    colorscale <- colorRamp(c("#56B1F7", "#132B43"))
    sales_scaled <- scales::rescale(data_sum$TotalSales, to = c(0, 1))
    color_values <- rgb(colorscale(sales_scaled)/255)
    data_sum$Color <- color_values
    
    # Format Sales for labels in millions
    label_sales_million <- label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)
    
    # Animated Lollipop Chart
    plot_ly(data_sum, frame = ~Year) %>%
      add_segments(
        x = 0, xend = ~TotalSales,
        y = ~Brand, yend = ~Brand,
        line = list(color = '#bdc3c7'),
        showlegend = FALSE
      ) %>%
      add_markers(
        x = ~TotalSales,
        y = ~Brand,
        marker = list(size = 10, color = ~Color),
        text = ~paste("Brand:", Brand, "<br>Sales:", label_sales_million(TotalSales)),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = "Animated Lollipop Chart: Total Sales by Brand",
          font = list(family = "Arial", size = 16, color = "#2c3e50"),
          x = 0.5
        ),
        xaxis = list(
          title = list(text = "Total Sales (Millions)", font = list(color = "#2c3e50")),
          tickfont = list(color = "#7f8c8d"),
          tickformat = ".2s"
        ),
        yaxis = list(
          title = list(text = "Brand", font = list(color = "#2c3e50")),
          tickfont = list(color = "#34495e"),
          categoryorder = "total ascending"
        ),
        plot_bgcolor = "#FFFFFF",
        paper_bgcolor = "#FFFFFF",
        margin = list(l = 100)
      ) %>%
      animation_opts(
        frame = 1000, transition = 500, redraw = TRUE
      )
  })
  
  
  output$lineSales <- renderPlotly({
    df <- filtered_data()
    
    # Ensure Month and Year columns exist
    if (!("Month" %in% names(df)) | !("Year" %in% names(df))) {
      df$Date <- as.Date(df$Date)
      df$Month <- as.numeric(format(df$Date, "%m"))
      df$Year <- as.numeric(format(df$Date, "%Y"))
    }
    
    # Format MonthYear and aggregate data
    df <- df %>%
      mutate(MonthYear = paste(Year, Month, "01", sep = "-")) %>%
      mutate(MonthYear = format(as.Date(MonthYear), "%B")) %>%
      group_by(MonthYear) %>%
      summarise(TotalQuantity = sum(`Units Sold`, na.rm = TRUE)) %>%
      mutate(MonthYear = factor(MonthYear, levels = month.name)) %>%
      arrange(MonthYear)
    
    # Basic colorful chart
    p <- ggplot(df, aes(x = MonthYear, y = TotalQuantity, group = 1)) +
      geom_line(color = "#25B6D2", size = 1.3) +
      geom_point(color = "#25B6D2", size = 3) +
      labs(
        title = "Monthly Units Sold",
        x = "Month",
        y = "Units Sold"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#F8F9FA", color = NA),   # Light background
        panel.background = element_rect(fill = "#F8F9FA", color = NA),
        panel.grid.major = element_line(color = "gray90"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "#333333"),
        axis.title = element_text(color = "#444444"),
        axis.text = element_text(color = "#333333")
      )
    
    ggplotly(p)
  })
  
  output$areaSales <- renderPlotly({
    df <- filtered_data()
    
    # Extract Month and Year if not already present
    if (!("Month" %in% names(df)) | !("Year" %in% names(df))) {
      df$Date <- as.Date(df$Date)
      df$Month <- as.numeric(format(df$Date, "%m"))
      df$Year <- as.numeric(format(df$Date, "%Y"))
    }
    
    # Group by Month and summarize Sales across all years
    df_summary <- df %>%
      group_by(Month) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      mutate(MonthLabel = month.abb[Month]) %>%  # Use Jan, Feb, etc.
      mutate(MonthLabel = factor(MonthLabel, levels = month.abb)) %>%
      arrange(MonthLabel)
    
    # Plot Area Chart with Month names on x-axis
    plot_ly(
      data = df_summary,
      x = ~MonthLabel,
      y = ~TotalSales,
      type = 'scatter',
      mode = 'lines+markers',
      fill = 'tozeroy',
      line = list(color = '#25B6D2', width = 4),
      marker = list(size = 8)
    ) %>%
      layout(
        title = "Total Sales by Month",
        xaxis = list(title = "Month"),
        yaxis = list(title = "Total Sales"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  
  
  output$salesHeatmap <- renderPlotly({
    df <- filtered_data()
    
    # Step 1: Find top 5 mobile models
    top_models <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(Total_Sales = sum(Sales, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales)) %>%
      slice_head(n = 5)
    
    # Step 2: Filter data to only include top models
    filtered_top <- df %>%
      filter(`Mobile Model` %in% top_models$`Mobile Model`)
    
    # Step 3: Calculate % contribution of each model to total sales in each city
    heatmap_data <- filtered_top %>%
      group_by(City, `Mobile Model`) %>%
      summarise(Model_Sales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      group_by(City) %>%
      mutate(Percent_Contribution = Model_Sales / sum(Model_Sales)) %>%
      ungroup()
    
    # Step 4: Plot heatmap using % contribution
    p <- ggplot(heatmap_data, aes(x = City, y = `Mobile Model`, fill = Percent_Contribution)) +
      geom_tile(color = "white") +
      scale_fill_gradientn(colors = c("lightblue", "skyblue", "darkblue"),
                           labels = scales::percent,
                           name = "% of City Sales") +
      labs(title = "Heatmap: % Contribution of Top 5 Mobile Models in Each City",
           x = "City", y = "Mobile Model") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  output$barRatingChart <- renderPlotly({
    df <- filtered_data()
    rating_data <- df %>%
      count(`Mobile Model`, Brand, Rating_Status)
    
    # Simulated gradient with same hue in different shades
    gradient_colors <- c(
      "Poor" = "lightblue",       # Light pink
      "Average" = "skyblue",    # Medium pink
      "Best" = "darkblue"      # Darker pink-purple
      
    )
    
    p <- ggplot(rating_data, aes(x = `Mobile Model`, y = n, fill = Rating_Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = gradient_colors) +
      facet_wrap(~Brand, scales = "free_x") +
      labs(title = "Grouped Bar Chart: Ratings by Mobile Model", x = "Mobile Model", y = "Count") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, size = 5),
        strip.text = element_text(face = "bold", size = 12)
      )
    
    ggplotly(p)
  })
  
  
  output$brandUnitBarChart <- renderPlotly({
    df <- filtered_data()
    
    df <- df %>%
      mutate(MonthYear = format(as.Date(Date), "%Y")) %>%
      group_by(MonthYear, Brand) %>%
      summarise(TotalUnits = sum(`Units Sold`, na.rm = TRUE), .groups = "drop")
    
    df$Brand <- factor(df$Brand, levels = df %>%
                         group_by(Brand) %>%
                         summarise(SumUnits = sum(TotalUnits)) %>%
                         arrange(SumUnits) %>%
                         pull(Brand))
    
    plot_ly(
      data = df,
      x = ~TotalUnits,
      y = ~Brand,
      frame = ~MonthYear,
      type = "bar",
      orientation = 'h',
      text = ~paste("Brand:", Brand, "<br>Units:", comma(TotalUnits)),
      hoverinfo = "text",
      marker = list(color = '#25B6D2')
    ) %>%
      layout(
        xaxis = list(title = "Units Sold", tickformat = ",d"),
        yaxis = list(title = "Brand", categoryorder = "total ascending"),
        margin = list(l = 100),
        showlegend = FALSE
      ) %>%
      animation_opts(
        frame = 1000, transition = 500, redraw = TRUE
      )
  })
  
  output$brandSummaryTable <- renderDT({
    df <- filtered_data()
    brand_summary <- df %>%
      group_by(Brand) %>%
      summarise(
        TotalSales = sum(Sales, na.rm = TRUE),
        Transactions = n()
      ) %>%
      arrange(desc(TotalSales))
    
    brand_summary$TotalSales <- scales::label_number(
      scale = 1e-6, suffix = "M", accuracy = 1
    )(brand_summary$TotalSales)
    
    datatable(
      brand_summary,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        dom = 'tip',
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = 'display compact stripe hover cell-border'
    )
  })
  output$ribbonChart <- renderPlotly({
    df <- filtered_data()
    
    if (nrow(df) == 0 || n_distinct(df$`Mobile Model`) < 2) return(NULL)
    
    # Step 1: Summarize values
    summary_df <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(
        AvgPrice = mean(`Price Per Unit`, na.rm = TRUE),
        AvgRatingNum = mean(as.numeric(factor(Rating_Status,
                                              levels = c("Bad", "Average", "Good", "Best"))), na.rm = TRUE)
      ) %>%
      mutate(
        RoundedRating = round(AvgRatingNum),
        RatingLabel = case_when(
          RoundedRating == 1 ~ "Bad",
          RoundedRating == 2 ~ "Average",
          RoundedRating == 3 ~ "Good",
          RoundedRating == 4 ~ "Best",
          TRUE ~ "Unknown"
        )
      ) %>%
      arrange(desc(AvgPrice)) %>%
      slice_head(n = 5)
    
    if (nrow(summary_df) < 2) return(NULL)
    
    # Step 2: Prepare long format with rank and labels
    price_rank <- summary_df %>%
      mutate(Rank = rank(-AvgPrice),
             ValueLabel = paste0("Avg Price: $", round(AvgPrice, 0))) %>%
      select(`Mobile Model`, Rank, ValueLabel) %>%
      mutate(Metric = "Avg Price")
    
    rating_rank <- summary_df %>%
      mutate(Rank = rank(-AvgRatingNum),
             ValueLabel = paste0("Rating: ", RatingLabel)) %>%
      select(`Mobile Model`, Rank, ValueLabel) %>%
      mutate(Metric = "Rating Status")
    
    ribbon_data <- bind_rows(price_rank, rating_rank)
    
    # Step 3: Define your custom blue palette
    brand_colors <- c(
      "#ADD8E6",  # lightblue
      "#87CEEB",  # skyblue
      "#4682B4",  # steelblue
      "#1E90FF",  # dodgerblue
      "#00008B"   # darkblue
    )
    
    # Map colors to mobile models
    models <- unique(ribbon_data$`Mobile Model`)
    names(brand_colors) <- models
    
    # Step 4: Plot
    p <- ggplot(ribbon_data, aes(
      x = Metric,
      y = Rank,
      group = `Mobile Model`,
      color = `Mobile Model`
    )) +
      geom_line(size = 2, show.legend = FALSE) +
      geom_point(size = 4) +
      scale_y_reverse(breaks = 1:5) +
      scale_color_manual(values = brand_colors) +  # Apply your color palette
      labs(
        title = "Ribbon Chart: Avg Price vs Rating Status by Mobile Model",
        y = "Rank (1 = Highest)", x = ""
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 14),
        plot.background = element_rect(fill = "#F8F9FA", color = NA),
        panel.background = element_rect(fill = "#F8F9FA", color = NA)
      )
    
    ggplotly(p)
  })
  
  
  
  
  library(RColorBrewer)
  
  output$bubbleChart <- renderPlotly({
    df <- filtered_data()
    
    summary_df <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(
        TotalSales = sum(Sales, na.rm = TRUE),
        TotalUnits = sum(`Units Sold`, na.rm = TRUE),
        AvgPrice = mean(`Price Per Unit`, na.rm = TRUE)
      ) %>%
      arrange(desc(TotalSales)) %>%
      slice_head(n = 13)
    
    # Generate blue shades
    blue_shades <- colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(nrow(summary_df))
    
    p <- ggplot(summary_df, aes(x = AvgPrice, y = TotalUnits, size = TotalSales, color = `Mobile Model`)) +
      geom_point(alpha = 0.7) +
      geom_text(aes(label = `Mobile Model`), size = 2, color = "black", fontface = "bold", show.legend = FALSE) +
      scale_size(range = c(10, 30), name = "Sales") +
      scale_color_manual(values = blue_shades) +
      labs(
        title = " Price vs Units Sold (Bubble Chart)",
        x = "Average Price Per Unit",
        y = "Total Units Sold",
        color = "Mobile Model"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  output$topSalesBarChart <- renderPlotly({
    df <- filtered_data()
    
    # Validate required columns
    if (!all(c("Mobile Model", "Sales") %in% names(df))) return(NULL)
    
    summary_df <- df %>%
      group_by(`Mobile Model`) %>%
      summarise(TotalSales = sum(Sales, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalSales)) %>%
      slice_head(n = 6)
    
    # Define custom color palette
    brand_colors <- c(
      "#00008B",  # lightblue
      "#1E90FF",  # skyblue
      "#4682B4",  # steelblue
      "#87CEEB",  # dodgerblue
      "#ADD8E6",   # darkblue
      "#4682B4"
    )
    
    # Assign fill colors to each model
    model_levels <- summary_df$`Mobile Model`
    fill_colors <- rep(brand_colors, length.out = length(model_levels))
    names(fill_colors) <- model_levels
    
    p <- ggplot(summary_df, aes(x = reorder(`Mobile Model`, -TotalSales), y = TotalSales, fill = `Mobile Model`)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = fill_colors) +
      labs(
        title = "Top 6 Mobile Models by Total Sales",
        x = "Mobile Model",
        y = "Total Sales"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "#333333"),
        axis.text.y = element_text(color = "#333333"),
        axis.title = element_text(color = "#444444"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16, color = "#2c3e50"),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#F8F9FA", color = NA),
        panel.background = element_rect(fill = "#F8F9FA", color = NA),
        legend.position = "none"
      )
    
    ggplotly(p)
  })
  
  output$dumbbellChart <- renderPlotly({
    df <- filtered_data()  # Or use your dataset directly if no filter function
    
    rating_summary <- df %>%
      group_by(Brand, Rating_Status) %>%
      summarise(Count = n_distinct(`Transaction ID`), .groups = 'drop')
    
    
    wide_data <- rating_summary %>%
      pivot_wider(names_from = Rating_Status, values_from = Count, values_fill = list(Count = 0)) %>%
      mutate(
        Poor = ifelse(is.na(Poor), 0, Poor),
        Best = ifelse(is.na(Best), 0, Best)
      ) %>%
      filter(Poor > 0 | Best > 0)  # Optional: only keep brands with at least one rating
    
    
    
    p <- ggplot(wide_data, aes(y = Brand)) +
      geom_segment(aes(x = Poor, xend = Best, yend = Brand), color = "#56B1F7", size = 2) +
      geom_point(aes(x = Poor), color = "#3498DB", size = 2) +
      geom_point(aes(x = Best), color = "#2C3E50", size = 2) +
      labs(
        title = "Poor vs Best Ratings by Brand",
        x = "Count of Ratings",
        y = "Brand"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, color = "#2c3e50"),
        axis.title = element_text(color = "#2c3e50"),
        axis.text = element_text(color = "#34495e"),
        panel.grid.major.y = element_blank()
      )
    
    
    ggplotly(p)
  })
  
  output$funnelChart <- renderPlotly({
    df <- filtered_data()
    
    # Set order of Rating Status manually
    df$Rating_Status <- factor(df$Rating_Status, levels = c("Best", "Average", "Poor"))
    
    # Count occurrences
    rating_counts <- df %>%
      group_by(Rating_Status) %>%
      summarise(Count = n(), .groups = "drop") %>%
      arrange(Rating_Status)
    
    # Create the funnel chart
    plot_ly(
      type = "funnel",
      y = rating_counts$Rating_Status,
      x = rating_counts$Count,
      textinfo = "value",  # show only value (not percentage)
      marker = list(color = rep('#25B6D2', 3)),
      connector = list(line = list(color = 'rgba(0,0,0,0)'))  # remove connectors
    ) %>%
      layout(
        title = "Customer Rating Funnel",
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        transition = list(duration = 800, easing = "cubic-in-out")  # smoother animation
      )
  })
  
  # --- Pie Chart ---
  payment_data <- reactive({
    query <- "SELECT `Payment Method`, COUNT(*) AS Count FROM mobileinfo GROUP BY `Payment Method`"
    dbGetQuery(storiesDb, query)
  })
  
  output$paymentPie <- renderPlotly({
    data <- payment_data()
    plot_ly(data,
            labels = ~`Payment Method`,
            values = ~Count,
            type = 'pie',
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            hole = 0.5) %>%  # 👈 This makes it a donut chart
      layout(title = 'Number of Transactions by Payment Method')
  })
  
  observeEvent(input$theme_filter, {
    theme <- tolower(input$theme_filter)  # Convert to "light", "dark", or "blue"
    session$sendCustomMessage("setTheme", theme)
  })
  
  output$introDataTable <- renderDT({
    datatable(
      combined_data(),  # or filtered_data(), depending on what you want to show
      options = list(
        scrollX = TRUE,  # ✅ this enables horizontal scrolling
        pageLength = 10
      ),
      rownames = FALSE
    )
  })
  
  
  
  
  
}

# Run app
shinyApp(ui, server)
