# Library Setup ----
library(shinydashboard)
library(shiny)
library(DT)
library(tibble)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
# END ----

# Data Pre-processing (setup before app entry) ----
importedData <- tibble(
  Invoice.Date = sample(seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"), 50000, replace = TRUE),
  Invoice.No = sample(1:15000, 50000, replace = TRUE),
  Taker = sample(
    c("Travis", "Eric", "Miranda", "Selene", "Mike", "Richard", "Jake", "Eve", "Adam", "Darlene", "Myrtle", "Danny"),
    50000, replace = TRUE
  ),
  Salesrep.Id = sample(1:4, 50000, replace = TRUE),
  Customer.Name = sample(
    c("FLY INC", "ELECTRIC COMPANY", "GOVERNMENT", "YORK&DARRYL", "COOKIES INC", "INTERNET SITE", "FLORIDA TOURISM",
      "JACKET MANUFACTURER", "PAPERCUP FACTORY", "PIZZA PLAZA", "PEOPLE PLEASERS", "CREATIVITY DISTRIBUTED",
      "HAZELNUT SPLASH", "WIRESANDCONNECTION", "STRONGMAN", "LEGALLY ENTITLED", "HORIZONSWIMMERS", "CLOUDGAZERS"),
    50000, replace = TRUE),
  Vendor = sample(
    c("PRODUCT PUSHER", "MISC ITEMS", "FACTORY", "IN-HOUSE", "CEMENT LIMITED", "SUPPLIES AND SUNDRIES", "DROP HUT"),
    50000, replace = TRUE
  ),
  Item.Id = sample(1:6000, 50000, replace = TRUE),
  Total.Item.Price = abs(rnorm(50000, 150, 50)),
  Total.Item.Cost = abs(rnorm(50000, 100, 25)),
  ) %>%
  mutate(Total.Item.Profit = Total.Item.Price - Total.Item.Cost,
         Department = case_when(
           str_detect(Taker, "Travis|Eric|Miranda|Selene") ~ "COUNTER",
           str_detect(Taker, "Mike|Richard|Jake") ~ "SNOWFLAKE",
           str_detect(Taker, "Eve|Adam|Darlene") ~ "OUTSIDE",
           str_detect(Taker, "Myrtle|Danny") ~ "INSIDE",
           TRUE ~ "NA"
           )
  )
# END ----

# Ui Body Setup ----
body <- dashboardBody(
  tags$style(
    ".fa-coins {color:white}",
    ".fa-file-invoice-dollar {color:white}",
    ".fa-user-group {color:white}",
    "body {overflow-y: hidden;}",
  ),
  fluidRow(
    valueBoxOutput("OverallRevenue_Value"),
    valueBoxOutput("OverallInvoices_Value"),
    valueBoxOutput("OverallUniqueCustomers_Value")
  ),
  fluidRow(
    tabBox(
      width = 12,
      height = 400,
      tabPanel(
        title = "Summary",
        br(),
        br(),
        fluidRow(
          valueBoxOutput("OverallProfit_Summary"),
          valueBoxOutput("OverallMargin_Summary"),
          valueBoxOutput("OverallCost_Summary")
        ),
        br(),
        fluidRow(
          valueBoxOutput("AverageRevenue_Summary"),
          valueBoxOutput("AverageInvoices_Summary"),
          valueBoxOutput("AverageCustomer_Summary")
        )
      ),
      tabPanel(
        title = "Sales Data By Taker",
        plotlyOutput("SalesByTaker_Plot1", height = 170),
        plotlyOutput("SalesByTaker_Plot2", height = 170)
      ),
      tabPanel(
        title = "Sales Data By Salesrep ID",
        plotlyOutput("SalesByRepID_Plot1", height = 170),
        plotlyOutput("SalesByRepID_Plot2", height = 170)
      ),
      tabPanel(
        title = "Sales Data By Customer",
        plotlyOutput("SalesByCustomer_Plot1", height = 170),
        fluidRow(
          column(6, plotOutput("SalesByCustomer_Plot2", height = 170)),
          column(6, plotOutput("SalesByCustomer_Plot3", height = 170))
        )
      ),
      tabPanel(
        title = "Sales Data By Vendor",
        width = 12,
        plotlyOutput("SalesByVendor_Plot1", height = 340)
      ),
      tabPanel(
        title = "Raw Sales Data",
        height = 400,
        DTOutput("Data_Table", height = 340)
      )
    )
  )
)

# END ----

# Ui Sidebar Setup ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    dateRangeInput(
      "MInput",
      "Day Range",
      start = as.Date("2022/01/01"),
      end = as.Date("2022/12/31")
    ),
    selectInput(
      "DInput",
      "Filter By Department",
      unique(importedData$Department),
      multiple = TRUE
    ),
    selectInput(
      "TInput",
      "Filter By Taker",
      unique(importedData$Taker),
      multiple = TRUE
    ),
    selectInput(
      "RInput",
      "Filter By Salesrep ID",
      unique(importedData$Salesrep.Id),
      multiple = TRUE
    ),
    selectInput(
      "CInput",
      "Filter by Customer Name",
      unique(importedData$Customer.Name),
      multiple = TRUE
    ),
    selectInput(
      "VInput",
      "Filter by Vendor Name",
      unique(importedData$Vendor),
      multiple = TRUE
    )
  )
)
# END ----

# Ui Setup Proper ----
ui <- dashboardPage(
  dashboardHeader(title = "Sales Reporting Tool"),
  sidebar = sidebar,
  body = body,
  skin = "black"
)
# END ----

# Server Setup ----
server <- function(input, output, session) {
  ## Server Data Setup ----
  Sales_Values <- reactive({
    tempData <- importedData %>%
      dplyr::filter(Invoice.Date >= input$MInput[1] & Invoice.Date <= input$MInput[2])
    if (!is.null(input$DInput)) {
      tempData <- tempData %>%
        filter(Department %in% input$DInput)
    }
    if (!is.null(input$TInput)) {
      tempData <- tempData %>%
        filter(Taker %in% input$TInput)
    }
    if (!is.null(input$RInput)) {
      tempData <- tempData %>%
        filter(Salesrep.Id %in% input$RInput)
    }
    if (!is.null(input$CInput)) {
      tempData <- tempData %>%
        filter(Customer.Name %in% input$CInput)
    }
    if (!is.null(input$VInput)) {
      tempData <- tempData %>%
        filter(Vendor %in% input$VInput)
    }
    tempData
  })

  SalesByTaker_Data <- reactive({
    tempData <- Sales_Values() %>%
      group_by(Taker,
               Department) %>%
      summarize(Total.Profit = sum(Total.Item.Profit),
                Total.Revenue = sum(Total.Item.Price),
                Invoice.Number = n_distinct(Invoice.No),
                Unique.Customers = length(unique(Customer.Name))) %>%
      ungroup()
    tempData
  })

  SalesByRepID_Data <- reactive({
    tempData <- Sales_Values() %>%
      group_by(Salesrep.Id,
               Department) %>%
      summarize(Total.Profit = sum(Total.Item.Profit),
                Total.Revenue = sum(Total.Item.Price),
                Invoice.Number = n_distinct(Invoice.No),
                Unique.Customers = length(unique(Customer.Name))) %>%
      ungroup()
    tempData
  })

  SalesByCustomer_Data <- reactive({
    tempData <- Sales_Values() %>%
      group_by(Customer.Name,
               Department) %>%
      summarize(Total.Profit = sum(Total.Item.Profit),
                Total.Revenue = sum(Total.Item.Price),
                Invoice.Number = n_distinct(Invoice.No)) %>%
      ungroup() %>%
      mutate(`Spending Classification` = case_when(Total.Profit > 1000000 ~ "Over $1,000,000",
                                                   Total.Profit > 100000 ~ "Over $100,000",
                                                   Total.Profit > 10000 ~ "Over $10,000",
                                                   Total.Profit > 1000 ~ "Over $1,000",
                                                   TRUE ~ "Under $1,000"),
             `Frequency Classification` = case_when(Invoice.Number > 100 ~ "Over 100 Orders",
                                                    Invoice.Number > 50 ~ "Over 50 Orders",
                                                    Invoice.Number > 10 ~ "Over 10 Orders",
                                                    TRUE ~ "Under 10 Orders")) %>%
      mutate(`Spending Classification` = factor(`Spending Classification`,
                                                levels = c("Over $1,000,000",
                                                           "Over $100,000",
                                                           "Over $10,000",
                                                           "Over $1,000",
                                                           "Under $1,000")),
             `Frequency Classification` = factor(`Frequency Classification`,
                                                 levels = c("Over 100 Orders",
                                                            "Over 50 Orders",
                                                            "Over 10 Orders",
                                                            "Under 10 Orders")))
    tempData
  })

  SalesByVendor_Data <- reactive({
    tempData <- Sales_Values() %>%
      group_by(Vendor,
               Department) %>%
      summarize(Total.Profit = sum(Total.Item.Profit),
                Total.Revenue = sum(Total.Item.Price),
                Invoice.Number = n_distinct(Invoice.No),
                Unique.Customers = length(unique(Customer.Name))) %>%
      ungroup()
    tempData
  })

  SalesByTime_Data <- reactive({
    tempData <- Sales_Values() %>%
      group_by(Invoice.Date) %>%
      summarize(Total.Revenue = sum(Total.Item.Price),
                Invoice.Number = n_distinct(Invoice.No),
                Unique.Customers = length(unique(Customer.Name))) %>%
      ungroup()
    tempData
  })
  ## END ----

  ## Sales By Overall Indicator Setup ----
  output$OverallRevenue_Value <- renderValueBox({
    tempValue <- round(sum(Sales_Values()$Total.Item.Price), 2)
    valueBox(
      paste0("$ ", tempValue),
      "Overall Revenue (CAD)",
      color = "black",
      icon("coins")
    )
  })

  output$OverallInvoices_Value <- renderValueBox({
    valueBox(
      length(unique(Sales_Values()$Invoice.No)),
      "Overall # of Invoices",
      color = "black",
      icon = icon("file-invoice-dollar")
    )
  })

  output$OverallUniqueCustomers_Value <- renderValueBox({
    valueBox(
      length(unique(Sales_Values()$Customer.Name)),
      "Overall # of Unique Customers",
      color = "black",
      icon = icon("user-group")
    )
  })

  output$Data_Table <- renderDT(
    Sales_Values(),
    filter = "top",
    option = list(
      scrollX = T,
      scrollY = 170,
      paging = T,
      pageLength = 10
    )
  )
  ## END ----

  ## Sales Summary Indicator Setup ----
  output$OverallProfit_Summary <- renderValueBox({
    tempValue <- round(sum(Sales_Values()$Total.Item.Profit), 2)
    valueBox(
      paste0("$ ", tempValue),
      "Overall Profit (CAD)",
      color = "black",
    )
  })

  output$OverallMargin_Summary <- renderValueBox({
    tempValue <- round(
      sum(Sales_Values()$Total.Item.Profit)/sum(Sales_Values()$Total.Item.Price)*100,
      2
    )
    valueBox(
      paste0(tempValue," %"),
      "Overall Profit Margin",
      color = "black",
    )
  })

  output$OverallCost_Summary <- renderValueBox({
    tempValue <- round(sum(Sales_Values()$Total.Item.Cost),2)
    valueBox(
      paste0("$ ", tempValue),
      "Overall Cost (CAD)",
      color = "black",
    )
  })

  output$AverageRevenue_Summary <- renderValueBox({
    tempValue <- round(
      sum(SalesByTime_Data()$Total.Revenue)/as.numeric(difftime(input$MInput[2],
                                                                input$MInput[1],
                                                                units = "days")),
      2
    )
    valueBox(
      paste0("$ ", tempValue),
      "Average Revenue per Day (CAD)",
      color = "black",
    )
  })

  output$AverageInvoices_Summary <- renderValueBox({
    tempValue <- round(
      sum(SalesByTime_Data()$Invoice.Number)/as.numeric(difftime(input$MInput[2],
                                                                 input$MInput[1],
                                                                 units = "days")),
      2)
    valueBox(
      tempValue,
      "Average Invoices per Day",
      color = "black",
    )
  })

  output$AverageCustomer_Summary <- renderValueBox({
    tempValue <- round(
      sum(SalesByTime_Data()$Unique.Customers)/as.numeric(difftime(input$MInput[2],
                                                                   input$MInput[1],
                                                                   units = "days")),
      2)
    valueBox(
      tempValue,
      "Average Unique Customers per Day",
      color = "black",
    )
  })
  ## END ----

  ## Sales By Taker Indicators Setup ----
  output$SalesByTaker_Plot1 <- renderPlotly({
    p <- ggplot(
      SalesByTaker_Data(),
      aes(
        x = reorder(Taker, Total.Profit),
        y = Total.Profit,
        color = Department,
        size = Unique.Customers
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Department: ", Department,
                          "<br>Taker: ", Taker,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Profit: ", Total.Profit,
                          "<br>Unique Customers: ", Unique.Customers))
      ) +
      scale_alpha(range = c(0.4, 1)) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Taker") +
      ylab("Profit (CAD)") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp <- ggplotly(p, tooltip = "text")
    ggp
  })

  output$SalesByTaker_Plot2 <- renderPlotly({
    p2 <- ggplot(
      SalesByTaker_Data(),
      aes(
        x = reorder(Taker,  Invoice.Number),
        y = Invoice.Number,
        color = Department,
        size = Unique.Customers
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Department: ", Department,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Taker: ", Taker,
                          "<br>Profit: ", Total.Profit,
                          "<br>Unique Customers: ", Unique.Customers))
      ) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Taker") +
      ylab("# of Invoices") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp2 <- ggplotly(p2, tooltip = "text")
    ggp2
  })
  ## END ----

  ## Sales By Salesrep ID Indicators Setup ----
  output$SalesByRepID_Plot1 <- renderPlotly({
    p <- ggplot(
      SalesByRepID_Data(),
      aes(
        x = reorder(Salesrep.Id, Total.Profit),
        y = Total.Profit,
        color = Department,
        size = Unique.Customers
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Department: ", Department,
                          "<br>Salesrep ID: ", Salesrep.Id,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Profit: ", Total.Profit,
                          "<br>Unique Customers: ", Unique.Customers))
      ) +
      scale_alpha(range = c(0.4, 1)) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Salesrep ID") +
      ylab("Profit (CAD)") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp <- ggplotly(p, tooltip = "text")
    ggp
  })

  output$SalesByRepID_Plot2 <- renderPlotly({
    p2 <- ggplot(
      SalesByRepID_Data(),
      aes(
        x = reorder(Salesrep.Id, Invoice.Number),
        y = Invoice.Number,
        color = Department,
        size = Unique.Customers
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Department: ", Department,
                          "<br>Salesrep ID: ", Salesrep.Id,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Profit: ", Total.Profit,
                          "<br>Unique Customers: ", Unique.Customers))
      ) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Salesrep ID") +
      ylab("# of Invoices") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp2 <- ggplotly(p2, tooltip = "text")
    ggp2
  })
  ## END ----

  ## Sales By Customer Indicators Setup ----
  output$SalesByCustomer_Plot1 <- renderPlotly({
    p <- ggplot(
      SalesByCustomer_Data(),
      aes(
        x = Invoice.Number,
        y = Total.Profit,
        color = Department,
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Customer Name: ", Customer.Name,
                          "<br>Department: ", Department,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Total Profit: ", Total.Profit,
                          "<br>Total # of Invoices: ", Invoice.Number))
      ) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Total # of Invoices") +
      ylab("Total Profit (CAD)") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp <- ggplotly(p, tooltip = "text")
    ggp
  })

  output$SalesByCustomer_Plot2 <- renderPlot({
    hsize <- 4
    p <- ggplot(
      SalesByCustomer_Data(),
      aes(
        x = hsize,
        y = Total.Profit,
        fill = `Spending Classification`
      )
    ) +
      geom_col() +
      coord_polar(theta = "y") +
      xlim(c(0.2, hsize + 0.5)) +
      theme_void() +
      theme(legend.title = element_text("Total Profit")) +
      scale_fill_brewer(palette = "Dark2")
    p
  })

  output$SalesByCustomer_Plot3 <- renderPlot({
    hsize <- 4
    p <- ggplot(
      SalesByCustomer_Data(),
      aes(
        x = hsize,
        y = Invoice.Number,
        fill = `Frequency Classification`
      )
    ) +
      geom_col() +
      coord_polar(theta = "y") +
      xlim(c(0.2, hsize + 0.5)) +
      theme_void() +
      theme(legend.title = element_text("# of Orders Placed")) +
      scale_fill_brewer(palette = "Dark2")
    p
  })
  ## END ----

  ## Sales By Vendor Indicators Setup ----
  output$SalesByVendor_Plot1 <- renderPlotly({
    p <- ggplot(
      SalesByVendor_Data(),
      aes(
        x = Invoice.Number,
        y = Total.Profit,
        color = Department,
        size = Unique.Customers,
      )
    ) +
      geom_point(
        aes(text = paste0("<br>Vendor Name: ", Vendor,
                          "<br>Department: ", Department,
                          "<br>Total Profit: ", Total.Profit,
                          "<br>Revenue: ", Total.Revenue,
                          "<br>Total # of Invoices: ", Invoice.Number,
                          "<br>Total Unique Customers: ", Unique.Customers))
      ) +
      theme_bw() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +
      xlab("Total # of Invoices") +
      ylab("Total Profit (CAD)") +
      labs(size = NULL) +
      scale_color_brewer(palette = "Dark2")
    ggp <- ggplotly(p, tooltip = "text")
    ggp
  })
  ## END ----

  ## Server close assurance ----
  session$onSessionEnded(function(){
    stopApp()
  })
  ## END ----
}
# END ----

shinyApp(ui, server)
