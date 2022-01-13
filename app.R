
rm(list = ls())

library(shiny)
# library(shinyjs)
# library(shiny.semantic)
library(semantic.dashboard)
library(tidyverse)
library(echarts4r)

source("data.R")


conflict_prefer_all("semantic.dashboard", 
                    c("shiny", "shinyjs", "graphics"), quiet = TRUE)


# menu_content <- list(
#     list(name = "AA", link = "http://example.com", icon = "dog"),
#     list(name = "BB", link = "#", icon="cat"),
#     list(name = "CC")
# )


ui <- shinyUI(semanticPage(
    header(title = "Asset Valuation App", description = "Used to estimate value of assets"),
    header(title = "Discounted Cash Flow Valuation", description = "Estimate intrinsic value", icon = "chart bar outline"),
    # horizontal_menu(menu_content),
    box(
        
        action_button("action_button", "Press Me!"),
        textOutput("button_output"),
        p("Simple checkbox:"),
        checkbox_input("example", "Check me", is_marked = FALSE),
        p(),
        p("Simple toggle:"),
        toggle("tog1", "My Label", TRUE)
    ),
    br(),
    box(
        width = 4,
        h4("DCF Valuation"),
        text_input("revenue_1Y",
                   "Revenue 1Y: ",
                   value = "1000000"),
        br(),
        text_input("revenue_growth",
                   "Revenue Growth (% per year): ",
                   value = "6%"),
        br(),
        text_input("profit_margin",
                   "Profit Margin (%): ",
                   value = "12%"),
        br(),
        text_input("discount_rate",
                   "Discount Rate (% per year): ",
                   value = "10%"),
        h4("Valuation:"),
        textOutput("valuation")
        ),
    
    card(
        div(class="content",
            div(class="header", "Aaron Hardy"),
            div(class="meta", "Creator"),
            div(class="description", "Aaron Hardy is a aspiring financial analyst and programmer.")
        )
    ),
    selectInput("ticker", "Ticker:", choices = choices_tickers, width = "200px"),
    selectInput("field", "Field:", choices = choices_field, width = "1000px",
                multiple = TRUE, selected = c("revenue_1Q", "gross_profit_1Q",
                                              "operating_income_loss_1Q")),
    echarts4rOutput("chart"),
    segment(
        cards(
            class = "two",
            card(class = "red",
                 div(class = "content",
                     div(class = "header", "Main title card 1"),
                     div(class = "meta", "Sub title card 1"),
                     div(class = "description", "More detail description card 1")
                 )
            ),
            card(class = "blue",
                 div(class = "content",
                     div(class = "header", "Main title card 2"),
                     div(class = "meta", "Sub title card 2"),
                     div(class = "description", "More detail description card 2")
                 )
            )
        )
    )
))

server <- shinyServer(function(input, output) {
    output$button_output <- renderText(as.character(input$action_button))
    
    output$chart <- renderEcharts4r({
        
        ratios %>% 
            filter(ticker %in% input$ticker) %>%
            select(report_date, where(is.numeric)) %>% 
            pivot_longer(-report_date) %>%
            filter(name %in% input$field) %>% 
            group_by(name) %>% 
            e_charts(x = report_date) %>%
            # e_line(serie = value, smooth = TRUE) %>%
            e_scatter(serie = value, symbol_size = 7, legend = FALSE) %>% 
            # e_bar(serie = index) %>% 
            # e_area(index) %>% 
            e_tooltip(trigger = "axis") %>% 
            e_axis_labels(x = "Date") %>% 
            e_title(text = "Hedge fund Strategy Indices", 
                    subtext = "Source: PerformanceAnalytics",
                    sublink = "https://rdrr.io/cran/PerformanceAnalytics/man/PerformanceAnalytics-package.html",
                    left = "center") %>% 
            e_theme("infographic") %>% 
            # e_theme("walden") %>% 
            # e_legend(FALSE) %>% 
            e_legend(orient = "vertical",
                     right = '5',
                     top = '15%',
                     selector = list(
                         list(type = 'inverse', title = 'Invert'),
                         list(type = 'all', title = 'Reset')
                     )) %>% 
            e_grid(right = '20%') %>% 
            e_toolbox_feature("dataZoom") %>% 
            e_toolbox_feature(feature = "reset") %>% 
            e_toolbox_feature("dataView") %>% 
            e_toolbox_feature("saveAsImage") %>% 
            e_show_loading()
        
    })
    
    output$valuation <- renderText({
        
        ########
        # input <- list(revenue_1Y = "100000",
        #               revenue_growth = "5%",
        #               discount_rate = "7%")
        ########
        
        revenue_1Y <- as.numeric(input$revenue_1Y)
        revenue_growth_rate <- 
            as.numeric(str_remove(input$revenue_growth, "\\%"))/100
        profit_margin <- as.numeric(str_remove(input$profit_margin, "\\%"))/100
        discount_rate <- as.numeric(str_remove(input$discount_rate, "\\%"))/100
        
        return(revenue_1Y*profit_margin * (1 + revenue_growth_rate) /
            (discount_rate - revenue_growth_rate))
    })
    
    
    })

shinyApp(ui, server)
