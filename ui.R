
suppressWarnings(library(shinythemes))
suppressWarnings(library(plotly))

getDate<- function() {
  d<- Sys.Date()-2
  if (weekdays(d)=="Sunday") {d<- d-2} 
  if (weekdays(d)=="Saturday") {d<- d-1} 
  return(d)
}

shinyUI(navbarPage(
  #theme = shinytheme("spacelab"),
  theme="http://bootswatch.com/spacelab/bootstrap.css",
  title="MarketData Visualization",
    tabPanel( 'Charts',
      uiOutput('name'),
            tags$style(type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            htmlOutput('mess'),
            hr(),
              fluidRow(
                tags$head(tags$style("#date{font-size: 15px;font-style: italic;}")),
                #tags$head(tags$style("#strat{font-size: 12px;}")),
                column(2, style="color:steelblue", 
                       wellPanel(
                          #style = "background-color: #001133;",
                          dateInput('date',label = 'Date input:',value = getDate(), width="150"),
                          #uiOutput("GetDate"),
                          div(style="display:inline-block;vertical;",textInput("from", label = "From:", value="09:25:00", width="100")),
                          div(style="display:inline-block;vertical-align:top; width: 20px;"),
                          div(style="display:inline-block;vertical;",textInput("to", label = "To:", value="11:00:00", width="100")),
                          HTML("<br>"),
                          textInput("text", label = "Symbol:", value="XBI", width="100"),
                          tags$style(type='text/css', ".selectize-input { padding: 3px; min-height: 0;} .selectize-dropdown { line-height: 10px; }"),
                          uiOutput("strat"),
                          
                          div(style="display: inline-block", checkboxInput('spread', 'Bid-Ask', value = TRUE)),
                          div(style="display:inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                          div(style="display: inline-block;vertical",checkboxInput('level1', 'Level1')),
                          div(class="row", HTML("<span>")),
                          
                          div(style="display: inline-block", checkboxInput('prevclx', 'YClose', value = FALSE)),
                          div(style="display:inline-block;vertical-align:top; width: 23px;",HTML("<br>")),
                          div(style="display: inline-block;vertical",checkboxInput('colorEx', 'ColorExchange', value = FALSE)),
                          
                          checkboxInput('news', 'News', value = FALSE),
                          checkboxInput('OverLap', 'Avoid overlapping'),
                          checkboxGroupInput("icbc", label = h4("iCBC:"), choices = list("NSDQ" = "Q", "NYSE" = "Y", "ARCA" = "A"), inline=TRUE),
                          checkboxGroupInput("nav", label = h4("Nav:"), choices = list("B_Nav" = "B", "M_Nav" = "M", "A_NAV" = "A"), inline=TRUE),
                          radioButtons("host", label = h4("Host:"), choices = list("UA" = 1, "US" = 2), selected = 2, inline = TRUE),
                          radioButtons("radio", label = h4("Style:"), choices = list("White" = 1, "Black" = 2), selected = 1, inline = TRUE),
                          submitButton("Submit")
                        )
                    ),
                column(10, 
                       uiOutput("plotui"),
                       uiOutput("plotui3"),
                       uiOutput("plotui2"),
                       verbatimTextOutput("brush")
                )
              )
  ),
  tabPanel( 'Market Data',
            downloadButton('downloadData', 'Download'),
            hr(),
            DT::dataTableOutput('mytable'),
            tags$head(tags$style("#mytable{color: black; font-size: 14px;}"))
            ),
  tabPanel( 'Orders',
            downloadButton('downloadOrders', 'Download'),
            hr(),
            DT::dataTableOutput('OrdersTable')
            #tags$head(tags$style("#OrdersTable{color: red; font-size: 14px;}"))
           )

      )
 )

