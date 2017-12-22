
suppressWarnings(library(shinythemes))
suppressWarnings(library(plotly))
library(shinyjs)
library(shinyBS)

getDate<- function() {
  d<- Sys.Date()-2
  if (weekdays(d)=="Sunday") {d<- d-2} 
  if (weekdays(d)=="Saturday") {d<- d-1} 
  return(d)
}


shinyUI(navbarPage(
  #theme = shinytheme("spacelab"),
  #shinyjs::useShinyjs(),
  theme="http://bootswatch.com/spacelab/bootstrap.css",
  title="MarketData Visualization",
    tabPanel( 'Charts',
      uiOutput('name'),
            #tags$style(type="text/css",
            #        ".shiny-output-error { visibility: hidden; }",
            #           ".shiny-output-error:before { visibility: hidden; }"
            #),
            htmlOutput('mess'),
              fluidRow(
                tags$head(tags$style("#date{font-size: 15px;font-style: italic;}")),
                #tags$head(tags$style("#strat{font-size: 12px;}")),
                column(2, style="color:steelblue", 
                       wellPanel(
                          tags$head(
                            tags$style(type="text/css", ".well { max-width: 300px; }")
                          ),
                          #style = "background-color: #001133;",
                          useShinyjs(),
     
                          div(style="display:inline-block;vertical", dateInput('date',label = 'Date input:',value = "2017-12-13", width="120")),
                          div(style="display:inline-block;vertical-align:top; width: 15px;",HTML("<br>")),
                          div(style="display:inline-block;vertical", checkboxInput('futures', 'Futures', value = FALSE)),

                          div(class="row", HTML("<span>")),
                          
                          #div(style="display: inline-block;vertical-align:top; width: 30px;",selectInput("ddllgra", "Function:",c('mean','median','sd','count','min','max'), selected='mean')),
                          #div(style="display: inline-block;vertical-align:top; width: 30px;",textInput(inputId="xlimitsmax", label="x-max", value = 0.5)),
                          
                          div(style="display:inline-block;vertical;",textInput("from", label = "From:", value="09:25:00", width="100")),
                          div(style="display:inline-block;vertical-align:top; width: 20px;"),
                          div(style="display:inline-block;vertical;",textInput("to", label = "To:", value="09:35:00", width="100")),
                          #HTML("<br>"),
                          div(class="row", HTML("<span>")),
                          div(style="display:inline-block;vertical;", textInput("text", label = "Symbol:", value="FB", width="100")),
                          div(style="display:inline-block;vertical-align:top; width: 20px;"),
                          #tags$style(type='text/css', ".selectize-input { padding: 3px; min-height: 0;} .selectize-dropdown { line-height: 10px; }"),
                          div(style="display:inline-block;vertical;", uiOutput("strat")),
                          #HTML("<br>"),
                          div(class="row", HTML("<span>")),
                          
                          div(style="display:inline-block", checkboxInput('spread', 'Bid-Ask', value = TRUE)),
                          div(style="display:inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
                          div(style="display:inline-block;vertical",checkboxInput('level1', 'Level1')),
                          div(class="row", HTML("<span>")),
                          
                          div(style="display:inline-block", checkboxInput('prevclx', 'YClose', value = FALSE)),
                          div(style="display:inline-block;vertical-align:top; width: 23px;",HTML("<br>")),
                          div(style="display:inline-block;vertical",checkboxInput('colorEx', 'ColorExchange', value = FALSE)),
                          div(class="row", HTML("<span>")),
                          
                          div(style="display:inline-block", checkboxInput('news', 'News', value = FALSE)),
                          div(style="display:inline-block;vertical-align:top; width: 34px;",HTML("<br>")),
                          div(style="display:inline-block;vertical",checkboxInput('OverLap', 'Overlap')),
                          checkboxInput('volumeChart', 'VolumeChart', value = TRUE),
                          
                          checkboxGroupInput("icbc", label = h4("Imbalances:"), choices = list("NSDQ" = "Q", "NYSE" = "Y", "ARCA" = "A"), inline=TRUE),
                          checkboxGroupInput("nav", label = h4("Nav:"), choices = list("B_Nav" = "B", "M_Nav" = "M", "A_NAV" = "A"), inline=TRUE),
                          radioButtons("host", label = h4("Host:"), choices = list("UA" = 1, "US" = 2), selected = 1, inline = TRUE),
                          radioButtons("radio", label = h4("Style:"), choices = list("White" = 1, "Black" = 2), selected = 1, inline = TRUE),
                          #submitButton("Submit"),
                          
                          tags$script('$(document).on("keydown",
                                      function (e) {
                                         if(e.which == 13) {
                                          Shiny.onInputChange("go", new Date());
                                         } 
                                        });
                                      '),
                          
                          div(style="display:inline-block", actionButton("go", "Submit", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          div(style="display:inline-block;vertical-align:top; width: 25px;",HTML("<br>")),
                          div(style="display:inline-block", actionLink(inputId = 'help',label = 'Help?')),
                          hidden(div(id='text_help',
                                     hr(),
                                      helpText("Default page is empty."),
                                      helpText("For generating charts fill corresponding inputs field and press 'Submit' button."),
                                      helpText("After switching on 'Futures' checkBox data will be taken from FuturesDB, and some inputs will be not available."),
                                      helpText("'Submit' button is linked to 'enter' button on keyboard."),
                                      helpText(a("Link to Confluence",href="https://confluence.attocapital.com/display/AP/MarketDataVisualization"))
                                     
                                  )
                               )
                        )
                    ),
                column(10, 
                       bsCollapse(id = "collapseExample", open = "Market data", multiple = TRUE,
                                  bsCollapsePanel("Market data", "", style = "primary", uiOutput("plotui")),
                                  bsCollapsePanel("Imbalances", "", style = "primary", uiOutput("plotui3")),
                                  bsCollapsePanel("Scaling", "", style = "primary", uiOutput("plotui2"))
                       )
                       
                       #uiOutput("plotui"),
                       #uiOutput("plotui3"),
                       #uiOutput("plotui2"),
                       #verbatimTextOutput("brush")
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

