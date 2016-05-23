
suppressWarnings(library(shinythemes))
library(plotly)

d<- Sys.Date()-1
if (weekdays(Sys.Date())=="Monday") {d<- Sys.Date()-3} 
if (weekdays(Sys.Date())=="Sunday") {d<- Sys.Date()-2} 

shinyUI(navbarPage(
  theme = shinytheme("spacelab"),
  title="MarketData Visualization",
  tabPanel( 'Charts',
            uiOutput('name'),
            #    tags$style(type="text/css", "body { background-color: #00001a; }"),
            #       tags$style(type="text/css",
            #                 ".shiny-output-error { visibility: hidden; }",
            #                ".shiny-output-error:before { visibility: hidden; }"
            #    ),
            htmlOutput('mess'),
            #helpText(   a("Click",     href="http://192.168.31.21/index.php?p=newsFullText&id=20160422000867.000867&msgTimestamp=04:45:16.466942&channel=5")),
            hr(),
            fluidRow(
              tags$head(tags$style("#date{font-size: 15px;font-style: italic;}")),
              #tags$head(tags$style("#strat{font-size: 12px;}")),
              column(2, style="color:steelblue", 
                     wellPanel(
                       #style = "background-color: #001133;",
                       dateInput('date',label = 'Date input:',value = d, width="150"),
                       textInput("from", label = "From:", value="09:20:00", width="100"),
                       textInput("to", label = "To:", value="09:40:00", width="100"),
                       textInput("text", label = "Symbol:", value="GOOG", width="100"),
                       uiOutput("strat"),
                       checkboxInput('spread', 'Bid-Ask', value = TRUE),
                       checkboxInput('icbcNSDQ', 'iCBC NSDQ', value = FALSE),
                       checkboxInput('icbcNYSE', 'iCBC NYSE', value = FALSE),
                       checkboxInput('icbcARCA', 'iCBC ARCA', value = FALSE),
                       checkboxInput('prevclx', 'Prev.day Close', value = FALSE),
                       checkboxInput('news', 'News', value = FALSE),
                       radioButtons("radio", label = h3("Style"), choices = list("White" = 1, "Black" = 2), selected = 1, inline = TRUE),
                       submitButton("Submit")
                     )
              ),
              column(10, 
                     uiOutput("plotui"),
                     uiOutput("plotui3"),
                     uiOutput("plotui2")
              )
            )
  ),
  tabPanel( 'Data',
            downloadButton('downloadData', 'Download'),
            hr(),
            DT::dataTableOutput('mytable'),
            tags$head(tags$style("#mytable{color: black; font-size: 14px;}"))
  )
 )
)



# div(style="display:inline-block",checkboxInput('orders', 'Orders', value = FALSE)),
#tags$style(type='text/css', "#from { height: 400px; }"),,