## 27/04/2016 : Shiny som sur iris - camemberts js


## Organisation de la page
shinyUI(fluidPage(
  headerPanel(HTML("Kohcorico")),
  
  tabsetPanel(
    #### Panel 'Welcome, Import Data'
    #########################################################################
    tabPanel("Import Data", 
             includeHTML("js/lodash.min.js"),
             includeHTML("js/d3.min.js"),
             includeHTML("js/hexbin.js"),
             includeHTML("js/svg_todataurl.js"),
             includeHTML("js/rgbcolor.js"),
             includeHTML("js/canvg.js"),
             includeHTML("js/box.js"),
             includeHTML("js/radar-chart-d3.js"),
             includeHTML("js/word-cloud.js"),
             
             h2("Welcome"), 
             fluidRow(
               column(4,
                      wellPanel(
                        fileInput('file1', 'Choose CSV/TXT File'),
                        h4("Set import parameters until the table appears correctly on the right."),
                        fluidRow(column(6, checkboxInput('header', ' Header?', TRUE)), 
                                 column(6, checkboxInput('rownames', ' Row names?', FALSE))), 
                        selectInput('sep', 'Separator:',
                                    c("Comma ','","Semicolon ';'","Tab","Space"), 
                                    "Comma ','"),
                        selectInput('quote', 'Quote:',
                                    c("None","Double Quote \"","Single Quote '"), 
                                    'Double Quote "'),
                        selectInput('dec', 'Decimal mark', 
                                    c("Period '.'", "Comma ','"),
                                    "Period '.'"),
                        helpText("Note: The preview only shows a restricted
                                 number of observations, the full set is imported."))),
               column(8,
                      fluidRow(
                        column(6, 
                               numericInput('nrow.preview','Nb. rows preview:',20)), 
                        column(6, 
                               numericInput('ncol.preview', 'Nb. cols preview:', 10))),
                      uiOutput("rownames.col"),
                      tableOutput("view"))
               )), 
    
    tabPanel("Train", 
             wellPanel(fluidRow(column(2, h3("Map info:")),
                                column(10, verbatimTextOutput("Message")))),
             h3("Training options:"),
             actionButton("trainbutton", "Train"),
             fluidRow(column(4, 
                             fluidRow(column(4, p("Map size (X,Y)")), 
                                      column(4, numericInput('kohDimx', NULL, 4, min= 1)), 
                                      column(4, numericInput('kohDimy', NULL, 4, min= 1))),
                             selectInput('kohTopo', 'Topology', c("rectangular", "hexagonal"))),
                      column(8, 
                             fluidRow(column(4, actionButton("varNum", "Select numeric variables")), 
                                      column(4, actionButton("varAll", "Select all variables")), 
                                      column(4, actionButton("varNone", "Unselect all variables"))), 
                             uiOutput("varchoice")))),
    
    tabPanel("Graph", 
             fluidRow(column(4, 
                             ## Sélection du graphique et des variables
                             selectInput("graphType", "Graph:", 
                                         choices= c("Hitmap", "Radar", "Line", 
                                                    "Camembert", "Barplot", "Boxplot",
                                                    "Color", "Star", "Names", 
                                                    "Dendrogram")),
                             conditionalPanel('input.graphType == "Camembert" | input.graphType == "Color"', 
                                              uiOutput("plotVarOne")),
                             conditionalPanel(paste0('input.graphType == "Radar" | ', 
                                                     'input.graphType == "Line" | ', 
                                                     'input.graphType == "Barplot" | ', 
                                                     'input.graphType == "Boxplot" | ', 
                                                     'input.graphType == "Star"'), 
                                              uiOutput("plotVarMult"))),
                      column(8, 
                             fluidRow(column(6, numericInput("plotSize", "Plot size:", 100, min= 10)), 
                                      column(6, numericInput('kohSuperclass', 'Nb. superclasses', 2, min= 1))),
                             ## Pour afficher seulement le graphique choisi :
                             conditionalPanel('input.graphType == "Dendrogram"', 
                                              plotOutput("screeplot")),
                             conditionalPanel('input.graphType != "Dendrogram"', 
                                              includeHTML("graphs.html"), 
                                              HTML('<h4 id="plot-message">Hover over the plot for information.</h4>'),
                                              HTML('<div id="thePlot" class="shiny-Plot"><svg /></div>'))
                             )))
  )
  
))
