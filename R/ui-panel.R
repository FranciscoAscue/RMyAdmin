################################################################################################################################################################

DataExp <- tabPanel("ASIGNAR CORRIDA",
                    column(12,
                           column(4, 
                                  h3(p(style="color:black;text-align:left", 
                                       tags$img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/thumbs/shiny.png",width="60px",height="60px"),
                                       tags$img(src="https://i1.wp.com/fileserialkey.com/wp-content/uploads/2019/07/2-2.png?fit=300%2C300&ssl=1",width="60px",height="60px"),
                                  )),
                                  
                                  h1(" "),
                                  shiny::br(),
                                  actionButton(inputId = "Actualizar", 
                                               label =  "Actualizar",
                                               width = "200px"),
                                  
                                  h1(" "),
                                  
                                  textInput(inputId = "Soficio",
                                            label = "Escribir Oficio",
                                            placeholder = "codigo oficio"),
                                  
                                  numericInput(inputId = "Corrida",
                                               label = "Selecciona Corrida",
                                               value = 4, min = 1, max = 12),
                                  
                                  selectInput(inputId = "Placa",
                                              label = "Seleccionar Placa",
                                              choices = c("placa1","placa2","placa3","placa4"),
                                              selected = "placa1"),
                                  
                                  actionButton(inputId = "Asignar", 
                                               label =  "Asignar Corrida",
                                               width = "200px"),
                                  h3(" "),
                                  h3(icon("spell-check")),
                                  
                                  actionButton(inputId = "Reasignar", 
                                               label =  "Reasignar enumeracion",
                                               width = "200px"),
                                  
                           ),
                           column(8,align="center",
                                  
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Pendientes",  DT::dataTableOutput("tablemysql")),
                                              tabPanel("Asignados", DT::dataTableOutput("tableasignados"))
                                  )
                                  
                           )
                    )
)


Reports <- tabPanel("REPORTES DE CORRIDA",
                    
                    column(12,align="center",
                           
                           tabsetPanel(type = "tabs",
                                       
                                       tabPanel("Descarga Base",
                                                column(12, 
                                                column(2, 
                                                       numericInput(inputId = "FCorrida",
                                                                    label = "Selecciona Corrida",
                                                                    value = 42, min = 1, max = 1000),
                                                       
                                                ),
                                                column(10,
                                                       DT::dataTableOutput("Fmatrix")
                                                ))
                                       ),
                                       tabPanel("Matrices",
                                                column(12, 
                                                column(2, 
                                                       radioButtons("type", "Seleccionar tipo de matriz",
                                                                    choices = list("SemanaEpidemio" = "SemanaEpidemio", "Corrida" = "Corrida"),
                                                                    selected = "SemanaEpidemio"),
                                                        uiOutput("Supdate"),
                                                        uiOutput("Sprov"),
                                                        uiOutput("motiv")
                                                       ),
                                                
                                                column(10,
                                                       DT::dataTableOutput("matrix"),
                                                       uiOutput("disperO")
                                                       ))
                                       ),
                                       tabPanel("Graficas por Variante",
                                                
                                                column(12, 
                                                       column(6, shinycssloaders::withSpinner(plotlyOutput("lineplot"))),
                                                       column(2, 
                                                              
                                                              column(7,
                                                              textInput(inputId = "lineage",
                                                                           label = "Write a linage",
                                                                           value = "AY.117"),
                                                              ),
                                                              column(4,
                                                              shiny::actionButton(inputId = "plothist",
                                                                                  label   = "Plot",
                                                                                  icon = shiny::icon("arrow-right"),
                                                                                  class = "btn-info"),
                                                              ),
                                                              numericInput("ngenomes", label = "Minimun genomes", min = 1, 
                                                                           max = 30, value = 1 ),
                                                              
                                                              dateRangeInput("lineageDate", "Select date range",
                                                                             start  = "2021-06-01",
                                                                             end    = "2022-01-27"),
                                                              
                                                              column(4, 
                                                                     radioButtons("stack", "Select plot",
                                                                                  choices = list("stack" = "stack", "lines" = "lines"),
                                                                                  selected = "stack")),
                                                              column(8, 
                                                                     radioButtons("Varline", "Select (VOC.VOI/Lineages)",
                                                                                  choices = list("VOC.VOI" = "VOC.VOI","Lineages" = "Lineages"), 
                                                                                  selected = "VOC.VOI")),
                                                       ),
                                                       column(4, shinycssloaders::withSpinner(plotlyOutput("hist")))
                                                ),
                                                
                                                
                                                column(12, 
                                                       
                                                       column(6, shinycssloaders::withSpinner(plotlyOutput("histSep"))),
                                                       column(6, shinycssloaders::withSpinner(plotlyOutput("histSep2"))),
                                                       
                                                       )
                                                
                                       ),
                                       
                                       tabPanel("Mapas",
                                                
                                                column(12, 
                                                       column(6, 
                                                              shinycssloaders::withSpinner( leafletOutput(outputId = "leaflet_map", height = 550))),
                                                       column(6,
                                                              column(4,
                                                                     column(12, dateRangeInput("Daterange", 
                                                                                               "Select date range", 
                                                                                               start  = "2020-03-01",
                                                                                               end    = "2022-05-01")      
                                                                     ),
                                                                     column(12,uiOutput("selectVariants")),
                                                                     column(12,radioButtons("Escala", "Select scale",
                                                                                            choices = list("linear" = "linear", 
                                                                                                           "logarithmic" = "log"), 
                                                                                            selected = "log", 
                                                                                            inline = TRUE)),
                                                                     column(12,radioButtons("switch", "Switch Lineages / VOC-VOI",
                                                                                            choices = list("lineage" = "lineage",
                                                                                                           "VocVoi" = "VocVoi"),
                                                                                            selected = "VocVoi",
                                                                                            inline = TRUE))),
                                                              
                                                              column(8,  shinycssloaders::withSpinner(leafletOutput("map", height = 550)))
                                                       )
                                                )
                                       )
                           )
                           
                    )
)


###############################################################################################################################################################

UploadData <- navbarPage(theme = shinytheme("flatly"), 
                         "ADMIN", 
                         id="nav", tabPanel("INGRESO DE DATOS",
                                            useShinyjs(),
                                            column(3, 
                                                   h3(p(style="color:black;text-align:left", 
                                                        tags$img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/thumbs/shiny.png",width="60px",height="60px"),
                                                        tags$img(src="https://i1.wp.com/fileserialkey.com/wp-content/uploads/2019/07/2-2.png?fit=300%2C300&ssl=1",width="60px",height="60px")
                                                   )),
                                                   textInput(inputId = "Oficio",
                                                             label = h4("Ingrese Oficio"),
                                                             value = NULL),
                                                   textInput(inputId = "Netlab",
                                                             label = "Netlab",
                                                             value = NULL),
                                                   actionButton(inputId = "Guardar", 
                                                                label =  "Guardar",
                                                                width = "200px"),
                                                   
                                                   h1(" "),
                                                   actionButton(inputId = "Buscar", 
                                                                label =  "Buscar Oficio",
                                                                width = "200px"),
                                                   
                                                   
                                            ),
                                            shiny::includeScript("script.js"),
                                            column(8,
                                                   DT::dataTableOutput("SqlInput")
                                            ),
                                            column(1," "),
                                            
                         ), DataExp, Reports)



