################################################################################################################################################################
# Ui panel


################################################################################################################################################################

DataExp <- tabPanel("EXPLORACION",
                    
                    column(3, 
                           h3(p(style="color:black;text-align:left", 
                                tags$img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/thumbs/shiny.png",width="60px",height="60px"),
                                tags$img(src="https://i1.wp.com/fileserialkey.com/wp-content/uploads/2019/07/2-2.png?fit=300%2C300&ssl=1",width="60px",height="60px"),
                                tags$img(src="https://media.slid.es/uploads/1121994/images/6529504/minsa.jpg",width="190px",height="70px"),
                           )),
                           numericInput("corrida", label = "Ingrese una corrida", value = 1028, min = 27, max = 1040),
                           selectInput(inputId = "placa",
                                       label = "Selecciona una placa",
                                       choices = c("placa1","placa2","placa3","placa4","placa5","placa6"),
                                       selected = NULL),
                           downloadButton("downloadData", "Download", label = "Descarga corrida")),
                    column(9,
                           shinycssloaders::withSpinner(
                             DT::dataTableOutput("tablemysql"), type = 3, color.background = "white", color = "blue")
                    )
)
###############################################################################################################################################################

Analysis <- tabPanel(
  "ANALISIS",
  column(3, 
         h3(p(style="color:black;text-align:left", 
              tags$img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/thumbs/shiny.png",width="60px",height="60px"),
              tags$img(src="https://i1.wp.com/fileserialkey.com/wp-content/uploads/2019/07/2-2.png?fit=300%2C300&ssl=1",width="60px",height="60px"),
              tags$img(src="https://media.slid.es/uploads/1121994/images/6529504/minsa.jpg",width="190px",height="70px"),
         )),
         radioButtons("Tabs", 
                      label = h4("Estadisticas Disponibles"), 
                      choices = list("Graficas Dispersion" = "disp","Secuenciados x Mes" = "seqM", "Rechazados x Oficio" = "rechO", "Linajes x Corrida" = "linC"),
                      selected = "disp"),
        numericInput("corrA", label = "Seleccione corrida", value = 1030, min = 1, max = 1031)),
  
  conditionalPanel( condition = "input.Tabs == 'disp'",
                    column(8,plotlyOutput("point"))
                    
                    ),
  
  conditionalPanel( condition = "input.Tabs == 'seqM'",
                    column(8,style = "background-color:#DAF5E1;",
                           h3("Resumen de secuenciados por Mes"),
                           column(3, dateInput("SeqMesI", label = "Fecha Inicio", language = "es",value = "2021-12-01" , min = "2020-01-01", max = "2022-03-28")),
                           column(3, dateInput("SeqMesF", label = "Fecha Fin", language = "es", min = "2020-01-01", max = "2022-03-28")),
                           DT::dataTableOutput("tablemysql2"),
                    )
  ),
  
  conditionalPanel( condition = "input.Tabs == 'rechO'",
                    column(8, style = "background-color:#DAEEF5;",
                           h3("Rechazados por Oficio"),
                           textInput(inputId = "oficio",
                                     label = "Ingrese oficio",
                                     value = "0010-LOR-2022"),
                           DT::dataTableOutput("tablemysql22"),
                    )
  ),
  
  conditionalPanel(condition = "input.Tabs == 'linC'",
                   column(8,style = "background-color:#F5DADA;",
                          h3("Asignacion de Linajes"),
                          
                          
                          column(4,
                                 selectInput(inputId = "corrB",
                                             label = "Selecciona una placa",
                                             choices = c( "placa1","placa2", "placa3", "placa4", "placa5", "placa6"),
                                             selected = "placa1"),),
                          
                          DT::dataTableOutput("Coverage")
                          
                   )
  )
)

################################################################################################################################################################


UploadData <- tabPanel("INGRESO DE DATOS",
                       useShinyjs(),
                       column(3, 
                              h3(p(style="color:black;text-align:left", 
                                   tags$img(src="https://raw.githubusercontent.com/rstudio/hex-stickers/master/thumbs/shiny.png",width="60px",height="60px"),
                                   tags$img(src="https://i1.wp.com/fileserialkey.com/wp-content/uploads/2019/07/2-2.png?fit=300%2C300&ssl=1",width="60px",height="60px"),
                                   tags$img(src="https://media.slid.es/uploads/1121994/images/6529504/minsa.jpg",width="190px",height="70px"),
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
                              
                       ),
                       column(8,
                              DT::dataTableOutput("SqlInput")
                       ),
                       shiny::includeScript("script.js"),
                       column(1," "),
                       
)


################################################################################################################################################################


