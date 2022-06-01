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
                                            
                         ), DataExp)



