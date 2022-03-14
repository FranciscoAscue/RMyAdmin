source("R/dependencies.R", local = TRUE)
source("R/ui-panel.R", local = TRUE)
source("R/data-mysql.R", local = TRUE)
source("config.R", local = TRUE)


create_btns <- function(x) {
  x %>% purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-info action_button" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-edit"></i></button>
                   <button class="btn btn-default action-button btn-danger action_button" id="delete_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fa fa-trash-alt"></i></button></div>'
                     ))
}



ui <- navbarPage(theme = shinytheme("flatly"), "RMyAdmin-Ingreso", id="nav", UploadData, DataExp, Analysis )


server <- function(input, output, session) {
  
  mysql_explore <- reactive({
    data <- metadata_sql(corrida = input$corrida, placa = input$placa)
    data # <- data[,-c(6, 20, 22,24,25,29)]
  })
  
  mysql_order <- reactive({
    
    datos <- metadataOrder(corrida = input$corrida)
    datos <- as.data.frame(datos)
    DD <- datos[order(datos$NUMERACION_PLACA),] %>% arrange(PLACA)
    DD <- DD %>% mutate_if(is.character, trimws)
  })
  
  dataOrder <- reactive({
    
    datos <- metadataRechazado(input$oficio)
    #metadata <- data[order(metadata$CORRIDA, metadata$PLACA), ]
    datos
    
  })
  
  seqMes <- reactive({
    datos <- metadataSeq(input$SeqMesI,input$SeqMesF)
    datos <- as.data.frame(table(datos$PROCEDENCIA))
    colnames(datos) <- c("PROCEDENCIA","CANTIDAD")
    datos$PORCENTAJE <- round((datos$CANTIDAD / sum(datos$CANTIDAD))*100, digits = 3)
    datos
  })
  
  coverage <- reactive({
    
    # Extraemos las varialbles que nos interesan de las bases de datos por corrida 
    df_a <- metadaquery(input$corrB, input$corrA)
    df_b <- metadataS("nextrain2gisaid",input$corrA)
    
    df_a1 <- df_a[,c("NETLAB","NUMERACION_PLACA")]
    df_b1 <- df_b[,c("NETLAB","LINAGES","COVERAGE","N_PERCENTAGE")]
    
    # Unimos en un solo data frame para mejor manejo de la data 
    datamerge <- merge(x = df_a1, y = df_b1, by = "NETLAB", all = TRUE)
    datamerge$COVERAGE <- gsub("(coverage)(amplicons)", "x", datamerge$COVERAGE, fixed = T )
    datamerge <- datamerge %>% filter(!is.na(NUMERACION_PLACA))
    datamerge
  })
  
  inputSQL <- reactive({
    
    oficio <- input$Oficio
    netlab <- input$Netlab
    return(list(nt = netlab, of = oficio))
    
  })
  
  inputData <- reactive({
    req(input$Guardar)
    data <- metadaupdate(input$Oficio)
    x <- create_btns(data$NETLAB)
    data <- data %>%
    dplyr::bind_cols(tibble("ACCIONES" = x))
    data
  
  })
  
  corrida <- reactive({
    data <- metadataOrder(input$corrA)
    data$FECHA_TM <- as.Date(data$FECHA_TM)
    data
  })
  ############################################################################################################################################################
  

  output$point <- renderPlotly({
    
    plot_ly(corrida(), x= ~FECHA_TM, y = ~CT, color = ~MOTIVO, text= ~NETLAB, type = 'scatter', mode = "markers")
  
  })
  
  
  output$tablemysql <- DT::renderDataTable(mysql_explore(),
                                           options = list(scrollX = TRUE),
                                           rownames = FALSE)
  
  output$tablemysql22 <- DT::renderDataTable(dataOrder(), extensions = 'Buttons',
                                             options = list( dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                             rownames = FALSE)
  
  output$tablemysql2 <- DT::renderDataTable(seqMes(),
                                            options = list(dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                            rownames = FALSE)
  
  output$Coverage <- DT::renderDataTable(coverage(), extensions = 'Buttons',
                                         options = list( dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                         rownames = FALSE)
  
  output$SqlInput <- DT::renderDataTable(inputData(), extensions = 'Buttons',
                                         options = list( dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                         rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  ############################################################################################################################################################ 
  observeEvent(input$Guardar,{
    tryCatch({
      metadataSendquery(inputSQL()$nt,inputSQL()$of)
      updateTextInput(session, "Netlab", value = NA)
      tmp <- input$Oficio 
      updateTextInput(session, "Oficio", value = NA)
      updateTextInput(session, "Oficio", value = tmp)
      output$SqlInput <- DT::renderDataTable(inputData(), extensions = 'Buttons',
                                               options = list( dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                               rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
    },
    
    error = function(e){
      showModal(
        modalDialog(
          title = "Ocurrio un Error!",
          tags$i("Ingrese nuevamente los datos"),br(),br(),
          tags$b("Error:"),br(),
          tags$code(e$message)
        )
      )
    })
    
  })
  

  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "delete"))
    #delet_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    #sql_id <- inputData()[delet_row, ][["NETLAB"]] 
    shiny::modalDialog(
      title = h3("Se borrara permanentemente los datos!!"),
       div(
        shiny::actionButton(inputId = "final_delete",
                            label   = "Confirmar",
                            icon = shiny::icon("trash"),
                            class = "btn-danger")
        
      )
    ) %>% shiny::showModal()
  
  })
  
  shiny::observeEvent(input$final_delete, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "delete"))
    delet_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    sql_id <- inputData()[delet_row, ][["NETLAB"]] 
    query <- paste0("DELETE FROM `metadata` WHERE `metadata`.`NETLAB` = \'",sql_id,"\'")
    delete_sql(query)
  })
  
  shiny::observeEvent(input$current_id, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "edit"))
    edit_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    sql_id <- inputData()[edit_row, ][["NETLAB"]]
    ct <- inputData()[edit_row, ][["CT"]]
    ct2 <- inputData()[edit_row, ][["CT2"]]
    fecha_tm <- inputData()[edit_row, ][["FECHA_TM"]]
    motivo <- inputData()[edit_row, ][["MOTIVO"]]
    
    shiny::modalDialog(
      title = h3(sql_id),
      column(12,
      column(6,
          numericInput(inputId = "ct",
                       label = "Ingresar CT",
                       value = ct, 
                       width = "200px")),
      column(6,
            numericInput(inputId = "ct2",
                         label = "Ingresar CT2",
                         value = ct2, 
                         width = "200px")),
      ),
      
      column(12, 
      column(6,
          disabled(dateInput(inputId = "fecha_tm",
                    label = "Fecha de toma de muestra",
                    language = "es", value = fecha_tm , min = "2020-01-01", max = "2022-05-28"
                    ))),
      column(6, icon("radiation"), icon("bomb"),"-NULL-", 
          checkboxInput("checkbox", label = "Fecha Null", value = TRUE)),
      ),
      
      column(12,
          column(3, " "),
          column(6,   
          selectInput(inputId = "motivo",
                      label = "Selecciona un motivo",
                      choices = c( "VIGILANCIA ALEATORIA",
                                   "ESPECIAL","BARRIDO",
                                   "MINISEQ REGIONES", 
                                   "ESPECIAL HOSPITALIZADOS", 
                                   "VARIANTE", 
                                   "CLINICAS_PRIVADAS",
                                   "BARRIDO","NULL"),
                      selected = motivo)),
          column(3, " ")
          
      ), easyClose = TRUE,
      footer = div(
        shiny::actionButton(inputId = "final_edit",
                            label   = "Ingresar",
                            icon = shiny::icon("edit"),
                            class = "btn-info"),
        shiny::actionButton(inputId = "dismiss_modal",
                            label   = "Cancelar",
                            class   = "btn-danger")
      )
    ) %>% shiny::showModal()
    
  })

  observeEvent(input$checkbox, {
    if(input$checkbox == FALSE)
      enable("fecha_tm")
  })
  
  
  shiny::observeEvent(input$final_edit, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "edit"))
    edit_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
    
    sql_id <- inputData()[edit_row, ][["NETLAB"]]

    update_sql(sql_id, ct = input$ct, ct2 = input$ct2, fecha_tm = input$fecha_tm, check = input$checkbox, motivo = input$motivo)
  })
  
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$final_delete, {
    shiny::removeModal()
    tmp <- input$Oficio 
    updateTextInput(session, "Oficio", value = NA)
    updateTextInput(session, "Oficio", value = tmp)
  })
  
  shiny::observeEvent(input$final_edit, {
    
    shiny::removeModal()
    tmp <- input$Oficio 
    updateTextInput(session, "Oficio", value = NA)
    updateTextInput(session, "Oficio", value = tmp)
    
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Download.xlsx")
    },
    content = function(file) {
      write_xlsx(mysql_order(), path = file)
    }
  )
  
  
}
  shinyApp(ui = ui, server = server)

# Run the application 
