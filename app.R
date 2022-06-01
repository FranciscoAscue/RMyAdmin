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

ui <- fluidPage( title = "Administrador",  
                 div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
                 
                 # login section
                 shinyauthr::loginUI(id = "login", title = h3(icon("server"),icon("atom"),"Ingreso de Datos COVID"), 
                                     user_title = "Usuario", pass_title = "Contraseña"),
                 uiOutput("Page") )


server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  mysql_explore <- reactive({
    req(input$Actualizar | TRUE)
    data <- metadata_sql(oficio = input$Soficio)
    data # <- data[,-c(6, 20, 22,24,25,29)]
  })
  
  asignados <- reactive({
    req(input$Actualizar | TRUE)
    data <- metadataOrder(input$Corrida, input$Placa, TRUE)
    data
  })
  
  inputSQL <- reactive({
    
    oficio <- input$Oficio
    netlab <- input$Netlab
    return(list(nt = netlab, of = oficio))
    
  })
  
  inputData <- reactive({
    req(input$Guardar | input$Buscar)
    data <- metadaupdate(input$Oficio)
    x <- create_btns(data$NETLAB)
    data <- data %>%
      dplyr::bind_cols(tibble("ACCIONES" = x))
    data
  })
  
  
  ############################################################################################################################################################
  
  output$tablemysql <- DT::renderDataTable(mysql_explore(),
                                           options = list(scrollX = TRUE),
                                           rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  
  output$tableasignados <- DT::renderDataTable(asignados(),
                                           options = list(scrollX = TRUE),
                                           rownames = FALSE, server = FALSE, escape = FALSE, selection = 'none')
  
  output$SqlInput <- DT::renderDataTable(inputData(), extensions = 'Buttons',
                                         options = list( pageLength = 25, dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                         rownames = FALSE, server = FALSE, escape = FALSE)
  
  ############################################################################################################################################################ 
  
  shiny::observeEvent(input$Buscar, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "delete"))
    output$SqlInput <- DT::renderDataTable(inputData(), extensions = 'Buttons',
                                           options = list( pageLength = 25, dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                           rownames = FALSE, server = FALSE, escape = FALSE)
  })
  
  
  observeEvent(input$Guardar,{
    tryCatch({
      metadataSendquery(toupper(inputSQL()$nt),toupper(inputSQL()$of))
      tmp <- input$Oficio
      
      updateTextInput(session, "Oficio", value = NA)
      updateTextInput(session, "Oficio", value = tmp)
      updateTextInput(session, "Netlab", value = NA)
      output$SqlInput <- DT::renderDataTable(inputData(), extensions = 'Buttons',
                                             options = list( pageLength = 25, dom = 'Blfrtip', buttons = c('copy', 'excel')),
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
    dni <- inputData()[edit_row, ][["DNI_CE"]]
    
    shiny::modalDialog(
      title = h3("Cod. Netlab :", sql_id),
      column(12,style = "background-color:#AED6F1;",
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
      column(12, style = "background-color:#82E0AA;",#82E0AA 
             column(6,
                    dateInput(inputId = "fecha_tm",
                              label = "Fecha de toma de muestra",
                              language = "es", value = fecha_tm , min = "2020-01-01", max = "2023-05-28"
                    )),
             column(6,   
                    selectInput(inputId = "motivo",
                                label = "Selecciona un motivo",
                                choices = c( "VIGILANCIA ALEATORIA",
                                             "ESPECIAL",
                                             "MINISEQ REGIONES", 
                                             "ESPECIAL HOSPITALIZADOS", 
                                             "VARIANTE","CLINICAS_PRIVADAS",
                                             "BARRIDO","NULL"),
                                selected = motivo)),
      ), 
      column(12,
             
             column(6,
             textInput(inputId = "dni",
                       label = "Ingresar DNI",
                       value = dni,
                       placeholder = "DNI o CE")
             )
             ),column(12, h3(" ")), easyClose = TRUE,
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
  
  
  shiny::observeEvent(input$final_edit, {
    shiny::req(!is.null(input$current_id) &
                 stringr::str_detect(input$current_id,pattern = "edit"))
    tryCatch({
      edit_row <- which(stringr::str_detect(inputData()$ACCIONES, pattern = paste0("\\b", input$current_id, "\\b") ))
      sql_id <- inputData()[edit_row, ][["NETLAB"]]
      update_sql(sql_id, ct = input$ct, ct2 = input$ct2, fecha_tm = input$fecha_tm, motivo = input$motivo, dni = input$dni)
    },
    error = function(e){
      #shiny::removeModal()%>%
      showModal(
        modalDialog(
          title = "Ocurrio un Error!",
          tags$i("DNI DUPLICADO"),br(),br(),
          tags$b("Error:"),br(),
          tags$code(e$message)
        )
      )
    })
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
  
  observeEvent(input$Asignar,{
    #req(input$Corrida & input$placa)
    shiny::modalDialog(
      title = h3("Verificar los datos antes de Confirmar"),
      div(
        shiny::actionButton(inputId = "asignar_conf",
                            label   = "Confirmar",
                            icon = shiny::icon("arrow-up"),
                            class = "btn-danger")
        
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$asignar_conf, {
    data <- metadata_sql(input$Soficio)
    row <- dim(metadataOrder(input$Corrida, input$Placa))[1]
    enumerar(data$NETLAB, row)
    metadataAsignar(Corrida = input$Corrida, Placa = input$Placa, Oficio = input$Soficio)
    shiny::removeModal()
  })
  
  
  observeEvent(input$Reasignar,{
    #req(input$Corrida & input$placa)
    shiny::modalDialog(
      title = h4("La reasignacion de corrida cambiara la numeracion de la corrida y placa actual"),
      div(
        shiny::actionButton(inputId = "reasignar_conf",
                            label   = "Confirmar",
                            icon = shiny::icon("arrow-up"),
                            class = "btn-danger")
        
      )
    ) %>% shiny::showModal()
    
  })
  
  shiny::observeEvent(input$reasignar_conf, {
    data <- metadataOrder(input$Corrida, input$Placa, TRUE)
    enumerar(data$NETLAB, 0)
    #metadataAsignar(Corrida = input$Corrida, Placa = input$Placa)
    shiny::removeModal()
  })
  
  output$Page <- renderUI({
    req(credentials()$user_auth)
    if(is.null(credentials()$user_auth)){
      return()
    }
    UploadData
  })
  
}
shinyApp(ui = ui, server = server)