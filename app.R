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

#### plot de funciones ####

line_stack_plot <-  function(data, stack){
  data <- ungroup(data)
  if(stack == "stack"){
    plot <- plot_ly(data, x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines", stackgroup = 'two')
  }else{
    plot <- plot_ly(data , x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines")
  }
  
  return(plot)
}

hist_plot <- function(data, lineage = "AY.102", ndf = 5){
  
  if(is.null(data$Date)){
    return(plot_ly(data.frame(NULL), type = "scatter", mode ="line"))
  }else{
    model <- lm(Frecuency ~ ns(Date, df = ndf), data = data)
    plot <- plot_ly(data, x = ~Date , y = ~Frecuency, name = lineage, type = 'bar', color = I("light blue"),
                    hoverinfo = ~Frecuency, text = ~paste("CDC week:", epi_week)) %>%
      add_trace(x = ~Date, y = ~fitted(model), type = "scatter", mode ="line", color = I("red"))
    
    return(plot)
  }
}


leaflet_plot <- function(data, palette, titleLegend, scale=FALSE, long = FALSE, lat = FALSE, 
                         var = FALSE, total = FALSE){
  basemap <- leaflet(data) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 1, fillColor = ~palette(N),
                label = ~paste0(Location, ": ", formatC(N, big.mark = ","))) %>%
    addLegend(pal = palette, values = ~N, title = titleLegend, opacity = 1.0) 
  
  if(!isFALSE(lat) & !isFALSE(long) & !isFALSE(var) & !isFALSE(total)){
    basemap <- basemap %>% addMinicharts(long, lat ,type = "pie", chartdata = var, opacity = 0.8, 
                                         colorPalette = brewer.pal(n = ncol(var), name = "Paired"), 
                                         width = 50 * sqrt(total) / sqrt(max(total)), transitionTime = 0)
    
    return(basemap)
  }
  
  return(basemap)
}

######## 
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
  
  ##### graph data input
  
  upload <- reactive({
    if(is.null(input$UploadC)){
      return(NULL)
    }
    metadata <- read.table(file = input$UploadC$datapath, sep = input$separator ,header = TRUE)
    return(metadata)
  })
  
  geojson <- reactive({
    
    url <- "https://raw.githubusercontent.com/FranciscoAscue/geojason-peru/master/peru_departamental.geojson"
    map <- geojsonsf::geojson_sf(geojson = url)
    map$Location <- toupper(map$Location)
    return(map)
  })
  
  # metadata upload (GISAID metadata / custom metadata) 
  meta <- reactive({
    metadata <- read.csv("/home/fascue/Documents/Git/metadata_shiny.csv", header = TRUE)
    metadata$date <- as.Date(metadata$date)
    metadata$Date <- as.Date(metadata$Date)
    colnames(metadata) <- c("CS","strain","gisaid","date", "location", "host","gender","age", "VAC","lineage", "VOC.VOI","epi_week","epi_year","Date")
    metadata
    return(metadata)
  })
  
  # epidemio metadata
  
  epidem_data <- reactive({
    emetadata <- read.csv("/home/fascue/Documents/Git/dead.csv")
    emetadata$date <- as.Date(emetadata$date)
    return(emetadata)
  })
  
  
  
  var_datamap <- reactive({
    
    datamap <- variant_distribution(map = geojson(), epidem = epidem_data(), 
                                    metadata = meta(), input$Daterange[1], input$Daterange[2],input$switch)
    
    return( list( df = datamap$df, pal = datamap$pal, long = datamap$long, lat = datamap$lat, 
                  var = datamap$var, total = datamap$total))
  })
  
  sampling_datamap <- reactive({
    #req(input$Variant)
    data_map <- sampling_distribution(map = geojson(), metadata = meta(), mindate = input$Daterange[1], 
                                      maxdate = input$Daterange[2], input$Variant, input$Escala)
    
    return(list(df = data_map$df, pal = data_map$pal))
    
  })
  
  lineage_var_data <- reactive({
    metadata <- as.data.frame(meta())
    metadata <- stackvariant(metadata,  input$lineageDate[1], input$lineageDate[2], 
                             input$ngenomes, input$Varline)
    
  })
  
  hist_data <- reactive({
    
    metadata <- as.data.frame(meta())
    metadata$date <- as.Date(metadata$date)
    data_lineage <- freq_voc_voi(metadata, input$lineage)
    data_lineage
  })
  
  heatmap_data <- reactive({
    cuadro_motivo <- matrix_distribution(metadata = meta(), type = input$type, 
                                         upload = upload(), prov = input$prov)
    cuadro_motivo
  })
  
  merge_dataF <- reactive({

    df_a <- metadata("seqcoviddb", "metadata", input$FCorrida)
    df_b <- metadata("SARS_GENOMES", "nextrain2gisaid", input$FCorrida)
    df_b1 <- df_b[,c("NETLAB","LINAGES","VOC_VOI","COVERAGE","N_PERCENTAGE")]
    
    datamerge <- merge(x = df_a, y = df_b1, by = "NETLAB", all = TRUE)
    as.data.frame(datamerge)
    return(datamerge)
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
  
  output$matrix <- DT::renderDataTable(heatmap_data(), extensions = 'Buttons',
                                         options = list( scrollX = TRUE, pageLength = 50, dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                         rownames = TRUE, server = FALSE, escape = FALSE)
  
  output$Fmatrix <- DT::renderDataTable(merge_dataF(), extensions = 'Buttons',
                                       options = list( scrollX = TRUE, pageLength = 50, dom = 'Blfrtip', buttons = c('copy', 'excel')),
                                       rownames = TRUE, server = FALSE, escape = FALSE)
  
  
  output$map <- renderLeaflet({
    basemap <- leaflet_plot(data = sampling_datamap()$df, palette = sampling_datamap()$pal, 
                            titleLegend = "Nº\nGenomes" )   
    
  })
  
  output$leaflet_map <- renderLeaflet({
    basemap <- leaflet_plot(data = var_datamap()$df, palette = var_datamap()$pal,
                            long = var_datamap()$long, lat = var_datamap()$lat, 
                            titleLegend = "Mortality rate x10⁵", var = var_datamap()$var,
                            total = var_datamap()$total)
  })
  
  
  output$lineplot <- renderPlotly({ 
    line_stack_plot(lineage_var_data(), input$stack)
  })
  
  output$hist <- renderPlotly({ 
    hist_plot(hist_data(), lineage = input$lineage) 
  })
  
  ############################################################################################################################################################ 
  
  output$selectVariants <- renderUI({
    # if(is.null(input$metadata))
    #   return()
    
    Vocvoi <- c("Total")
    Vocvoi <- append(Vocvoi, unique(meta()$VOC.VOI))
    
    selectInput(inputId = "Variant", 
                label = "Select a variant (VOC-VOI)", 
                choices = as.list(Vocvoi),
                selected = "Total")
  })
  
  output$selectLineages <- renderUI({
    # if(is.null(input$metadata))
    #   return()
    # 
    selectInput(inputId = "Lineages", 
                label = "Select a lineage", 
                choices = as.list(unique(meta()$lineage)),
                selected = NA)
    
  })
  
  #################################################################################################################################################################
  
  
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
  
  output$Supdate <- renderUI({
    req(input$type)
    if(input$type == "SemanaEpidemio"){
      return()
    }
    column(12,
    fileInput(inputId = "UploadC", 
              h4("Upload table"), accept='.csv'),
    
    radioButtons("separator", "Select separator",
                 choices = list("Tab" = "\t", "Comma" = ",", "Semicolon"=";"), selected = ";"))

  })
  
  
  output$Sprov <- renderUI({
    req(input$type)
    if(input$type == "SemanaEpidemio"){
      return()
    }
    
    radioButtons("prov", "Seleccionar",
                 choices = list("Region" = "Region", 
                                "Provincia" = "Provincia"),
                 selected = "Region")
    
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