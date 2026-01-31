library(shiny)
library(reactable)
library(dplyr)
library(RSQLite)
library(DBI)
library(bslib)
library(DiagrammeR)
library(shinyjs)

# --- Configuración de Base de Datos y Almacenamiento ---
dir_storage <- "archivos_pdf"
if (!dir.exists(dir_storage)) dir.create(dir_storage)

db_path <- "registro_documentos.db"

# Crear la tabla si no existe
con <- dbConnect(SQLite(), db_path)
dbExecute(con, "CREATE TABLE IF NOT EXISTS documentos (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 Nombre_documento TEXT,
                 Ramo TEXT,
                 Producto TEXT,
                 Documento_numero TEXT,
                 Actuario_responsable TEXT,
                 Fecha_emision TEXT,
                 Fecha_recepcion TEXT,
                 Categoria TEXT,
                 Fecha_carga TEXT,
                 Fecha_limite TEXT,
                 Archivo_Real TEXT,
                 Estado TEXT DEFAULT 'Cargado')")

# Tabla de Usuarios (Seguridad)
dbExecute(con, "CREATE TABLE IF NOT EXISTS usuarios (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 user TEXT UNIQUE, 
                 pass TEXT)")

#Tabla de Información y condiciones de contratos de Reaseguros
dbExecute(con, "CREATE TABLE IF NOT EXISTS reaseguros (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 Serie TEXT,
                 Ramo TEXT,
                 Tipo_contrato TEXT,
                 Contrato_numero TEXT,
                 Actuario_responsable TEXT,
                 Fecha_emision TEXT,
                 Fecha_recepcion TEXT,
                 Reasegurador TEXT,
                 Fecha_carga TEXT,
                 Fecha_limite TEXT,
                 Contrato_Real TEXT,
                 Estado TEXT DEFAULT 'Cargado')")


# --- Helpers de UI ---
status_colors <- c(
  'Cargado' = '#d1ecf1',
  'En Elaboración' = '#00FFF7',
  'Aprobado Área Funcional' = '#ADFF2F',
  'Aprobado Gerencia Actuarial' = '#ff6675',
  'Aprobado Junta Directiva' = '#b23f44',
  'Remitido a la SUDEASEG' = '#f1c40f',
  'En Revisión en la SUDEASEG' = '#ffa500',
  'Devuelto Corrección / Modificación' = '#dddddd',
  'Devuelto Negado' = '#777777',
  'Aprobado' = '#2ecc71'
)

# Insertar usuario inicial si la tabla está vacía (Usuario: admin / Pass: 1234)
users_count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM usuarios")$n
if(users_count == 0) {
  dbExecute(con, "INSERT INTO usuarios (user, pass) VALUES ('admin', '1234')")
}
dbDisconnect(con)

ui <- fluidPage(
  useShinyjs(),
  theme = bslib::bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .login-bg {
        background: linear-gradient(rgba(0, 0, 0, 0.6), rgba(0, 0, 0, 0.6)),
                    url('CENTRO-LIDO.png');
        background-size: cover;
        background-position: center;
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .login-box {
        background: white;
        padding: 40px;
        border-radius: 15px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.3);
        width: 350px;
        text-align: center;
      }
      .footer-custom {
        display: flex;          /* Activa Flexbox */
        flex-direction: column; /* Alinea los textos uno sobre otro */
        align-items: center;    /* Centra horizontalmente */
        justify-content: center;/* Centra verticalmente */
        padding: 40px 0;        /* Espacio arriba y abajo */
        margin-top: 50px;       /* Distancia con el contenido superior */
        width: 100%;            /* Ocupa todo el ancho */
        border-top: 1px solid #e0e0e0;
        color: #7f8c8d;
      }
      .footer-text {
        margin: 5px 0;          /* Espacio entre líneas */
        font-size: 14px;
      }
    "))),

  # tags$div(
  #   style = "display: flex; 
  #            flex-direction: column; 
  #            align-items: center; 
  #            justify-content: center; 
  #            padding: 10px 0;             /* Padding reducido arriba/abajo */
  #            background-color: #ffffff; 
  #            width: 100%;",
  #   # Aquí cargamos la imagen desde la carpeta www/
  #   tags$img(src = "logo_color_LM.png", style = "height: 200px; width: auto;")
  # ),
  # 
  # sidebarLayout(
  #   sidebarPanel(
  #     h4("Control de documentos"),
  #     tabsetPanel(
  #       tabPanel("Carga",
  #                br(),
  #     fileInput("file_input", "Seleccione PDF", accept = c(".pdf"), buttonLabel = "Buscar archivo...", placeholder = "Ningún archivo seleccionado"),
  #     textInput("doc_name", "Nombre del Documento", ""),
  #     textInput("ramo", "Ramo del Documento", ""),
  #     textInput("producto", "Nombre del Producto", ""),
  #     textInput("doc_num", "Número del Documento", ""),
  #     textInput("actuario", "Actuario Responsable", ""),
  #     dateInput("fecha_emi", "Fecha de emisión del Documento", value = "", format = "dd/mm/yyyy", language = "es"),
  #     dateInput("fecha_rec", "Fecha de recepción del Documento", value = "", format = "dd/mm/yyyy", language = "es"),
  #     selectInput("category", "Categoría", choices = c("Legajo", 
  #                                                      "Condicionado General", 
  #                                                      "Condicionado Particular",
  #                                                      "Reglamento Actuarial",
  #                                                      "Anexo de Cobertura",
  #                                                      "Contrato Servicio Producto",
  #                                                      "Comunicación SUDEASEG Remisión",
  #                                                      "Comunicación SUDEASEG Corrección/Modificación",
  #                                                      "Comunicación SUDEASEG Aprobación",
  #                                                      "Comunicación SUDEASEG Negación",
  #                                                      "Otros Documentos de la Póliza")),
  #     dateInput("fecha_limite", "Fecha límite de espera del producto", value = "", format = "dd/mm/yyyy", language = "es"),
  #     actionButton("save_btn", "Guardar", class = "btn-primary", icon = icon("save"))),
  #     tabPanel("Estado", 
  #              uiOutput("status_ui"))
  #     ),
  #     hr(),
  #     h4("Opciones"),
  #     uiOutput("download_ui"),
  #     br(),
  #     uiOutput("delete_ui")
  #   ),
  #   
  #   mainPanel(
  #     tabsetPanel(
  #       tabPanel("Seguimiento Documentación de Productos", 
  #                DTOutput("tabla_docs")),
  #       tabPanel("Mapa de Ruta", 
  #                br(),
  #                div(class = "diagram-box",
  #                    h4("Progreso del Documento Seleccionado", align="center"),
  #                    DiagrammeROutput("interact_diagram", height = "400px"),
  #                    uiOutput("current_step_text")
  #                ),
  #                br(),
  #                wellPanel(
  #                  h5("Guía de colores:"),
  #                  tags$span(style="color:#3498db;", "● Azul:"), " Cargado | ",
  #                  tags$span(style="color:#f1c40f;", "● Amarillo:"), " En Revisión | ",
  #                  tags$span(style="color:#2ecc71;", "● Verde:"), " Aprobado Final"
  #                )
  #       ),
  #       tabPanel("Visor de Documentos", 
  #                uiOutput("pdf_viewer"))
  #     )
  #   )
  # ),
  # 
  # tags$footer(
  #   class = "footer-custom",
  #   tags$p(paste0("© ", format(Sys.Date(), "%Y"), " - Todos los derechos reservados"), style = "align-items: center; justify-content: center;"),
  #   tags$p("Gerencia Corporativa Actuarial y Reaseguros | Sistema de Seguimiento de Productos | Versión 1.0 PoC")
  # ),
  
  uiOutput("body_ui")
)

server <- function(input, output, session) {
  
  # Variable reactiva para controlar el acceso
  logged_in <- reactiveVal(FALSE)
  user_data <- reactiveVal(NULL)
  selected_row <- reactiveVal(NULL)
  
  
  # --- UI DE LOGIN ---
  login_ui <- div(class = "login-bg",
                  div(class = "login-box",
                      tags$img(src = "logo_color_LM.png", height = "130px", style="margin-bottom:20px;"),
                      h3("Iniciar Sesión", style="margin-bottom:20px;"),
                      textInput("user", NULL, placeholder = "Usuario"),
                      passwordInput("pass", NULL, placeholder = "Contraseña"),
                      br(),
                      actionButton("login_btn", "Entrar", class = "btn-primary", width = "100%"),
                      uiOutput("error_msg")
                  )
  )
  
  
  # --- RENDERIZADO CONDICIONAL ---
  output$body_ui <- renderUI({
    if (logged_in() == FALSE) {
      login_ui
    } else {
      # Aquí va toda la estructura que creamos antes
      ui <- fluidPage(
        theme = bslib::bs_theme(bootswatch = "flatly"),
        tags$head(
          tags$style(HTML("
      .login-bg {
        background: linear-gradient(rgba(0, 0, 0, 0.6), rgba(0, 0, 0, 0.6)), 
                    url('https://images.unsplash.com/photo-1497366216548-37526070297c?auto=format&fit=crop&w=1350&q=80');
        background-size: cover;
        background-position: center;
        height: 100vh;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .login-box {
        background: white;
        padding: 40px;
        border-radius: 15px;
        box-shadow: 0 10px 25px rgba(0,0,0,0.3);
        width: 350px;
        text-align: center;
      }
      .footer-custom {
        display: flex;          /* Activa Flexbox */
        flex-direction: column; /* Alinea los textos uno sobre otro */
        align-items: center;    /* Centra horizontalmente */
        justify-content: center;/* Centra verticalmente */
        padding: 40px 0;        /* Espacio arriba y abajo */
        margin-top: 50px;       /* Distancia con el contenido superior */
        width: 100%;            /* Ocupa todo el ancho */
        border-top: 1px solid #e0e0e0;
        color: #7f8c8d;
      }
      .footer-text {
        margin: 5px 0;          /* Espacio entre líneas */
        font-size: 14px;
      }
    "))),
        
        tags$div(
          style = "display: flex; 
             flex-direction: column; 
             align-items: center; 
             justify-content: center; 
             padding: 10px 0;             /* Padding reducido arriba/abajo */
             background-color: #ffffff; 
             width: 100%;",
          # Aquí cargamos la imagen desde la carpeta www/
          tags$img(src = "logo_color_LM.png", style = "height: 200px; width: auto;")
        ),
        
        sidebarLayout(
          sidebarPanel(
            h4("Control de documentos"),
            tabsetPanel(
              tabPanel("Carga",
                       br(),
                       fileInput("file_input", "Seleccione PDF", accept = c(".pdf"), buttonLabel = "Buscar archivo...", placeholder = "Ningún archivo seleccionado"),
                       textInput("doc_name", "Nombre del Documento", ""),
                       textInput("ramo", "Ramo del Documento", ""),
                       textInput("producto", "Nombre del Producto", ""),
                       textInput("doc_num", "Número del Documento", ""),
                       textInput("actuario", "Actuario Responsable", ""),
                       dateInput("fecha_emi", "Fecha de emisión del Documento", format = "dd/mm/yyyy", language = "es"),
                       dateInput("fecha_rec", "Fecha de recepción del Documento", format = "dd/mm/yyyy", language = "es"),
                       selectInput("category", "Categoría", choices = c("Legajo", 
                                                                        "Condicionado General", 
                                                                        "Condicionado Particular",
                                                                        "Reglamento Actuarial",
                                                                        "Anexo de Cobertura",
                                                                        "Contrato Servicio Producto",
                                                                        "Comunicación SUDEASEG Remisión",
                                                                        "Comunicación SUDEASEG Corrección/Modificación",
                                                                        "Comunicación SUDEASEG Aprobación",
                                                                        "Comunicación SUDEASEG Negación",
                                                                        "Otros Documentos de la Póliza")),
                       dateInput("fecha_limite", "Fecha límite de espera del producto", format = "dd/mm/yyyy", language = "es"),
                       actionButton("save_btn", "Guardar", class = "btn-primary", icon = icon("save"))),
              tabPanel("Estado", 
                       uiOutput("status_ui"))
            ),
            hr(),
            h4("Opciones"),
            uiOutput("download_ui"),
            br(),
            uiOutput("delete_ui")
          ),
          
          mainPanel(
            tabsetPanel(
              tabPanel("Seguimiento Documentación de Productos", 
                       reactableOutput("tabla_docs")),
              tabPanel("Mapa de Ruta", 
                       br(),
                       div(class = "diagram-box",
                           h4("Progreso del Documento Seleccionado", align="center"),
                           DiagrammeROutput("interact_diagram", height = "1000px"),
                           uiOutput("current_step_text")
                       ),
                      
              ),
              tabPanel("Visor de Documentos", 
                       uiOutput("pdf_viewer"))
            )
          )
        ),
        
        tags$footer(
          class = "footer-custom",
          tags$p(paste0("© ", format(Sys.Date(), "%Y"), " - Todos los derechos reservados"), style = "align-items: center; justify-content: center;"),
          tags$p("Gerencia Corporativa Actuarial y Reaseguros | Sistema de Seguimiento de Productos | Versión 1.0 PoC")
        ))
    }
  })
  
  
  # --- LÓGICA DE AUTENTICACIÓN CORREGIDA ---
  observeEvent(input$login_btn, {
    con <- dbConnect(SQLite(), db_path)
    
    # Buscamos al usuario en la tabla de la base de datos
    query <- "SELECT * FROM usuarios WHERE user = ? AND pass = ?"
    res <- dbGetQuery(con, query, params = list(input$user, input$pass))
    dbDisconnect(con)
    
    # Si 'res' tiene al menos una fila, las credenciales son correctas
    if (nrow(res) > 0) {
      user_data(res) # Guardamos los datos del usuario logueado
      logged_in(TRUE) # Cambiamos el estado a 'autenticado'
    } else {
      output$login_err <- renderUI({
        p("Usuario o contraseña incorrectos", style="color:red; margin-top:10px;")
      })
    }
  })
  
  observeEvent(input$logout, {
    session$reload() # Recarga la app para volver al login
  })
  
  # Función para leer los datos de la DB
  cargar_datos <- function() {
    con <- dbConnect(SQLite(), db_path)
    res <- dbReadTable(con, "documentos")
    dbDisconnect(con)
    return(res)
  }
  
  # Capturar fila seleccionada en reactable
  observe({
    selected <- getReactableState("tabla_docs", "selected")
    selected_row(selected)
  })
  
  
  # Valor reactivo para refrescar la UI
  trigger_update <- reactiveVal(0)
  
  datos_db <- reactive({
    trigger_update()
    cargar_datos()
  })
  
  # Guardar Archivo y Registro
  observeEvent(input$save_btn, {
    req(input$file_input)
    
    file_name <- paste0(as.numeric(Sys.time()), "_", input$file_input$name)
    dest_path <- file.path(dir_storage, file_name)
    file.copy(input$file_input$datapath, dest_path)
    
    # Insertar en SQL
    con <- dbConnect(SQLite(), db_path)
    query <- "INSERT INTO documentos (Nombre_documento, Ramo, Producto, Documento_numero, Actuario_responsable, Fecha_emision, Fecha_recepcion, Categoria, Fecha_carga, Fecha_limite, Archivo_Real, Estado) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'Cargado')"
    dbExecute(con, query, params = list(
      ifelse(input$doc_name == "", input$file_input$name, input$doc_name),
      input$ramo,
      input$producto,
      input$doc_num,
      input$actuario,
      as.character(input$fecha_emi),
      as.character(input$fecha_rec),
      input$category,
      as.character(Sys.Date()),
      as.character(input$fecha_limite),
      file_name
    ))
    dbDisconnect(con)
    
    trigger_update(trigger_update() + 1)
    showNotification("Documento registrado en la base de datos.", type = "message")
  })
  
  # Eliminar Registro
  observeEvent(input$delete_btn, {
    req(selected_row())
    df <- datos_db()
    fila <-df[selected_row(), ]
    
    # Eliminar archivo físico
    file.remove(file.path(dir_storage, fila$Archivo_Real))
    
    # Eliminar de SQL
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "DELETE FROM documentos WHERE id = ?", params = list(fila$id))
    dbDisconnect(con)
    
    trigger_update(trigger_update() + 1)
    showNotification("Registro eliminado.", type = "warning")
  })
  
  # Renderizar Tabla
  output$tabla_docs <- renderReactable({
    reactable(datos_db(), selection = 'single', 
              onClick = "select",
              defaultColDef = colDef(style = list(whiteSpace = "normal"),
                                     width = 180,
                                     minWidth = 100),
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              language = reactableLang(searchPlaceholder = "Buscar documento...", noData = "No hay registros"),
              columns = list(
                id = colDef(show = FALSE),
                Archivo_Real = colDef(show = FALSE),
                Estado = colDef(
                  cell = function(estado_actual) {
                    color <- status_colors[estado_actual]
                    span(class = "status-badge", style = list(background = color), estado_actual)
                  }
                )
              )
    )
  })
  
  
  # --- LÓGICA DEL DIAGRAMA INTERACTIVO (DiagrammeR) ---
  output$interact_diagram <- renderDiagrammeR({
    # Estado por defecto si no hay selección
    estado_actual <- "Ninguno"
    
    if (!is.null(selected_row())) {
      df <- datos_db()
      fila <- df[selected_row(), ]
      estado_actual <- fila$Estado
    }
    
    # Definimos los estilos según el estado seleccionado
    style_A <- if(estado_actual == "Cargado") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_B <- if(estado_actual == "En Elaboración") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_C <- if(estado_actual == "Aprobado Área Funcional") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_D <- if(estado_actual == "Aprobado Gerencia Actuarial") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_E <- if(estado_actual == "Aprobado Junta Directiva") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_F <- if(estado_actual == "Remitido a la SUDEASEG") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_G <- if(estado_actual == "En Revisión en la SUDEASEG") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_H <- if(estado_actual == "Devuelto Corrección / Modificación") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_I <- if(estado_actual == "Devuelto Negado") "fill:#fff3cd, stroke:#ffc107, stroke-width:4px, color:#856404" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    style_J <- if(estado_actual == "Aprobado") "fill:#10b981, stroke:#10b981, stroke-width:4px, color:#ffffff" else "fill:#d1ecf1, stroke:#2980b9, stroke-width:1px"
    
    mermaid(paste0("
    graph TD
      A(Cargado) --> B{En Elaboración}
      B --> C(Aprobado Área Funcional)
      C --> D(Aprobado Gerencia Actuarial)
      D --> E(Aprobado Junta Directiva)
      E --> F(Remitido a la SUDEASEG)
      F --> G(En Revisión en la SUDEASEG)
      G --> H(Devuelto Corrección / Modificación)
      H -.-> G
      G --> I(Devuelto Negado)
      I -.-> B
      G --> J{Aprobado}
      
      

      style A ", style_A, "
      style B ", style_B, "
      style C ", style_C, "
      style D ", style_D, "
      style E ", style_E, "
      style F ", style_F, "
      style G ", style_G, "
      style H ", style_H, "
      style I ", style_I, "
      style J ", style_J, "
    "))
    
     # style_active <- "fill:#ffe5d9, stroke:#ff5722, stroke-width:5px"
    # mermaid(paste0("
    # graph LR
    #   A(Cargado) --> B(En Elaboración)
    #   B --> C(Aprobado Interno)
    #   C --> D(En SUDEASEG)
    #   D --> E(Aprobado Final)
    #   
    #   style ", if(estado_actual == 'Cargado') "A" else if(estado_actual == 'Aprobado') "E" else "B", " ", style_active
    # ))
  })
  
  output$current_step_text <- renderUI({
    if (is.null(selected_row())) {
      div(class="status-info", "Seleccione un documento en el historial para ver su progreso.")
    } else {
      df <- datos_db()
      fila <- df[selected_row(), ]
      div(class="status-info", paste("Documento:", fila$Nombre, "| Estado actual:", fila$Estado))
    }
  })
  
  output$status_ui <- renderUI({
    req(selected_row())
    df <- datos_db()
    fila <- df[selected_row(), ]
    wellPanel(
      h5("Cambiar Estado"),
      selectInput("new_status", "Nuevo estado para el archivo:", 
                  choices = c('Cargado', 'En Elaboración', 'Aprobado Área Funcional', 'Aprobado Gerencia Actuarial', 'Aprobado Junta Directiva', 'Remitido a la SUDEASEG', 'En Revisión en la SUDEASEG', 'Devuelto Corrección / Modificación', 'Devuelto Negado', 'Aprobado'), 
                  selected = fila$Estado),
      actionButton("update_status_btn", "Actualizar Ahora", class="btn-info btn-sm", width="100%")
    )
  })
  
  observeEvent(input$update_status_btn, {
    df <- datos_db()
    fila <- df[selected_row(), ]
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "UPDATE documentos SET Estado = ? WHERE id = ?", params = list(input$new_status, fila$id))
    dbDisconnect(con)
    trigger_update(trigger_update() + 1)
    showNotification("Estado actualizado y diagrama reflejado.", type = "message")
  })
  
  # Visor
  output$pdf_viewer <- renderUI({
    req(selected_row())
    df <- datos_db()
    fila <- df[selected_row(), ]
    addResourcePath("pdf_dir", dir_storage)
    tags$iframe(style = "height:700px; width:100%; border:none;", 
                src = paste0("pdf_dir/", fila$Archivo_Real))
  })
  
  # Botones Dinámicos
  output$download_ui <- renderUI({
    req(selected_row())
    downloadButton("download_file", "Descargar PDF", class = "btn-success")
  })
  
  output$delete_ui <- renderUI({
    req(selected_row())
    actionButton("delete_btn", "Eliminar Registro", class = "btn-danger", icon = icon("trash"))
  })
  
  output$download_file <- downloadHandler(
    filename = function() {
      df <- datos_db()
      fila <- df[selected_row(), ]
      fila$Archivo_Real
    },
    content = function(file) {
      df <- datos_db()
      fila <- df[selected_row(), ]
      file.copy(file.path(dir_storage, fila$Archivo_Real), file)
    }
  )
}

shinyApp(ui, server)