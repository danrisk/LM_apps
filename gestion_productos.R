library(shiny)
library(reactable)
library(dplyr)
library(RSQLite)
library(DBI)
library(bslib)
library(DiagrammeR)
library(shinyjs)

# --- Configuración de Base de Datos ---
dir_storage <- "archivos_pdf"
if (!dir.exists(dir_storage)) dir.create(dir_storage)

db_path <- "registro_documentos.db"

init_db <- function() {
  con <- dbConnect(SQLite(), db_path)
  # Tabla Documentos
  dbExecute(con, "CREATE TABLE IF NOT EXISTS documentos (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 Nombre_documento TEXT, Ramo TEXT, Producto TEXT,
                 Documento_numero TEXT, Actuario_responsable TEXT,
                 Fecha_emision TEXT, Fecha_recepcion TEXT,
                 Categoria TEXT, Fecha_carga TEXT, Fecha_limite TEXT,
                 Archivo_Real TEXT, Estado TEXT DEFAULT 'Cargado')")
  
  # Tabla Usuarios
  dbExecute(con, "CREATE TABLE IF NOT EXISTS usuarios (
                 id INTEGER PRIMARY KEY AUTOINCREMENT, user TEXT UNIQUE, pass TEXT)")
  
  # Insertar admin por defecto
  if(dbGetQuery(con, "SELECT COUNT(*) as n FROM usuarios")$n == 0) {
    dbExecute(con, "INSERT INTO usuarios (user, pass) VALUES ('admin', '1234')")
  }
  dbDisconnect(con)
}
init_db()

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

# --- INTERFAZ DE USUARIO ---
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
  
  tags$head(
    tags$style(HTML("
      .login-bg { background: linear-gradient(rgba(0,0,0,0.6), rgba(0,0,0,0.6)), url('CENTRO-LIDO.png'); 
                  background-size: cover; height: 100vh; display: flex; align-items: center; justify-content: center; }
      .login-box { background: white; padding: 40px; border-radius: 15px; width: 350px; text-align: center; }
      .footer-custom { padding: 30px; margin-top: 50px; border-top: 1px solid #e0e0e0; text-align: center; color: #7f8c8d; }
      .status-badge { padding: 5px 10px; border-radius: 12px; font-weight: bold; font-size: 0.85em; }
    "))
  ),
  
  uiOutput("body_ui")
)

# --- SERVIDOR ---
server <- function(input, output, session) {
  
  logged_in <- reactiveVal(FALSE)
  trigger_update <- reactiveVal(0)
  selected_row <- reactiveVal(NULL)
  
  # Lógica de Autenticación
  observeEvent(input$login_btn, {
    con <- dbConnect(SQLite(), db_path)
    res <- dbGetQuery(con, "SELECT * FROM usuarios WHERE user = ? AND pass = ?", 
                      params = list(input$user, input$pass))
    dbDisconnect(con)
    
    if (nrow(res) > 0) {
      logged_in(TRUE)
    } else {
      output$error_msg <- renderUI({ p("Credenciales incorrectas", style="color:red; margin-top:10px;") })
    }
  })
  
  # Carga de Datos
  datos_db <- reactive({
    trigger_update()
    con <- dbConnect(SQLite(), db_path)
    df <- dbReadTable(con, "documentos")
    dbDisconnect(con)
    df
  })
  
  # --- Vistas de UI ---
  output$body_ui <- renderUI({
    if (!logged_in()) {
      # UI LOGIN
      div(class = "login-bg",
          div(class = "login-box",
              h3("Sistema Actuarial"),
              textInput("user", NULL, placeholder = "Usuario"),
              passwordInput("pass", NULL, placeholder = "Contraseña"),
              actionButton("login_btn", "Entrar", class = "btn-primary", width = "100%"),
              uiOutput("error_msg")
          )
      )
    } else {
      # UI PRINCIPAL
      layout_sidebar(
        sidebar = sidebar(
          title = "Control de Documentos",
          accordion(
            accordion_panel("Carga de Archivo",
                            fileInput("file_input", "PDF", accept = ".pdf"),
                            textInput("doc_name", "Nombre"),
                            textInput("ramo", "Ramo"),
                            selectInput("category", "Categoría", choices = c("Legajo", "Condicionado General", "Reglamento Actuarial", "Otros")),
                            dateInput("fecha_limite", "Fecha Límite"),
                            actionButton("save_btn", "Guardar", class = "btn-primary w-100")
            ),
            accordion_panel("Gestión",
                            uiOutput("status_ui"),
                            hr(),
                            uiOutput("action_buttons_ui")
            )
          )
        ),
        navset_card_underline(
          nav_panel("Seguimiento", reactableOutput("tabla_docs")),
          nav_panel("Mapa de Ruta", 
                    card(
                      card_header("Progreso Visual"),
                      DiagrammeROutput("interact_diagram", height = "400px")
                    )),
          nav_panel("Visor", uiOutput("pdf_viewer"))
        ),
        footer = div(class="footer-custom", "Gerencia Corporativa Actuarial | 2026")
      )
    }
  })
  
  # --- Lógica de Tabla (Reactable) ---
  output$tabla_docs <- renderReactable({
    data <- datos_db()
    reactable(
      data,
      selection = "single",
      onClick = "select",
      searchable = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      language = reactableLang(searchPlaceholder = "Buscar documento...", noData = "No hay registros"),
      columns = list(
        id = colDef(show = FALSE),
        Archivo_Real = colDef(show = FALSE),
        Estado = colDef(
          cell = function(value) {
            color <- status_colors[value]
            span(class = "status-badge", style = list(background = color), value)
          }
        )
      )
    )
  })
  
  # Capturar fila seleccionada en reactable
  observe({
    selected <- getReactableState("tabla_docs", "selected")
    selected_row(selected)
  })
  
  # --- Acciones (Guardar, Actualizar, Eliminar) ---
  observeEvent(input$save_btn, {
    req(input$file_input)
    file_name <- paste0(as.numeric(Sys.time()), "_", input$file_input$name)
    file.copy(input$file_input$datapath, file.path(dir_storage, file_name))
    
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "INSERT INTO documentos (Nombre_documento, Ramo, Categoria, Fecha_carga, Fecha_limite, Archivo_Real, Estado) VALUES (?, ?, ?, ?, ?, ?, 'Cargado')",
              params = list(input$doc_name, input$ramo, input$category, as.character(Sys.Date()), as.character(input$fecha_limite), file_name))
    dbDisconnect(con)
    
    trigger_update(trigger_update() + 1)
    showNotification("Guardado con éxito")
  })
  
  observeEvent(input$update_status_btn, {
    req(selected_row())
    df <- datos_db()
    id_doc <- df$id[selected_row()]
    
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "UPDATE documentos SET Estado = ? WHERE id = ?", params = list(input$new_status, id_doc))
    dbDisconnect(con)
    trigger_update(trigger_update() + 1)
  })
  
  # --- UI Dinámica de Controles ---
  output$status_ui <- renderUI({
    req(selected_row())
    df <- datos_db()
    current_status <- df$Estado[selected_row()]
    
    tagList(
      selectInput("new_status", "Cambiar Estado:", choices = names(status_colors), selected = current_status),
      actionButton("update_status_btn", "Actualizar", class="btn-info btn-sm w-100")
    )
  })
  
  output$action_buttons_ui <- renderUI({
    req(selected_row())
    tagList(
      downloadButton("download_file", "Descargar", class = "btn-success btn-sm w-100 mb-2"),
      actionButton("delete_btn", "Eliminar", class = "btn-danger btn-sm w-100")
    )
  })
  
  # --- Diagrama y Visor ---
  output$interact_diagram <- renderDiagrammeR({
    estado <- "Cargado"
    if (!is.null(selected_row())) {
      estado <- datos_db()$Estado[selected_row()]
    }
    
    # Lógica de resaltado simple para Mermaid
    style_active <- "fill:#2c3e50,color:#fff,stroke-width:4px"
    
    mermaid(paste0("
    graph TD
      A(Cargado) --> B(En Elaboración)
      B --> C(Aprobado Interno)
      C --> D(En SUDEASEG)
      D --> E(Aprobado Final)
      
      style ", if(estado == 'Cargado') "A" else if(estado == 'Aprobado') "E" else "B", " ", style_active
    ))
  })
  
  output$pdf_viewer <- renderUI({
    req(selected_row())
    fila <- datos_db()[selected_row(), ]
    addResourcePath("pdf_dir", dir_storage)
    tags$iframe(style = "height:700px; width:100%; border:none;", 
                src = paste0("pdf_dir/", fila$Archivo_Real))
  })
}

shinyApp(ui, server)