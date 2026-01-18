library(shiny)
library(DT)
library(dplyr)
library(RSQLite)
library(DBI)
library(bslib)
library(DiagrammeR)
library(shinyjs)
library(openxlsx)
library(readr)
library(janitor)
library(scales)
library(tidyverse)

# --- Configuración de Base de Datos y Almacenamiento ---
dir_storage <- "archivos_pdf"
if (!dir.exists(dir_storage)) dir.create(dir_storage)

db_path <- "registro_documentos.db"
con <- dbConnect(SQLite(), db_path)

clave_diaria <- read.xlsx("clave_diaria.xlsx")


# Configuración global de idioma para tablas
traduccion_es <- list(
  processing = "Procesando...",
  search = "Buscar:",
  lengthMenu = "Mostrar _MENU_ registros",
  info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
  infoEmpty = "Mostrando registros del 0 al 0 de un total de 0 registros",
  infoFiltered = "(filtrado de un total de _MAX_ registros)",
  loadingRecords = "Cargando...",
  zeroRecords = "No se encontraron resultados",
  emptyTable = "Ningún dato disponible en esta tabla"
)

dbExecute(con, "CREATE TABLE IF NOT EXISTS usuarios (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 user TEXT UNIQUE,
                 pass TEXT)")


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
  
  uiOutput("body_ui")
)

server <- function(input, output, session) {
  
  # Variable reactiva para controlar el acceso
  logged_in <- reactiveVal(FALSE)
  user_data <- reactiveVal(NULL)
  
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
            h4("Balance Preliminar"),
            tabsetPanel(
              tabPanel("Carga",
                       br(),
                       fileInput("file_input", "Seleccione xlsx", accept = c(".csv",".xlsx"), buttonLabel = "Buscar archivo...", placeholder = "Ningún archivo seleccionado"),
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
              tabPanel("Contabilidad Preliminar", 
                       DTOutput("tabla_docs")
                       ),
              tabPanel("Validación de Formato", 
                       DTOutput("validacion_formato")
                       ),
              tabPanel("Validación de Fondo", 
                       br(),
                       DTOutput("control")
                      
              ),
              tabPanel("Descarga archivo definitivo", 
                       downloadButton("descargar_txt", "Descargar Archivo .txt")
                       )
            )
          )
        ),
        
        tags$footer(
          class = "footer-custom",
          tags$p(paste0("© ", format(Sys.Date(), "%Y"), " - Todos los derechos reservados"), style = "align-items: center; justify-content: center;"),
          tags$p("Gerencia Corporativa Actuarial y Reaseguros | Sistema de Automatización SEFA | Versión 1.0 PoC")
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

  # Valor reactivo para refrescar la UI
  trigger_update <- reactiveVal(0)

  datos_db <- reactive({
  req(input$file_input)
  trigger_update()
  read.xlsx(input$file_input$datapath)
  })
  
    
    balance_filtrado <- reactive({
      
      # 1. Procesamiento inicial de los datos cargados
      datos <- datos_db() |> 
        clean_names() |> 
        mutate(fecha = as.character("09-01-2026"),
               saldo_inicial = abs(as.numeric(saldo_inicial)),
               debe = abs(as.numeric(debe)),
               haber = abs(as.numeric(haber)),
               # Formateo inicial para cálculos
               calc_saldo = saldo_inicial + debe - haber)
      
      # 2. Unión con la clave diaria
      df_base <- left_join(clave_diaria, datos, by = c("clave" = "codigo")) |> 
        drop_na(saldo_inicial)
      
      # 3. Cálculo de la Diferencia (Activo - Pasivo/Capital)
      # Nota: Usamos la lógica de tus reactivos 'activo' y 'pasivo_capital'
      # val_activo <- df_base |> 
      #   filter(str_starts(as.character(clave), "^220[1-9]")) |>
      #   summarise(total = sum(as.numeric(saldo_inicial)) + sum(as.numeric(debe)) - sum(as.numeric(haber))) |> 
      #   pull(total)
      # 
      # val_pasivo_cap <- df_base |> 
      #   filter(str_starts(as.character(clave), "^(440[1-9]|4410)")) |>
      #   summarise(total = sum(as.numeric(saldo_inicial)) + sum(as.numeric(haber)) - sum(as.numeric(debe))) |> 
      #   pull(total)
      
      val_egreso <- df_base |> 
        filter(str_starts(as.character(clave), "^(3301|3321|3331|3341|3381|3395)")) |>
        summarise(total = sum(as.numeric(saldo_inicial)) + sum(as.numeric(debe)) - sum(as.numeric(haber))) |> 
        pull(total)
      
      val_ingreso <- df_base |> 
        filter(str_starts(as.character(clave), "^(5501|5521|5531|5541|5581|5595)")) |>
        summarise(total = sum(as.numeric(saldo_inicial)) + sum(as.numeric(haber)) - sum(as.numeric(debe))) |> 
        pull(total)
      
      
      
      
      diferencia_val <- val_ingreso - val_egreso
      
      # 4. Condición: Si la diferencia es menor a cero, agregar filas
      if (!is.na(diferencia_val) && diferencia_val < 0) {
        # Valor absoluto de la diferencia para el ajuste
        ajuste <- abs(diferencia_val)

        filas_ajuste <- data.frame(
          clave = c("44090403", "559502"),
          fecha = rep("09-01-2026", 2),
          saldo_inicial = c("0.00", "0.00"),
          debe = c(number(ajuste, accuracy = 0.01, decimal.mark = ".", big.mark = ""), "0.00"),
          haber = c("0.00", number(ajuste, accuracy = 0.01, decimal.mark = ".", big.mark = ""))
        )

        df_base <- df_base |>
          select(clave, fecha, saldo_inicial, debe, haber) |>
          mutate(across(c(saldo_inicial, debe, haber),
                        ~number(as.numeric(.), accuracy = 0.01, decimal.mark = ".", big.mark = ""))) |>
          bind_rows(filas_ajuste)
      } else {
        ajuste <- diferencia_val

        filas_ajuste <- data.frame(
          clave = c("44090302", "339502"),
          fecha = rep("09-01-2026", 2),
          saldo_inicial = c("0.00", "0.00"),
          debe = c("0.00", number(ajuste, accuracy = 0.01, decimal.mark = ".", big.mark = "")),
          haber = c(number(ajuste, accuracy = 0.01, decimal.mark = ".", big.mark = ""), "0.00")
        )

        df_base <- df_base |>
          select(clave, fecha, saldo_inicial, debe, haber) |>
          mutate(across(c(saldo_inicial, debe, haber),
                        ~number(as.numeric(.), accuracy = 0.01, decimal.mark = ".", big.mark = ""))) |>
          bind_rows(filas_ajuste)
      }

      return(df_base)
    })
  
  activo <- reactive({
    
    balance_filtrado() |> 
      filter(str_starts(as.character(clave), "^220[1-9]")) |>
      summarise(`Total Activo` = sum(as.numeric(saldo_inicial)) + sum(as.numeric(debe)) - sum(as.numeric(haber)))
      
    
  })
  
  
  pasivo_capital <- reactive({
    
    balance_filtrado() |> 
      # group_by(clave) |> 
      filter(str_starts(as.character(clave), "^(440[1-9]|4410)")) |>
      summarise(`Total Pasivo + Capital` = sum(as.numeric(saldo_inicial)) + sum(as.numeric(haber)) - sum(as.numeric(debe)))
    
  })
  
  
  egreso <- reactive({
    
    balance_filtrado() |> 
      filter(str_starts(as.character(clave), "^(3301|3321|3331|3341|3381|3395)")) |> 
      summarise(`Total Egresos` = sum(as.numeric(saldo_inicial)) + sum(as.numeric(debe)) - sum(as.numeric(haber)))
    
    
    
  })
  
  
  ingreso <- reactive({
    
    balance_filtrado() |>
      # group_by(clave) |> 
      filter(str_starts(as.character(clave), "^(5501|5521|5531|5541|5581|5595)")) |>
      summarise(`Total Ingresos` = sum(as.numeric(saldo_inicial)) + sum(as.numeric(haber)) - sum(as.numeric(debe)))
    
    
    
  })
  
  cuadro_control <- reactive({
    
    
    df <- cbind(activo(), pasivo_capital(), ingreso(), egreso())
    df <- df |>
      mutate(Diferencia = `Total Activo` - `Total Pasivo + Capital`,
             Diferencia_EI = ingreso() - egreso(),
             Diferencia_amp = Diferencia - Diferencia_EI) |>
      relocate(Diferencia, .after = `Total Pasivo + Capital`)

    datatable(df, options = list(language =list(url = traduccion_es),
                                 dom = 't',
                                 ordering = FALSE)) |>
      formatStyle(c('Diferencia', 'Diferencia_EI'), color = styleInterval(0, c('red','green'))) |>
      formatCurrency(columns = c('Total Activo', 'Total Pasivo + Capital', 'Diferencia', 'Total Ingresos', 'Total Egresos', 'Diferencia_EI', 'Diferencia_amp'),
                     currency = "Bs. ",
                     interval = 3,
                     mark = ".",
                     dec.mark = ",",
                     digits = 2)

    
    
   
  })
  
  output$descargar_txt <- downloadHandler(
    filename = function() {
      # Nombre del archivo con la fecha actual
      paste("datos-extraidos-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Escribir el contenido en el archivo temporal 'file'
      # Usamos write_tsv o write_delim para formato TXT
      write_delim(balance_filtrado(), file, delim = ";")
    }
  )
 
#   # Renderizar Tabla
  output$tabla_docs <- renderDT({
  datatable(datos_db(), selection = 'single', options = list(pageLength = 10,
                                                             dom = 'tp',
                                                             ordering = FALSE,
                                                               language = list(traduccion_es))) |> 
      formatCurrency(columns = c('Saldo_inicial', 'Debe', 'Haber', 'Saldo_final'), 
                     currency = "Bs. ", 
                     interval = 3, 
                     mark = "", 
                     digits = 2)
    
    
    
    
    
  })
  
  
  output$validacion_formato <- renderDT({
  
    datatable(balance_filtrado(), selection = 'single', options = list(pageLength = 10,
                                                                       dom = 'tp',
                                                                       ordering = FALSE,
                                                                       language = list(traduccion_es)))
  
   })
  
  
  output$control <- renderDT({
         
    cuadro_control()
   
  })
  
}

shinyApp(ui, server)