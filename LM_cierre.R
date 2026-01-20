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
library(pointblank)




PROFIT <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "192.168.8.14",
                         Database = "CMUNDIAL",
                         UID      = "danny2",
                         PWD      = "ReadyLove100*",
                         Port     = 1433)


SYSIP <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "dmorales",
                        PWD      = "lamundial*2025*morales",
                        Port     = 1433)

# --- Configuración de Base de Datos y Almacenamiento ---
dir_storage <- "archivos_pdf"
if (!dir.exists(dir_storage)) dir.create(dir_storage)

db_path <- "registro_documentos.db"
con <- dbConnect(SQLite(), db_path)


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
              tabPanel("DataOps (Auditoría Calidad de Datos)", 
                       uiOutput("dataops")
              ),
              tabPanel("Primas/ Comisiones / Devoluciones", 
                       DTOutput("primas_SYSIP"),
                       br(),
                       DTOutput("primas_PROFIT")
              ),
              tabPanel("Siniestros", 
                       DTOutput("siniestros")
              ),
              tabPanel("Reaseguro", 
                       DTOutput("reaseguro")
              ),
              tabPanel("Reservas de Riesgo en Curso", 
                       DTOutput("rrc")
              ),
              tabPanel("Reserva Contingente de Fianzas", 
                       DTOutput("contingencia_f")
              ),
              tabPanel("Reserva Riesgo Catastróficos", 
                       DTOutput("catastrofico")
              ),
              tabPanel("Reserva de Siniestros / IBNR", 
                       DTOutput("reserva_siniestros")
              ),
              tabPanel("Reserva de Insuficiencia de Primas", 
                       DTOutput("rcip")
              ),
              tabPanel("Anexo 17 / 19 SUDEASEG", 
                       DTOutput("anexos")
              ),
              tabPanel("Balance Preliminar", 
                       DTOutput("balance")
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
          tags$p("Gerencia Corporativa Actuarial y Reaseguros | Sistema de Cierre Orquestado | Versión 1.0 PoC")
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
    read.xlsx(input$file_input$datapath)
    trigger_update()
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
  
  
  
  dataops_source <- reactive({
    
    
    small_table
    
  })
  
  
  primas_SYSIP <- reactive({
  
    
    Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
      filter(
        fcobro >= "2026-01-01",
        fcobro <= "2026-01-15",
        iestadorec == "C") |> 
      collect()
    
    maramos <- tbl(SYSIP, "MARAMOS") |> 
      collect()
    
    Recibos_ramos <- Recibos_SYSIP |> 
      left_join(maramos, by ="cramo")
    
    Recibos_detalle <- Recibos_ramos |> 
      select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
             cnrecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, msumabrutaext, mprimabruta, mprimabrutaext,
             pcomision, mcomision, mcomisionext, mpcedida, mpcedidaext, mpret, mpretext, mpfp, mpfpext) |> 
      rename("Nº de Póliza" = cnpoliza,
             Ramo = xdescripcion_l,
             "Fecha de Emision Recibo" = femision,
             "Fecha desde Póliza" = fdesde_pol,
             "Fecha Hasta Póliza" = fhasta_pol,
             "Cédula Tomador" = ctenedor,
             "Nro de Recibo" = cnrecibo,
             "Fecha desde Recibo" = fdesde,
             "Fecha hasta Recibo" = fhasta,
             "Fecha de Cobro" = fcobro,
             Moneda = cmoneda,
             "Tasa de Cambio" = ptasamon_pago,
             "Suma Asegurada" = msumabruta,
             "Suma Asegurada Moneda Extranjera" = msumabrutaext,
             "Prima Bruta" = mprimabruta,
             "Prima Bruta Moneda Extranjera" = mprimabrutaext,
             "Porcentaje de Comisión" = pcomision,
             "Monto de Comisión" = mcomision,
             "Monto Comision Extranjera" = mcomisionext,
             "Prima Cedida en Reaseguro" = mpcedida,
             "Prima Cedida Moneda Extranjera"= mpcedidaext,
             "Prima Cedida Facultativo" = mpfp,
             "Prima Cedida Facultativo Moneda Extranjera" = mpfpext,
             "Prima Retenida" = mpret,
             "Prima Retenida Moneda Extranjera" = mpretext) %>% 
      group_by(Ramo) %>% 
      summarise(`Prima Bruta` = sum(`Prima Bruta`),
                `Monto de Comisión` = sum(`Monto de Comisión`))
    
    Recibos_detalle
    
  })
  
  
  primas_PROFIT <- reactive({
    
    
    cuentas <- tbl(PROFIT, "SCCUENTA") |> 
      collect()
    
    saldos <- tbl(PROFIT, "SCREN_CO") |> 
      filter(fec_emis >= as.Date("2026-01-01"),
             fec_emis <= as.Date("2026-01-15")) |> 
      collect()
    
    
    Contabilidad <- left_join(saldos, cuentas, by = "co_cue")
    
    Contabilidad_consolidada <- Contabilidad |> 
      mutate(saldo = monto_d - monto_h,
             nro_recibo = str_extract(descri, "(?<=Nro_Recibo\\s|RECIBO\\s)[0-9-]+")) |> 
      select(co_cue, des_cue, nro_recibo, fec_emis, descri, monto_d, monto_h, saldo)
    
    prima_bruta <- Contabilidad_consolidada |>
      filter(fec_emis >= as.Date("2026-01-01"),
             fec_emis <= as.Date("2026-01-15")) |> 
      mutate(Ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
      group_by(Ramo) |> 
      summarise(`Prima Bruta` = sum(abs(saldo))) |> 
      drop_na(Ramo)
    
   prima_bruta
    
  })
  
  
  rrc <- reactive({
    
    Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
    filter(
      fcobro >= "2026-01-01",
      fcobro <= "2026-01-15",
      iestadorec == "C") |> 
    collect()
  
  maramos <- tbl(SYSIP, "MARAMOS") |> 
    collect()
  
  Recibos_ramos <- Recibos_SYSIP |> 
    left_join(maramos, by ="cramo")
  
  Recibos_detalle <- Recibos_ramos |> 
    select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
           cnrecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, msumabrutaext, mprimabruta, mprimabrutaext,
           pcomision, mcomision, mcomisionext, mpcedida, mpcedidaext, mpret, mpretext, mpfp, mpfpext) |> 
    rename("Nº de Póliza" = cnpoliza,
           Ramo = xdescripcion_l,
           "Fecha de Emision Recibo" = femision,
           "Fecha desde Póliza" = fdesde_pol,
           "Fecha Hasta Póliza" = fhasta_pol,
           "Cédula Tomador" = ctenedor,
           "Nro de Recibo" = cnrecibo,
           "Fecha desde Recibo" = fdesde,
           "Fecha hasta Recibo" = fhasta,
           "Fecha de Cobro" = fcobro,
           Moneda = cmoneda,
           "Tasa de Cambio" = ptasamon_pago,
           "Suma Asegurada" = msumabruta,
           "Suma Asegurada Moneda Extranjera" = msumabrutaext,
           "Prima Bruta" = mprimabruta,
           "Prima Bruta Moneda Extranjera" = mprimabrutaext,
           "Porcentaje de Comisión" = pcomision,
           "Monto de Comisión" = mcomision,
           "Monto Comision Extranjera" = mcomisionext,
           "Prima Cedida en Reaseguro" = mpcedida,
           "Prima Cedida Moneda Extranjera"= mpcedidaext,
           "Prima Cedida Facultativo" = mpfp,
           "Prima Cedida Facultativo Moneda Extranjera" = mpfpext,
           "Prima Retenida" = mpret,
           "Prima Retenida Moneda Extranjera" = mpretext)
  
  
  RRC <- Recibos_detalle |> 
    mutate(`Fecha desde Recibo`= as.Date(`Fecha desde Recibo`),
           `Fecha hasta Recibo` = as.Date(`Fecha hasta Recibo`),
           `Fecha de Cobro` = as.Date(`Fecha de Cobro`),
           ANIO = year(`Fecha de Cobro`),
           Mes = month(`Fecha de Cobro`, label = TRUE),
           prima_neta = as.numeric(`Prima Bruta`) - as.numeric(`Monto de Comisión`),
           fecha_evaluacion = as.Date("2026-01-15"),
           dias_por_transcurrir = case_when(
             as.numeric(`Fecha hasta Recibo`) <= fecha_evaluacion ~ 0,
             as.numeric(`Fecha desde Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`),
             as.numeric(`Fecha hasta Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(fecha_evaluacion),
             TRUE ~ 0),
           proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`)),
           reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(prima_neta),
           proporcion_RRC = replace_na(proporcion_RRC, 0),
           reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0),
           prima_cedida = ifelse(as.numeric(prima_neta) * 0.8 < 0, 0,as.numeric(prima_neta) * 0.8),
           rrc_reaseguro = as.numeric(proporcion_RRC) * prima_cedida,
           prima_retenida = as.numeric(prima_neta) - as.numeric(prima_cedida),
           rrc_retenida = as.numeric(reserva_de_riesgo_en_curso) - as.numeric(rrc_reaseguro),
           rrc_reaseguro = replace_na(rrc_reaseguro, 0),
           prima_retenida = replace_na(prima_retenida, 0),
           rrc_retenida = replace_na(rrc_retenida, 0),
           prima_cedida = replace_na(prima_cedida, 0)
    )
  
  
  RRC_RAMO <- RRC |> 
    group_by(Ramo) |> 
    summarise(Prima = sum(`Prima Bruta`),
              `Reserva de Riesgo en Curso Totales` = sum(reserva_de_riesgo_en_curso),
              `Prima Cedida` = sum(prima_cedida),
              `RRC Reaseguradores` = sum(rrc_reaseguro),
              `Prima Retenida` = sum(prima_retenida),
              `RRC Retenida` = sum(rrc_retenida)
    ) %>% 
    mutate(`Prima Cedida` = replace_na(`Prima Cedida`, 0))
  
  RRC_RAMO
  
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
                                                               paging = FALSE,
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
                                                                       paging = FALSE,
                                                                       language = list(traduccion_es)),
              )
    
  })
  
  
  output$control <- renderDT({
    
    cuadro_control()
    
  })
  
  
  
  output$dataops <- renderUI({
    
    dataops_source()
    
    # scan_data(dataops_source(), 
    #           lang = "es",
    #           sections = "OVM")
    
  })
  
  output$primas_SYSIP <- renderDT({
    
   
    
    datatable(primas_SYSIP(), rownames = FALSE, options = list(language =list(url = traduccion_es),
                                 dom = 't',
                                 ordering = FALSE,
                                 paging = FALSE)) |>
      # formatStyle(c('Diferencia', 'Diferencia_EI'), color = styleInterval(0, c('red','green'))) |> 
      formatCurrency(columns = c('Prima Bruta', 'Monto de Comisión'), 
                     currency = "Bs. ", 
                     interval = 3, 
                     mark = ".", 
                     dec.mark = ",",
                     digits = 2)
    
    
  })
  
  output$primas_PROFIT <- renderDT({
    
    
    datatable(primas_PROFIT(), rownames = FALSE, options = list(language =list(url = traduccion_es),
                                             dom = 't',
                                             ordering = FALSE,
                                             paging = FALSE)) |>
      # formatStyle(c('Diferencia', 'Diferencia_EI'), color = styleInterval(0, c('red','green'))) |> 
      formatCurrency(columns = c('Prima Bruta'), 
                     currency = "Bs. ", 
                     interval = 3, 
                     mark = ".", 
                     dec.mark = ",",
                     digits = 2)
    
  })
  
  output$siniestros <- renderDT({
    
    dataops_source()
    
  })
  
  output$reaseguro <- renderDT({
    
    dataops_source()
    
  })

  output$rrc <- renderDT({
    
    datatable(rrc(), rownames = FALSE, options = list(language =list(url = traduccion_es),
                                              dom = 't',
                                              ordering = FALSE,
                                              paging = FALSE)) |>
      # formatStyle(c('Diferencia', 'Diferencia_EI'), color = styleInterval(0, c('red','green'))) |> 
      formatCurrency(columns = c('Prima', 'Reserva de Riesgo en Curso Totales', 'Prima Cedida','RRC Reaseguradores', 'Prima Retenida', 'RRC Retenida'), 
                     currency = "Bs. ", 
                     interval = 3, 
                     mark = ".", 
                     dec.mark = ",",
                     digits = 2)
    
    
  })
  
  output$contingencia_f <- renderDT({
    
    dataops_source()
    
  })
  
  output$catastrofico <- renderDT({
    
    dataops_source()
    
  })
  
  output$reserva_siniestros <- renderDT({
    
    dataops_source()
    
  })
  
  output$rcip <- renderDT({
    
    dataops_source()
    
  })
  
  output$anexos <- renderDT({
    
    dataops_source()
    
  })
  
  output$balances <- renderDT({
    
    dataops_source()
    
  })
}

shinyApp(ui, server)