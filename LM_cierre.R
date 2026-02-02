library(shiny)
library(DT)
library(dplyr)
library(RSQLite)
library(DBI)
library(odbc)
library(pool)
library(bslib)
library(DiagrammeR)
library(shinyjs)
library(openxlsx)
library(readr)
library(janitor)
library(scales)
library(tidyverse)
library(pointblank)

options(scipen = 999)


PROFIT <- dbPool(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "192.168.8.14",
                         Database = "CMUNDIAL",
                         UID      = "danny2",
                         PWD      = "ReadyLove100*",
                         Port     = 1433,
                 TrustServerCertificate = "yes")


SYSIP <- dbPool(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "valentin",
                        PWD      = "4GnZAwfSvxMxrkID",
                        Port     = 1433,
                TrustServerCertificate = "yes")


onStop(function() {
  poolClose(PROFIT)
})

onStop(function() {
  poolClose(SYSIP)
})

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


homologar_ramos <- function(df_datos, diccionario) {
  # Intentamos primero por coincidencia exacta
  df_limpio <- df_datos %>%
    left_join(diccionario, by = c("Ramo" = "ramo_original")) 
  # |>
  #   mutate(ramo_final = coalesce(ramo_estandar, ramo)) # Si no hay match, deja el original
  return(df_limpio)
}


tabla_mapeo <- tribble(
  ~ramo_original,              ~ramo_estandar,
  "Acc Pers Colectivo",        "Accidentes Personales Colectivo",
  "Acc Pers Colec",            "Accidentes Personales Colectivo",
  "ACCIDENTES PERSONALES COLECTIVOS", "Accidentes Personales Colectivo",
  "MICROSEGUROS COMBINADO DE PERSONAS 4IN1", "Accidentes Personales Individual",
  "Acc Pers Individual",       "Accidentes Personales Individual",
  "MICROSEGUROS DE ACCIDENTES PERSONALES",  "Accidentes Personales Individual",
  "ACCIDENTES PERSONALES",     "Accidentes Personales Individual",
  "Automovil Colectivo o Flota", "Automóviles",
  "Automovil Individual",        "Automóviles",
  "Automóvil Individual",        "Automóviles",
  "AUTOMOVIL",                   "Automóviles",
  "AVIACION",                       "Aviación",
  "AVIACIÓN",                       "Aviación",
  "Aeronaves",                      "Aviación",
  "INCENDIO",                       "Incendio",
  "Incendio",                       "Incendio",
  "NAVES",                          "Naves",
  "Naves",                          "Naves",
  "Vida Indiv - Renovación",   "Vida Individual",
  "Vida Indiv Renovación",     "Vida Individual",
  "VIDA INDIVIDUAL",           "Vida Individual",
  "Vida Colectivo",     "Vida Colectivo",
  "VIDA COLECTIVO",     "Vida Colectivo",
  "RCV Individual",            "Responsabilidad Civil Vehículos",
  "Responsabilidad Civil Vehículos",  "Responsabilidad Civil Vehículos",
  "Resp. Civil General",       "Responsabilidad Civil General",
  "RESPONSABILIDAD CIVIL GENERAL", "Responsabilidad Civil General",
  "Resp. Civil Empresarial",   "Responsabilidad Civil Empresarial",
  "R.C. PROFESIONAL MÉDICOS Y ODONTOLÓGOS", "Responsabilidad Civil Empresarial",
  "Resp. Civil Profesional", "Responsabilidad Civil Empresarial",
  "RESPONSABILIDAD CIVIL EMPRESARIAL", "Responsabilidad Civil Empresarial",
  "Resp. Civil Patronal", "Responsabilidad Civil Empresarial",
  "RESPONSABILIDAD CIVIL PATRONAL", "Responsabilidad Civil Empresarial",
  "Funerarios Individual",     "Servicios Funerarios",
  "Funerarios Colectivo",      "Servicios Funerarios",
  "GASTOS FUNERARIOS", "Servicios Funerarios",
  "GASTOS FUNERARIOS COLECTIVO", "Servicios Funerarios",
  "Funerarios Colectivo",  "Servicios Funerarios",
  "Funerarios Individual", "Servicios Funerarios",
  "PÓLIZA DE SEGURO MASIVO DE GASTOS FUNERARIO INDIVIDUAL", "Servicios Funerarios",
  "COMBINADO FAMILIAR", "Combinado",
  "COMBINADO RESIDENCIAL", "Combinado",
  "COMBINADOS EMPRESARIAL", "Combinado",
  "Combinados", "Combinado",
  "RIESGOS ESPECIALES",   "Riesgo Diversos",
  "Otros Riesgos Diversos", "Riesgo Diversos",
  "FIANZAS", "Fianzas",
  "FIANZA", "Fianzas",
  "Fianzas", "Fianzas",
  "TODO RIESGO INDUSTRIAL", "Todo Riesgo Industrial",
  "Ramos Técnicos", "Todo Riesgo Industrial",
  "TRANSPORTE TERRESTRE", "Transporte",
  "Transporte",       "Transporte",
  "SALUD", "Hospitalización Individual",
  "Salud Colectivo", "Hospitalización Colectiva",
  "Salud Individual", "Hospitalización Individual",
  "SALUD COLECTIVO", "Hospitalización Colectiva",
  "SEGUROS DE CRÉDITOS", "Seguros de Crédito"
)

# -------------------------------------------------------------------------




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
        
            tabsetPanel(
              tabPanel("Fecha de Cierre", 
                       dateRangeInput("f_cierre", "Introduzca la fecha a declarar:",
                                 start = "2026-01-01",
                                 end = "2026-01-31",
                                 format = "dd/mm/yyyy",
                                 language = "es"),
              ),
              tabPanel("Primas/ Comisiones / Devoluciones", 
                       DTOutput("primas")
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
              tabPanel("Siniestros", 
                       verbatimTextOutput("siniestros")
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
  
  
  
  dataops_source <- reactive({
    #small_table
    format(input$f_cierre[2], "%Y-%m-%d")
  })
  
  
  primas_SYSIP <- reactive({
    req(input$f_cierre)
    f1 <- format(input$f_cierre[1], "%Y-%m-%d")
    f2 <- format(input$f_cierre[2], "%Y-%m-%d")
    
    Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
      filter(
        fcobro >= f1,
        fcobro <= f2,
        iestadorec == "C") |> 
      collect()
    
    maramos <- tbl(SYSIP, "MARAMOS") |> 
      collect()
    
    Recibos_ramos <- Recibos_SYSIP |> 
      left_join(maramos, by ="cramo") |>
      mutate(cnrecibo = str_trim(cnrecibo))
    
    Recibos_POL <- tbl(SYSIP, "ADPOLTAR") |> 
      select(cnrecibo, ccober) |>
      mutate(cnrecibo = str_trim(cnrecibo),
             ccober = str_trim(ccober)) |>
      distinct(cnrecibo, .keep_all = TRUE) |>
      collect()
    
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
             "Prima Retenida Moneda Extranjera" = mpretext) |>
      mutate(Ramo = str_trim(Ramo),
             `Nro de Recibo` = str_trim(`Nro de Recibo`))
      
      RCV <- left_join(Recibos_detalle, Recibos_POL, by = c("Nro de Recibo" = "cnrecibo"))
      
      Recibos_ramos_plan <- RCV |>
      mutate(
        Ramo2 = case_when(
          Ramo == "AUTOMOVIL" & ccober == "1" ~ "AUTOMOVIL",
          Ramo == "AUTOMOVIL" & ccober == "2" ~ "AUTOMOVIL",
          TRUE ~ "Responsabilidad Civil Vehículos"),
        Ramo = ifelse(Ramo == "AUTOMOVIL" & Ramo2 == "Responsabilidad Civil Vehículos", Ramo2, Ramo)
      ) |>
      group_by(Ramo) |> 
      summarise(`Prima Bruta` = sum(`Prima Bruta`),
                `Monto de Comisión` = sum(`Monto de Comisión`))
    
    prima_tecnica_h <- homologar_ramos(Recibos_ramos_plan, tabla_mapeo) |>
      mutate(`Prima Bruta` = replace_na(`Prima Bruta`, 0),
             `Monto de Comisión` = replace_na(`Monto de Comisión`, 0)) |>
      select(Ramo = ramo_estandar, `Prima Bruta`, `Monto de Comisión`)
    
    prima_tecnica <- prima_tecnica_h |>
      group_by(Ramo) |>
      summarise(`Prima Bruta` = sum(`Prima Bruta`),
                `Monto de Comisión` = sum(`Monto de Comisión`))
    
   prima_tecnica
    
  })
  
  
  primas_PROFIT <- reactive({
    req(input$f_cierre)
    f1 <- input$f_cierre[1]
    f2 <- input$f_cierre[2]
    
    cuentas <- tbl(PROFIT, "SCCUENTA") |> 
      collect()
    
    saldos <- tbl(PROFIT, "SCREN_CO") |> 
      filter(as.Date(fec_emis) >= f1,
             as.Date(fec_emis) <= f2) |> 
      collect()
    
    
    Contabilidad <- left_join(saldos, cuentas, by = "co_cue")
    
    Contabilidad_consolidada <- Contabilidad |> 
      mutate(saldo = monto_d - monto_h,
             nro_recibo = str_extract(descri, "(?<=Nro_Recibo\\s|RECIBO\\s)[0-9-]+")) |> 
      select(co_cue, des_cue, nro_recibo, fec_emis, descri, monto_d, monto_h, saldo)
    
    prima_bruta <- Contabilidad_consolidada |>
      filter(fec_emis >= f1,
             fec_emis <= f2) |> 
      mutate(Ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
      group_by(Ramo) |> 
      summarise(`Prima Bruta` = sum(abs(saldo))) |> 
      drop_na(Ramo)
    
    
    comisiones <- Contabilidad_consolidada |>
      filter(fec_emis >= f1,
             fec_emis <= f2) |> 
      mutate(Ramo = str_extract(des_cue, "(?<=Comisiones -\\s).*")) |>
      filter(Ramo != "Bancarios", 
             Ramo != "Sociedades de Corretaje",
             Ramo != "Corredores de Seguros") |>
      group_by(Ramo) |> 
      summarise(Comisiones = sum(abs(saldo))) |> 
      drop_na(Ramo)
    
    prima_com <- full_join(prima_bruta, comisiones, by = "Ramo")
    
    prima_h <- homologar_ramos(prima_com, tabla_mapeo) |>
      mutate(`Prima Bruta` = replace_na(`Prima Bruta`, 0),
             Comisiones = replace_na(Comisiones, 0)) |>
      filter(ramo_estandar != "Sociedades de Corretaje ") |>
      select(Ramo = ramo_estandar, `Prima Bruta`, Comisiones)
    
    
    prima_contable <- prima_h |>
      group_by(Ramo) |>
      summarise(`Prima Bruta` = sum(`Prima Bruta`),
                Comisiones = sum(Comisiones))
    
    prima_contable
    
  })
  
  
  prima_DEFINITIVA <- reactive({
    
    Prima_definitiva <- full_join(primas_PROFIT(), primas_SYSIP(), by = "Ramo")
    
    Prima_definitiva <- Prima_definitiva |>
      rename(
        "Prima Bruta Contable"       = `Prima Bruta.x`,
        "Monto de Comisión Contable" = Comisiones,
        "Prima Bruta Tecnica"        = `Prima Bruta.y`,
        "Monto de Comisión Tecnica"  = `Monto de Comisión` # <-- Se cerró la tilde correctamente
      ) |>
      mutate(
        `Prima Bruta Contable`       = replace_na(`Prima Bruta Contable`, 0), # <-- Nombre corregido
        `Monto de Comisión Contable` = replace_na(`Monto de Comisión Contable`, 0),
        `Prima Bruta Tecnica`        = replace_na(`Prima Bruta Tecnica`, 0), # <-- Nombre corregido
        `Monto de Comisión Tecnica`  = replace_na(`Monto de Comisión Tecnica`, 0),
        `Diferencia Primas`          = `Prima Bruta Contable` - `Prima Bruta Tecnica`,
        `Diferencia Comisiones`      = `Monto de Comisión Contable` - `Monto de Comisión Tecnica`
      )
    
    Prima_definitiva
    
  })
  
  
  rrc <- reactive({
   
    req(input$f_cierre)
    f1 <- format(input$f_cierre[1], "%Y-%m-%d")
    f2 <- format(input$f_cierre[2], "%Y-%m-%d")
    
    Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
      filter(
        fcobro >= f1,
        fcobro <= f2,
        iestadorec == "C") |> 
      collect()
    
    maramos <- tbl(SYSIP, "MARAMOS") |> 
      collect()
    
    Recibos_ramos <- Recibos_SYSIP |> 
      left_join(maramos, by ="cramo") |>
      mutate(cnrecibo = str_trim(cnrecibo))
    
    Recibos_POL <- tbl(SYSIP, "ADPOLTAR") |> 
      select(cnrecibo, ccober) |>
      mutate(cnrecibo = str_trim(cnrecibo),
             ccober = str_trim(ccober)) |>
      distinct(cnrecibo, .keep_all = TRUE) |>
      collect()
    
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
             "Prima Retenida Moneda Extranjera" = mpretext) |>
      mutate(Ramo = str_trim(Ramo),
             `Nro de Recibo` = str_trim(`Nro de Recibo`))
    
    RCV <- left_join(Recibos_detalle, Recibos_POL, by = c("Nro de Recibo" = "cnrecibo"))
    
    Recibos_ramos_plan <- RCV |>
      mutate(
        Ramo2 = case_when(
          Ramo == "AUTOMOVIL" & ccober == "1" ~ "AUTOMOVIL",
          Ramo == "AUTOMOVIL" & ccober == "2" ~ "AUTOMOVIL",
          TRUE ~ "Responsabilidad Civil Vehículos"),
        Ramo = ifelse(Ramo == "AUTOMOVIL" & Ramo2 == "Responsabilidad Civil Vehículos", Ramo2, Ramo)
      )
    
    Recibo_detallado_h <- homologar_ramos(Recibos_ramos_plan, tabla_mapeo)
    
    Recibo_detallado_h <- Recibo_detallado_h |>
      mutate(Ramo = ramo_estandar)
    
    
    recibos_re <- Recibo_detallado_h |>
      mutate(`% de Cesion` = case_when(
        Ramo == "Incendio"                            ~ 0.80,
        Ramo == "Transporte"                          ~ 0.80,
        Ramo == "Combinado"                           ~ 0.80,
        Ramo == "Riesgo Diversos"                     ~ 0.80,
        Ramo == "Aviación"                            ~ 0.80,
        Ramo == "Naves"                               ~ 0.80,
        Ramo == "Todo Riesgo Industrial"              ~ 0.80,
        Ramo == "Responsabilidad Civil General"       ~ 0.80,
        Ramo == "Responsabilidad Civil Empresarial"   ~ 0.80,
        Ramo == "Responsabilidad Civil Vehículos"     ~ 0.70,
        Ramo == "Fianzas"                             ~ 0.45,
        Ramo == "Seguros de Crédito"                  ~ 0.55,
        Ramo == "Vida Individual"                     ~ 0.70,
        TRUE                                          ~ 0.00  
      ),
      `Prima Cedida` = `% de Cesion` * `Prima Bruta`)
    
    
   ########## transformar y unificar oon la prima profit
  
    RRC <- recibos_re |> 
      mutate(`Fecha desde Recibo`= as.Date(`Fecha desde Recibo`),
             `Fecha hasta Recibo` = as.Date(`Fecha hasta Recibo`),
             `Fecha de Cobro` = as.Date(`Fecha de Cobro`),
             ANIO = year(`Fecha de Cobro`),
             Mes = month(`Fecha de Cobro`, label = TRUE),
             prima_neta = as.numeric(`Prima Bruta`) - as.numeric(`Monto de Comisión`),
             fecha_evaluacion = as.Date(f2),
             dias_por_transcurrir = case_when(
               as.numeric(`Fecha hasta Recibo`) <= fecha_evaluacion ~ 0,
               as.numeric(`Fecha desde Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`),
               as.numeric(`Fecha hasta Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(fecha_evaluacion),
               TRUE ~ 0),
             proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`)),
             reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(`Prima Bruta`),
             proporcion_RRC = replace_na(proporcion_RRC, 0),
             reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0),
             rrc_reaseguro = as.numeric(proporcion_RRC) * `Prima Cedida`,
             prima_retenida = as.numeric(`Prima Bruta`) - as.numeric(`Prima Cedida`),
             rrc_retenida = as.numeric(reserva_de_riesgo_en_curso) - as.numeric(rrc_reaseguro),
             rrc_reaseguro = replace_na(rrc_reaseguro, 0),
             prima_retenida = replace_na(prima_retenida, 0),
             rrc_retenida = replace_na(rrc_retenida, 0)
      )
    
  
    RRC_RAMO <- RRC |> 
      group_by(Ramo) |> 
      summarise(Prima = sum(`Prima Bruta`),
                `Reserva de Riesgo en Curso Totales` = sum(reserva_de_riesgo_en_curso),
                `Prima Cedida` = sum(`Prima Cedida`),
                `RRC Reaseguradores` = sum(rrc_reaseguro),
                `Prima Retenida` = sum(prima_retenida),
                `RRC Retenida` = sum(rrc_retenida)
      )
  
    RRC_RAMO
  
  })
  
  
  bordereaux <- reactive({
    
    req(input$f_cierre)
    f1 <- format(input$f_cierre[1], "%Y-%m-%d")
    f2 <- format(input$f_cierre[2], "%Y-%m-%d")
    
    Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
      filter(
        fcobro >= f1,
        fcobro <= f2,
        iestadorec == "C") |> 
      collect()
    
    maramos <- tbl(SYSIP, "MARAMOS") |> 
      collect()
    
    Recibos_ramos <- Recibos_SYSIP |> 
      left_join(maramos, by ="cramo")
    
    Recibos_detallado <- Recibos_ramos |> 
      select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
             cnrecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, msumabrutaext, mprimabruta, mprimabrutaext,
             pcomision, mcomision, mcomisionext) |> 
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
             "Monto Comision Extranjera" = mcomisionext) |>
      mutate(Ramo = str_trim(Ramo))
    
    Recibo_detallado_h <- homologar_ramos(Recibos_detallado, tabla_mapeo)
    
    Recibo_detallado_h <- Recibo_detallado_h |>
      mutate(Ramo = ramo_estandar)
    
    
    recibos_re <- Recibo_detallado_h |>
      mutate(`% de Cesion` = case_when(
        Ramo == "Incendio"                            ~ 0.80,
        Ramo == "Transporte"                          ~ 0.80,
        Ramo == "Combinado"                           ~ 0.80,
        Ramo == "Riesgo Diversos"                     ~ 0.80,
        Ramo == "Aviación"                            ~ 0.80,
        Ramo == "Naves"                               ~ 0.80,
        Ramo == "Todo Riesgo Industrial"              ~ 0.80,
        Ramo == "Responsabilidad Civil General"       ~ 0.80,
        Ramo == "Responsabilidad Civil Empresarial"   ~ 0.80,
        Ramo == "Responsabilidad Civil Vehículos"     ~ 0.70,
        Ramo == "Fianzas"                             ~ 0.45,
        Ramo == "Seguros de Crédito"                  ~ 0.55,
        Ramo == "Vida Individual"                     ~ 0.70,
        TRUE                                          ~ 0.00  
      ),
      `Prima Cedida` = `% de Cesion` * `Prima Bruta`)
    
  
  })
  
  
  
  output$descargar_txt <- downloadHandler(
    filename = function() {
      # Nombre del archivo con la fecha actual
      paste("datos-extraidos-", format(input$f_cierre[2], "%Y-%m-%d"), ".csv", sep = "")
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
  
  
  output$primas <- renderDT({
    
   
    
    datatable(prima_DEFINITIVA(), class = 'cell-border hover', rownames = FALSE, options = list(language =list(url = traduccion_es),
                                 dom = 't',
                                 ordering = FALSE,
                                 paging = FALSE)) |>
      formatStyle(c('Diferencia Primas', 'Diferencia Comisiones'), 
                  color = styleInterval(c(-0.01, 0.01), c("#721c24", "#2c3e50", "#155724")),
                  backgroundColor = styleInterval(c(-0.01, 0.01), c("#ffcccc", "#ffffff", "#d4edda"))) |> 
      formatCurrency(columns = c('Prima Bruta Contable',
                                 'Monto de Comisión Contable', 
                                 'Prima Bruta Tecnica', 
                                 'Monto de Comisión Tecnica',
                                 'Diferencia Primas',
                                 'Diferencia Comisiones'), 
                     currency = "Bs. ", 
                     interval = 3, 
                     mark = ".", 
                     dec.mark = ",",
                     digits = 2)
    
    
  })
  
 
  output$siniestros <- renderText({
    dataops_source()
  
    
  })
  
  output$reaseguro <- renderDT({
    
    bordereaux()
    
  })

  output$rrc <- renderDT({
    
    datatable(rrc(), class = 'cell-border hover', rownames = FALSE, options = list(language =list(url = traduccion_es),
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
    
    primas_SYSIP()
    
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