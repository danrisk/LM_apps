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
library(waiter)

options(scipen = 999)


PROFIT <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "192.168.8.14",
                         Database = "CMUNDIAL",
                         UID      = "danny2",
                         PWD      = "ReadyLove100*",
                         Port     = 1433)

PROFIT_25 <- DBI::dbConnect(odbc::odbc(),
                         Driver   = "ODBC Driver 17 for SQL Server",
                         Server   = "192.168.8.14",
                         Database = "CLAMUND",
                         UID      = "danny2",
                         PWD      = "ReadyLove100*",
                         Port     = 1433)


SYSIP <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "valentin",
                        PWD      = "4GnZAwfSvxMxrkID",
                        Port     = 1433)

SYSIP <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "dmorales",
                        PWD      = "lamundial*2025*morales",
                        Port     = 1433)


cuentas <- tbl(PROFIT_25, "SCCUENTA") |> 
  collect()

saldos <- tbl(PROFIT_25, "SCREN_CO") |> 
  filter(as.Date(fec_emis) >= "2025-01-01",
         as.Date(fec_emis) <= "2025-12-31") |> 
  collect()


Contabilidad <- left_join(saldos, cuentas, by = "co_cue")

Contabilidad_inicial <- Contabilidad |> 
  filter(fec_emis == as.Date("2026-01-25")) |>
  mutate(saldo_inicial = monto_d - monto_h) |>
  select(co_cue, des_cue, fec_emis, descri, monto_d, monto_h, saldo_inicial)

Contabilidad_final <- Contabilidad |> 
  filter(fec_emis == as.Date("2026-01-25")) |>
  mutate(saldo_final = monto_d - monto_h) |>
  select(co_cue, saldo_final)

Contabilidad_Consolidada <- Contabilidad |> 
  # filter(fec_emis >= as.Date("2026-01-01"),
  #        fec_emis <= as.Date("2026-01-31")) |>
  mutate(saldo = monto_d - monto_h) |>
  select(co_cue, des_cue, fec_emis, descri, monto_d, monto_h, saldo)

Contabilidad_preliminar <- left_join(Contabilidad_inicial, Contabilidad_final, by = "co_cue", relationship = "many-to-many")

Contabilidad_trabajada <- Contabilidad_preliminar |>
  group_by(co_cue) |>
  summarise(saldo_inicial = sum(saldo_inicial),
            saldo_final = sum(saldo_final))


prima_bruta <- Contabilidad_Consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-31")) |> 
  mutate(Ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
  drop_na(Ramo) |>
  group_by(Ramo) |>
  summarise(saldo = sum(saldo))
  

comisiones <- Contabilidad_Consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-31")) |> 
  mutate(Ramo = str_extract(des_cue, "(?<=Comisiones -\\s).*")) |>
  filter(Ramo != "Bancarios", 
         Ramo != "Sociedades de Corretaje",
         Ramo != "Corredores de Seguros") |>
  drop_na(Ramo)

prima_com <- full_join(prima_bruta, comisiones, by = "Ramo")

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


homologar_ramos <- function(df_datos, diccionario) {
  # Intentamos primero por coincidencia exacta
  df_limpio <- df_datos %>%
    left_join(diccionario, by = c("Ramo" = "ramo_original")) 
  # |>
  #   mutate(ramo_final = coalesce(ramo_estandar, ramo)) # Si no hay match, deja el original
  return(df_limpio)
}

prima_h <- homologar_ramos(prima_com, tabla_mapeo) |>
  mutate(`Prima Bruta` = replace_na(`Prima Bruta`, 0),
         Comisiones = replace_na(Comisiones, 0)) |>
  filter(ramo_estandar != "Sociedades de Corretaje ") |>
select(Ramo = ramo_estandar, `Prima Bruta`, Comisiones)


prima_contable <- prima_h |>
  group_by(Ramo) |>
  summarise(`Prima Bruta` = sum(`Prima Bruta`),
            Comisiones = sum(Comisiones))


Recibos_tmp <- tbl(SYSIP, "ADRECIBOS") |> 
  filter( as.Date(fcobro) == "2026-01-31") |>
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
         ramo = xdescripcion_l,
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
  mutate(ramo = str_trim(ramo),
         `Nro de Recibo` = str_trim(`Nro de Recibo`)) |>
  group_by(ramo) |> 
  summarise(`Prima Bruta` = sum(`Prima Bruta`),
            `Monto de Comisión` = sum(`Monto de Comisión`))

prima_tecnica_h <- homologar_ramos(Recibos_detalle, tabla_mapeo) |>
  mutate(`Prima Bruta` = replace_na(`Prima Bruta`, 0),
         `Monto de Comisión` = replace_na(`Monto de Comisión`, 0)) |>
  select(Ramo = ramo_estandar, `Prima Bruta`, `Monto de Comisión`)

prima_tecnica <- prima_tecnica_h |>
  group_by(Ramo) |>
  summarise(`Prima Bruta` = sum(`Prima Bruta`),
            `Monto de Comisión` = sum(`Monto de Comisión`))


Prima_definitiva <- full_join(prima_contable, prima_tecnica, by = "Ramo")

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
    Diferencia_primas            = `Prima Bruta Contable` - `Prima Bruta Tecnica`
  )
  
POLCOB <- tbl(SYSIP, "ODSRECIBO") |>
  filter(fanopol == "2026",
         fmespol == 1) |>
  # filter(
  #   fcobro >= "2026-01-01",
  #   fcobro <= "2026-01-25",
  #   iestadorec == "C") |> 
  collect()


coberturas <- tbl(SYSIP, "MACOBERTURAS") |>
  collect()


res <- left_join(Rcv, coberturas, by = c("ccober" = "ccobertura")) |>
  distinct(crecibo, .keep_all = TRUE)

RCV <- Rcv |>
  mutate(cnpoliza = str_trim(cnpoliza)) |>
  distinct(cnpoliza, .keep_all = TRUE)

ramo_rcv <- left_join(Rcv, RCV, by = "cnpoliza") |>
  distinct(cnpoliza, .keep_all = TRUE) |>
  filter(tipo_de_ramo.x == "Automovil",
         cobertura.x == "RCV") |>
  select(cnpoliza, cobertura.x)


def <- left_join(Recibos_detalle, ramo_rcv, by = c("Nº de Póliza" = "cnpoliza")) |>
  mutate(ramo = str_trim(ramo)) |>
  filter(ramo == "AUTOMOVIL")

def1 <- def |>
  mutate(ramo2 = ifelse(cobertura.x =="RCV", "Responsabilidad Civil de Vehículos", "Casco"))




db_path <- "registro_documentos.db"

con <- dbConnect(SQLite(), db_path)
dbGetQuery(con, "SELECT * FROM usuarios")
dbExecute(con, "INSERT INTO usuarios (user, pass) VALUES ('master', 'c1037729.'),('chernandez','4ctu4314l')")


####prima para reserva




Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
  filter(
    as.Date(fcobro) >= "2026-01-01",
    as.Date(fcobro) <= "2026-01-31",
    iestadorec == "C") |> 
  collect()

maramos <- tbl(SYSIP, "MARAMOS") |> 
  collect()

Recibos_ramos <- Recibos_SYSIP |> 
  left_join(maramos, by ="cramo")

Recibos_detallado <- Recibos_ramos |> 
  select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
         cnrecibo, crecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, 
         msumabrutaext, mprimabruta, mprimabrutaext,pcomision, mcomision, 
         mcomisionext, mpcedida, mpcedidaext, mpfp, mpfpext, mpret, mpretext) |> 
  rename("Nº de Póliza" = cnpoliza,
         Ramo = xdescripcion_l,
         "Fecha de Emision Recibo" = femision,
         "Fecha desde Póliza" = fdesde_pol,
         "Fecha Hasta Póliza" = fhasta_pol,
         "Cédula Tomador" = ctenedor,
         "Nro de Recibo" = cnrecibo,
         "Codigo Recibo" = crecibo,
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
         "Prima Cedida en Reaseguro SYSIP" = mpcedida,
         "Prima Cedida Moneda Extranjera SYSIP"= mpcedidaext,
         "Prima Cedida Facultativo SYSIP" = mpfp,
         "Prima Cedida Facultativo Moneda Extranjera SYSIP" = mpfpext,
         "Prima Retenida SYSIP" = mpret,
         "Prima Retenida Moneda Extranjera SYSIP" = mpretext)|>
         mutate(Ramo = str_trim(Ramo),
                `Codigo Recibo` = str_trim(as.character(`Codigo Recibo`)),
                `Nro de Recibo` = str_trim(`Nro de Recibo`))

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
    `Prima Cedida` = `% de Cesion` * `Prima Bruta`) |>
  group_by(Ramo) |>
  summarise(
    `Prima Bruta` = sum(`Prima Bruta`),
    `Prima Cedida en Reaseguro SYSIP` = sum(`Prima Cedida en Reaseguro SYSIP`),
    `Prima Cedida` = sum(`Prima Cedida`),
    Diferencia =  `Prima Cedida en Reaseguro SYSIP` -  `Prima Cedida`
  )

RRC <- recibos_re |> 
  mutate(`Fecha desde Recibo`= as.Date(`Fecha desde Recibo`),
         `Fecha hasta Recibo` = as.Date(`Fecha hasta Recibo`),
         `Fecha de Cobro` = as.Date(`Fecha de Cobro`),
         ANIO = year(`Fecha de Cobro`),
         Mes = month(`Fecha de Cobro`, label = TRUE),
         prima_neta = as.numeric(`Prima Bruta`) - as.numeric(`Monto de Comisión`),
         fecha_evaluacion = as.Date("2026-01-31"),
         dias_por_transcurrir = case_when(
           as.numeric(`Fecha hasta Recibo`) <= fecha_evaluacion ~ 0,
           as.numeric(`Fecha desde Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`),
           as.numeric(`Fecha hasta Recibo`) > fecha_evaluacion ~ as.numeric(`Fecha hasta Recibo`) - as.numeric(fecha_evaluacion),
           TRUE ~ 0),
         proporcion_RRC = as.numeric(dias_por_transcurrir) / (as.numeric(`Fecha hasta Recibo`) - as.numeric(`Fecha desde Recibo`)),
         reserva_de_riesgo_en_curso = as.numeric(proporcion_RRC) * as.numeric(`Prima Bruta`),
         proporcion_RRC = replace_na(proporcion_RRC, 0),
         reserva_de_riesgo_en_curso = replace_na(reserva_de_riesgo_en_curso, 0),
        # prima_cedida = ifelse(as.numeric(`Prima Bruta`) * 0.8 < 0, 0,as.numeric(`Prima Bruta`) * 0.8),
         rrc_reaseguro = as.numeric(proporcion_RRC) * `Prima Cedida`,
         prima_retenida = as.numeric(`Prima Bruta`) - as.numeric(`Prima Cedida`),
         rrc_retenida = as.numeric(reserva_de_riesgo_en_curso) - as.numeric(rrc_reaseguro),
         rrc_reaseguro = replace_na(rrc_reaseguro, 0),
         prima_retenida = replace_na(prima_retenida, 0),
         rrc_retenida = replace_na(rrc_retenida, 0)
        # ,
        # prima_cedida = replace_na(prima_cedida, 0)
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

  
  
  
  Recibos_plan <- left_join(Recibos_detalle, recibos_ods_filter, by = c("Nro de Recibo"="cnrecibo"))
  
    
    recibos_plan_filter <- Recibos_plan |>
    mutate(ramo = str_trim(ramo),
           xplan = str_trim(xplan)) |>
     filter(xplan %in% c("Plan Básico RCV", "Plan Básico RCV Particulares", "Plan Básico RCV Motos")) |>
    mutate(ramo = case_when(
      ramo == "AUTOMOVIL" & str_detect(xplan, "Plan Básico RCV") ~ "Responsabilidad Civil Vehículos",
      ramo == "AUTOMOVIL" & str_detect(xplan, "Plan Básico RCV Particulares") ~ "Responsabilidad Civil Vehículos",
      ramo == "AUTOMOVIL" & str_detect(xplan, "Plan Básico RCV Motos") ~ "Responsabilidad Civil Vehículos",
      TRUE ~ ramo
    ))
   
    Recibos_POL <- tbl(SYSIP, "ADPOLTAR") |> 
      select(cnrecibo, ccober) |>
      mutate(cnrecibo = str_trim(cnrecibo),
             ccober = str_trim(ccober)) |>
      distinct(cnrecibo, .keep_all = TRUE) |>
      collect()
    
    Recibos_COB <- tbl(SYSIP, "ADPOLCOB") |> 
      select(crecibo, ccober) |>
      mutate(crecibo = str_trim(crecibo),
             ccober = str_trim(ccober)) |>
      distinct(crecibo, .keep_all = TRUE) |>
      collect()
    
    macober <- tbl(SYSIP, "MACOBERTURAS") |>
      collect()
    
    
    validador_con <- left_join(Recibos_COB, macober, by = c("ccober"= "ccobertura"))
    
    RCV2 <- left_join(Recibos_detallado, Recibos_COB, by = c("Codigo Recibo" = "crecibo"))
   
   RCV <- left_join(Recibos_detallado, Recibos_POL, by = c("Nro de Recibo" = "cnrecibo"))
   
   rcv <- RCV |>
     mutate(
     Ramo2 = case_when(
     Ramo == "AUTOMOVIL" & ccober == "1" ~ "AUTOMOVIL",
     Ramo == "AUTOMOVIL" & ccober == "2" ~ "AUTOMOVIL",
     Ramo == "AUTOMOVIL" & ccober == "6" ~ "AUTOMOVIL",
     Ramo == "AUTOMOVIL" & ccober == "10" ~ "AUTOMOVIL",
     # Ramo == "AUTOMOVIL" & ccober == "15" ~ "AUTOMOVIL",
     TRUE ~ "Responsabilidad Civil Vehículos"),
     Ramo = ifelse(Ramo == "AUTOMOVIL" & Ramo2 == "Responsabilidad Civil Vehículos", Ramo2, Ramo)
   )
   
   
   yess <- rcv |>
     select(`Nro de Recibo`, `Fecha desde Recibo`, `Fecha hasta Recibo`, `Prima Bruta`, `Monto de Comisión`, Ramo, `Fecha de Cobro`)
   
   write.xlsx(yess, "yess.xlsx", overwrite = TRUE)
   
   Comisiones_ADSOLPG <- tbl(SYSIP, "ADSOLPG") |> 
     mutate(csolpag = str_trim(csolpag),
            crecibo = str_trim(crecibo)) |>
     collect()
   
   Comisiones_ADMOVCOM <- tbl(SYSIP, "ADMOVCOM") |> 
     mutate(csolpag = str_trim(csolpag),
            cnrecibo = str_trim(cnrecibo)) |>
     collect()

   Comisiones_ADSOLPG |>
     group_by(cramo) |>
     summarise(comision = sum(mpagosol))
   
   resultado <-  inner_join(Comisiones_ADMOVCOM, Comisiones_ADSOLPG , by = "csolpag") |>
     mutate(isolpag = str_trim(isolpag),
            istatsol = str_trim(istatsol),
            cnrecibo = str_trim(cnrecibo)) |>
     filter(imovcom == "CO", 
            istatsol == "C",
            isolpag == "CO",
            isolpag != "SIN",
            as.Date(fmovim) >= "2026-01-01",
            as.Date(fmovim) <= "2026-01-31") |>
     select(cnrecibo, mpagosol)
     
   res <- resultado |> 
     mutate(isolpag = str_trim(isolpag),
            istatsol = str_trim(istatsol),
            cnrecibo = str_trim(cnrecibo)) |>
     filter(isolpag == "CO", istatsol == "C") |>
     mutate(cnrecibo = str_trim(cnrecibo)) |>
     select(
       "Nro de Recibo" = cnrecibo,
       "Codigo Pago" = csolpag,
       "Codigo Moneda" = cmoneda_1,
       "Fecha Movimiento Comision" = fmovcom,
       "Fecha Movimiento" = fmovim,
       "Moneda" = cmoneda.y,
       "Monto Pagado" = mpagosol, 
       "Monto Pagado Moneda Extranjera" = mpagosolext, 
       "Tasa de Cambio" = ptasamon.y, 
       "Codigo Productor" = cproductor.x, 
       "Cedula Beneficiario" = cid_ben, 
       "Nombre Beneficiario" = xbeneficiario, 
       "Concepto" = xconcepto_1
     )

   
   det_com <- left_join(Recibos_detallado, resultado, by = c("Nro de Recibo" = "cnrecibo")) |>
     mutate(mpagosol = replace_na(mpagosol, 0))
   
   det_com_ramo <- det_com |> 
     group_by(Ramo) |>
     summarise(Comision = sum(mpagosol),
               `Monto de Comisión`= sum(`Monto de Comisión`)) |>
     mutate(comision_definitiva = ifelse(Ramo == "AUTOMOVIL", `Monto de Comisión`, Comision)) |>
     adorn_totals("row", fill = "-", na.rm = TRUE, name = "TOTAL GENERAL")
   
   com <- resultado |>
     group_by(Ramo) |>
     summarise(Comision = sum(mpago),
               `Monto de Comisión`=sum(`Monto de Comisión`))
   
   
   
   
   
  rc <- Recibos_Consolidados |>
     distinct(Ramo) 
   
  
  data_congelada <- tbl(SYSIP, "ODSRECIBO") |>
    filter(as.Date(fproceso) == "2025-12-31",
           iestadorec == "C") |>
    collect()

  
  data_congelada |>
    filter(iestadorec == "C")
  
  
  colores_LM <- list(
    primary   = "#162a7f",
    secondary = "#ff6675",
    tertiary  = "#acacac",
    success   = "#091133",
    danger    = "#ff6675",
    font      = "Poppins"
  )
   
  
  LM_CLEAN <- LM_analytics |>
    mutate(fecha = as.Date(paste(año, mes, "01", sep = "-"))) |>
    clean_names()

  
  plot_ly(LM_CLEAN, x = ~fecha) |>
    add_lines(y = ~primas_netas_cobradas_1, name = "Primas", line = list(color = colores_LM$primary)) |>
    add_lines(y = ~siniestros_totales_2_3_5, name = "Siniestros", line = list(color = colores_LM$secondary)) |>
    layout(yaxis = list(title = "Monto (Miles de Bs.)"), hovermode = "x unified")
  
  
 LM_CLEAN |>
    select(ano, mes, gastos_de_administracion_vs_primas_netas_cobradas_percent_6, tasa_combinada_percent_8) |>
    tail(12) |>
    datatable(options = list(dom = 't', pageLength = 12)) |>
    formatStyle('gastos_de_administracion_vs_primas_netas_cobradas_percent_6', 
                backgroundColor = styleInterval(35, c('white', '#ff6675'))) 
 
 plot_ly(LM_CLEAN, x = ~fecha) |>
   add_lines(y = ~gastos_de_administracion_vs_primas_netas_cobradas_percent_6, name = "Gasto Administrativo", line = list(color = colores_LM$primary)) |>
   add_lines(y = ~gastos_de_adquisicion_vs_primas_netas_cobradas_percent_5 , name = "Gastos de Adquisicion", line = list(color = colores_LM$secondary)) |>
   add_lines(y = ~comisiones_vs_primas_netas_cobradas_percent_4 , name = "Comisiones", line = list(color = colores_LM$tertiary)) |>
   layout(yaxis = list(title = "Monto (Miles de Bs.)"), hovermode = "x unified")
 
 plot_ly(LM_CLEAN, x = ~fecha) |>
   add_lines(y = ~siniestros_pagados_vs_primas_netas_cobradas_percent_1, name = "Siniestros Pagados", line = list(color = colores_LM$primary)) |>
   add_lines(y = ~reservas_para_prestaciones_y_siniestros_pendientes_brutas_vs_primas_netas_cobradas_percent_2, name = "Reserva Siniestros Pendientes", line = list(color = colores_LM$secondary)) |>
   layout(yaxis = list(title = "Porcentaje (%)"), hovermode = "x unified")
 
 
 
 # Procesamiento de indicadores por año
 data_anual <- LM_CLEAN |>
   group_by(ano) |>
   summarise(
     Siniestralidad = mean(siniestros_incurridos_vs_prima_devengada_percent_3, na.rm = TRUE),
     Comisiones = mean(comisiones_vs_primas_netas_cobradas_percent_4, na.rm = TRUE),
     Adquisicion = mean(gastos_de_adquisicion_vs_primas_netas_cobradas_percent_5, na.rm = TRUE),
     Administracion = mean(gastos_de_administracion_vs_primas_netas_cobradas_percent_6, na.rm = TRUE),
     Reaseguro = mean(costo_del_reaseguro_vs_prima_devengada_percent_7, na.rm = TRUE),
     Tasa_Combinada = mean(tasa_combinada_percent_8, na.rm = TRUE),
     Cobertura = mean(indice_de_cobertura_de_reservas_9, na.rm = TRUE)
   )

 
 output$plot_composicion_tc <- renderPlotly({
   plot_ly(data_anual, x = ~as.factor(ano)) |>
     add_bars(y = ~Siniestralidad, name = "Siniestralidad", marker = list(color = "#162a7f")) |>
     add_bars(y = ~Comisiones, name = "Comisiones", marker = list(color = "#ff6675")) |>
     add_bars(y = ~Adquisicion, name = "Gatos Adquisición", marker = list(color = "#acacac")) |>
     add_bars(y = ~Administracion, name = "Gastos Adm.", marker = list(color = "#091133")) |>
     add_bars(y = ~Reaseguro, name = "Costo Reaseguro", marker = list(color = "#5cb85c")) |>
     layout(
       barmode = 'relative', # Permite valores negativos (reaseguro)
       title = "<b>Composición de la Tasa Combinada por Año</b>",
       xaxis = list(title = "Año"),
       yaxis = list(title = "Porcentaje (%)", ticksuffix = "%"),
       font = list(family = "Poppins"),
       legend = list(orientation = 'h', y = -0.2)
     )
 })  
 
 
 data_anual |>
   datatable(
     colnames = c("Año", "Siniestralidad (%)", "Comisiones (%)", "Gastos Adq. (%)", 
                  "Gastos Adm. (%)", "Reaseguro (%)", "Tasa Combinada (%)", "Cobertura (x)"),
     options = list(dom = 't', ordering = FALSE),
     rownames = FALSE
   ) |>
   formatRound(columns = 2:8, digits = 2) |>
   formatStyle(
     'Tasa_Combinada',
     backgroundColor = styleInterval(100, c('rgba(40, 167, 69, 0.2)', 'rgba(220, 53, 69, 0.2)')),
     fontWeight = 'bold'
   )
 
  
  