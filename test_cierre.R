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




cuentas <- tbl(SYSIP, "CBREPORTE_PAGO") |>
  # filter(fingreso == as.Date("2026-01-10")) |>
  collect()


cuentas1 <- tbl(SYSIP, "CBDOCCOB_M") |>
  # filter(fingreso == as.Date("2026-01-10")) |>
  collect()

cuentas <- tbl(PROFIT, "SCCUENTA") |> 
  collect()

saldos <- tbl(PROFIT, "SCREN_CO") |> 
  filter(as.Date(fec_emis) >= "2026-01-01",
         as.Date(fec_emis) <= "2026-01-25") |> 
  show_query() |>
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
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-25")) |>
  mutate(saldo = monto_d - monto_h) |>
  select(co_cue, des_cue, fec_emis, descri, monto_d, monto_h, saldo)

Contabilidad_preliminar <- left_join(Contabilidad_inicial, Contabilidad_final, by = "co_cue", relationship = "many-to-many")

Contabilidad_trabajada <- Contabilidad_preliminar |>
  group_by(co_cue) |>
  summarise(saldo_inicial = sum(saldo_inicial),
            saldo_final = sum(saldo_final))


prima_bruta <- Contabilidad_Consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-25")) |> 
  mutate(Ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
  drop_na(Ramo)
  

comisiones <- Contabilidad_Consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-25")) |> 
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



Recibos_SYSIP <- tbl(SYSIP, "ADRECIBOS") |> 
  filter(
    fcobro >= "2026-01-01",
    fcobro <= "2026-01-31",
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
  
Rcv <- tbl(SYSIP, "adpolcob") |>
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
    fcobro >= "2026-01-01",
    fcobro <= "2026-01-31",
    iestadorec == "C") |> 
  collect()

maramos <- tbl(SYSIP, "MARAMOS") |> 
  collect()

Recibos_ramos <- Recibos_SYSIP |> 
  left_join(maramos, by ="cramo")

Recibos_detallado <- Recibos_ramos |> 
  select(cnpoliza, xdescripcion_l, femision, fdesde_pol, fhasta_pol, ctenedor, 
         cnrecibo, fdesde, fhasta, fcobro, cmoneda, ptasamon_pago, msumabruta, 
         msumabrutaext, mprimabruta, mprimabrutaext,pcomision, mcomision, 
         mcomisionext, mpcedida, mpcedidaext, mpfp, mpfpext, mpret, mpretext) |> 
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
         "Prima Cedida en Reaseguro SYSIP" = mpcedida,
         "Prima Cedida Moneda Extranjera SYSIP"= mpcedidaext,
         "Prima Cedida Facultativo SYSIP" = mpfp,
         "Prima Cedida Facultativo Moneda Extranjera SYSIP" = mpfpext,
         "Prima Retenida SYSIP" = mpret,
         "Prima Retenida Moneda Extranjera SYSIP" = mpretext)|>
         mutate(Ramo = str_trim(Ramo),
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
    
   
   
   RCV <- left_join(Recibos_detallado, Recibos_POL, by = c("Nro de Recibo" = "cnrecibo"))
   
   rcv <- RCV |>
     mutate(
     Ramo2 = case_when(
     Ramo == "AUTOMOVIL" & ccober == "1" ~ "AUTOMOVIL",
     Ramo == "AUTOMOVIL" & ccober == "2" ~ "AUTOMOVIL",
     TRUE ~ "Responsabilidad Civil Vehículos"),
     Ramo = ifelse(Ramo == "AUTOMOVIL" & Ramo2 == "Responsabilidad Civil Vehículos", Ramo2, Ramo)
   )
   
   