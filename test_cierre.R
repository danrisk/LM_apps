
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
                        PWD      = "[E2ST}=r",
                        Port     = 1433)

SYSIP <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 17 for SQL Server",
                        Server   = "172.30.149.67",
                        Database = "Sis2000",
                        UID      = "dmorales",
                        PWD      = "lamundial*2025*morales",
                        Port     = 1433)


cuentas <- tbl(PROFIT, "SCCUENTA") |> 
  collect()

saldos <- tbl(PROFIT, "SCREN_CO") |> 
  filter(fec_emis >= as.Date("2026-01-14"),
         fec_emis <= as.Date("2026-01-20")) |> 
  collect()


Contabilidad <- left_join(saldos, cuentas, by = "co_cue")

Contabilidad_inicial <- Contabilidad |> 
  filter(fec_emis == as.Date("2026-01-15")) |>
  mutate(saldo_inicial = monto_d - monto_h) |>
  select(co_cue, des_cue, fec_emis, descri, monto_d, monto_h, saldo_inicial)

Contabilidad_final <- Contabilidad |> 
  filter(fec_emis == as.Date("2026-01-16")) |>
  mutate(saldo_final = monto_d - monto_h) |>
  select(co_cue, saldo_final)

Contabilidad_preliminar <- left_join(Contabilidad_inicial, Contabilidad_final, by = "co_cue", relationship = "many-to-many")

Contabilidad_trabajada <- Contabilidad_preliminar |>
  group_by(co_cue) |>
  summarise(saldo_inicial = sum(saldo_inicial),
            saldo_final = sum(saldo_final))


prima_bruta <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-15")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Prima Bruta` = sum(abs(saldo))) |> 
  drop_na(ramo)


comisiones <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-15")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=Comisiones -\\s).*")) |>
  filter(ramo != "Bancarios", 
         ramo != "Sociedades de Corretaje",
         ramo != "Corredores de Seguros") |>
  group_by(ramo) |> 
  summarise(Comisiones = sum(abs(saldo))) |> 
  drop_na(ramo)

prima_com <- full_join(prima_bruta, comisiones, by = "ramo")

tabla_mapeo <- tribble(
  ~ramo_original,              ~ramo_estandar,
  "Acc Pers Colectivo",        "Accidentes Personales Colectivo",
  "Acc Pers Colec",            "Accidentes Personales Colectivo",
  "ACCIDENTES PERSONALES COLECTIVOS", "Accidentes Personales Colectivo",
  "Acc Pers Individual",       "Accidentes Personales Individual",
  "ACCIDENTES PERSONALES",     "Accidentes Personales Individual",
  "Automovil Colectivo o Flota", "Automóviles",
  "Automovil Individual",        "Automóviles",
  "Automóvil Individual",        "Automóviles",
  "Vida Indiv - Renovación",   "Vida Individual",
  "Vida Indiv Renovación",     "Vida Individual",
  "RCV Individual",            "Responsabilidad Civil Vehículos",
  "Resp. Civil General",       "Responsabilidad Civil General",
  "Resp. Civil Empresarial",   "Responsabilidad Civil Empresarial",
  "Funerarios Individual",     "Servicios Funerarios",
  "Funerarios Colectivo",      "Servicios Funerarios"
)


homologar_ramos <- function(df_datos, diccionario) {
  # Intentamos primero por coincidencia exacta
  df_limpio <- df_datos %>%
    left_join(diccionario, by = c("ramo" = "ramo_original")) %>%
    mutate(ramo_final = coalesce(ramo_estandar, ramo)) # Si no hay match, deja el original
  
  return(df_limpio)
}

prima_h <- homologar_ramos(prima_com, tabla_mapeo) |>
  mutate(`Prima Bruta` = replace_na(`Prima Bruta`, 0),
         Comisiones = replace_na(Comisiones, 0)) |>
select(ramo_final, `Prima Bruta`, Comisiones)


prima <- prima_h |>
  group_by(ramo_final) |>
  summarise(`Prima Bruta` = sum(`Prima Bruta`),
            Comisiones = sum(Comisiones))



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


