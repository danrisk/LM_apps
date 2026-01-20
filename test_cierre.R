
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


cuentas <- tbl(PROFIT, "SCCUENTA") |> 
  collect()

saldos <- tbl(PROFIT, "SCREN_CO") |> 
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-15")) |> 
  collect()


Contabilidad <- left_join(saldos, cuentas, by = "co_cue")

Contabilidad_consolidada <- Contabilidad |> 
  mutate(saldo = monto_d - monto_h,
         nro_recibo = str_extract(descri, "(?<=Nro_Recibo\\s|RECIBO\\s)[0-9-]+"),
         Comisiones = str_extract(des_cue, "(?<=Comisiones -\\s).*")) |> 
  select(co_cue, des_cue, nro_recibo, fec_emis, descri, monto_d, monto_h, saldo, Comisiones)

prima_bruta <- Contabilidad_consolidada |>
  filter(fec_emis >= as.Date("2026-01-01"),
         fec_emis <= as.Date("2026-01-15")) |> 
  mutate(ramo = str_extract(des_cue, "(?<=PRIMAS COBRADAS -\\s|Prima Cobrada -\\s).*")) |>
  group_by(ramo) |> 
  summarise(`Prima Bruta` = sum(abs(saldo))) |> 
  drop_na(ramo)


analisis <- scan_data(as.data.frame(Contabilidad))
