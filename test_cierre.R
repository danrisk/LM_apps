


agent <- 
  create_agent(
    tbl = small_table,
    tbl_name = "small_table",
    lang = "es",
    label = "VALID-I Example No. 1"
  ) %>%
  col_is_posix(date_time) %>%
  col_vals_in_set(f, set = c("low", "mid", "high")) %>%
  col_vals_lt(a, value = 10) %>%
  col_vals_regex(b, regex = "^[0-9]-[a-z]{3}-[0-9]{3}$") %>%
  col_vals_between(d, left = 0, right = 5000) %>%
  interrogate()


  agent$validation_set
export_report(scan_data(small_table, lang = "es")  )
