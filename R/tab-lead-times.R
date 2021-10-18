# tabela de lead times ---------------------------------------------------------
easypackages::libraries(c("tidyverse", "lubridate"))

year_mon <- function(dates){
  paste0(lubridate::month(dates, label = TRUE, abbr = TRUE),
         "-",
         lubridate::year(dates)
  )
}




pretty_lead_times <- function(ini_mon, nmonths = 12, yr_ref = 2021){
  # ini_mon = 11; nmonths = 12; yr_ref = 2000
  #cat(ini_mon, "\n")
  start_mon <- as.Date(paste0(yr_ref,"-", ini_mon, "-15"))
  #start_mon + lubridate::duration(12, "months")
  
  #lead1 <- start_mon + 30
  lead1 <- seq(start_mon, by = "month", length.out = 2)[2]
  
  lead_times <- seq(
    lead1,
    by = "month",
    length.out = nmonths
  )
  #lead_times <- c(start_mon, lead_times)
  year_mon(lead_times) %>%
    as_tibble() %>%
    mutate(S = year_mon(start_mon), 
           lead_time = 1:nmonths,
           dates = lead_times,
           dif = c(NA, diff(dates))) %>%
    relocate(S, lead_time) 
  
}



#pretty_lead_times(2)
#pretty_lead_times(11)

tab_lead_times <- map_df(1:12, ~pretty_lead_times(.x)) %>%
  pivot_wider(id_cols = -c(dates, dif),
              names_from = lead_time, 
              values_from = value, 
              names_prefix = "L"
  )

tab_lead_times %>%
  mutate(posto = 74,
         model = "PSF",
         k = NA,
         w = NA
  ) %>%
  relocate(posto, model, k, w) %>%
  knitr::kable()


  # | posto|model |k  |w  |S        |L1       |L2       |L3       |L4       |L5       |L6       |L7       |L8       |L9       |L10      |L11      |L12      |
  # |-----:|:-----|:--|:--|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|:--------|
  # |    74|PSF   |NA |NA |jan-2021 |fev-2021 |mar-2021 |abr-2021 |mai-2021 |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |
  # |    74|PSF   |NA |NA |fev-2021 |mar-2021 |abr-2021 |mai-2021 |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |
  # |    74|PSF   |NA |NA |mar-2021 |abr-2021 |mai-2021 |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |
  # |    74|PSF   |NA |NA |abr-2021 |mai-2021 |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |
  # |    74|PSF   |NA |NA |mai-2021 |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |
  # |    74|PSF   |NA |NA |jun-2021 |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |
  # |    74|PSF   |NA |NA |jul-2021 |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |
  # |    74|PSF   |NA |NA |ago-2021 |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |ago-2022 |
  # |    74|PSF   |NA |NA |set-2021 |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |ago-2022 |set-2022 |
  # |    74|PSF   |NA |NA |out-2021 |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |ago-2022 |set-2022 |out-2022 |
  # |    74|PSF   |NA |NA |nov-2021 |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |ago-2022 |set-2022 |out-2022 |nov-2022 |
  # |    74|PSF   |NA |NA |dez-2021 |jan-2022 |fev-2022 |mar-2022 |abr-2022 |mai-2022 |jun-2022 |jul-2022 |ago-2022 |set-2022 |out-2022 |nov-2022 |dez-2022 |
   
  
