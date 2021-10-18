prep_stn_data <- function(stn_data) {
  # stn_data <- qnat_posto
  #checkmate::assert_true(data.table::uniqueN(stn_data[["code_stn"]]) == 1)
  
  #dailyQ <- dplyr::select(stn_data, -code_stn) %>%
  dailyQ <- stn_data %>%
    dplyr::rename("discharge" = qnat) %>%
    dplyr::filter(!is.na(discharge)) %>%
    as.data.frame()
  
  x <- dailyQ %>%
    dplyr::mutate(
      year_val = EflowStats::get_waterYear(date, numeric = T),
      day = EflowStats::get_waterYearDay(date),
      leapYear = EflowStats:::is.leapyear(as.numeric(as.character(year_val)))
    )
  
  fullYearCheck <- dplyr::summarize(dplyr::group_by(x, year_val),
                                    completeYear =
                                      if (!any(leapYear)) {
                                        ifelse(length(day) == 365, T, F)
                                      } else if (any(leapYear)) {
                                        ifelse(length(day) == 366, T, F)
                                      }
  )
  
  incomplete_yrs <- fullYearCheck %>%
    dplyr::filter(!completeYear) %>%
    dplyr::pull(year_val)
  
  dailyQClean <- x %>%
    dplyr::filter(!year_val %in% incomplete_yrs)
  
  tibble::as_tibble(dailyQClean)
}





baseflow_index <- function(date_qdata) {
  # date_qdata = qnat_data %>% sel_station(.,station = 74)
  
  
  date_qdata <- date_qdata %>%
    dplyr::select(date, qnat) %>%
    dplyr::mutate(date = as.POSIXct(date, tz = "UTC")) %>%
    dplyr::rename("Date" = date, "Q" = qnat)

  hydrostats::baseflows(date_qdata, a = 0.975, ts = "mean")[["mean.bfi"]]
}
#qnat_data %>% sel_station(.,station = 74) %>% baseflow_index()


fdc.hydromad <- function(Q){
  
  pp <- qnorm(ppoints(NROW(Q)))
  space <- diff(pp[1:2])
  
  eval.prob <- pnorm(seq(min(pp), max(pp), by = space))
  fdc.q <- quantile(Q, 1 - eval.prob, na.rm = TRUE)
  
  fdc_tbl <- tibble(
    p_exc = readr::parse_number(names(fdc.q)),
    q = as.numeric(fdc.q)
  )
  fdc_tbl
}

# https://gitlab.irstea.fr/HYCAR-Hydro/baseflow
fdc.baseflow <- function(Q){
  
  # REQUER P e ET
  #library(baseflow)
  
  # Loading example data from airGR package
  data(L0123001, package = 'airGR')
  
  # Defining BasinData object
  
  Name <- BasinInfo$BasinName
  startDate <- BasinObs$DatesR[1]
  endDate <- BasinObs$DatesR[length(BasinObs$DatesR)]
  P <- BasinObs$P
  PET <- BasinObs$E
  Qobs <- BasinObs$Qmm
  
  BasinData_Example <- BasinData(Name, startDate, endDate, P, PET, Qobs, fill = "GR4J")
  
  # Creating BaseflowFilter object
  BaseflowFilter_Example <- BaseflowFilter(BasinData_Example, 1000, updateFunction = 'quadr')
  
  # Computing baseflow
  BaseflowFilter_Example <- perform_filtering(BaseflowFilter_Example)
  
  # Plotting computed separation
  plot(BaseflowFilter_Example, ylog = TRUE)
  
  # Computing baseflow index
  bfi(BaseflowFilter_Example)
  
}


fdc_sample <- function(Q, method = "hydromad") {
  
  switch (object,
          "hydromad" = fdc.hydromad(Q)
  )
  
  
  fdc_htsm <- hydroTSM::fdc(Q, hQ.thr = 0.6666, lQ.thr = 0.3333, plot = FALSE, )
}



magnif7 <- function(stn_data) {
  
  checkmate::assert_data_frame(stn_data, types = c("Date", "numeric"))
  
  mag7 <- stn_data %>%
    prep_stn_data() %>%
    as.data.frame() %>%
    calc_magnifSeven(yearType = "water", digits = 3) %>%
    #mutate(code_stn = stn_data$code_stn[1]) %>%
    as_tibble() #%>%
  #dplyr::relocate(code_stn)
  
  # agregando assinaturas
  bind_rows(mag7, tibble(indice = "bfi",
                         statistic = baseflow_index(stn_data)
                         )
  )
}
