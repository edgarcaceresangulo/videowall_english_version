cargadata_1h <- function(id_st, ant_cru1, nue_cru1, VF_1h, CF, fact, bdv_1h){

# Ahora -------------------------------------------------------------------

  ahora <- ymd_hms(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
# Actualizando CF ---------------------------------------------------------
  
  CF <- 
    CF %>% 
    dplyr::mutate(CFF = dplyr::if_else(is.na(CFI), NA_POSIXct_, ahora))

# Existencia de datos crudos o validados ----------------------------------

  swv <-
    VF_1h %>%
    dplyr::pull(VAL) %>% {
      1 %in% .
    } # True: hay datos validados
  
  swc <-
    CF %>%
    dplyr::pull(EDN) %>% {
      1 %in% .
    } # True: hay datos crudos, sin considerar estado
  
  if(swv){
    
    f1_va <-
      VF_1h %>%
      dplyr::pull(VFI) %>%
      lubridate::ymd_hms() %>%
      min(na.rm = T)
    
  }

  if(swc){

    f1_cr <-
      CF %>%
      dplyr::filter(!stringr::str_detect(VARIABLES, "_ESTADO$|_FLAGS$")) %>%
      dplyr::pull(CFI) %>%
      min(na.rm = T)

    f2_cr <- ahora

  ope1 <- 
    CF %>% 
    dplyr::transmute(
      ope1 = paste0("dplyr::if_else(date %within% lubridate::interval('", CFI,"', '" , CFF, "'), ", VARIABLES, ", ", AUX, ")")) %>% 
    dplyr::pull(ope1)
  
  arg1 <-
    setNames(
      object = rlang::parse_exprs(ope1),
      nm = CF %>% dplyr::pull(VARIABLES) %>% as.character(.)
    )
  
  ope2 <-
    ppb_ugm3 %>% 
    dplyr::mutate(ope2 = paste0("round(", FACT," * ", ANTES, ", digits = 2)")) %>%
    dplyr::pull(ope2)
  
  arg2 <-
    setNames(
      object = rlang::parse_exprs(ope2),
      nm = ppb_ugm3 %>% dplyr::pull(DESPUES)
    )
  
  rm.neg <- function(x) recoder::recoder(var = x, recode = ">= 0: $", other = NA, as.what = "numeric") # Retira negativos

# Datos crudos ------------------------------------------------------------
# 
#   drv <-
#     RJDBC::JDBC(driverClass = "oracle.jdbc.driver.OracleDriver", classPath = "www/ojdbc8.jar")
#   
#   conn <-
#     DBI::dbConnect(drv, "jdbc:oracle:thin:@localhost:1521:xe", "", "")
# 
#   alt_ses <-
#     DBI::dbSendQuery(conn, statement = "ALTER SESSION SET NLS_DATE_FORMAT = 'MM/DD/YYYY'")
#   
#   DBI::dbClearResult(alt_ses)
#   
#   result <- 
#     DBI::dbSendQuery(conn, statement = paste0(
#     "SELECT ", ant_c1 %>% stringi::stri_paste(collapse = ", "),
#     " FROM VIGAMB.VIGAMB_TRAMA_AIRE VIG",
#     " WHERE VIG.ID_ESTACION = ", id_st," AND VIG.FECHA_DATA_LOGER",
#     " BETWEEN TO_DATE(TO_CHAR('",
#     format(f1_cr, '%m/%d/%Y'),
#     "'),'MM/DD/YYYY') AND TO_DATE(TO_CHAR('",
#     format(f2_cr, '%m/%d/%Y'),
#     "'),'MM/DD/YYYY')"))
  # 
  # bdc_0_5min <-
  #   DBI::dbFetch(result)
  
  bdc_0_5min <- readRDS(file = "datos/bdc_0_5min.rds")
  
  bdc_5min <- 
    bdc_0_5min %>% 
    dplyr::rename(!!!setNames(ant_c1, nue_c1)) %>%
    dplyr::mutate(date = as.POSIXct(strptime(
      paste(FECHA_DATA, HORA_DATA, sep = " "),
      format = "%m/%d/%Y %H:%M:%S",
      tz = "UTC"
    ))) %>%
    dplyr::select(date, everything(), -c(ID_ESTACION, FECHA_DATA, HORA_DATA)) %>%
    dplyr::arrange(., date) %>%
    tidyr::complete(date = base::seq(
      min(lubridate::ymd_hms(date), na.rm = T),
      max(lubridate::ymd_hms(date), na.rm = T),
      by = "5 min"
    )) %>% 
    dplyr::mutate(!!!arg1) %>% 
    dplyr::filter_at(vars(-date), any_vars(!is.na(.))) %>% 
    dplyr::mutate_at(.vars = vars(ends_with("_PPB") | ends_with("_UGM3")), .funs = rm.neg)
  
  # DBI::dbClearResult(result)
  # DBI::dbDisconnect(conn)
  
  bdc_1h_tem1 <- bdc_5min %>%
    dplyr::select(-PP) %>%
    openair::timeAverage(mydata = ., avg.time = "1 hour", data.thresh = 75, statistic = "mean")
  
  bdc_1h_tem2 <- bdc_5min %>%
    dplyr::select(date, PP) %>%
    openair::timeAverage(mydata = ., avg.time = "1 hour", data.thresh = 75, statistic = "sum")
  
  bdc_1h <-
    dplyr::inner_join(x = bdc_1h_tem1, y = bdc_1h_tem2, by = "date") %>%
    dplyr::mutate_at(.vars = vars(ends_with("_PPB") | ends_with("_UGM3")), round, digits = 2) %>%
    dplyr::mutate(!!!arg2) %>%
    dplyr::mutate_at(.vars = vars(-date, -ends_with("_PPB"), -ends_with("_UGM3")), round, digits = 1) %>%
    openair::rollingMean(., pollutant = "SO2_UGM3", new.name = "SO2_UGM3_m3h", width = 3, data.thresh = 75, align = "right") %>% 
    dplyr::mutate(SO2_UGM3_m3h = round(SO2_UGM3_m3h, digits = 2)) %>%
    dplyr::select(!!nue_v1_1h)
  
  CF_1h <-
    bdc_1h %>%
    tidyr::gather(key = "VARIABLES", value = "VALOR", 2:ncol(.), factor_key = T) %>% 
    tidyr::drop_na(VALOR) %>% 
    dplyr::group_by(VARIABLES) %>% 
    dplyr::summarise(CFI = min(date), CFF = max(date), .groups = "drop") %>% 
    dplyr::mutate(
      CRU = dplyr::if_else(is.na(CFI), 0, 1)) # 1: presenta crudos, 0: no presenta crudos
  
  }

# Unir datos --------------------------------------------------------------
  
  # Validado : 1, prevalidado : 2 y NA: 3
  
  if(swv && swc){
    
  ope3 <-
    dplyr::tibble(parametro = nue_v1_1h) %>% 
    dplyr::slice(-1) %>% 
    dplyr::mutate(ope3 = paste0("dplyr::if_else(is.na(", parametro,".x), ", parametro, ".y, ", parametro, ".x)")) %>%
    dplyr::pull(ope3)
    
  arg3 <-
    setNames(
      object = rlang::parse_exprs(ope3),
      nm = dplyr::tibble(parametro = nue_v1_1h) %>% dplyr::slice(-1) %>% dplyr::pull(parametro)
    )
    
  ope4 <-
    dplyr::full_join(x = VF_1h, y = CF_1h, by = "VARIABLES") %>% 
    dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", VFI,"','", VFF,"') ~ 1,",
                                     "date %within% lubridate::interval('", CFI,"','", CFF,"') ~ 2, TRUE ~ 3)")) %>% 
    dplyr::pull(ope4)
    
  arg4 <-
    setNames(
      object = rlang::parse_exprs(ope4),
      nm = dplyr::full_join(x = VF_1h, y = CF_1h, by = "VARIABLES") %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
    )
  
  bd_1h <-
    dplyr::tibble(date = base::seq(f1_va, f2_cr, by = "1 hour")) %>%
    dplyr::left_join(x = ., y = bdv_1h, by = "date") %>%
    dplyr::left_join(x = ., y = bdc_1h, by = "date") %>%
    dplyr::mutate(!!!arg3) %>%
    dplyr::select(!!nue_v1_1h) %>%
    #dplyr::filter(date %within% lubridate::interval(f1, f2)) %>%
    dplyr::filter(date != lubridate::ymd_hms(format(Sys.time(), "%Y-%m-%d %H:00:00"))) %>% # Corrector de hora actual
    dplyr::mutate(!!!arg4) 
  
  }else{

    if(swv && swc == F){
      
      ope4 <-
        VF_1h %>% 
        dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", VFI,"','", VFF,"') ~ 1, TRUE ~ 3)")) %>% 
        dplyr::pull(ope4)
      
      arg4 <-
        setNames(
          object = rlang::parse_exprs(ope4),
          nm = VF_1h %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
        )

      bd_1h <- 
        bdv_1h %>% 
        dplyr::as_tibble(.) %>% 
        #dplyr::filter(date %within% lubridate::interval(f1, f2)) %>%
        dplyr::filter(date != lubridate::ymd_hms(format(Sys.time(), "%Y-%m-%d %H:00:00"))) %>% # Corrector de hora actual
        dplyr::mutate(!!!arg4)
      
    }else{

      if(swv == F && swc){

        ope4 <-
          CF_1h %>% 
          dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", CFI,"','", CFF,"') ~ 2, TRUE ~ 3)")) %>% 
          dplyr::pull(ope4)
        
        arg4 <-
          setNames(
            object = rlang::parse_exprs(ope4),
            nm = CF_1h %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
          )
 
        bd_1h <- 
          bdc_1h %>%
          #dplyr::filter(date %within% lubridate::interval(f1, f2)) %>%
          dplyr::filter(date != lubridate::ymd_hms(format(Sys.time(), "%Y-%m-%d %H:00:00"))) %>% # Corrector de hora actual
          dplyr::mutate(!!!arg4)

      }

    }

  }
  
  return(bd_1h)

}