cargadata_24h <- function(VF_24h, CF, bd_1h, bdv_24h){

# Día de hoy --------------------------------------------------------------

  hoy <- ymd(format(Sys.Date(), "%Y-%m-%d"))

# Existencia de datos crudos o validados ----------------------------------

  swv <-
    VF_24h %>%
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
      VF_24h %>%
      dplyr::pull(VFI) %>%
      min(na.rm = T)
    
  }
  
# Actualizando CF ---------------------------------------------------------
  
  if(swc){
  
  fun_cfi <- function(x) {
    if_else(lubridate::hour(x) <= 5,
            lubridate::date(x),
            lubridate::date(x) - days(1))
  }

  CF_24h <- 
    CF %>%
    dplyr::mutate_at("CFI", fun_cfi) %>%
    dplyr::mutate(CFF = dplyr::if_else(is.na(CFI), NA_Date_, hoy)) %>% 
    dplyr::filter(stringr::str_detect(VARIABLES, "_PPB$|_UGM3$")) %>% 
    dplyr::mutate(VARIABLES1 = stringr::str_replace(VARIABLES, "_PPB$|_UGM3$", "")) %>% 
    dplyr::right_join(
      x = .,
      y = VF_24h %>%
        dplyr::mutate(VARIABLES1 = stringr::str_replace(VARIABLES, "_PPB$|_UGM3$", "")),
      by = "VARIABLES1"
    ) %>% 
    dplyr::select(VARIABLES.y, CFI, CFF, AUX) %>% 
    dplyr::rename(VARIABLES = VARIABLES.y)
  
  f1_cr <-
    CF_24h %>%
    dplyr::pull(CFI) %>%
    min(na.rm = T)
  
  f2_cr <- 
    CF_24h %>%
    dplyr::pull(CFF) %>%
    max(na.rm = T)
  
# Datos crudos ------------------------------------------------------------ 

  ope1 <- 
    CF_24h %>% 
    dplyr::transmute(
      ope1 = paste0("dplyr::if_else(date %within% lubridate::interval('", CFI,"', '" , CFF, "'), ", VARIABLES, ", ", AUX, ")")) %>% 
    dplyr::pull(ope1)

  arg1 <-
    setNames(
      object = rlang::parse_exprs(ope1),
      nm = CF_24h %>% dplyr::pull(VARIABLES) %>% as.character(.)
    )

  sel_var_24h <-
    CF_24h %>% 
    dplyr::pull(VARIABLES) %>%
    as.character(.)

  ope2 <-
    paste0("round(", sel_var_24h, ", digits = 2)")
  
  arg2 <-
    setNames(
      object = rlang::parse_exprs(ope2),
      nm = sel_var_24h
    )
  
  bdc_24h <-
    bd_1h %>% 
    dplyr::select(date, !!sel_var_24h) %>% 
    dplyr::mutate(!!!arg1) %>% 
    dplyr::filter_at(vars(-date), any_vars(!is.na(.))) %>% 
    openair::timeAverage(., avg.time = "day", data.thresh = 75, statistic = "mean") %>% 
    dplyr::mutate(date = lubridate::date(date)) %>% 
    dplyr::mutate(!!!arg2) %>% 
    dplyr::filter(date %within% lubridate::interval(f1_cr, f2_cr))
  
  }
  
  # Unir datos --------------------------------------------------------------
  
  # Validado : 1, Prevalidado : 2 y NA: 3
  
  if(swv && swc){

    ope3 <-
      dplyr::tibble(parametro = nue_v1_24h) %>% 
      mutate(ope3 = paste0("dplyr::if_else(is.na(", parametro,".x), ", parametro, ".y, ", parametro, ".x)")) %>%
      dplyr::pull(ope3)
    
    arg3 <-
      setNames(
        object = rlang::parse_exprs(ope3),
        nm = dplyr::tibble(parametro = nue_v1_24h) %>% dplyr::pull(parametro)
      )
    
    ope4 <-
      dplyr::full_join(x = VF_24h, y = CF_24h, by = "VARIABLES") %>% 
      dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", VFI,"','", VFF,"') ~ 1,",
                                     "date %within% lubridate::interval('", CFI,"','", CFF,"') ~ 2, TRUE ~ 3)")) %>% 
      dplyr::pull(ope4)
    
    arg4 <-
      setNames(
        object = rlang::parse_exprs(ope4),
        nm = dplyr::full_join(x = VF_24h, y = CF_24h, by = "VARIABLES") %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
      )   
    
    bd_24h <-
      dplyr::tibble(date = base::seq(f1_va, f2_cr, by = "1 day")) %>%
      dplyr::left_join(x = ., y = bdv_24h, by = "date") %>%
      dplyr::left_join(x = ., y = bdc_24h, by = "date") %>%
      dplyr::mutate(!!!arg3) %>%
      dplyr::select(date, !!nue_v1_24h) %>% # dplyr::filter(date %within% lubridate::interval(f1, f2))
      dplyr::mutate(!!!arg4)

  }else{
    
    if(swv && swc == F){
      
      ope4 <-
        VF_24h %>% 
        dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", VFI,"','", VFF,"') ~ 1, TRUE ~ 3)")) %>% 
        dplyr::pull(ope4)
      
      arg4 <-
        setNames(
          object = rlang::parse_exprs(ope4),
          nm = VF_24h %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
        )
      
      bd_24h <- 
        bdv_24h %>% 
        dplyr::as_tibble(.) %>% #dplyr::filter(date %within% lubridate::interval(f1, f2))
        dplyr::mutate(!!!arg4)
      
      
    }else{
      
      if(swv == F && swc){
        
        ope4 <-
          CF_24h %>% 
          dplyr::transmute(ope4 = paste0("case_when(date %within% lubridate::interval('", CFI,"','", CFF,"') ~ 2, TRUE ~ 3)")) %>% 
          dplyr::pull(ope4)
        
        arg4 <-
          setNames(
            object = rlang::parse_exprs(ope4),
            nm = CF_24h %>% dplyr::pull(VARIABLES) %>% as.character(.) %>% paste0("B_", .)
          )
        
        bd_24h <- bdc_24h %>% #dplyr::filter(date %within% lubridate::interval(f1, f2)) 
        dplyr::mutate(!!!arg4)

      }
      
    }
    
  }
  
  return(bd_24h)
    
}
