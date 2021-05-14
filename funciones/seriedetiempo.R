seriedetiempo <- function(bd, temporalidad, variable, tb){
  
  cols <- c("1" = "#144AA7", "2" = "#666666")
  
  tb_aux <- tb %>% dplyr::filter(TEMPORALIDAD == temporalidad & VARIABLES == variable)
  
  fun_scale_x <-
    function(cond) {
      if (cond != "24h")
        scale_x_datetime(date_labels = "%b %y")
      else
        scale_x_date(date_labels = "%b %y")
    }
  
  fun_hline <-
    function(cond) {
      if (cond != "24h") {
        if (cond == "m3h") {
          list(
            geom_hline(
              aes(yintercept = tb_aux %>% dplyr::pull(EC),
                  linetype = "EC"), 
              color = "turquoise3"
            ),
            geom_hline(
              aes(yintercept = tb_aux %>% dplyr::pull(EP),
                  linetype = "EP"), 
              color = "purple3"
            ),
            geom_hline(
              aes(yintercept = tb_aux %>% dplyr::pull(EE),
                  linetype = "EE"), 
              color = "red3"
            ),
            scale_linetype_manual(name = "", 
                                  values = c("dashed", "dashed", "dashed"), 
                                  guide = guide_legend(override.aes = list(color = c("turquoise3","purple3","red3"), size = 2, order = 2)),
                                  labels = list(tb_aux %>% dplyr::pull(TITULOEC) %>% TeX(., output = "character") %>% parse(text = .),
                                                tb_aux %>% dplyr::pull(TITULOEP) %>% TeX(., output = "character") %>% parse(text = .),
                                                tb_aux %>% dplyr::pull(TITULOEE) %>% TeX(., output = "character") %>% parse(text = .))),
            scale_y_continuous(breaks = c(500, 1000, 1500, 2000, 2500))
            )
          }
      } else{
        list(
          geom_hline(
            aes(yintercept = tb_aux %>% dplyr::pull(ECA),
                linetype = "ECA"), 
            color = "red3"),
          scale_linetype_manual(name = "", 
                                values = c("dashed"),
                                guide = guide_legend(override.aes = list(color = "red3", size = 2)),
                                labels = list(tb_aux %>% dplyr::pull(TITULOECA) %>% TeX(., output = "character") %>% parse(text = .))),
          scale_y_continuous(breaks = c(200, 365, 400, 600))
        )
      }
    }
  
  tituloprincipal <- tb_aux %>% dplyr::pull(TITULOPRINCIPAL) %>% TeX()
  tituloejey <- tb_aux %>% dplyr::pull(TITULOEJEY) %>% TeX()
  tituloseriey1 <- tb_aux %>% dplyr::pull(TITULOSERIEY) %>% TeX()
  tituloseriey2 <- tb_aux %>% dplyr::pull(TITULOSERIEY2) %>% TeX()
  bandera <- paste0("B_", variable)

  p <- 
    ggplot(
      data = bd %>% dplyr::filter(!!rlang::sym(bandera) != 3), 
           mapping = aes(x = date, 
                         y = !!rlang::sym(variable), 
                         colour = factor(!!rlang::sym(bandera)) )) +
    geom_line(na.rm = T) +
    scale_colour_manual(name = "", values = cols, breaks = c("1", "2"), labels = list(tituloseriey1, tituloseriey2)) +
    fun_scale_x(cond = temporalidad) +
    fun_hline(cond = temporalidad) +
    labs(title = tituloprincipal,
         x = TeX("Fecha"), 
         y = tituloejey) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 13),
      legend.position = "top",
      legend.box = "vertical",
      legend.spacing.x = unit(0.5, "cm")
    ) +
    guides(color = guide_legend(override.aes = list(size = 2, order = 1)))

  p 

}
