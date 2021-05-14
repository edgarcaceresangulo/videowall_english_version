server <- function(input, output, session){
  
  autoInvalidate <- reactiveTimer(50*1000)
  
  datos <- reactive({
    
    autoInvalidate()

    bd_1h <-
      cargadata_1h(
        id_st = id_st,
        ant_cru1 = ant_cru1,
        nue_cru1 = nue_cru1,
        VF_1h = VF_1h,
        CF = CF,
        fact = fact,
        bdv_1h = bdv_1h
      )
    
    bd_24h <-
      cargadata_24h(
        VF_24h = VF_24h, 
        CF = CF, 
        bd_1h = bd_1h,
        bdv_24h = bdv_24h)
    
    list(bd_1h = bd_1h, bd_24h = bd_24h)

  })

  output$ro_vi <- renderLeaflet({
    
    bd_1h <- datos()$bd_1h

    #rosadeviento(bd = bd_1h, tb = tb_ro_vi)

    #Sys.sleep(2)

    rosadevientomapa(coord, tb = tb_ro_vi, ruta = "ro_vi_cc.png")

  })
  
  output$ro_co <- renderLeaflet({
    
    bd_1h <- datos()$bd_1h
    
    #rosadecontaminante(bd = bd_1h, tb = tb_ro_co, param = "SO2_UGM3")
    
    #Sys.sleep(2)
    
    rosadecontaminantemapa(coord, tb = tb_ro_co, ruta = "ro_co_cc.png")
    
  })
  
  output$se_ti_so2_1h <- renderPlot({
    
    bd_1h <- datos()$bd_1h
    
    seriedetiempo(bd = bd_1h, temporalidad = "1h", variable = "SO2_UGM3", tb = tb_se_ti)
    
  })
  
  output$se_ti_so2_24h <- renderPlot({
    
    bd_24h <- datos()$bd_24h
    
    seriedetiempo(bd = bd_24h, temporalidad = "24h", variable = "SO2_UGM3", tb = tb_se_ti)
    
  })
  
  output$se_ti_so2_m3h <- renderPlot({
    
    bd_1h <- datos()$bd_1h
    
    seriedetiempo(bd = bd_1h, temporalidad = "m3h", variable = "SO2_UGM3_m3h", tb = tb_se_ti)
    
  })
  
}
