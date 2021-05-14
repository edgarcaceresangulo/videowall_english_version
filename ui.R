ui <-
  dashboardPage(
    dashboardHeader(title = "Air quality monitoring station 'La Oroya' (Junin - Peru)",
                    titleWidth = 550),
    dashboardSidebar(disable = T),
    dashboardBody(fluidRow(
      box(
        title = "Wind rose",
        status = "primary",
        solidHeader = T,
        collapsible = T,
        leafletOutput("ro_vi")
      ),
      box(
        title = HTML("Pollution rose (SO<sub>2</sub>)"),
        status = "primary",
        solidHeader = T,
        collapsible = T,
        leafletOutput("ro_co")
      ),
      box(
        title = HTML("Hourly time series data plot (SO<sub>2</sub>)"),
        status = "primary",
        solidHeader = T,
        collapsible = T,
        plotOutput("se_ti_so2_1h")
      ),
      box(
        title = HTML("Daily time series data plot (SO<sub>2</sub>)"),
        status = "primary",
        solidHeader = T,
        collapsible = T,
        plotOutput("se_ti_so2_24h")
      ),
      box(
        title = HTML("3-hour rolling mean time series data plot (SO<sub>2</sub>)"),
        status = "primary",
        solidHeader = T,
        collapsible = T,
        plotOutput("se_ti_so2_m3h")
      )
    ))
  )
