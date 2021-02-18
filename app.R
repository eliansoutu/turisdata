library(shiny)
library(tidyverse)
library(lubridate)
library(highcharter)
library(shinythemes)
library(forecast)
library(tseries)
library(zoo)
library(ggplot2)
library(data.table)
library(wordcloud2)
library(shinydashboard)
library(DT)

options(scipen = 40)

ui <- dashboardPage(
  
    header <- dashboardHeader(title = "TURISDATA", titleWidth = 280,
                              tags$li(
                                  a(strong("Desarrollado por Elián Soutullo"),
                                      height = 40,
                                      href = "https://www.linkedin.com/in/eli%C3%A1n-soutullo/",
                                      target = "_blank",
                                    title = "Ir a Linkedin"
                                  ),
                                  class = "dropdown"
                              )),
    
    sidebar <- dashboardSidebar(width = 280, sidebarMenu(
      menuItem("Turismo en Argentina", icon = icon("map-marker", lib = "glyphicon"),
               menuSubItem("Turismo receptivo", tabName = "receptivoA"),
               menuSubItem("Turismo receptivo y alojamiento", tabName = "alojamientoA"),
               menuSubItem("Turismo emisivo", tabName = "emisivoA"),
               menuSubItem("Turismo interno", tabName = "internoA"),
               menuSubItem("Ocupación hotelera", tabName = "ocupacionA"),
               menuSubItem("Proyecciones", tabName = "proyeccionesA"),
               menuSubItem("Modelización de la demanda", tabName = "demandaModelo"),
               menuSubItem("Parques Nacionales", tabName = "pnA")),
        menuItem("Turismo mundial", tabName = "1", icon = icon("globe", lib = "glyphicon"), 
                 menuSubItem("Turismo receptivo", tabName = "receptivoM"),
                 menuSubItem("Turismo emisivo", tabName = "emisivoM"),
                 menuSubItem("Ingresos por turismo", tabName = "ingresosM"),
                 menuSubItem("Egresos por turismo", tabName = "egresosM"),
                 menuSubItem("Participación en las exportaciones", tabName = "exportacionesM"),
                 menuSubItem("Participación en las importaciones", tabName = "importacionesM"),
                 menuSubItem("Otros indicadores", tabName = "indicadoresM"),
                 menuSubItem("Proyecciones", tabName = "proyeccionesM"))
        
    )),
    
    body <- dashboardBody(
      tags$head( 
        tags$style(HTML("
      .main-sidebar {
        font-size: 19px;
        font-family: Arial, Helvetica, sans-serif;
      }"))),

        tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
        tabItems(
        tabItem(tabName = "receptivoM",
                selectInput('output_anio1', 
                           label=h3('Seleccione un año'),
                           choices = unique(indicadores$anio),
                           width = 600),
                highchartOutput("mapa_arrivals", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
        ),
        
        tabItem(tabName = "emisivoM",
                selectInput('output_anio2', 
                            label=h3('Seleccione un año'),
                            choices = unique(indicadores$anio),
                            width = 600),
                highchartOutput("mapa_departures", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
        ),
        tabItem(tabName = "ingresosM",
                selectInput('output_anio3', 
                            label=h3('Seleccione un año'),
                            choices = unique(indicadores$anio),
                            width = 600),
                highchartOutput("mapa_incomes", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
        ),
        tabItem(tabName = "egresosM",
                selectInput('output_anio4', 
                            label=h3('Seleccione un año'),
                            choices = unique(indicadores$anio),
                            width = 600),
                highchartOutput("mapa_expenses", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
        ),
        tabItem(tabName = "exportacionesM",
                selectInput('output_anio5', 
                            label=h3('Seleccione un año'),
                            choices = unique(indicadores$anio),
                            width = 600),
                highchartOutput("mapa_exports", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
                ),
        tabItem(tabName = "importacionesM",
                selectInput('output_anio6', 
                            label=h3('Seleccione un año'),
                            choices = unique(indicadores$anio),
                            width = 600),
                highchartOutput("mapa_imports", height = 800),
                br(),
                helpText("Nota°1: los países en gris no presentaban datos para el año seleccionado."),
                helpText("Nota°2: las Islas Malvinas figuran como territorio británico por configuraciones predeterminadas de la librería Highcharts, utilizada para graficar los mapas.")
        ),
        tabItem(tabName = "indicadoresM",
                selectizeInput('output_country', 
                               label = h3('Seleccione uno o más países (máximo 5)'),
                               choices = unique(indicadores$country),
                               multiple = TRUE,
                               selected = indicadores$country[1],
                               options = list(maxItems = 5),
                               width = 500
                ),
                fluidRow(column(6,highchartOutput("gdp_receptor_graph")),
                         column(6,highchartOutput('gdp_emisor_graph'))),
                br(),
                fluidRow(column(6,highchartOutput("balanza_graph")),
                         column(6,highchartOutput('carga_graph'), 
                                helpText("Nota: el valor de carga turística se puede interpretar como la cantidad de visitantes que recibe el país por cada un habitante local.")))
        ),
        tabItem(tabName = "proyeccionesM",
                fluidRow(column(6,
                                selectInput('output_country2', 
                                            label = h3('Seleccione un país'),
                                            choices = unique(df_pronosticos_arrivals$country),
                                            width = 500),
                                highchartOutput("pronostico_arrivals", height = 500)),
                         column(6,
                                selectInput('output_country3', 
                                            label = h3('Seleccione un país'),
                                            choices = unique(df_pronosticos_departures$country),
                                            width = 500),
                                highchartOutput('pronostico_departures', height = 500))),
                br(),
                selectInput('output_country4', 
                            label = h3('Seleccione un país'),
                            choices = unique(df_pronosticos_balanza$country),
                            width = 500),
                highchartOutput("pronostico_balanza", height = 600),
                br(),
                helpText("Nota: no se tuvo en consideración el impacto de la pandemia por la COVID-19 del año 2020 para el cálculo de las proyecciones.")
                ),
        
        tabItem(tabName = "demandaModelo",
                selectInput('output_pais_modelo', 
                            label = h3('Seleccione un país/región de origen'),
                            choices = unique(modelo_demanda$pais_origen),
                            selected = modelo_demanda$pais_origen[1],
                            width = 500
                ),
                
                infoBoxOutput("tipoCambio"),
                infoBoxOutput("pbiPais"),
                infoBoxOutput("turistasPais"),
                br(),
                br(),
                helpText(h3("Determinantes de la demanda turística")),
                fluidRow(
                  column(8, DTOutput("tablaModelo")),
                  column(4, helpText(h4("Aclaraciones:")),
                         helpText("1- Se utilizaron datos desde enero de 2016 hasta febrero de 2020, el equivalente a 50 observaciones por país/región."),
                         helpText("2- Las filas resaltadas corresponden a las variables que tienen un efecto significativo en la llegada de turistas. En términos estadísticos, esto sucede cuando el P Valor es menor a 0.05."),
                         helpText("3- Los coeficientes se interpretan como la variación que provoca en la llegada de turistas el incremento de un punto en la variable analizada. En el caso de los valores designados a los meses, los mismos se expresan respecto del mes de abril."),
                         helpText("4- Para considerar el efecto de la estacionalidad del turismo se optó por incluir el mes de cada observación como una variable del modelo, entendiendo que aquellos meses correspondientes a períodos vacacionales registran normalmente un mayor número de turistas."),
                         helpText("5- El índice de tipo de cambio real bilateral tiene como base el 17 de diciembre de 2015."),
                         br(),
                         helpText(h4("Especificaciones del modelo:")),
                         verbatimTextOutput("estadisticos")))
        ),
        
        tabItem(tabName = "receptivoA",
                selectInput('output_region', 
                            label = h3('Seleccione un país/región de origen'),
                            choices = unique(receptivo_argentina$pais_origen),
                            selected = receptivo_argentina$pais_origen[1],
                            width = 500
                ),
                fluidRow(column(7,highchartOutput("receptivo_arg_graph")),
                         column(5,highchartOutput("rec_transporte_arg", height = 320))
                ),
                fluidRow(column(6,highchartOutput("rec_gasto_arg")),
                         column(6,highchartOutput("rec_estadia_arg"))
                )
                ),
        tabItem(tabName = "alojamientoA",
                sliderInput('output_anioAloj', 
                            label = h3('Seleccione un año'),
                            min = 2014,
                            max = 2020,
                            value = 2014,
                            width = 1000
                ),
                fluidRow(column(6,highchartOutput("aloj_pern")),
                         column(6,highchartOutput("aloj_gasto"))
                ),
                
                br(),
                highchartOutput("aloj_dest", height = 600),
                br(),
                helpText("Nota°1: los datos del año 2020 corresponden únicamente al primer trimestre, previo a las restricciones por la pandemia."),
                helpText("Nota°2: no se cuenta con datos sobre el gasto según alojamiento para el año 2015.")
        ),
        tabItem(tabName = "emisivoA",
                selectInput('output_region2', 
                            label = h3('Seleccione un país/región de destino'),
                            choices = unique(emisivo_argentina$pais_destino),
                            selected = emisivo_argentina$pais_destino[1],
                            width = 500
                ),
                fluidRow(column(7,highchartOutput("emisivo_arg_graph")),
                         column(5,highchartOutput("emi_transporte_arg", height = 320))
                ),
                fluidRow(column(6,highchartOutput("emi_gasto_arg")),
                         column(6,highchartOutput("emi_estadia_arg"))
                ),
                br()
        ),
        tabItem(tabName = "internoA",
                sliderInput('output_anio0', 
                            label = h3('Seleccione un año'),
                            min = 2014,
                            max = 2020,
                            value = 2014,
                            width = 1000
                ),
                fluidRow(column(6,highchartOutput("int_destino")),
                         column(6,highchartOutput("int_origen"))
                ),
                fluidRow(column(6,highchartOutput("int_edad")),
                         column(6,highchartOutput("int_sexo"))
                ),
                fluidRow(column(6,highchartOutput("int_transporte")),
                         column(6,highchartOutput("int_motivo"))
                ),
                br(),
                helpText("Nota: los datos del año 2020 corresponden únicamente al primer trimestre, previo a las restricciones de viaje por la pandemia.")
                
        ),
        
        tabItem(tabName = "ocupacionA",
                selectInput('output_eoh', 
                            label = h3('Seleccione un destino'),
                            choices = unique(viajeros_eoh$region_de_destino),
                            selected = viajeros_eoh$region_de_destino[1],
                            width = 500
                ),
                
                fluidRow(column(6,highchartOutput("viajeros_eoh")),
                         column(6,highchartOutput("estadia_eoh"))),
                fluidRow(column(6,highchartOutput("pernoctes_eoh")),
                         column(6,highchartOutput("categoria_eoh")))
                
        ),
        
        tabItem(tabName = "proyeccionesA",
                selectInput('output_pron', 
                            label = h3('Seleccione un país/región'),
                            choices = unique(proy_rec$pais_origen),
                            selected = proy_rec$pais_origen[1],
                            width = 500
                ),
                highchartOutput("pronostico_arg_rec"),
                highchartOutput("pronostico_arg_emi"),
                br(),
                helpText("Nota: las proyecciones se realizaron en base a los datos disponibles hasta marzo del 2020. Las mismas reflejan cuál hubiese sido el escenario turístico esperado sin la pandemia.")
                
        ),
        tabItem(tabName = "pnA",
                selectInput('output_pn', 
                            label = h3('Seleccione una región de destino'),
                            choices = unique(parques_nacionales$region_de_destino),
                            selected = parques_nacionales$region_de_destino[1],
                            width = 700
                ),
                highchartOutput("pn_region", height = 500),
                helpText("Nota: los datos del año 2020 corresponden únicamente al primer trimestre, previo a las restricciones de viaje por la pandemia."),
                br(),
                helpText(h2("Opiniones de turistas que visitaron los principales parques nacionales de la región")),
                uiOutput('select_pn'),
                wordcloud2Output("wordcloud_pn", height = 500),
                helpText("Fuente: elaboración propia en base a comentarios de turistas en TripAdvisor y Google."),
                br()
        )
    )
    )
    )

server <- function(input, output) {
    
    data_indicators <- reactive({
        data_indicators = indicadores2[indicadores2$country==input$output_country,]
        data_indicators
    })
    
    data_indicators1 <- reactive({
        data_indicators1 = indicadores[indicadores$anio==input$output_anio1,]
        data_indicators1
    })

    data_indicators2 <- reactive({
        data_indicators2 = indicadores[indicadores$anio==input$output_anio2,]
        data_indicators2
    })
    
    data_indicators3 <- reactive({
        data_indicators3 = indicadores[indicadores$anio==input$output_anio3,]
        data_indicators3
    })
    
    data_indicators4 <- reactive({
        data_indicators4 = indicadores[indicadores$anio==input$output_anio4,]
        data_indicators4
    })
    
    data_indicators5 <- reactive({
        data_indicators5 = indicadores[indicadores$anio==input$output_anio5,]
        data_indicators5
    })
    
    data_indicators5 <- reactive({
        data_indicators5 = indicadores[indicadores$anio==input$output_anio5,]
        data_indicators5
    })
    
    data_indicators6 <- reactive({
        data_indicators6 = indicadores[indicadores$anio==input$output_anio6,]
        data_indicators6
    })
    
    arg_receptivo <- reactive({
        arg_receptivo = receptivo_argentina[receptivo_argentina$pais_origen==input$output_region,]
    })
    
    arg_receptivo_ge <- reactive({
        arg_receptivo = receptivo_gasto_estadia[receptivo_gasto_estadia$pais_origen==input$output_region,]
    })
    
    arg_emisivo <- reactive({
        arg_emisivo = emisivo_argentina[emisivo_argentina$pais_destino==input$output_region2,]
    })
    
    arg_emisivo_ge <- reactive({
        arg_emisivo_ge = emisivo_gasto_estadia[emisivo_gasto_estadia$pais_destino==input$output_region2,]
    })
    
    int_edad <- reactive({
        int_edad = interno_edad[interno_edad$indice_tiempo==input$output_anio0,]
    })
    
    int_sexo <- reactive({
        int_sexo = interno_sexo[interno_sexo$indice_tiempo==input$output_anio0,]
    })
    
    int_tran <- reactive({
        int_tran = interno_transporte[interno_transporte$indice_tiempo==input$output_anio0,]
    })
    
    int_moti <- reactive({
        int_moti = interno_motivo[interno_motivo$indice_tiempo==input$output_anio0,]
    })
    
    int_dest <- reactive({
        int_dest = interno_destino[interno_destino$indice_tiempo==input$output_anio0,]
    })
    
    int_orig <- reactive({
        int_orig = interno_origen[interno_origen$indice_tiempo==input$output_anio0,]
    })
    
    pron_arrivals <- reactive({
        pron_arrivals = df_pronosticos_arrivals[df_pronosticos_arrivals$country==input$output_country2,]
    })
    
    pron_departures <- reactive({
        pron_departures = df_pronosticos_departures[df_pronosticos_departures$country==input$output_country3,]
    })
    
    pron_balanza <- reactive({
        pron_balanza = df_pronosticos_balanza[df_pronosticos_balanza$country==input$output_country4,]
    })
    
    alojamiento_dest <- reactive({
        alojamiento_dest = destino_receptivo[destino_receptivo$anio==input$output_anioAloj,]
    })
    
    alojamiento_gasto <- reactive({
        alojamiento_gasto <- alojamiento_receptivo[alojamiento_receptivo$anio==input$output_anioAloj,]
    })
    
    eohViajeros <- reactive({
      eohViajeros = viajeros_eoh[viajeros_eoh$region_de_destino==input$output_eoh,]
    })
    
    eohEstadia <- reactive({
      eohEstadia = estadia_media[estadia_media$region_de_destino==input$output_eoh,]
    })
    
    eohPernoctes <- reactive({
      eohPernoctes = pernoctes[pernoctes$region_de_destino==input$output_eoh,]
    })
    
    eohCategoria <- reactive({
      eohCategoria = tasa_ocupacion[tasa_ocupacion$region_de_destino==input$output_eoh,]
    })
    
    pron_arg <- reactive({
        pron_arg = proy_rec[proy_rec$pais_origen==input$output_pron,]
    })
    
    pron_arg_emi <- reactive({
        pron_arg_emi = proy_emi[proy_emi$pais_destino==input$output_pron,]
    })
    
    pn_df <- reactive({
        pn_df = parques_nacionales[parques_nacionales$region_de_destino==input$output_pn,]
    })
    
    pn_word <- reactive({
        pn_word = pn_token[pn_token$region_de_destino==input$output_pn,]
    })
    
    mercados <- reactive({
      mercados = mercados_df[mercados_df$pais_origen==input$output_pais_modelo,]
    })
    
    modelo <- reactive({
      modelo = modelo_demanda[modelo_demanda$pais_origen==input$output_pais_modelo,]
    })
    
    estadisticos <- reactive({
      estadisticos = estadisticos_modelo[estadisticos_modelo$pais_origen==input$output_pais_modelo,]
    })
    
    
    output$mapa_arrivals <- renderHighchart({
        
        llegadas <- data_indicators1() %>% hcmap(map = "custom/world", 
                  joinBy = "iso-a3", 
                  value = "arrivals",
                  name = "Llegadas de turistas",
                  dataLabels = list(enabled = TRUE, format = '{country}'),
                  download_map_data = T,
                  nullColor = "#DBDBDC",
                  borderColor = "#FAFAFA", 
                  borderWidth = 0.5) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Llegadas de turistas", style = list(color = "#49CBAB", 
                                                                 fontSize = 28, 
                                                                 fontFamily = "Tahoma",
                                                                 fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "Llegadas", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        llegadas
    })
    
    output$mapa_departures <- renderHighchart({
        
        salidas <- data_indicators2() %>% hcmap(map = "custom/world", 
                                         joinBy = "iso-a3", 
                                         value = "departures",
                                         name = "Salidas de turistas",
                                         dataLabels = list(enabled = TRUE, format = '{country}'),
                                         download_map_data = T,
                                         nullColor = "#DBDBDC",
                                         borderColor = "#FAFAFA", 
                                         borderWidth = 0.5) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Salidas de turistas", style = list(color = "#49CBAB", 
                                                                 fontSize = 28, 
                                                                 fontFamily = "Tahoma",
                                                                 fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "Salidas", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        salidas
    })
    
    output$mapa_incomes <- renderHighchart({
        
        ingresos <- data_indicators3() %>% hcmap(map = "custom/world", 
                                       joinBy = "iso-a3", 
                                       value = "incomes",
                                       name = "Ingresos por turismo",
                                       dataLabels = list(enabled = TRUE, format = '{country}'),
                                       download_map_data = T,
                                       nullColor = "#DBDBDC",
                                       borderColor = "#FAFAFA", 
                                       borderWidth = 0.5,
                                       tooltip = list(valueSuffix = " USD")) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Ingresos por turismo en USD", style = list(color = "#49CBAB", 
                                                                fontSize = 28, 
                                                                fontFamily = "Tahoma",
                                                                fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "Ingresos en USD", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        ingresos
    })
    
    output$mapa_expenses <- renderHighchart({
        
        gastos <- data_indicators4() %>% hcmap(map = "custom/world", 
                                       joinBy = "iso-a3", 
                                       value = "expenses",
                                       name = "Egresos por turismo",
                                       dataLabels = list(enabled = TRUE, format = '{country}'),
                                       download_map_data = T,
                                       nullColor = "#DBDBDC",
                                       borderColor = "#FAFAFA", 
                                       borderWidth = 0.5,
                                       tooltip = list(valueSuffix = " USD")) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Egresos por turismo en USD", style = list(color = "#49CBAB", 
                                                                 fontSize = 28, 
                                                                 fontFamily = "Tahoma",
                                                                 fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "Egresos en USD", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        gastos
    })
    
    output$mapa_exports <- renderHighchart({
        
        exportaciones <- data_indicators5() %>% hcmap(map = "custom/world", 
                                       joinBy = "iso-a3", 
                                       value = "exports",
                                       name = "Participación en las exportaciones",
                                       dataLabels = list(enabled = TRUE, format = '{country}'),
                                       download_map_data = T,
                                       nullColor = "#DBDBDC",
                                       borderColor = "#FAFAFA", 
                                       borderWidth = 0.5,
                                       tooltip = list(valueSuffix = " %", valueDecimals = 2)) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Participación en las exportaciones", style = list(color = "#49CBAB", 
                                                                       fontSize = 28, 
                                                                       fontFamily = "Tahoma",
                                                                       fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "% de participación", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        exportaciones
    })
    
    output$mapa_imports <- renderHighchart({
        
        importaciones <- data_indicators6() %>% hcmap(map = "custom/world", 
                                       joinBy = "iso-a3", 
                                       value = "imports",
                                       name = "Participación en las importaciones",
                                       dataLabels = list(enabled = TRUE, format = '{country}'),
                                       download_map_data = T,
                                       nullColor = "#DBDBDC",
                                       borderColor = "#FAFAFA", 
                                       borderWidth = 0.5,
                                       tooltip = list(valueSuffix = " %", valueDecimals = 2)) %>%
            hc_mapNavigation(enabled = T) %>%
            hc_title(text = "Participación en las importaciones", style = list(color = "#49CBAB", 
                                                                               fontSize = 28, 
                                                                               fontFamily = "Tahoma",
                                                                               fontWeight = "bold")) %>% 
            hc_legend(title = list(text = "% de participación", align = "Center")) %>% 
            hc_colorAxis(stops = 
                             color_stops(colors = viridisLite::cividis(10, 
                                                                       begin = 0.2,
                                                                       end = 1,
                                                                       direction = 1)), 
                         type = "logarithmic") %>%  
            hc_credits(enabled = F) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
        importaciones
    })
    
    output$gdp_receptor_graph <- renderHighchart({
        data_indicators() %>%  
            hchart("line", hcaes(year(anio), consumo_receptivo, group = country), 
                   tooltip = list(valueSuffix = " %", valueDecimals = 2)) %>%
            hc_title(text = "Peso del turismo receptivo en el PBI", style = list(color = "#49CBAB", 
                                                                               fontSize = 16, 
                                                                               fontFamily = "Tahoma",
                                                                               fontWeight = "bold")) %>%
            hc_xAxis(title = NULL,
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Porcentaje"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_legend(align = "center", title = list(text = "Países")) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
    })
    
    output$gdp_emisor_graph <- renderHighchart({
        data_indicators() %>%  
            hchart("line", hcaes(year(anio), consumo_emisivo, group = country), 
                   tooltip = list(valueSuffix = " %", valueDecimals = 2)) %>%
            hc_title(text = "Peso del turismo emisivo en el PBI", style = list(color = "#49CBAB", 
                                                                               fontSize = 16, 
                                                                               fontFamily = "Tahoma",
                                                                               fontWeight = "bold")) %>%
            hc_xAxis(title = NULL,
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Porcentaje"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_legend(align = "center", title = list(text = "Países")) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
    })
    
    output$balanza_graph <- renderHighchart({
        data_indicators() %>% 
            hchart("column", hcaes(year(anio), balanza, group = country),
                   tooltip = list(valueSuffix = " USD")) %>%
            hc_title(text = "Balanza turística", style = list(color = "#49CBAB", 
                                                              fontSize = 16, 
                                                              fontFamily = "Tahoma",
                                                              fontWeight = "bold")) %>%
            hc_xAxis(title = NULL,
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Déficit/superávit"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_legend(align = "center", title = list(text = "Países")) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
    })
    
    output$carga_graph <- renderHighchart({
        data_indicators() %>% 
            hchart("column", hcaes(year(anio), carga_turistica, group = country),
                   tooltip = list(valueDecimals = 2)) %>%
            hc_title(text = "Carga turística", style = list(color = "#49CBAB", 
                                                            fontSize = 16, 
                                                            fontFamily = "Tahoma",
                                                            fontWeight = "bold")) %>%
            hc_xAxis(title = NULL,
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Visitantes/residentes"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_legend(align = "center", title = list(text = "Países")) %>% 
            hc_caption(text = "Fuente: Banco Mundial", align = "right")
    })
    
    output$pronostico_arrivals <- renderHighchart({
        arima_arrivals <- pron_arrivals() %>%  select(arrivals) 
        arima_arrivals <- ts(arima_arrivals, start = 1995, frequency = 1) 
        forecast(auto.arima(arima_arrivals), h = 10) %>% hchart(name = "Llegadas",
                                                                tooltip = list(valueDecimals = 0)) %>% 
            hc_title(text = "Proyección del turismo receptivo", style = list(color = "#49CBAB", 
                                                                             fontSize = 16, 
                                                                             fontFamily = "Tahoma",
                                                                             fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Año"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Llegadas"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: elaboración propia en base a datos del Banco Mundial", align = "right")
    })
    
    output$pronostico_departures <- renderHighchart({
        arima_departures <- pron_departures() %>% select(departures) 
        arima_departures <- ts(arima_departures, start = 1995, frequency = 1) 
        forecast(auto.arima(arima_departures), h = 10) %>% hchart(name = "Salidas",
                                                                  tooltip = list(valueDecimals = 0)) %>% 
            hc_title(text = "Proyección del turismo emisivo", style = list(color = "#49CBAB", 
                                                                           fontSize = 16, 
                                                                           fontFamily = "Tahoma",
                                                                           fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Año"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Salidas"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: elaboración propia en base a datos del Banco Mundial", align = "right")
    })
    
    output$pronostico_balanza <- renderHighchart({
        arima_balanza <- pron_balanza() %>% select(balanza)
        arima_balanza <- ts(arima_balanza, start = 1995, frequency = 1) 
        forecast(arima(arima_balanza, order =  c(1,2,3))) %>% hchart(name = "Balanza turística",
                                                               tooltip = list(valueDecimals = 0,
                                                                              valueSuffix = " USD")) %>% 
            hc_title(text = "Proyección de la balanza turística", style = list(color = "#49CBAB", 
                                                                               fontSize = 16, 
                                                                               fontFamily = "Tahoma",
                                                                               fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Año"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Balanza turística"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: elaboración propia en base a datos del Banco Mundial", align = "right")
    })
    
    output$receptivo_arg_graph <- renderHighchart({
        arg_receptivo() %>%  hchart("line", hcaes(indice_tiempo, total, group = pais_origen),
                                    tooltip = list(valueDecimals = 0)) %>%
        hc_title(text = "Evolución de turistas no residentes, 2016-2020", style = list(color = "#49CBAB", 
                                                                           fontSize = 16, 
                                                                           fontFamily = "Tahoma",
                                                                           fontWeight = "bold")) %>%
        hc_xAxis(title = list(text = "Fecha"), 
                 labels = list(style = list(color = "#49CBAB"))) %>% 
        hc_yAxis(title = list(text = "Cantidad de turistas")) %>% 
        hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$rec_transporte_arg <- renderHighchart({
        a <-arg_receptivo() %>% 
            rename("Aéreo" = aereo, "Terrestre" = terrestre, "Fluvial/marítimo" = fluvial) %>% 
            gather("medio_de_transporte", "turistas_no_residentes", 3:5) %>%  
            select(pais_origen, medio_de_transporte, turistas_no_residentes) %>% 
            group_by(pais_origen, medio_de_transporte) %>% 
            summarise(turistas_no_residentes = sum(turistas_no_residentes))
        hchart(a, "pie", hcaes(medio_de_transporte, turistas_no_residentes), 
                   tooltip = list(valueDecimals = 2),
               name = "Total llegadas") %>%
            hc_title(text = "Medio de transporte más utilizado", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$rec_gasto_arg <- renderHighchart({
        arg_receptivo_ge() %>% filter(!gasto_promedio_diario == 0) %>% 
            hchart("column", hcaes(trimestre, gasto_promedio_diario),
               tooltip = list(valueDecimals = 2, valueSuffix = " USD"),
               name = "Gasto promedio diario") %>%
            hc_title(text = "Evolución del gasto promedio diario", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Trimestre"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Gasto promedio diario en USD"),
                     labels = list(style = list(color = "#49CBAB"))) %>%
            hc_colorAxis(stops = color_stops(colors = viridisLite::cividis(1, 
                                                                           begin = 0.8,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right")
    })
    
    output$rec_estadia_arg <- renderHighchart({
        arg_receptivo_ge() %>% filter(!estadia_promedio == 0) %>% 
            hchart("column", hcaes(trimestre, estadia_promedio),
                                       tooltip = list(valueDecimals = 2),
                                       name = "Estadía promedio") %>%
            hc_title(text = "Evolución de la estadía promedio", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Trimestre"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Estadía promedio"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::cividis(1, 
                                                                       begin = 0.8,
                                                                       end = 1,
                                                                       direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right")
    })
    
    output$emisivo_arg_graph <- renderHighchart({
        arg_emisivo() %>%  hchart("line", hcaes(indice_tiempo, total, group = pais_destino),
                                    tooltip = list(valueDecimals = 0)) %>%
            hc_title(text = "Evolución de turistas residentes, 2016-2020", style = list(color = "#49CBAB", 
                                                                                           fontSize = 16, 
                                                                                           fontFamily = "Tahoma",
                                                                                           fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Fecha"), 
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de turistas"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$emi_transporte_arg <- renderHighchart({
        a <-arg_emisivo() %>% 
            rename("Aéreo" = aereo, "Terrestre" = terrestre, "Fluvial/marítimo" = fluvial) %>% 
            gather("medio_de_transporte", "turistas_residentes", 3:5) %>%  
            select(pais_destino, medio_de_transporte, turistas_residentes) %>% 
            group_by(pais_destino, medio_de_transporte) %>% 
            summarise(turistas_residentes = sum(turistas_residentes))
        hchart(a, "pie", hcaes(medio_de_transporte, turistas_residentes), 
               tooltip = list(valueDecimals = 2),
               name = "Total salidas") %>%
            hc_title(text = "Medio de transporte más utilizado", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$emi_gasto_arg <- renderHighchart({
        arg_emisivo_ge() %>% filter(!gasto_diario_promedio == 0) %>% 
            hchart("column", hcaes(trimestre, gasto_diario_promedio),
                   tooltip = list(valueDecimals = 2, valueSuffix = " USD"),
                   name = "Gasto promedio diario") %>%
            hc_title(text = "Evolución del gasto promedio diario", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Trimestre"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Gasto promedio diario en USD"),
                     labels = list(style = list(color = "#49CBAB"))) %>%
            hc_colorAxis(stops = color_stops(colors = viridisLite::cividis(1, 
                                                                           begin = 0.8,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right")
    })
    
    output$emi_estadia_arg <- renderHighchart({
        arg_emisivo_ge() %>% filter(!estadia_promedio == 0) %>% 
            hchart("column", hcaes(trimestre, estadia_promedio),
                   tooltip = list(valueDecimals = 2),
                   name = "Estadía promedio") %>%
            hc_title(text = "Evolución de la estadía promedio", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Trimestre"),  
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Estadía promedio"),  
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::cividis(1, 
                                                                           begin = 0.8,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right")
    })
    
    output$int_destino <- renderHighchart({
        int_dest() %>%  hchart("bar", hcaes(region_destino, turistas),
                               name = "Turistas por destino") %>%
            hc_title(text = "Turismo receptivo interno", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>%
            hc_xAxis(title = list(text = "Regiones"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de turistas"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, 
                                                                           begin = 0.2,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$int_origen <- renderHighchart({
        int_orig() %>%  hchart("bar", hcaes(region_origen, turistas),
                               name = "Turistas por origen") %>%
            hc_title(text = "Turismo emisivo interno", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>%
            hc_xAxis(title = list(text = "Regiones"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de turistas"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, 
                                                                           begin = 0.2,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$int_edad <- renderHighchart({
        int_edad() %>%  hchart("column", hcaes(edad, turistas),
                               name = "Turistas") %>%
            hc_title(text = "Turistas por rango etario", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>%
            hc_xAxis(title = list(text = "Edad"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de turistas"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, 
                                                                           begin = 0.4,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$int_sexo <- renderHighchart({
        int_sexo() %>%  hchart("pie", hcaes(sexo, turistas),
                               name = "Turistas") %>%
            hc_title(text = "Turistas por sexo", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>% 
            hc_xAxis(title = list(text = "Sexo")) %>% 
            hc_yAxis(title = list(text = "Cantidad de turistas")) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(10, 
                                                                           begin = 0.4,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$int_motivo <- renderHighchart({
        int_moti() %>%  hchart("treemap", hcaes(motivo, value = turistas, color = turistas)) %>%
            hc_title(text = "Turistas por motivo de viaje", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>%
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(8, 
                                                                           begin = 0.2,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$int_transporte <- renderHighchart({
        int_tran() %>%  hchart("treemap", hcaes(tipo_transporte, value = turistas, color = turistas),
               name = "Turistas") %>%
            hc_title(text = "Turistas por medio de transporte", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(8, 
                                                                           begin = 0.4,
                                                                           end = 1,
                                                                           direction = 1))) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right") 
    })
    
    output$aloj_pern <- renderHighchart({
        alojamiento_gasto() %>%  hchart("treemap", hcaes(tipo_alojamiento, value = pernoctaciones, color = pernoctaciones),
                                       name = "Pernoctes") %>% 
        hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, 
                                                                   begin = 0.2,
                                                                   end = 1,
                                                                   direction = 1)), 
                     type = "logarithmic") %>%
            hc_title(text = "Pernoctes por tipo de alojamiento", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right") 
    })
    
    output$aloj_dest <- renderHighchart({
        alojamiento_dest() %>%  hchart("bar", hcaes(destino, pernoctaciones),
                                       name = "Pernoctes") %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10, 
                                                                           begin = 0.2,
                                                                           end = 1,
                                                                           direction = 1)), 
                         type = "logarithmic") %>%
            hc_title(text = "Pernoctes por destino", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold"))  %>%
            hc_xAxis(title = list(text = "Destino"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de pernoctes"),  labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right") 
    })
    
    output$aloj_gasto <- renderHighchart({
        alojamiento_gasto() %>%  hchart("pyramid", hcaes(tipo_alojamiento, gasto_total),
                                       name = "Gasto",
                                       tooltip = list(valueDecimals = 0, valueSuffix = " USD")) %>% 
            hc_title(text = "Gasto total según tipo de alojamiento", 
                     style = list(color = "#49CBAB", 
                                  fontSize = 16, 
                                  fontFamily = "Tahoma", 
                                  fontWeight = "bold")) %>% 
            hc_colorAxis(stops = color_stops(colors = viridisLite::plasma(10, 
                                                                           begin = 0.2,
                                                                           end = 1,
                                                                           direction = 1)),
                         
                         type = "logarithmic") %>%
            hc_legend(NULL) %>% 
            hc_caption(text = "Fuente: Encuesta de Turismo Internacional, INDEC", align = "right") 
    })
    
    output$viajeros_eoh <- renderHighchart({
      eohViajeros() %>%  hchart("line", hcaes(indice_tiempo, viajeros, group = origen_viajeros)) %>% 
        hc_title(text = "Viajeros hospedados en hoteles y parahoteles", 
                 style = list(color = "#49CBAB", 
                              fontSize = 16, 
                              fontFamily = "Tahoma", 
                              fontWeight = "bold"))  %>%
        hc_xAxis(title = list(text = "Año"),
                 labels = list(style = list(color = "#49CBAB"))) %>% 
        hc_yAxis(title = list(text = "Viajeros"),
                 labels = list(style = list(color = "#49CBAB"))) %>%
        hc_caption(text = "Fuente: Encuesta de Ocupación Hotelera, Ministerio de Turismo y Deporte", align = "right") 
    })
    
    output$estadia_eoh <- renderHighchart({
      eohEstadia() %>%  hchart("column", hcaes(indice_tiempo, estadia_media_en_noches, group = origen_pernoctes),
                               tooltip = list(valueDecimals = 2)) %>% 
        hc_title(text = "Estadía promedio en hoteles y parahoteles", 
                 style = list(color = "#49CBAB", 
                              fontSize = 16, 
                              fontFamily = "Tahoma", 
                              fontWeight = "bold"))   %>%
        hc_xAxis(title = list(text = "Año"),
                 labels = list(style = list(color = "#49CBAB"))) %>% 
        hc_yAxis(title = list(text = "Estadía promedio en noches"),
                 labels = list(style = list(color = "#49CBAB"))) %>%
        hc_caption(text = "Fuente: Encuesta de Ocupación Hotelera, Ministerio de Turismo y Deporte", align = "right") 
    })
    
    output$pernoctes_eoh <- renderHighchart({
      eohPernoctes() %>%  hchart("line", hcaes(indice_tiempo, pernoctes, group = origen_pernoctes)) %>% 
        hc_title(text = "Pernoctes en hoteles y parahoteles", 
                 style = list(color = "#49CBAB", 
                              fontSize = 16, 
                              fontFamily = "Tahoma", 
                              fontWeight = "bold"))   %>%
        hc_xAxis(title = list(text = "Año"),
                 labels = list(style = list(color = "#49CBAB"))) %>% 
        hc_yAxis(title = list(text = "Pernoctes"),
                 labels = list(style = list(color = "#49CBAB"))) %>%
        hc_caption(text = "Fuente: Encuesta de Ocupación Hotelera, Ministerio de Turismo y Deporte", align = "right") 
    })
    
    output$categoria_eoh <- renderHighchart({
      eohCategoria() %>%  hchart("column", hcaes(indice_tiempo, tasa_de_ocupacion_plazas, group = categoria_del_hotel),
                               tooltip = list(valueDecimals = 2, valueSuffix = " %")) %>% 
        hc_title(text = "Tasa de ocupación según categoría", 
                 style = list(color = "#49CBAB", 
                              fontSize = 16, 
                              fontFamily = "Tahoma", 
                              fontWeight = "bold"))   %>%
        hc_xAxis(title = list(text = "Categoría"),
                 labels = list(style = list(color = "#49CBAB"))) %>% 
        hc_yAxis(title = list(text = "% de ocupación"),
                 labels = list(style = list(color = "#49CBAB"))) %>%
        hc_caption(text = "Fuente: Encuesta de Ocupación Hotelera, Ministerio de Turismo y Deporte", align = "right") 
    })
    output$pronostico_arg_rec <- renderHighchart({
        arima_llegadas <- pron_arg() %>%  select(total)
        arima_llegadas <- ts(arima_llegadas, start = c(2016,1), end = c(2020,2), frequency = 12) 
        forecast(auto.arima(arima_llegadas), h = 12) %>% hchart(name = "Llegadas",
                                                                tooltip = list(valueDecimals = 0)) %>% 
            hc_title(text = "Proyección del turismo receptivo sin COVID-19", style = list(color = "#49CBAB", 
                                                                             fontSize = 16, 
                                                                             fontFamily = "Tahoma",
                                                                             fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Fecha"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Llegadas"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: elaboración propia en base a datos del Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$pronostico_arg_emi <- renderHighchart({
        arima_salidas <- pron_arg_emi() %>%  select(total) 
        arima_salidas <- ts(arima_salidas, start = c(2016,1), end = c(2020,2), frequency = 12) 
        forecast(auto.arima(arima_salidas), h = 12) %>% hchart(name = "Salidas",
                                                                tooltip = list(valueDecimals = 0)) %>% 
            hc_title(text = "Proyección del turismo emisivo sin COVID-19", style = list(color = "#49CBAB", 
                                                                                          fontSize = 16, 
                                                                                          fontFamily = "Tahoma",
                                                                                          fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Fecha"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Salidas"),
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_caption(text = "Fuente: elaboración propia en base a datos del Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$tipoCambio <- renderValueBox({
      valueBox(
        paste0(format(mean(mercados()$tipo_cambio_real), 1)), "Promedio mensual Índice Tipo de Cambio Real Bilateral", 
        icon = icon("usd", lib = "glyphicon"),
        color = "blue"
      )
    })
    
    output$estadisticos <- renderPrint({
      estadisticos()[,c('r.squared','adj.r.squared', "sigma", "statistic", "df", "df.residual", "p.value")]
    })
    
    output$pbiPais <- renderValueBox({
      valueBox(
        paste0("$ ", as.integer(mean(mercados()$pbi))), "Promedio mensual PBI pér cápita en USD", 
        icon = icon("globe", lib = "glyphicon"),
        color = "green"
      )
    })
    
    output$turistasPais <- renderValueBox({
      valueBox(
        paste0(as.integer(mean(mercados()$turistas))), "Promedio mensual llegadas de turistas", 
        icon = icon("user", lib = "glyphicon"),
        color = "maroon"
      )
    })
    
    output$tablaModelo <- renderDT(options = list(searching = FALSE),{
      datatable(modelo(),
                options = list(pageLength = 13,
        columnDefs = list(list(targets = 4:5, visible = FALSE)))) %>%  formatStyle( "P Valor",
        "dummy", target = "row",
        backgroundColor = styleEqual(c(0, 1), c('white', 'lightblue'))) 
  
    })
    
    output$pn_region <- renderHighchart({
        pn_df() %>% hchart("line", hcaes(indice_tiempo, visitas, group = origen_visitantes),
                           tooltip = list(valueDecimals = 0)) %>%
            hc_title(text = "Evolución de visitas a parques nacionales", style = list(color = "#49CBAB", 
                                                                                           fontSize = 16, 
                                                                                           fontFamily = "Tahoma",
                                                                                           fontWeight = "bold")) %>%
            hc_xAxis(title = list(text = "Año"), 
                     labels = list(style = list(color = "#49CBAB"))) %>% 
            hc_yAxis(title = list(text = "Cantidad de visitas")) %>% 
            hc_caption(text = "Fuente: Ministerio de Turismo y Deporte", align = "right")
    })
    
    output$wordcloud_pn <- renderWordcloud2({
        pn_word() %>% filter(parque_nombre == input$output_parque) %>% 
            count(word, sort=T) %>% 
            wordcloud2(size=1, color = "random-dark", shape = "circle")
    })
    
    output$select_pn <- renderUI({
        selectInput(inputId="output_parque", h3("Seleccione un parque nacional"), 
                    choices = unique(pn_token[pn_token$region_de_destino==input$output_pn,'parque_nombre']),
                    width = 500
        )
    })
    
    output$logo <- renderImage({
        width<- "100%"
        height<- "100%"
        list(src = "logo_transparent.png",
             contentType = "image/png",
             width = width,
             height = height,
             deleteFile = F
        )
    },
    deleteFile = FALSE)
    
    output$text1 <- renderText({"Turistific"
    })
}


shinyApp(ui = ui, server = server)
