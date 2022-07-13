#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(sf)
library(stringr)
library(htmltools)
library(shinyWidgets)
library(shinycustomloader)
library(DT)


poblacion_pba <- readRDS("poblacion_pba.RDS")
densidad1 <- readRDS("densidad.RDS")
densidad_table <- readRDS("densidad_table.RDS")
comunidades <- readRDS("comunidades.RDS")
nbi_map <- readRDS("nbi_map.RDS")
nbi_table <- readRDS("nbi_table.RDS")
PNG <- readRDS("PNG.RDS")
empresas <- readRDS("empresas.RDS")
recaudacion1 <- readRDS("recaudacion.RDS")
transferencia_table <- readRDS("transferencia_table.RDS") %>% rename(Distrito = municipio_nombre) %>% ungroup() %>%
    select(Distrito, anio, concepto, monto)
transferencia_table = as.data.frame(transferencia_table)
transferencia_table <- transform(transferencia_table, anio = as.character(anio))
transferencia_mapa <- readRDS("transferencia_mapa.RDS") %>% rename(Distrito = distrito) %>% ungroup()
transferencia_mapa <- transferencia_mapa %>% filter(!anio == "2022") %>% arrange(desc(anio))
salarios_mapa <- readRDS("salarios_mapa.RDS")
salarios_tabla <- readRDS("salarios_tabla.RDS")

### para filtrar entre el interior y GBA
poblacion_pba1 <- poblacion_pba %>% filter(fecha %in% c("2010", "2016", "2022"))
pob_pba_interior <- poblacion_pba1 %>% filter(parte == "Interior")
pob_pba_interior <- pob_pba_interior %>% filter(!distrito == "La Plata")
### Filtros de mapa de PBG
PNG_graf <- PNG %>% filter(!Tipo == "Total")
PNG_total <- PNG %>% filter(Tipo == "Total")
PNG_agri <- PNG %>% filter(Tipo == "Agricultura y ganadería")
PNG_minas <- PNG %>% filter(Tipo == "Explot. de minas y canteras")
PNG_manu <- PNG %>% filter(Tipo == "Ind. Manufacturera")
PNG_elec <- PNG %>% filter(Tipo == "Electricidad, gas y agua")
PNG_const <- PNG %>% filter(Tipo == "Construcción")
PNG_comercio <- PNG %>% filter(Tipo == "Comercio")
PNG_inmob <- PNG %>% filter(Tipo == "Serv. inmobiliarios y empr.")
PNG_admin <- PNG %>% filter(Tipo == "Administración pública")
### Filtros de Empresas
empresas_totales <- empresas %>% filter(tipo == "total")
empresas_exp <- empresas %>% filter(tipo == "exportadoras")
### filtro recaudacion
recaudacion_int <- recaudacion1 %>% filter(parte == "Interior")
recaudacion_int <- recaudacion_int %>% filter(!distrito == "La Plata")
recaudacion_graf <- recaudacion1 %>% filter(!tipo == "Total")
## filtro rendimiento agricola

### filtro rendimientos de hospitales 

ui <- dashboardPage(
    dashboardHeader(title = "Panel de control - Buenos Aires", titleWidth = 350), 
    dashboardSidebar(width = 350,
                     sidebarMenu(                                  
                         id = "sidebar", 
                         menuItem(text = "Población", tabName = "pob", icon = icon("street-view")),
                         conditionalPanel("input.sidebar == 'pob' && input.t1 == 'fecha_pob'", selectInput(inputId = "pob_fecha", "Seleccione un año", choices = unique(poblacion_pba1$fecha))),
                         conditionalPanel("input.sidebar == 'pob' && input.t1 == 'fecha_pob'", prettyRadioButtons(inputId = "filter_pob", label = "Región:", c("PBA" = "PBA", "Interior" = "Interior"), animation = "pulse")),
                         menuItem(text = "Economia", tabName = "eco", icon = icon("chart-line")), 
                         conditionalPanel("input.sidebar == 'eco' && input.t2 == 'png_filter'", selectInput(inputId = "filter_pbg", "Seleccione un actividad", choices = unique(c("Total", "Administración pública", "Agricultura y ganadería", "Comercio", "Construcción", "Electricidad, gas y agua", "Serv. inmobiliarios", "Ind. Manufacturera", "Expl. de minas")))),
                         conditionalPanel("input.sidebar == 'eco' && input.t2 == 'emp_muni'", prettyRadioButtons(inputId = "filter_empresas", label = "Tipo de empresa:", c("Total de empresas" = "Total de empresas", "Empresas exportadoras" = "Empresas exportadoras"), animation = "pulse")),
                         conditionalPanel("input.sidebar == 'eco' && input.t2 == 'recau'", selectInput(inputId = "tipo_recau", "Seleccione un derecho", choices = unique(recaudacion1$tipo))),
                         conditionalPanel("input.sidebar == 'eco' && input.t2 == 'recau'", prettyRadioButtons(inputId = "filter_reca", label = "Lugar:", c("PBA" = "PBA", "Interior" = "Interior"), animation = "pulse")),
                         conditionalPanel("input.sidebar == 'eco' && input.t2 == 'tras'", selectInput(inputId = "fecha_tras", "Seleccione un año", choices = unique(transferencia_mapa$anio)))
                     ),
                     div(style="display:inline-block;width:32%;text-align: center;", actionButton("primera", label = NULL, style = "width: 300px; height: 110px;
background: url('https://lh3.googleusercontent.com/9sNfiMppl_ZzNOhjdwDZK2xvZRwINIHhRSU8UsHPfwW0hxYifkK2Z8ClL7_sygq_ojMm6fquxFuXmVmAikcLX8n7L5slWgoe1NN3E6X4vg0v8xxV2JcLjGlWTi7pZn5haxJ76Y-N2cFGM44Qig');  background-size: cover; background-position: center;"))),                      
    dashboardBody(
        tabItems(
            tabItem( 
                
                #pestaña de poblacion
                
                tabName = "pob", 
                tabBox(id = "t1", width = 15, 
                       tabPanel(title = "Evolución poblacional", value = "fecha_pob", withLoader(leafletOutput("mappob"), type="html", loader="pacman"), box(title =  strong("Evolución poblacional"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("plot1"))), 
                       tabPanel(title = "Densidad poblacional", withLoader(leafletOutput("pobdensidad"), type="html", loader="pacman"), box(title =  strong("Info. sobre densidad"), width = "100%", status = "info", solidHeader = T, collapsible = T, DT::dataTableOutput("mytable"))),
                       tabPanel(title = "Comunidades indigenas",  withLoader(leafletOutput("comunidades"), type="html", loader="pacman")), 
                       tabPanel(title = "Personas con NBI", withLoader(leafletOutput("nbi_map_le"), type="html", loader="pacman"), dataTableOutput("data_nbi")))), 
            tabItem(
                
                #pestaña de economia
                
                tabName = "eco", 
                tabBox(id = "t2", width = 15, 
                       tabPanel(title = "PBG por municipio y actividad", value = "png_filter", withLoader(leafletOutput("mappng"), type="html", loader="pacman"), box(title =  strong("Ranking de PBG"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_pbg"))), 
                       tabPanel(title = "Empresas por municipio", value = "emp_muni",  withLoader(leafletOutput("map_empr"), type="html", loader="pacman"), box(title =  strong("Cantidad de empresas"), status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_empre")),  box(title =  strong("Cantidad de empresas exportadoras"), status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_empre_ex"))), 
                       tabPanel(title = "Redaudación por municipio", value = "recau", withLoader(leafletOutput("mapreca"), type="html", loader="pacman"), box(title =  strong("Ranking de recaudaciones"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("Gra_re"))),
                       tabPanel(title = "Salarios mediano por municipio", value = "sal", withLoader(leafletOutput("mapsal"), type="html", loader="pacman"), box(title =  strong("Evolución de salarios"), width = "100%", status = "info", solidHeader = T, collapsible = T, plotOutput("plot_sal"))),
                       tabPanel(title = "Transferencia de Prov.", value = "tras", withLoader(leafletOutput("maptran"), type="html", loader="pacman"), DT::dataTableOutput("table_trans", width = "100%"))))
            

        )
    )
)





server <- function(input, output) {
    
    ## poblacion
    
    output$mappob <- renderLeaflet({
      
      pob_total <- reactive({
        lv <- poblacion_pba1 %>% filter(fecha == input$pob_fecha)
        return(lv)
      })
      
      pob_interior <- reactive({
        lv <- pob_pba_interior %>% filter(fecha == input$pob_fecha)
        return(lv)
      })
      
        
        bin_pob_total <- c(1500, 5000, 15000, 60000, 100000, 300000, 700000, 1500000, 2500000)
        
        bin_pob_interior <- c(1750, 3500, 7500, 15000, 40000, 70000, 100000, 400000, 725000)
        
        pal_pob_interior <- colorBin(palette = "OrRd", domain = pob_pba_interior$pob, bins = bin_pob_interior)
        
        pal_pob_total <- colorBin(palette = "OrRd", domain = poblacion_pba1$pob, bins = bin_pob_total)
        
        switch(input$filter_pob,
               "PBA" =
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = pob_total(), weight = 1, color = "black", label = 
                                   paste0(as.character(pob_total()$distrito), " | Población: ", as.character(format(pob_total()$pob, big.mark = "."))),
                               fillColor = ~pal_pob_total(pob_total()$pob), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_pob_total,
                             values = poblacion_pba$pob,
                             title = "Población"), 
               "Interior" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = pob_interior(), weight = 1, color = "black", label = 
                                   paste0(as.character(pob_interior()$distrito),
                                          " | Población: ", as.character(format(pob_interior()$pob, big.mark = "."))),
                               fillColor = ~pal_pob_interior(pob_interior()$pob), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_pob_interior,
                             values = pob_pba_interior$pob,
                             title = "Población")
        )
    })
    
    ggplot_data <- reactive({
        site <- input$mappob_shape_click$id
        poblacion_pba[poblacion_pba$distrito %in% site,]
    })
    
    output$plot1 <- renderPlot({
        ggplot(data = ggplot_data(), aes(x = fecha, y = pob, group = distrito))+
            geom_line(size = 1.5) + geom_point(size = 3) + theme_classic() + 
            xlab("año") + ylab("población") + 
            labs(title = req(ggplot_data()$distrito)) + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
                  axis.title.x = element_text(size = 15, face = "bold"), 
                  axis.title.y = element_text(size = 15, face = "bold")) +
            geom_label(
                aes(label = format(pob, big.mark = ".")), 
                nudge_x = 0.25, 
                nudge_y = 0.25 
                
            )
        
        
    })
    
    ## densidad
    
    output$pobdensidad <- renderLeaflet({
        
        bin_densidad <- c(1, 50, 100, 350, 750, 1500, 4000, 7000, 10000)
        
        pal_densidad <- colorBin(palette = "YlOrRd", domain = densidad1$densidad, bins = bin_densidad)
        
        leaflet() %>% 
            addTiles() %>%
            addPolygons(data = densidad1, weight = 1, color = "black", label = 
                            paste0(as.character(densidad1$distrito),
                                   " | Densidad(hab/km2): ", as.character(densidad1$densidad)),
                        fillColor = ~pal_densidad(densidad1$densidad), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T), 
                        layerId = ~distrito, group = "PBA") %>%
            addLegend(position = "bottomright", 
                      pal = pal_densidad,
                      values = densidad1$densidad,
                      title = "Poblacion", 
                      group = "PBA")
    })
    observeEvent(input$pobdensidad_shape_click, {
      
      #capture the info of the clicked polygon
      click <- input$pobdensidad_shape_click
      
      #subset your table with the id of the clicked polygon 
      selected <- densidad_table[densidad_table$Distrito == click$id,]
      
      #if click id isn't null render the table
      if(!is.null(click$id)){
        output$mytable = DT::renderDataTable({
          selected
        }) 
      } 
    })
    
    ## comunidades indigenas 
    
    output$comunidades <- renderLeaflet({
        
        icono_comu <- "https://upload.wikimedia.org/wikipedia/commons/thumb/b/b7/Banner_of_the_Qulla_Suyu.svg/800px-Banner_of_the_Qulla_Suyu.svg.png"
        
        leaflet() %>% 
            addTiles() %>% 
            addMarkers(lat = comunidades$latitud, lng = comunidades$longitud,
                       label = paste0("Municipio: ", as.character(comunidades$municipio_nombre),
                                      " | Nombre de comunidad: ", as.character(comunidades$nombre)), 
                       icon = list(
                           iconUrl = icono_comu, 
                           iconSize = c(20, 20)
                       ))
    })
    
    ## Necesidades basicas insatisfechas
    
    output$data_nbi <- renderDataTable(nbi_table)
    
    output$nbi_map_le <- renderLeaflet({
        
        bin_nbi<- c(0, 2.5, 5, 7.5, 10, 12.5, 16, 19.5, 24)
        
        pal_nbi<- colorBin(palette = "Reds", domain = nbi_map$Tasax100, bins = bin_nbi)
        
        leaflet() %>% 
            addTiles() %>%
            addPolygons(data = nbi_map, weight = 1, color = "black", label = 
                            paste0("Municipio: ", as.character(nbi_map$Distrito), 
                                   " | Cantidad de personas con NBI: ", as.character(nbi_map$Poblacion_con_NBI)),
                        fillColor = ~pal_nbi(nbi_map$Tasax100), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T)) %>%
            addLegend(position = "bottomright", 
                      pal = pal_nbi,
                      values = nbi_map$Tasax100,
                      title = "Tasa cada 100 personas", 
                      group = "PBA")
    })
    
    ## Producto bruto geografico
    
    output$mappng <- renderLeaflet({
        
        pal_png_total <- colorBin(palette = "OrRd", domain = PNG_total$PBG, n = 9)
        
        pal_png_agri <- colorBin(palette = "OrRd", domain = PNG_agri$PBG, n = 9)
        
        pal_png_admin <- colorBin(palette = "OrRd", domain = PNG_admin$PBG, n = 9)
        
        pal_png_comercio <- colorBin(palette = "OrRd", domain = PNG_comercio$PBG, n = 9)
        
        pal_png_const <- colorBin(palette = "OrRd", domain = PNG_const$PBG, n = 9)
        
        pal_png_elec <- colorBin(palette = "OrRd", domain = PNG_elec$PBG, n = 9)
        
        pal_png_inmob <- colorBin(palette = "OrRd", domain = PNG_inmob$PBG, n = 9)
        
        pal_png_manu <- colorBin(palette = "OrRd", domain = PNG_manu$PBG, n = 9)
        
        pal_png_minas <- colorBin(palette = "OrRd", domain = PNG_minas$PBG, n = 9)
        
        
        switch(input$filter_pbg,
               "Total" =
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_total, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_total$distrito), 
                                                                                             " | PBG total: ", as.character(format(PNG_total$PBG, big.mark = "."))),
                               fillColor = ~pal_png_total(PNG_total$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_total,
                             values = PNG_total$PBG,
                             title = "PBG - Total"), 
               "Administración pública" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_admin, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_admin$distrito), 
                                                                                             " | PBG de la adm. pública: ", as.character(format(PNG_admin$PBG, big.mark = "."))),
                               fillColor = ~pal_png_admin(PNG_admin$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_admin,
                             values = PNG_admin$PBG,
                             title = "PBG - Adm. pública"), 
               "Agricultura y ganadería" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_agri, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_agri$distrito), 
                                                                                            " | PBG de agri. y ganadería: ", as.character(format(PNG_agri$PBG, big.mark = "."))),
                               fillColor = ~pal_png_agri(PNG_agri$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_agri,
                             values = PNG_agri$PBG,
                             title = "PBG - Agri. y Ganadería"),
               "Comercio" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_comercio, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_comercio$distrito), 
                                                                                                " | PBG por Comercio: ", as.character(format(PNG_comercio$PBG, big.mark = "."))),
                               fillColor = ~pal_png_comercio(PNG_comercio$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_comercio,
                             values = PNG_comercio$PBG,
                             title = "PBG - comercio"), 
               "Construcción" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_const, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_const$distrito), 
                                                                                             " | PBG de la construcción: ", as.character(format(PNG_const$PBG, big.mark = "."))),
                               fillColor = ~pal_png_const(PNG_const$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_const,
                             values = PNG_const$PBG,
                             title = "PBG - Construcción"),
               "Electricidad, gas y agua" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_elec, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_elec$distrito), 
                                                                                            " | PBG de electricidad, gas y agua: ", as.character(format(PNG_elec$PBG, big.mark = "."))),
                               fillColor = ~pal_png_elec(PNG_elec$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_elec,
                             values = PNG_elec$PBG,
                             title = "PBG - Electricidad, gas y agua"), 
               "Serv. inmobiliarios" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_inmob, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_inmob$distrito), 
                                                                                             " | PBG de serv. inmobiliarios: ", as.character(format(PNG_inmob$PBG, big.mark = "."))),
                               fillColor = ~pal_png_inmob(PNG_inmob$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_inmob,
                             values = PNG_inmob$PBG,
                             title = "PBG - Serv. inmobiliarios"),
               "Ind. Manufacturera" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_manu, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_manu$distrito), 
                                                                                            " | PBG de Ind. Manufacturera: ", as.character(format(PNG_manu$PBG, big.mark = "."))),
                               fillColor = ~pal_png_manu(PNG_manu$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_manu,
                             values = PNG_manu$PBG,
                             title = "PBG - Ind. Manufacturera"), 
               "Expl. de minas" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = PNG_minas, weight = 1, color = "black", label = paste0("Municipio: ", as.character(PNG_minas$distrito), 
                                                                                             " | PBG de expl. de minas: ", as.character(format(PNG_minas$PBG, big.mark = "."))),
                               fillColor = ~pal_png_minas(PNG_minas$PBG), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_png_minas,
                             values = PNG_minas$PBG,
                             title = "Poblacion")
               
        )
    })
    
    
    ggplot_png <- reactive({
        site <- input$mappng_shape_click$id
        PNG_graf[PNG_graf$distrito %in% site,]})
    
    output$Gra_pbg <- renderPlot({
        
        pal_plot_pbg <- c('#800026','#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c',
                          '#feb24c', '#fed976', '#ffeda0')
        
        ggplot(data = ggplot_png(), mapping = aes(x = reorder(Tipo, PBG), 
                                                  y = PBG)) +
            geom_bar(stat = "identity", fill = pal_plot_pbg, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Ranking por PBG") + 
            labs(title = req(ggplot_png()$distrito)) + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) + 
            geom_label(
                aes(label = PBG), 
                nudge_x = 0.25, 
                nudge_y = 0.25 
                
            ) 
        
        
    })
    
    ## cantidad de empresas   
    
    output$map_empr <- renderLeaflet({
        
        pal_emp_totales <- colorBin(palette = "OrRd", domain = empresas_totales$empresas, n = 9)
        
        pal_emp_exp <- colorBin(palette = "OrRd", domain = empresas_exp$empresas, n = 9)
        
        switch(input$filter_empresas,
               "Total de empresas" =
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = empresas_totales, weight = 1, color = "black", label = 
                                   paste0(as.character(empresas_totales$distrito),
                                          " | Cantidad de empresas: ", as.character(empresas_totales$empresas)),
                               fillColor = ~pal_emp_totales(empresas_totales$empresas), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_emp_totales,
                             values = empresas_totales$empresas,
                             title = "Cantidad de empresas"), 
               "Empresas exportadoras" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = empresas_exp, weight = 1, color = "black", label = 
                                   paste0(as.character(empresas_exp$distrito),
                                          " | Cantidad de empresas exportadoras: ", as.character(empresas_exp$empresas)),
                               fillColor = ~pal_emp_exp(empresas_exp$empresas), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_emp_exp,
                             values = empresas_exp$empresas,
                             title = "Cantidad de empresas exportadoras")
        )
    })
    
    output$Gra_empre <- renderPlot({
        
        
        pal_plot_emp <- c('#800026','#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c',
                          '#feb24c', '#fed976', '#ffeda0','#ffffcc')
        
        empresas_totales <- empresas_totales %>%
            arrange(desc(empresas)) %>% head(9)
        
        ggplot(data = empresas_totales, mapping = aes(x = reorder(distrito, empresas), 
                                                      y = empresas)) +
            geom_bar(stat = "identity", fill = pal_plot_emp, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Cantidad de empresas")
        
        
    })
    
    output$Gra_empre_ex <- renderPlot({
        
        
        pal_plot_empex <- c('#800026','#bd0026', '#e31a1c', '#fc4e2a', '#fd8d3c',
                            '#feb24c', '#fed976', '#ffeda0','#ffffcc')
        
        empresas_exp <- empresas_exp %>%
            arrange(desc(empresas)) %>% head(9)
        
        ggplot(data = empresas_exp, mapping = aes(x = reorder(distrito, empresas), 
                                                  y = empresas)) +
            geom_bar(stat = "identity", fill = pal_plot_empex, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Cantidad de empresas exportadoras")
        
        
    })
    
    ## Recaudacion por municipio
    
    reca_total <- reactive({
        lv <- recaudacion1 %>% filter(tipo == input$tipo_recau)
        return(lv)
    })
    
    reca_int <- reactive({
        lv <- recaudacion_int %>% filter(tipo == input$tipo_recau)
        return(lv)
    })
    
    output$mapreca <- renderLeaflet({
        
        bin_reca_total <- c(0, 100, 300, 500, 800, 1500, 3000, 5000, 7500)
        
        bin_reca_interior <- c(0, 50, 200, 300, 600, 1000, 2000, 4000, 7000)
        
        pal_reca_interior <- colorBin(palette = "GnBu", domain = recaudacion_int$recaudacion, n = 9)
        
        pal_reca_total <- colorBin(palette = "GnBu", domain = recaudacion1$recaudacion, n = 9)
        
        switch(input$filter_reca,
               "PBA" =
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = reca_total(), weight = 1, color = "black", label = 
                                   paste0(as.character(reca_total()$distrito),
                                          " | Recaudación: ", as.character(reca_total()$recaudacion_la)),
                               fillColor = ~pal_reca_total(reca_total()$recaudacion), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_reca_total,
                             values = recaudacion1$recaudacion,
                             title = "Recaudación en millones de pesos"), 
               "Interior" = 
                   leaflet() %>% 
                   addTiles() %>%
                   addPolygons(data = reca_int(), weight = 1, color = "black", label = 
                                   paste0(as.character(reca_int()$distrito),
                                          " | Recaudación: ", as.character(reca_int()$recaudacion_la)),
                               fillColor = ~pal_reca_interior(reca_int()$recaudacion), 
                               fillOpacity = 0.8, 
                               highlight = highlightOptions(weight = 3, 
                                                            color = "black", 
                                                            bringToFront = T), 
                               layerId = ~distrito) %>%
                   addLegend(position = "bottomright", 
                             pal = pal_reca_interior,
                             values = recaudacion_int$recaudacion,
                             title = "Recaudación en millones de pesos")
        )
    })
    
      ggplot_reca <- reactive({
        site <- input$mapreca_shape_click$id
        recaudacion_graf[recaudacion_graf$distrito %in% site,]})
    
    output$Gra_re <- renderPlot({
        
        pal_plot_re <- c('#084081', '#0868ac', '#2b8cbe', '#4eb3d3', '#7bccc4')
        
        ggplot(data = ggplot_reca(), mapping = aes(x = reorder(tipo, recaudacion), 
                                                   y = recaudacion)) +
            geom_bar(stat = "identity", fill = pal_plot_re, colour = "black") + coord_flip() + theme_classic() + 
            xlab(NULL) + ylab("Ranking por recaudación") + 
            labs(title = req(ggplot_reca()$distrito)) + 
            theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) + 
            geom_label(
                aes(label = recaudacion_la), 
                nudge_x = 0.25, 
                nudge_y = 0.25 
                
            ) 
        
        
    })
    
    
    
    ## coparticipacion   
    
    output$maptran <- renderLeaflet({
        
        tras <- reactive({
            lv <- transferencia_mapa %>% filter(anio == input$fecha_tras)
            return(lv)
        })
        
        pal_trans <- colorBin(palette = "YlOrRd", domain = transferencia_mapa$monto, bins = 9)
        
        
        leaflet() %>% 
            addTiles() %>%
            addPolygons(data = tras(), weight = 1, color = "black", label = 
                            paste0(as.character(tras()$Distrito),
                                   " | Transferencia total: ", as.character(tras()$monto_la)),
                        fillColor = ~pal_trans(tras()$monto), 
                        fillOpacity = 0.8, 
                        highlight = highlightOptions(weight = 3, 
                                                     color = "black", 
                                                     bringToFront = T), 
                        layerId = ~Distrito) %>%
            addLegend(position = "bottomright", 
                      pal = pal_trans,
                      values = transferencia_mapa$monto,
                      title = "Transferencias")
        
    })
    
    observeEvent(input$maptran_shape_click, {
        
        #capture the info of the clicked polygon
        click <- input$maptran_shape_click
        
        #subset your table with the id of the clicked polygon 
        selected <- transferencia_table[transferencia_table$Distrito == click$id,]
        
        #if click id isn't null render the table
        if(!is.null(click$id)){
            output$table_trans = DT::renderDataTable({
                DT::datatable(selected, options = 
                                list(paging = TRUE,scrollX = T, scrollY = T,
                                     autoWidth = T, ordering = F, dom = 'Bfrtip', buttons = c('csv', 'excel'), 
                                     pageLength = 150, 
                                     lengthMenu = c(15,50,100)), 
                              filter = "top", rownames = FALSE, extensions = 'Buttons', selection = 'single')
            })
        } 
    }) 
    
    ### salarios
    
    output$mapsal<- renderLeaflet({
      
      pal_salario <- colorBin(palette = "RdYlGn", domain = salarios_mapa$w_median, n = 9)
      
               leaflet() %>% 
               addTiles() %>%
               addPolygons(data = salarios_mapa, weight = 1, color = "black", label = 
                             paste0(as.character(salarios_mapa$distrito), " | Salario mediano (marzo 2022): ", as.character(salarios_mapa$w_etiq)),
                           fillColor = ~pal_salario(salarios_mapa$w_median), 
                           fillOpacity = 0.8, 
                           highlight = highlightOptions(weight = 3, 
                                                        color = "black", 
                                                        bringToFront = T), 
                           layerId = ~distrito) %>%
               addLegend(position = "bottomright", 
                         pal = pal_salario,
                         values = salarios_mapa$w_median,
                         title = "Salarios mediano - marzo 2022")
            
    })
    
    ggplot_data <- reactive({
      site <- input$mapsal_shape_click$id
      salarios_tabla[salarios_tabla$distrito %in% site,]
    })
    
    output$plot_sal <- renderPlot({
      ggplot(data = ggplot_data(), aes(x = fecha, y = w_median, group = distrito))+
        geom_line(size = 1.5) + geom_point(size = 3) + theme_classic() + 
        xlab("fecha") + ylab("salario promedio") + 
        labs(title = req(ggplot_data()$distrito)) + 
        theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
              axis.title.x = element_text(size = 15, face = "bold"), 
              axis.title.y = element_text(size = 15, face = "bold")) +
        geom_label(
          aes(label = w_etiq), 
          nudge_x = 0.25, 
          nudge_y = 0.25 
          
        )
      
      
    })
    
    
 
}

# Run the application 
shinyApp(ui = ui, server = server)
