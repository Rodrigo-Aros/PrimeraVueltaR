library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)


# Cargar datos
datos <- read.csv("resultado_candidatos.csv", stringsAsFactors = FALSE)
datos$id_mesa <- as.character(datos$id_mesa)
datos$id_local <- as.character(datos$id_local)

# Filtrar columnas relevantes
datos <- datos %>%
  select(region, comuna, id_local, local, id_mesa, candidato, sigla_partido, votos, total_emitidos, electores, nulos, blancos) %>%
  filter(!is.na(region), !is.na(comuna), !is.na(candidato), !is.na(sigla_partido), !is.na(votos))

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(textOutput("titulo_region")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "I. Selecciona una Región:", choices = sort(unique(datos$region))),
      uiOutput("candidato_ui"),
      uiOutput("comuna_ui"),
      conditionalPanel(
        condition = "input.tabs == '2. Distribución por Local'",
        uiOutput("local_ui")
      ),
      downloadButton("descargar_grafico", "Descargar gráfico actual")
    ),
    
    mainPanel(
      fluidRow(
        column(3, h4("Total Votos Región:"), textOutput("total_votos")),
        column(3, h4("Participación Región:"), textOutput("participacion_promedio")),
        column(3, h4("Nulos Región:"), textOutput("total_nulos")),
        column(3, h4("Blancos Región:"), textOutput("total_blancos"))
      ),
      
      tabsetPanel(id = "tabs",
                  tabPanel("1. Comparación por Comuna",
                           plotlyOutput("grafico_comunas"),
                           br(),
                           h4("Nulos y Blancos por Comunas(s) Seleccionada(s)"),
                           plotlyOutput("grafico_torta")
                  ),
                  tabPanel("2. Distribución por Local", plotlyOutput("grafico_local")),
                  tabPanel("3. Participación por Local", plotlyOutput("participacion")),
                  tabPanel("4. Distribución por Partido", plotlyOutput("grafico_partido")),
                  tabPanel("5. Participación vs Votos", plotlyOutput("scatter_participacion")),
                  tabPanel("6. Ranking Votos", DT::dataTableOutput("tabla_ranking"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Actualizar candidatos según región seleccionada
  output$candidato_ui <- renderUI({
    req(input$region)
    candidatos_region <- datos %>%
      filter(region == input$region) %>%
      pull(candidato) %>%
      unique() %>%
      sort()
    selectizeInput("candidatos", "II. Selecciona candidato(s):", choices = candidatos_region, multiple = TRUE)
  })
  
  # Actualizar comunas según región seleccionada
  output$comuna_ui <- renderUI({
    req(input$region)
    comunas_region <- datos %>%
      filter(region == input$region) %>%
      pull(comuna) %>%
      unique() %>%
      sort()
    selectizeInput("comunas", "III. Selecciona hasta 10 comunas:", choices = comunas_region, multiple = TRUE, options = list(maxItems = 10))
  })
  
  # ✅ Actualizar locales según comunas seleccionadas (id_local como valor)
  output$local_ui <- renderUI({
    req(input$comunas)
    
    locales_filtrados <- datos %>%
      filter(comuna %in% input$comunas) %>%
      select(id_local, local) %>%
      distinct() %>%
      arrange(local)
    
    selectInput(
      inputId = "locales",
      label = "Selecciona Locales",
      choices = setNames(locales_filtrados$id_local, locales_filtrados$local),
      multiple = TRUE
    )
  })
  
  # Título dinámico
  output$titulo_region <- renderText({
    paste("Data Electoral - Región:", input$region)
  })
  
  # Función para ajustar leyenda abajo
  ajustar_leyenda <- function(plot) {
    plot %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.2))
  }
  
  # Indicadores dinámicos (a nivel regional)
  observe({
    datos_region <- datos %>%
      filter(region == input$region) %>%
      group_by(id_mesa) %>%
      summarise(
        votos_validos = sum(votos, na.rm = TRUE),
        nulos = first(nulos),
        blancos = first(blancos),
        total_emitidos = first(total_emitidos),
        electores = first(electores),
        .groups = "drop"
      )
    
    output$total_votos <- renderText({
      sum(datos_region$votos_validos, na.rm = TRUE)
    })
    
    output$participacion_promedio <- renderText({
      participacion <- ((sum(datos_region$total_emitidos, na.rm = TRUE) +
                           sum(datos_region$nulos, na.rm = TRUE) +
                           sum(datos_region$blancos, na.rm = TRUE)) /
                          sum(datos_region$electores, na.rm = TRUE)) * 100
      paste0(round(participacion, 2), "%")
    })
    
    output$total_nulos <- renderText({
      sum(datos_region$nulos, na.rm = TRUE)
    })
    
    output$total_blancos <- renderText({
      sum(datos_region$blancos, na.rm = TRUE)
    })
  })
  
  
  # ✅ Gráfico Comparación por Comuna
  output$grafico_comunas <- renderPlotly({
    req(input$region, input$comunas)
    
    datos_filtrados <- datos %>%
      filter(region == input$region,
             comuna %in% input$comunas,
             if(length(input$candidatos) > 0) candidato %in% input$candidatos else TRUE) %>%
      group_by(candidato, comuna) %>%
      summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(datos_filtrados) +
      aes(x = reorder(candidato, votos), y = votos, fill = comuna,
          text = paste("Comuna:", comuna, "<br>Votos:", votos)) +  # ✅ Etiqueta personalizada
      geom_col(position = "dodge") +
      scale_fill_viridis_d(option = "inferno", direction = 1) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total de votos por Comuna(s) seleccionada(s)",
           x = "", y = "", fill = "Comuna")
    
    ajustar_leyenda(ggplotly(p, tooltip = "text"))  # ✅ Mostrar solo la etiqueta personalizada
  })
  
  
  
  # ✅ Gráfico de torta corregido
  
  output$grafico_torta <- renderPlotly({
    req(input$comunas)
    
    datos_torta <- datos %>%
      filter(comuna %in% input$comunas) %>%
      group_by(id_mesa) %>%
      summarise(
        votos_validos = sum(votos, na.rm = TRUE),  # suma por mesa
        nulos = first(nulos),                      # valor único por mesa
        blancos = first(blancos),                  # valor único por mesa
        .groups = "drop"
      )
    
    # Sumar totales
    total_votos <- sum(datos_torta$votos_validos, na.rm = TRUE)
    total_nulos <- sum(datos_torta$nulos, na.rm = TRUE)
    total_blancos <- sum(datos_torta$blancos, na.rm = TRUE)
    
    # Crear dataframe para la torta
    df_torta <- data.frame(
      categoria = c("Votos Válidos", "Nulos", "Blancos"),
      valor = c(total_votos, total_nulos, total_blancos)
    )
    
    # Forzar que no haya NA
    df_torta$valor <- ifelse(is.na(df_torta$valor), 0, df_torta$valor)
    
    plot_ly(df_torta, labels = ~categoria, values = ~valor, type = "pie",
            textinfo = "label+percent", insidetextorientation = "radial",
            sort = FALSE) %>%
      layout(title = "",
             legend = list(orientation = "h", x = 0.3, y = -0.2))
  })
  
  
  
  
  # ✅ Distribución por Local (id_local como valor)
  output$grafico_local <- renderPlotly({
    req(input$locales)
    
    datos_local <- datos %>%
      filter(id_local %in% input$locales) %>%
      group_by(candidato) %>%
      summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(datos_local) +
      aes(x = reorder(candidato, votos), y = votos, fill = candidato,
          text = paste("Votos:", votos)) +  # ✅ Etiqueta personalizada
      geom_col() +
      scale_fill_viridis_d(option = "inferno", direction = 1) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Total de votos por candidato (Locales seleccionados)",
           x = "Candidato", y = "Votos") +
      guides(fill = "none")
    
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)  # ✅ Mostrar solo votos
  })
  

  
  
  # 3. Participación por Local
  output$participacion <- renderPlotly({
    req(input$locales)
    
    datos_part <- datos %>%
      filter(id_local %in% input$locales) %>%
      group_by(id_mesa, id_local, local) %>%
      summarise(
        total_emitidos = first(total_emitidos),
        nulos = first(nulos),
        blancos = first(blancos),
        electores = first(electores),
        .groups = "drop"
      ) %>%
      group_by(local) %>%
      summarise(participacion = ((sum(total_emitidos) + sum(nulos) + sum(blancos)) / sum(electores)) * 100) %>%
      mutate(
        etiqueta = paste0(format(round(participacion, 2), decimal.mark = ","), " %")  # ✅ Etiqueta con coma y %
      )
    
    p <- ggplot(datos_part) +
      aes(x = reorder(local, participacion), y = participacion, text = etiqueta) +  # ✅ Usamos etiqueta en tooltip
      geom_col(fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Participación por local", x = "Local", y = "% Participación")
    
    ggplotly(p, tooltip = "text")  # ✅ Tooltip muestra etiqueta formateada
  })
  
  
  
  
  
  
  # 4. Distribución por Partido (una sola barra apilada con el total)
  output$grafico_partido <- renderPlotly({
    req(input$comunas)
    
    # Sumar votos por partido en todas las comunas seleccionadas
    datos_partido <- datos %>%
      filter(comuna %in% input$comunas) %>%
      group_by(sigla_partido) %>%
      summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>%
      mutate(grupo = "TOTAL")  # etiqueta única
    
    # Gráfico apilado (una sola barra)
    p <- ggplot(datos_partido) +
      aes(x = grupo, y = votos, fill = sigla_partido,
          text = paste(sigla_partido, "=", votos)) +  # ✅ Etiqueta personalizada
      geom_col(position = "stack") +
      scale_fill_viridis_d(option = "magma") +
      theme_minimal() +
      labs(title = "Distribución total de votos por partido",
           x = "", y = "Votos")
    
    ajustar_leyenda(ggplotly(p, tooltip = "text"))  # ✅ Mostrar solo la etiqueta personalizada
  })
  
  
  
  
  # 5. Scatter Plot Participación vs Votos
  output$scatter_participacion <- renderPlotly({
    req(input$locales)
    
    datos_scatter <- datos %>%
      filter(id_local %in% input$locales) %>%
      group_by(id_mesa, id_local, local, candidato, sigla_partido) %>%
      summarise(
        votos = sum(votos, na.rm = TRUE),
        total_emitidos = first(total_emitidos),
        nulos = first(nulos),
        blancos = first(blancos),
        electores = first(electores),
        .groups = "drop"
      ) %>%
      mutate(participacion = ((total_emitidos + nulos + blancos) / electores) * 100)
    
    validate(
      need(nrow(datos_scatter) > 0, "No hay datos para los locales seleccionados")
    )
    
    p <- ggplot(datos_scatter) +
      aes(x = participacion, y = votos, color = sigla_partido,
          text = paste("Participación:", round(participacion, 2), "%",
                       "<br>Votos:", votos)) +  # ✅ Etiqueta definida
      geom_point(size = 4, alpha = 0.7) +
      scale_color_viridis_d(option = "plasma") +
      theme_minimal() +
      labs(title = "Relación entre Participación y Votos",
           x = "Participación (%)", y = "Votos")
    
    ajustar_leyenda(ggplotly(p, tooltip = "text"))  # ✅ tooltip correcto
  })
  
  
  # Ranking de votos por comuna, local y candidato
  output$tabla_ranking <- DT::renderDataTable({
    req(input$region)
    
    datos_ranking <- datos %>%
      filter(region == input$region,
             if(length(input$comunas) > 0) comuna %in% input$comunas else TRUE,
             if(length(input$locales) > 0) id_local %in% input$locales else TRUE,
             if(length(input$candidatos) > 0) candidato %in% input$candidatos else TRUE) %>%
      group_by(comuna, local, candidato) %>%
      summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(votos)) %>%
      mutate(Ranking = row_number()) %>%
      select(Ranking, comuna, local, candidato, votos)
    
    DT::datatable(datos_ranking,
                  options = list(pageLength = 10,
                                 autoWidth = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel')),
                  extensions = 'Buttons',
                  rownames = FALSE)
  })
  
  
  
  # Descargar gráfico actual (pestaña 1)
  output$descargar_grafico <- downloadHandler(
    filename = function() {
      paste0("grafico_comunas_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Filtrar datos igual que el gráfico original
      datos_filtrados <- datos %>%
        filter(region == input$region,
               comuna %in% input$comunas,
               if(length(input$candidatos) > 0) candidato %in% input$candidatos else TRUE) %>%
        group_by(candidato, comuna) %>%
        summarise(votos = sum(votos, na.rm = TRUE), .groups = "drop")
      
      # Crear el mismo gráfico que se muestra en la pestaña 1
      p <- ggplot(datos_filtrados) +
        aes(x = reorder(candidato, votos), y = votos, fill = comuna) +
        geom_col(position = "dodge") +
        scale_fill_viridis_d(option = "inferno", direction = 1) +
        coord_flip() +
        theme_minimal() +
        labs(title = "Total de votos por Comuna(s) seleccionada(s)",
             x = "", y = "", fill = "Comuna")
      
      # Guardar imagen
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
}

shinyApp(ui, server)