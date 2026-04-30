# ============================================================ #
# App Shiny - versão GitHub Pages / Shinylive
# Tendências Recentes de Eventos Climáticos Extremos
# em Áreas de Risco no Brasil
# ============================================================ #

library(shiny)
library(sf)
library(dplyr)
library(readr)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)
library(stringr)

# ------------------------------------------------------------ #
# 1. Ler dados leves
# ------------------------------------------------------------ #

municipios <- readRDS("data/municipios_simplificado.rds")

adapta <- readr::read_csv(
  "data/adapta_brasil_limpo.csv",
  show_col_types = FALSE
) |>
  mutate(
    code_muni = as.character(code_muni)
  )

prec_anual <- readr::read_csv(
  "data/precipitacao_anual.csv",
  show_col_types = FALSE
) |>
  mutate(
    code_muni = as.character(code_muni)
  )

# ------------------------------------------------------------ #
# 2. Juntar mapa com AdaptaBrasil
# ------------------------------------------------------------ #

mapa_municipios <- municipios |>
  mutate(code_muni = as.character(code_muni)) |>
  left_join(adapta, by = "code_muni")

# ------------------------------------------------------------ #
# 3. Níveis e cores da susceptibilidade
# ------------------------------------------------------------ #

niveis_risco <- c(
  "Muito baixo",
  "Baixo",
  "Médio",
  "Alto",
  "Muito alto",
  "Dado indisponível"
)

cores_risco <- c(
  "#2c7bb6",
  "#abd9e9",
  "#ffffbf",
  "#fdae61",
  "#d7191c",
  "#cccccc"
)

mapa_municipios <- mapa_municipios |>
  mutate(
    categoria = factor(categoria, levels = niveis_risco)
  )

pal_risco <- colorFactor(
  palette = cores_risco,
  domain = niveis_risco,
  ordered = TRUE,
  na.color = "#cccccc"
)

# ------------------------------------------------------------ #
# 4. Pré-calcular bbox dos municípios para zoom
# ------------------------------------------------------------ #

bbox_municipios_base <- mapa_municipios |>
  mutate(geom_id = row_number())

bbox_lista <- lapply(seq_len(nrow(bbox_municipios_base)), function(i) {
  
  bb <- st_bbox(bbox_municipios_base[i, ])
  
  data.frame(
    geom_id = bbox_municipios_base$geom_id[i],
    code_muni = bbox_municipios_base$code_muni[i],
    tipo_risco = bbox_municipios_base$tipo_risco[i],
    xmin = as.numeric(bb["xmin"]),
    ymin = as.numeric(bb["ymin"]),
    xmax = as.numeric(bb["xmax"]),
    ymax = as.numeric(bb["ymax"])
  )
})

bbox_municipios <- bind_rows(bbox_lista)

# ------------------------------------------------------------ #
# 5. Interface
# ------------------------------------------------------------ #

ui <- fluidPage(
  
  div(
    style = "
      background-color: #B53930;
      padding: 15px;
      border-radius: 6px;
      position: relative;
      margin-bottom: 15px;
    ",
    
    tags$img(
      src = "logo_ufal.png",
      height = "55px",
      style = "
        position: absolute;
        left: 20px;
        top: 50%;
        transform: translateY(-50%);
      "
    ),
    
    tags$h2(
      "Tendências Recentes de Eventos Climáticos Extremos em Áreas de Risco no Brasil",
      style = "
        color: white;
        text-align: center;
        margin: 0;
        font-family: 'Gill Sans MT', sans-serif;
      "
    )
  ),
  
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 100% !important;
      }

      body {
        font-family: 'Gill Sans MT', sans-serif;
        background-color: #FFFFFF;
      }

      .well {
        background-color: #294A6A !important;
        color: #FFFFFF !important;
        border: none;
      }

      .well label,
      .well h4,
      .well p {
        color: #FFFFFF !important;
      }

      .well .form-control {
        color: #000000;
      }

      .nav-tabs > li > a {
        background-color: #294A6A;
        color: #FFFFFF !important;
        border: none;
      }

      .nav-tabs > li.active > a {
        background-color: #B53930 !important;
        color: #FFFFFF !important;
      }

      .btn {
        background-color: #B53930;
        color: #FFFFFF;
        border: none;
      }

      .btn:hover {
        background-color: #294A6A;
        color: #FFFFFF;
      }
    "))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      h4("Filtros"),
      
      selectInput(
        inputId = "tipo_risco",
        label = "Tipo de risco",
        choices = c(
          "Deslizamento de terra",
          "Inundações, enxurradas e alagamentos"
        ),
        selected = "Inundações, enxurradas e alagamentos"
      ),
      
      selectInput(
        inputId = "categoria",
        label = "Categoria de susceptibilidade",
        choices = c("Todas", niveis_risco),
        selected = "Todas"
      ),
      
      selectInput(
        inputId = "uf",
        label = "UF",
        choices = c("Todas", sort(unique(municipios$abbrev_state))),
        selected = "Todas"
      ),
      
      selectizeInput(
        inputId = "municipio",
        label = "Município",
        choices = NULL,
        selected = "",
        options = list(
          placeholder = "Digite o nome do município...",
          maxOptions = 1000
        )
      ),
      
      hr(),
      
      p("Criado por Jean Souza dos Reis."),
      p("Versão web preliminar com precipitação acumulada anual."),
      p("Os índices ETCCDI/Climdex serão adicionados posteriormente.")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        
        tabPanel(
          "Mapa",
          leafletOutput("mapa", height = "720px")
        ),
        
        tabPanel(
          "Série temporal",
          
          fluidRow(
            column(
              width = 9,
              plotlyOutput("serie", height = "420px")
            ),
            column(
              width = 3,
              h4("Valores da série"),
              DTOutput("tabela_serie")
            )
          ),
          
          br(),
          
          fluidRow(
            column(
              width = 9,
              plotlyOutput("anomalia", height = "420px")
            ),
            column(
              width = 3,
              h4("Valores da anomalia"),
              DTOutput("tabela_anomalia")
            )
          )
        ),
        
        tabPanel(
          "Tabela",
          DTOutput("tabela")
        ),
        
        tabPanel(
          "Sobre",
          
          h3("Sobre o aplicativo"),
          
          p(
            "Este aplicativo apresenta uma versão preliminar do estudo ",
            strong("Tendências Recentes de Eventos Climáticos Extremos em Áreas de Risco no Brasil"),
            "."
          ),
          
          p(
            "O estudo pertence ao projeto ",
            strong("Mapeando o Futuro: Vulnerabilidade Socioeconômica e Ambiental em Face aos Desafios das Mudanças Climáticas"),
            ", processo nº ",
            strong("152529/2024-1"),
            "."
          ),
          
          p(
            "Nesta versão web, são apresentados os dados de precipitação acumulada anual ",
            "por município, integrados aos índices de susceptibilidade do AdaptaBrasil para ",
            "deslizamentos de terra e para inundações, enxurradas e alagamentos."
          ),
          
          p(
            "As séries de precipitação foram agregadas para a escala municipal no período de 1961 a 2025. ",
            "Também são apresentadas anomalias anuais de precipitação calculadas em relação ao período de referência 1980–2010."
          ),
          
          p(
            "Posteriormente, o aplicativo será expandido para incorporar os índices ETCCDI/Climdex de extremos climáticos, ",
            "como PRCPTOT, RX1day, RX5day, R10mm, R20mm, CDD, CWD e SDII."
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------ #
# 6. Servidor
# ------------------------------------------------------------ #

server <- function(input, output, session) {
  
  mapa_filtrado <- reactive({
    
    dados <- mapa_municipios |>
      filter(tipo_risco == input$tipo_risco)
    
    if (input$categoria != "Todas") {
      dados <- dados |>
        filter(categoria == input$categoria)
    }
    
    if (input$uf != "Todas") {
      dados <- dados |>
        filter(abbrev_state == input$uf)
    }
    
    dados
  })
  
  observe({
    
    dados <- mapa_filtrado() |>
      st_drop_geometry() |>
      arrange(abbrev_state, name_muni) |>
      mutate(
        label_muni = paste0(name_muni, " - ", abbrev_state)
      )
    
    choices_muni <- setNames(dados$code_muni, dados$label_muni)
    
    updateSelectizeInput(
      session,
      inputId = "municipio",
      choices = c("Selecionar no mapa" = "", choices_muni),
      selected = "",
      server = TRUE
    )
  })
  
  municipio_selecionado <- reactiveVal(NULL)
  
  observeEvent(input$mapa_shape_click, {
    
    click <- input$mapa_shape_click
    
    municipio_selecionado(click$id)
    
    updateSelectizeInput(
      session,
      inputId = "municipio",
      selected = click$id,
      server = TRUE
    )
  })
  
  observeEvent(input$municipio, {
    
    req(input$municipio)
    
    if (input$municipio != "") {
      
      cod <- input$municipio
      municipio_selecionado(cod)
      
      bbox <- bbox_municipios |>
        filter(
          code_muni == cod,
          tipo_risco == input$tipo_risco
        ) |>
        slice(1)
      
      if (nrow(bbox) == 1) {
        leafletProxy("mapa", session = session) |>
          fitBounds(
            lng1 = bbox$xmin,
            lat1 = bbox$ymin,
            lng2 = bbox$xmax,
            lat2 = bbox$ymax
          )
      }
    }
  })
  
  output$mapa <- renderLeaflet({
    
    dados <- mapa_filtrado()
    
    leaflet(
      dados,
      options = leafletOptions(preferCanvas = TRUE)
    ) |>
      addProviderTiles(providers$CartoDB.PositronNoLabels) |>
      addPolygons(
        layerId = ~code_muni,
        fillColor = ~pal_risco(categoria),
        color = "#333333",
        weight = 0.25,
        fillOpacity = 0.75,
        smoothFactor = 0.7,
        popup = ~paste0(
          "<b>Município:</b> ", name_muni, "<br>",
          "<b>UF:</b> ", abbrev_state, "<br>",
          "<b>Tipo de risco:</b> ", tipo_risco, "<br>",
          "<b>Índice AdaptaBrasil:</b> ", round(susceptibilidade, 2), "<br>",
          "<b>Categoria:</b> ", categoria
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#000000",
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = cores_risco,
        labels = niveis_risco,
        title = "Susceptibilidade",
        opacity = 0.8
      )
  })
  
  dados_serie <- reactive({
    
    cod <- municipio_selecionado()
    
    if (is.null(cod)) {
      return(NULL)
    }
    
    dados <- prec_anual |>
      filter(code_muni == cod)
    
    info <- mapa_municipios |>
      st_drop_geometry() |>
      filter(
        code_muni == cod,
        tipo_risco == input$tipo_risco
      ) |>
      select(
        code_muni,
        tipo_risco,
        susceptibilidade,
        categoria
      )
    
    dados |>
      left_join(info, by = "code_muni")
  })
  
  output$serie <- renderPlotly({
    
    dados <- dados_serie()
    
    validate(
      need(!is.null(dados), "Selecione um município no mapa ou no menu lateral.")
    )
    
    dados_plot <- dados |>
      mutate(
        data = as.Date(paste0(ano, "-01-01")),
        valor = pr_anual
      )
    
    nome_muni <- unique(dados_plot$name_muni)[1]
    uf_muni   <- unique(dados_plot$abbrev_state)[1]
    
    titulo <- paste0(
      nome_muni, " - ", uf_muni,
      " | Precipitação acumulada anual"
    )
    
    p <- ggplot(dados_plot, aes(x = data, y = valor)) +
      geom_col(fill = "#294A6A") +
      labs(
        x = NULL,
        y = "Precipitação acumulada anual (mm)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(p) |>
      layout(
        title = list(
          text = paste0(
            titulo,
            "<br><sup>Dataset: BR-DWGD</sup>"
          )
        ),
        annotations = list(
          list(
            text = "Source: Reis et al. (in preparation).",
            x = 1,
            y = -0.18,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            font = list(size = 11)
          )
        ),
        margin = list(b = 80)
      )
  })
  
  output$anomalia <- renderPlotly({
    
    dados <- dados_serie()
    
    validate(
      need(!is.null(dados), "Selecione um município no mapa ou no menu lateral.")
    )
    
    dados_plot <- dados |>
      mutate(
        data = as.Date(paste0(ano, "-01-01")),
        valor = pr_anual
      )
    
    climatologia <- dados_plot |>
      filter(ano >= 1980, ano <= 2010) |>
      summarise(clima = mean(valor, na.rm = TRUE)) |>
      pull(clima)
    
    dados_anom <- dados_plot |>
      mutate(
        anomalia = valor - climatologia
      )
    
    nome_muni <- unique(dados_anom$name_muni)[1]
    uf_muni   <- unique(dados_anom$abbrev_state)[1]
    
    titulo <- paste0(
      nome_muni, " - ", uf_muni,
      " | Anomalia anual"
    )
    
    p <- ggplot(dados_anom, aes(x = data, y = anomalia, fill = anomalia > 0)) +
      geom_hline(yintercept = 0, linewidth = 0.5) +
      geom_col() +
      scale_fill_manual(
        values = c(
          "TRUE"  = "#B53930",
          "FALSE" = "#294A6A"
        ),
        guide = "none"
      ) +
      labs(
        x = NULL,
        y = "Anomalia de precipitação anual (mm)"
      ) +
      theme_minimal(base_size = 12)
    
    ggplotly(p) |>
      layout(
        title = list(
          text = paste0(
            titulo,
            "<br><sup>Baseline: 1980–2010</sup>"
          )
        ),
        annotations = list(
          list(
            text = "Source: Reis et al. (in preparation).",
            x = 1,
            y = -0.18,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "top",
            font = list(size = 11)
          )
        ),
        margin = list(b = 80)
      )
  })
  
  output$tabela <- renderDT({
    
    dados <- dados_serie()
    
    validate(
      need(!is.null(dados), "Selecione um município para visualizar a tabela.")
    )
    
    datatable(
      dados,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$tabela_serie <- renderDT({
    
    dados <- dados_serie()
    
    validate(
      need(!is.null(dados), "Selecione um município.")
    )
    
    tabela <- dados |>
      transmute(
        Ano = ano,
        `Precipitação acumulada anual (mm)` = round(pr_anual, 2)
      )
    
    datatable(
      tabela,
      options = list(
        pageLength = 8,
        scrollY = "300px",
        scrollX = TRUE,
        dom = "tip"
      ),
      rownames = FALSE
    )
  })
  
  output$tabela_anomalia <- renderDT({
    
    dados <- dados_serie()
    
    validate(
      need(!is.null(dados), "Selecione um município.")
    )
    
    dados_plot <- dados |>
      mutate(valor = pr_anual)
    
    climatologia <- dados_plot |>
      filter(ano >= 1980, ano <= 2010) |>
      summarise(clima = mean(valor, na.rm = TRUE)) |>
      pull(clima)
    
    tabela <- dados_plot |>
      mutate(
        anomalia = valor - climatologia
      ) |>
      transmute(
        Ano = ano,
        `Precipitação acumulada anual (mm)` = round(valor, 2),
        `Climatologia 1980-2010 (mm)` = round(climatologia, 2),
        `Anomalia anual (mm)` = round(anomalia, 2)
      )
    
    datatable(
      tabela,
      options = list(
        pageLength = 8,
        scrollY = "300px",
        scrollX = TRUE,
        dom = "tip"
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)