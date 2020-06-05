# librerias
library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)
library(scales)
library(RColorBrewer)
library(readxl)
library(shiny)
library(DT)
library(rlang)
library(tools)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

# datasets para mostrar en el principio en una sola tabla con tabs
tab1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
tab2 <- read_csv("poblacion.csv", skip = 3)

# día de hoy
hoy <- format(Sys.time(), "%Y-%m-%d")

#tible con las fechas desde 1 enero hasta día de hoy 
fechas_2020 <-
  tibble(fecha = seq.Date(
    from = ymd("2020-01-01"),
    to = ymd(hoy), by = "1 day"
  ))


#theme_set(theme_bw())

# leemos dataframe de infectados y muertos covid
data_covid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) %>%
  select("fecha" = dateRep,
    "casos_nuevos" = cases,
    "muertes_nuevas" = deaths,
    "region" = countriesAndTerritories,
    "geo_id" = geoId) %>%
  mutate(region = case_when( 
    region == 'Congo' ~ 'Republic of Congo',
    region == 'Cote dIvoire' ~ 'Ivory Coast',
    region == 'Brunei Darussalam' ~ 'Brunei',
    region == 'Czechia' ~ 'Czech Republic', 
    region == 'North Macedonia' ~ 'Macedonia',
    region == 'Serbia' ~ 'Republic of Serbia',
    region == region ~ region
  ))

data_covid$fecha <- str_replace_all(data_covid$fecha,"/","-")
data_covid$fecha <- as.Date(data_covid$fecha, "%d-%m-%Y")

#procesamos

data_covid <- data_covid %>%
  group_by(geo_id) %>%
  arrange(fecha, .by_group = TRUE) %>%
  ungroup()

data_covid_by_geo_id <- data_covid %>%
  split(data_covid$geo_id)

funcion_auxiliar <- function(x) {
  x %>%
    right_join(fechas_2020, by = "fecha") %>%
    fill(c(region, geo_id), .direction ='downup') %>%
    mutate(casos_nuevos = ifelse(is.na(casos_nuevos), 0, casos_nuevos), muertes_nuevas = ifelse(is.na(muertes_nuevas), 0, muertes_nuevas)) %>%
    mutate(casos_acumulados = cumsum(casos_nuevos), muertes_acumuladas = cumsum(muertes_nuevas)) %>%
    #  filter(casos_acumulados > 0) %>%
    mutate(dia = row_number())
}

data_covid_cleaned <- map_df(data_covid_by_geo_id, funcion_auxiliar)


#########################################
# primer procesado pobalcion
poblacion <- read_csv("poblacion.csv", skip = 3) %>%
  select(
    "pob_mill" = Data,
    "geo_id" = FIPS,
    "tipo" = Type,
    'region' = Name
  ) %>%
  filter(tipo == "Country") %>%
  select(
    -tipo
  ) %>%
  mutate(pob_raw = pob_mill * 1000)

# segundo procesado
#Gran poblema! hay alguno paises en los que sus geo ID es diferente en ambos conjuntos ej United kindom est'a identificado como UK y GB
datos_combinados <- data_covid_cleaned %>%
  inner_join(poblacion, "geo_id")

arreglar <- setdiff(data_covid_cleaned$geo_id, datos_combinados$geo_id)
auxiliando <- function(x){
  poblacion[poblacion$region == toTitleCase(str_replace((data_covid_cleaned %>%filter(geo_id==x))[1, 4], "_", " ")), 2] <<- x
  
}

#poblacion[poblacion$region == toTitleCase(str_replace((data_covid_cleaned %>%filter(geo_id=='UK'))[1, 4], "_", " ")), 2] <- 'UK'


map(arreglar, auxiliando)

poblacion %<>%
  select(-region)

datos_combinados <- data_covid_cleaned %>%
  inner_join(poblacion, "geo_id") %>%
  mutate(casos_por_mil_habitantes = casos_acumulados / pob_raw, muertes_por_mil_habitante = muertes_acumuladas / pob_raw) %>%
  pivot_longer(-c(fecha, region, geo_id, dia, pob_mill, pob_raw), names_to = "tipo", values_to = "valor") %>%
  mutate_at(c("region", "tipo"), function(x) toTitleCase(str_replace_all(x, "_", " ")))


dia_hoy <-
  datos_combinados %>%
  filter(geo_id == "ES" & fecha == as.Date(hoy)) %>%
  head(1) %>%
  pull(dia)


# tema para usar en plot 

tema_plot <-
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "#eeeeee", color = NA)
  )

# dataframe para dibujar el mundo con geom_sf
world <- ne_countries(scale = "medium", returnclass = "sf")

#victimas covid + polydata
aux <- world %>%
  full_join(datos_combinados, by=c("sovereignt"="region"))




# ui
ui <- basicPage(
  titlePanel(
    h1(tags$b("COVID-19 😷"), align = "center")
    , windowTitle = "COVID-19 😷"),
  sidebarLayout(
    sidebarPanel(h2("Acerca de este proyecto", align = "center"),
                 p("Este proyecto ha sido realizado por Roberto Carlos Saavedra Baylon, puedes encontrar el código fuente de esta R Shiny App en Github:", align = "justify"),
                 tags$div(
                   tags$a(
                     href = "https://github.com/robertosaavedr",
                     tags$img(
                       src = "https://lh3.googleusercontent.com/MZfEdZ9Zw1n20vcsptOpwUDxixw7gS8aibMzLglsJt2HO7YOyd40g8-6eO8fyQsLyq3aniHlO1hpFOMdh_uoCHfGOYTngYtHGcHsGU46CGhy6JVvJZq37TFP4ZjxFRMyu3GEZbIW_IamzDt13YozFbZoBNpvQ2xpJvlwMNDRWt4Q-LKhg20AppdZXGO3d20YDDv4WThpBf5I68UxkvLvQeIh8APtdkSUwbTlYrjUBAA1OXXo-6BMo7rQlqLE2kdwQH__rpzfklWPbhxD-2j6-16V5DD1x7_d0yG4vZtqZeQ6cERI409qtGAqB1MkSWM8F7OpHF6tK-oSJ5CHbD32tCMLNpuJyWWEIugiWArmaqKFg9UBQQ-nf7qH391JlqZL1liMUjE7qyRPt2aZq2lepAJsI1ZIvqiBIGyyj9Uec_qhvD99b4MM3-eddQVv6TvXYnceQJN7zzUOjbTfSxtV59W3J8l-_hwr-O3pI5_xk2hOrU3R1CzgIMTWZPyaPvPVGZ7qQCwhxONPtKrG2sRtkVQ5o6v7l3uP_73WKDill1Xe0D8_X62eJZ3jHx5qxuWiCQzOp2tS0u-gDfX8UbLZsQE5hUNEXkvnrph7-rhizu2Eem9dIbxvwDLJI9aSgiZjipCaA9opo53LEw5NisrtdzRC0-MkYbMZnUFP7rU3SEBQhakZHCRW1gLUUKCKhQ=w800-h665-no?authuser=0",
                       title = "Github de Roberto Saavedra",
                       width = "40%",
                       height = "40%"
                       # style="display: block; margin-left: auto; margin-right: auto;")
                     ), target = "_blank"
                   ),
                   align = "center"
                 ), br(), 
                 width = 2
    ),
    mainPanel(
      p("Ante la situación que el mundo está viviendo surgen un gran número de interrogantes con respecto a las cifras de muertes y contagiados que se comparten de manera diaria por las autoridades oficiales,
              ¿Cómo se deben interpretar dichas cifras? Una acción recomendable sería la comparación de las mismas métricas a lo largo de todos los países del mundo, y eso es precisamente lo que se ha intentado hacer aquí, aunque es evidente que quedan algunas dudas con respecto a la fiabilidad de estos datos, donde el factor decisivo para causar dichas dudas son las distintas metodologías que se están llevando a cabo a la hora de recoger los datos, el proceso llevado a cabo intenta hacer el mejor análisis posible a pesar de las limitaciones existentes y con las que es difícil lidiar. Considerando lo anterior, aquí encontrarás un registro de las muertes y casos producidos por Coranavirus desde enero hasta día de hoy, se han introducido variables interesantes como las muertes y los casos acumulados, así como los casos y muertes cada mil habitantes. Uno de los aspectos más interesantes es que puedes visualizar dichas métricas(casos acumulados, muertes nuevas, muertes cada mil habitantes...) para los países de tu elección, además al final de la página encontrarás un mapa resumen donde se plasman dichas métricas. Eres libre de interactuar con las tablas, gráficos y mapas de está presentación.", style = "font-size:20px", align = "justify")
    )
  ),
  br(),
  p("Las cifras de personas contagiadas y  muertas por coronavirus se obtienen del", tags$a(
    href = "https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
    tags$b("Centro Europeo para la Prevención y Control Enfermedades."), target = "_blank"
  ), "Los datos poblacionales para cada paises han sido obtenidos del", tags$a(
    href = "https://datacenter.prb.org/download/international/indic",
    tags$b("Population Reference Bureau."), target = "_blank"
  ),  style = "font-size:20px", align = "justify"),
  
  br(),
  
  tabsetPanel(
    id = "dataset",
    tabPanel("Muertos e Infectados", DT::dataTableOutput("data_covid")),
    tabPanel("Población", DT::dataTableOutput("poblacion"))
  ),
  br(),
  h2("Depuración de datos"),
  p("Utilizando los dos conjuntos de datos anteriores se han conseguido para cada país, desde el primer día de contagio y hasta día de hoy, los casos y muertes 
    ocurridos en dicho día, así como los casos y muertes acumuladas hasta el día correspondiente, en la tabla de debajo vemos identificada la métrica a la que nos referimos bajo
    la variable", tags$em("tipo") ,"y su valor en la columna", tags$em("valor."), "Se han añadido las variables", tags$em("dia, "),"días sucedidos desde el primer contagio,", tags$em("pob_mill"), ", población en millones
    y",  tags$em('pob_raw'), "población en miles.", style = "font-size:20px", align = "justify"),
  br(),
  
  DT::dataTableOutput("combinados"),
  
  br(),
  
  h2("Visualizando la situación"),
  br(),
  sidebarLayout(
    sidebarPanel(
      radioButtons("fecha_dia", label = h3("Seleccione la escala del eje Y"), choices = list("Mes" = "fecha", "Día" = "dia"), selected = "fecha"),
      radioButtons("today", label = h3("Fecha actual"), choices = list("Si" = 2, "No" = 0), selected = 0)
    ),
    
    
    
    
    
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  checkboxGroupInput("paises", label = h3(""), 
                     choiceNames = as.list(unique(datos_combinados$region)),
                     choiceValues = as.list(unique(datos_combinados$geo_id)), inline = TRUE, selected = c("ES", "DE", "IT", "BE", "NZ")),
  br(),
  sidebarLayout(
    sidebarPanel(
      dateInput("fecha", label = h3("Fecha"), value = hoy),
      radioButtons("tipo", label = h3("Metrica"), choices = list("Casos Nuevos" = "Casos Nuevos", "Muertes Nuevas" = "Muertes Nuevas",
                                                                 "Casos Acumulados" = "Casos Acumulados", "Muertes Acumuladas" = "Muertes Acumuladas",
                                                                 "Casos Por Mil Habitantes" = "Casos Por Mil Habitantes", "Muertes Por Mil Habitante" = "Muertes Por Mil Habitante"),
                   selected = "Casos Nuevos")
      
    ),
    mainPanel(
      plotOutput('mundo')
    )
  )
)

#server
server <- function(input, output) {
  output$data_covid <- DT::renderDataTable({
    tab1
  })
  
  output$poblacion <- DT::renderDataTable({
    tab2
  })
  
  output$combinados <- DT::renderDataTable(datos_combinados)
  
  output$distPlot <- renderPlot({
    x <- parse_quo(input$fecha_dia, env = caller_env())
    aux <- ifelse(input$fecha_dia == "dia", "Días desde el primer caso", "Mes")
    
    
    datos_combinados %>%
      dplyr::filter(geo_id %in% input$paises) %>%
      ggplot(aes(!!x, valor, color = region)) +
      geom_line(size = .7) +
      facet_wrap(facets = vars(tipo), scales = "free_y") +
      scale_y_continuous(expand = c(0, 0), labels = comma_format()) +
      scale_color_manual(name = "País", values = brewer.pal(length(input$paises), "Set1")) +
      tema_plot +
      theme(legend.position = "top") +
      labs(x = aux, y = "Conteo") +
      geom_vline(xintercept = dia_hoy, color = "red", lty = as.integer(input$today))
  })
  
  output$mundo <- renderPlot({
    aux %>%
      filter(fecha==input$fecha, tipo==input$tipo) %>%
      ggplot() +
      geom_sf(aes(fill=valor)) +
      labs(fill='Cifras') +
      theme(legend.position = 'top', legend.key.width=unit(4,"cm")) +
      scale_fill_viridis_c(trans = "log", alpha = .4) 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
