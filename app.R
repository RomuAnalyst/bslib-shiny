
library(bslib)
library(shiny)
library(crosstalk)
library(plotly)
library(leaflet)
library(apexcharter)
library(lorem)
library(shinyWidgets)
library(dplyr)
library(shinythemes)
library(sf)
library(reactable)



# paste(t2$LIB_MOIS[index_valeur],t2$ANNEE[index_valeur]
# card_header("Accueil", class = "bg-light"),

# setwd("/Users/romuanalyst/Documents/Shiny_exercice/dossier sans titre")

#source('Manipulation_données.R')
orientation <- readRDS("datas/df_orientation.rds")
max_valeur <- max(diamonds$price)



graph_line <- plot_ly(t) %>%
  add_trace(
    x = ~date, y = ~Volume,
    color = I("white"), span = I(1),
    fill = 'tozeroy', alpha = 0.2
  ) %>%
  layout(
    xaxis = list(visible = F, showgrid = F, title = ""),
    yaxis = list(visible = F, showgrid = F, title = ""),
    hovermode = "x",
    margin = list(t = 0, r = 0, l = 0, b = 0),
    font = list(color = "white"),
    paper_bgcolor = "transparent",
    plot_bgcolor = "transparent"
  ) %>%
  config(displayModeBar = F) %>%
  htmlwidgets::onRender(
    "function(el) {
      var ro = new ResizeObserver(function() {
         var visible = el.offsetHeight > 200;
         Plotly.relayout(el, {'xaxis.visible': visible});
      });
      ro.observe(el);
    }"
  )

boxes <- layout_column_wrap(
  width = "200px",
  class = "mt-3",
  value_box(
    title = "Unemployment Rate",
    value = "2.7%",
    p("Started at 1.5%"),
    p("Averaging 3%"),
    p("Peaked at 5.2% in Dec 1982"),
    showcase = graph_line,
    showcase_layout = showcase_left_center(),
    theme = "success",
    full_screen = TRUE
  ),
  value_box(
    title = "Personal Savings Rate",
    value = "7.6%",
    p("Started at 12.6%"),
    p("Averaging 8.6%"),
    p("Peaked at 17.3% in May 1975"),
    showcase = graph_line,
    showcase_layout = showcase_bottom(),
    full_screen = TRUE,
    theme = "success"
  ),
  value_box(
    title = "Personal Consumption",
    value = "$3.8B",
    p("Started at $0.25B"),
    p("Averaging $1.7B"),
    showcase = bsicons::bs_icon("piggy-bank", size = "100%"),
    full_screen = TRUE,
    theme = "danger"
  )
)


data_filter <- div(
  selectInput("year_filter", "Année(s) à afficher :", 
              choices = unique(t$ANNEE), 
              selected = unique(t$ANNEE)[length(unique(t$ANNEE))], 
              multiple = TRUE))




# Creates the "filter link" between the controls and plots
dat <- SharedData$new(dplyr::sample_n(diamonds, 1000))
test <- uiOutput("dat")
# Sidebar elements (e.g., filter controls)
filters <- color_by <- varSelectInput(
  "color_by", "Color by",
  penguins[c("species", "island", "sex")],
  selected = "species"
)


# map filter and visual
quake_dat <- SharedData$new(quakes)
map_filter <- filter_slider("mag", "Magnitude", quake_dat, ~mag)
map_quakes <- leaflet(quake_dat) |>
  addTiles() |>
  addCircleMarkers()

accueil <- card(div(
  layout_columns(img(card_image(file = "https://romuanalyst.github.io/Website2/img/Avatar.png", height = 600)))
),
card_footer(div(code('romu@nalyst'), style = "text-align: right;")))
  
sidebar_quakes <- layout_sidebar(
  sidebar = map_filter,
  boxes,map_quakes
)

sidebar_diamonds <- layout_sidebar(
  sidebar = color_by,
  renderPlot(gg_plot)
)

sidebar_accueil <-
  accueil

sidebar_data <- layout_sidebar(
  sidebar = data_filter,h5("Base de données"),
  uiOutput("page_data")
)

ui <- page_navbar(
  title = "Application Shiny",
  tags$head(
    tags$style(HTML("

        /* Style pour les SelectInput */
        .select_input{
          outline: 6px solid white;
          border-radius: 8px;
          margin-bottom: 30px;
        }
        
        h2, h3{
          color: white;
        }
        h1, h2, h3{
          text-align: center;
          font-weight: bold;
          font-family: Times New Roman;
          color: purple;
        }
        h6, h5{
          color: purple;
          text-align: center;
          font-weight: bold;
        }
        "
    )),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  nav_spacer(),
  nav_panel("Accueil", sidebar_accueil),
  nav_panel("Introduction", sidebar_diamonds),
  nav_panel("Visualisation", sidebar_quakes),
  nav_panel("Données", sidebar_data)
)

server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) +
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  })
  
  output$unemploy <- renderPlotly({
    plot_ly(dat) |> add_histogram(x = ~price)
  })
  
  output$page_data <- renderUI({
      output$tableau <- renderReactable({
        reactable(data = t[t$ANNEE %in% input$year_filter, ])
      })})
}


shinyApp(ui, server)