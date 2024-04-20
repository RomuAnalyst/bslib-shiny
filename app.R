library(ggplot2)
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
library(palmerpenguins)



# paste(t2$LIB_MOIS[index_valeur],t2$ANNEE[index_valeur]
# card_header("Accueil", class = "bg-light"),

# setwd("/Users/romuanalyst/Documents/Shiny_exercice/dossier sans titre")

# source('Manipulation_données.R') https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQnwkFfKzrCAvFNJoC7xoflOKYgIPHFIIenolqBo2FO9A&s
# p(h6(paste0("Pour l'année ", unique(filtered_data()$ANNEE), ", le taux d'accompagnements est de ", paste0(round(sum(filtered_data()$Volume_p_charge) / sum(filtered_data()$Volume_orient) * 100, 2), "%")))),



orientation <- readRDS("datas/df_orientation.rds")
max_valeur <- max(diamonds$price)






data_filter <- div(
  selectInput("year_filt2", h5("Année(s) à afficher :"), 
              choices = unique(t2$ANNEE), 
              selected = unique(t2$ANNEE)[length(unique(t2$ANNEE))], 
              multiple = TRUE))

value_filter <- div(
  selectInput("year_filter", h5("Année à afficher :"), 
              choices = unique(t2$ANNEE), 
              selected = unique(t2$ANNEE)[length(unique(t2$ANNEE))], 
              multiple = FALSE))


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

accueil <- card(
(card_image(file = "image/image_fond.png", height = 600)
),
card_footer(div(code('romu@nalyst'), style = "text-align: right;")))
  
sidebar_quakes <- layout_sidebar(
  sidebar = value_filter,
  uiOutput("boxes"),map_quakes
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
  title = tags$span(
    tags$img(
      src = "https://d2q79iu7y748jz.cloudfront.net/s/_squarelogo/256x256/eb15c068e7e8df02348f84371d67ba06",
      width = "36px",
      height = "auto",
      class = "me-3",
      alt = "AccOrient"
    ),
    "Orientations et Prises en charge"
  ),
  setBackgroundImage(
    src = "https://wallpapers.com/images/hd/abstract-blueish-white-color-nrvpjoky2673bptv.jpg"),
  tags$head(
    tags$style(HTML("

        /* Style pour les SelectInput */
        .select_input{
          outline: 6px solid white;
          border-radius: 8px;
          margin-bottom: 30px;
        }
        
        
        code{
          color: purple;
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
          color: lightblue;
          text-align: center;
          font-family: Times New Roman;
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
  
  taux <- flexdashboard::renderGauge({
    flexdashboard::gauge((sum(filtered_data()$Volume_p_charge) / sum(filtered_data()$Volume_orient)*100), min = 0, max = 100, symbol = '%', label = paste("Taux d'accompagnements"),
                         flexdashboard::gaugeSectors(success = c(0.55, 1), warning = c(0.4,0.54), danger = c(0, 0.39), colors = c("#CC6699")
                         ))})
  
  
  
  filtered_data <- reactive({
    t2[t2$ANNEE %in% input$year_filter, ]
  })
  
  output$boxes <- renderUI({
    boxes <- layout_column_wrap(
      value_box(
        title = "Orientations", 
        value = format(sum(filtered_data()$Volume_orient), big.mark = " "), 
        theme = "bg-gradient-cyan-red",
        showcase = graph_line, showcase_layout = "top right",
        full_screen = TRUE, fill = FALSE, height = NULL
      ),
      value_box(
        title = "Accompagnements", 
        value = format(sum(filtered_data()$Volume_p_charge), big.mark = " "), 
        theme = "bg-gradient-cyan-red",
        showcase = graph_line2, showcase_layout = "top right",
        full_screen = TRUE, fill = FALSE, height = NULL
      ),
      value_box(
        title = "", 
        value = "Taux d'acc.",
        theme = "bg-gradient-green-red",
        showcase = taux, showcase_layout = "top right",
        full_screen = FALSE, fill = FALSE, height = NULL
      )
    )
    boxes
  })
  
  graph_line <- renderPlotly({
    plot_ly(filtered_data()) %>%
      add_bars(
        x = ~date, y = ~Volume_orient,
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
  })

  graph_line2 <- renderPlotly({
    plot_ly(filtered_data()) %>%
      add_trace(
        x = ~date, y = ~Volume_p_charge,
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
  })
  
  
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
        reactable(data = t2[t$ANNEE %in% input$year_filt2, ])
      })})
}


shinyApp(ui, server)
