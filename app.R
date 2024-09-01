library(shiny)
library(flexdashboard)
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
library(bsicons)



# paste(t2$LIB_MOIS[index_valeur],t2$ANNEE[index_valeur]
# card_header("Accueil", class = "bg-light"),

# setwd("/Users/romuanalyst/Documents/Shiny_exercice/dossier sans titre")

source('Manipulation_données.R')

# https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQnwkFfKzrCAvFNJoC7xoflOKYgIPHFIIenolqBo2FO9A&s
# p(h6(paste0("Pour l'année ", unique(filtered_data()$ANNEE), ", le taux d'accompagnements est de ", paste0(round(sum(filtered_data()$Volume_p_charge) / sum(filtered_data()$Volume_orient) * 100, 2), "%")))),


# Titre modifiable :
# card_header(
#   popover(
#     uiOutput("card_title", inline = TRUE),
#   title = "Tu peux changerle titre",
#   textInput("card_title", NULL, "Titre modifiable")
# )
# ),

# output$card_title <- renderUI({
#   list(
#     input$card_title, 
#     bsicons::bs_icon("pencil-square")
#   )
#})


# container <- layout_sidebar(
# class = "p-0",
# sidebar = sidebar(
#   title = "Earthquakes off Fiji",
#   bg = "#1E1E1E",
#   width = "35%",
#   class = "fw-bold font-monospace",
#   filter_slider("mag", "Magnitude", squake, ~mag)
# ),
# leaflet(squake) |> addTiles() |> addCircleMarkers()
# )



orientation <- readRDS("datas/df_orientation.rds")
max_valeur <- max(diamonds$price)

texte_intro <- "France Travail, anciennement connu sous le nom de Pôle emploi, est une institution française axée sur les services d'emploi. Elle vise à mieux accompagner les demandeurs d'emploi dans leur recherche et à aider les entreprises dans le recrutement. France Travail joue un rôle essentiel dans l'écosystème de l'emploi, notamment en fournissant des conseils et un soutien aux demandeurs d'emploi, en particulier aux cadres. L'institution s'engage à faciliter les opportunités d'emploi et à favoriser un environnement propice aux demandeurs d'emploi et aux employeurs.  <br>
	•	L'Apec propose des offres d'emploi pour les cadres, offrant des conseils et un accompagnement à chaque étape de leur vie professionnelle. Vous pouvez trouver des offres d'emploi cadre sur le site officiel de l'Apec, ainsi que sur l'application mobile dédiée, permettant aux candidats de rechercher des opportunités correspondant à leur profil. L'Apec s'engage à soutenir les cadres dans leur recherche d'emploi et à les aider à progresser dans leur carrière professionnelle.  
Collaboration entre France Travail et APEC :
	•	Partenariat Renforcé : Pôle emploi (France Travail) et l'APEC ont renforcé leur collaboration pour relever de manière efficace les défis de l'emploi. Leur partenariat inclut des initiatives communes et des efforts coordonnés pour soutenir les demandeurs d'emploi et promouvoir les opportunités d'emploi, en particulier pour les cadres. L'expertise régionale de l'APEC complète le focus local de France Travail, permettant une approche globale pour répondre aux besoins divers des demandeurs d'emploi et des employeurs. 
	•	Soutien aux Demandeurs d'Emploi : La collaboration entre Pôle emploi (France Travail) et l'APEC comprend des initiatives telles que l'événement #TousMobilisés dédié aux jeunes demandeurs d'emploi. Ces efforts conjoints visent à fournir un soutien spécialisé et des ressources aux demandeurs d'emploi, facilitant leur accès aux opportunités d'emploi et au développement professionnel. En tirant parti de leur expertise et de leurs ressources combinées, Pôle emploi et l'APEC peuvent offrir une gamme plus large de services et de programmes pour aider les individus dans leur recherche d'emploi et leur croissance professionnelle.  
En résumé, France Travail (Pôle emploi) et l'APEC collaborent pour renforcer l'efficacité des services d'emploi, soutenir les demandeurs d'emploi et promouvoir le développement économique. Leur partenariat allie l'expertise locale et régionale pour répondre aux besoins divers des demandeurs d'emploi et des employeurs, favorisant un environnement propice à l'emploi et à l'avancement professionnel."

heure <-   value_box(
  title = h5("Date et heure :"),
  value = h4(textOutput("time")),
  showcase = bs_icon("clock")
)


data_filter <- div(
  selectInput("year_filt2", h5("Année(s) à afficher :"), 
              choices = unique(copie_table$ANNEE), 
              selected = unique(copie_table$ANNEE)[length(unique(copie_table$ANNEE))], 
              multiple = TRUE))

value_filter <- div(
  selectInput("year_filter", h5("Année à afficher :"), 
              choices = unique(copie_table$ANNEE), 
              selected = unique(copie_table$ANNEE)[length(unique(copie_table$ANNEE))], 
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
(card_image(file = "image/accueil_image.png", height = 600)
),
card_footer(code('romu@nalyst'), style = "text-align: right;"))
  
sidebar_quakes <- layout_sidebar(
  sidebar = value_filter,
  uiOutput("boxes"),map_quakes
)

sidebar_diamonds <- layout_sidebar(
  sidebar = heure,
  card(card_header("Présentation"), card_body(texte_intro),card_footer(code('romu@nalyst'), style = "text-align: right;"))
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
    "Orientations et Accompagnements"
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
        
        h4{
          text-align: center;
          font-weight: bold;
          font-family: Times New Roman;
          color: blue;
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
  
  output$time <- renderText({
    invalidateLater(1000)
    format(Sys.time())
  })
  
  
  taux <- flexdashboard::renderGauge({
    flexdashboard::gauge((sum(filtered_data()$Accompagnés) / sum(filtered_data()$Orientés)*100), min = 0, max = 100, symbol = '%', label = paste("Taux"),
    flexdashboard::gaugeSectors(success = c(0.55, 1), warning = c(0.4,0.54), danger = c(0, 0.39), colors = c("#CC6699")
                         ))})
  
  
  
  filtered_data <- reactive({
    copie_table[copie_table$ANNEE %in% input$year_filter, ]
  })
  
  output$boxes <- renderUI({
    boxes <- layout_column_wrap(
      value_box(
        title = "Orientations", 
        value = format(sum(filtered_data()$Orientés), big.mark = " "), 
        theme = "bg-gradient-cyan-red",
        showcase = graph_line, showcase_layout = "top right",
        full_screen = TRUE, fill = FALSE, height = NULL
      ),
      value_box(
        title = "Accompagnements", 
        value = format(sum(filtered_data()$Accompagnés), big.mark = " "), 
        theme = "bg-gradient-cyan-red",
        showcase = graph_line2, showcase_layout = "top right",
        full_screen = TRUE, fill = FALSE, height = NULL
      ),
      value_box(
        title = "", 
        value = "% acc.",
        theme = "bg-gradient-blue-cyan",
        showcase = taux, showcase_layout = "top right",
        full_screen = FALSE, fill = FALSE, height = NULL
      )
    )
  })
  
  graph_line <- renderPlotly({
    plot_ly(filtered_data()) %>%
      add_bars(
        x = ~date, y = ~Orientés,
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
        x = ~date, y = ~Accompagnés,
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
        reactable(data = copie_table[table_join$ANNEE %in% input$year_filt2, ])
      })})
}


shinyApp(ui, server)
