library(shiny)
library(bslib)
library(palmerpenguins)
library(ggplot2)

ui <- page_fillable(
  card(
    card_header(
      textOutput("time"),
      tooltip(
        bsicons::bs_icon("question-circle"),
        "Mass measured in grams.",
        placement = "right"
      ),
      popover(
        bsicons::bs_icon("gear", class = "ms-auto"),
        selectInput("yvar", "Split by", c("sex", "species", "island")),
        selectInput("color", "Color by", c("species", "island", "sex"), "island"),
        title = "Plot settings"
      ),
      class = "d-flex align-items-center gap-1"
    ),  value_box(
      title = "Heure actuelle",
      value = textOutput("time"),
      showcase = bs_icon("clock"),
    ),
    plotOutput("plt"),
    card_footer(
      textOutput("time")
      
      )
    )
  )


server <- function(input, output, session) {
  
  output$time <- renderText({
    invalidateLater(1000)
    format(Sys.time())
  })
  
  output$plt <- renderPlot({
    ggplot(penguins, aes(x = body_mass_g, y = !!sym(input$yvar), fill = !!sym(input$color))) +
      ggridges::geom_density_ridges(scale = 0.9, alpha = 0.5) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      ggokabeito::scale_fill_okabe_ito() +
      theme_minimal(base_size = 20) +
      theme(legend.position = "top")
  })
}

shinyApp(ui, server)