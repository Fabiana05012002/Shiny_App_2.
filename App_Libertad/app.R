library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "App por tipo de trabajo", 
                  titleWidth = 300),
  
  dashboardSidebar(
    selectInput("pais", "Escoja el país", 
                choices = NULL, selected = NULL),
    
    sliderInput("anio", "Selecciona un año:", 
                min = 2008, max = 2016, value = c(2008, 2016)),
    
    radioButtons("variable", "Elige visualización:", 
                 choices = c("Puntaje", "Ranking"), selected = "Puntaje"),
    
    downloadButton("downloadData", "Descargar datos")
  ),
  dashboardBody(
    tabItem(
      title = "Contenido de la pestaña Gráficos",
      tabName = "grafico",
      tabsetPanel(
        tabPanel("Libertad Humana",
                 br(),
                 plotOutput("plot_libertad_humana")
        ),
        tabPanel("Libertad Personal",
                 br(),
                 plotOutput("plot_libertad_personal")
        ),
        tabPanel("Libertad Económica",
                 br(),
                 plotOutput("plot_libertad_economica")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos_libertad <- read.csv("C:/Users/Fabi Hidalgo/Desktop/CETAV/PROGRAMACIÓN II/2_Shiny_App/datos/datos_libertad.csv")
  
  updateSelectInput(session, "pais", choices = unique(datos_libertad$pais))
  
  observe({
    updateSelectInput(session, "pais", 
                      choices = unique(datos_libertad$pais))
  })
  
  generate_plot <- function(variable, title) {
    req(input$pais, input$anio, input$variable)
    
    selected_variable <- if (input$variable == "Puntaje") {
      
      paste0("libertad_", tolower(variable), "_puntaje")
    } else {
      paste0("libertad_", tolower(variable), "_ranking")
    }
    
    data_filtered <- datos_libertad[datos_libertad$pais == input$pais & 
                                      datos_libertad$anio >= input$anio[1] &
                                      datos_libertad$anio <= input$anio[2], ]
    ggplot(data_filtered, aes(x = anio, y = .data[[selected_variable]])) +
      geom_line(color = "red", size = 2) +
      theme_minimal()
  }
  
  output$plot_libertad_humana <- renderPlot({
    generate_plot("Humana", "Libertad Humana")
  })
  
  output$plot_libertad_personal <- renderPlot({
    generate_plot("Personal", "Libertad Personal")
  })
  
  output$plot_libertad_economica <- renderPlot({
    generate_plot("Economica", "Libertad Económica")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos_", input$pais, "_", input$anio[1], "_", input$anio[2], ".csv", sep = "")
    },
    content = function(file) {
      data_filtered <- datos_libertad[datos_libertad$pais == input$pais & 
                                        datos_libertad$anio >= input$anio[1] &
                                        datos_libertad$anio <= input$anio[2], ]
      write.csv(data_filtered, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)