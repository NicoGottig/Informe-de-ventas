library(shiny)
library(ggplot2)
library(dplyr)
library(Metrics)
library(DT)

data <- readxl::read_excel("DATOS-APP.xlsx", sheet = "Hoja 1")


ui <- fluidPage(
  
  tabsetPanel(
    
    tabPanel("Datos Originales",
             h3("Seleccioná con qué datos querés trabajar"),
             p("Las opciones disponibles", em(" actualmente "), " son las correspondientes a los ejercicios del libro de cátedra."),
             hr(),
             column(6, br(), fileInput('file1', 'Cargar datos manuamente (no disponible)',
                                  accept = c(".xlsx"))),
             
             column(6, selectInput("select", label = h3("Seleccionar variable"), 
                                    choices = colnames(data), 
                                    selected = 1)),
             
             # actionButton("manualInput", "Agregar Datos Manualmente"),
             
             # Por alguna razón no carga los datos manaules
             
             # modalDialog(
             #   textInput("manualInputText", "Ingrese valores separados por coma:"),
             #   footer = tagList(
             #     actionButton("addManualData", "Agregar"),
             #     modalButton("Cerrar")
             #   )
             # ),
             hr(),
             plotOutput("histograma_original"),
             hr(),
             hr(),
             DTOutput("tabla_original")),
    
    tabPanel("Suma",
             sliderInput("constante", "Selecciona una constante para sumar a los datos:",
                         min = -3, max = 3, step = 0.1, value = 1),
             column(3,
                    plotOutput("boxplot_suma")),
             column(9,
                    plotOutput("histograma_suma")),
             DTOutput("tabla_suma")),
    
    tabPanel("Multiplicación",
             sliderInput("multiplicador", "Selecciona una constante para multiplicar los datos:",
                         min = 0, max = 3, step = 0.2, value = 1),
             column(3,
                    plotOutput("boxplot_multiplicacion")),
             column(9,
                    plotOutput("histograma_multiplicacion")),
             DTOutput("tabla_multiplicacion")),
    
    tabPanel("Suma y Multiplicación",
             numericInput("multiplicador_ambos", "Introduce una constante para multiplicar:", 2),
             sliderInput("constante_ambos", "Selecciona una constante para sumar al resultado anterior:",
                         min = -3, max = 3, step = 0.1, value = 1),
             column(3,
                    plotOutput("boxplot_ambos")),
             column(9,
                    plotOutput("histograma_ambos")),
             
             DTOutput("tabla_ambos"))
  )
)


server <- function(input, output) {
  
  datos_problemas <- reactive({
    
    tmp <- select(data, input$select) 
    
    tmp_limpio <- filter(tmp, !is.na(tmp[[1]]))
    
    return(tmp_limpio)
    
  })
  
  variable_normal <- reactive({
    rnorm(1000, mean = 10, sd = 1)
  })
  
  # Calcula la suma y genera el histograma para la primera pestaña
  suma <- reactive({
    suma_result <- datos_problemas()[[1]] + input$constante
    return(suma_result)
  })
  
  # Unir conjunto de datos
  ambos <- reactive({
    df <- NULL
    df <- data.frame(datos = c(datos_problemas()[[1]], suma()))
    tmp <- rep("original", length(datos_problemas()[[1]]))
    tmp2 <- rep("transformado", length(datos_problemas()[[1]]))
    df$tipo <- c(tmp, tmp2)
    return(df)
  })
  
  output$histograma_suma <- renderPlot({
    ggplot(ambos(), aes(x=datos, fill=tipo)) +
      geom_histogram(color="#e9ecef", alpha=0.9, position = 'identity') +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  
  output$histograma_original <- renderPlot({
    ggplot() + aes(x = datos_problemas()[[1]]) +
      geom_histogram(fill= "dodgerblue3", col = "white") +
      theme_minimal() +
      labs(fill="") +
      xlab("X")
  })
  output$boxplot_suma <- renderPlot({
    ggplot(ambos(), aes(y=datos, fill=tipo)) +
      geom_boxplot(color="black", alpha=1) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  output$tabla_original <- renderDT({
    
    tabla <- data.frame(Medida = c("Media", "Mediana", "Q3", "SD", "RIQ"),
                        Original = round(c(mean(datos_problemas()[[1]]),
                                           median(datos_problemas()[[1]]),
                                           quantile(datos_problemas()[[1]], .75),
                                           sd(datos_problemas()[[1]]),
                                           IQR(datos_problemas()[[1]])),2)                        )
    datatable(tabla,  options = list(dom = 't'))
  })
  output$tabla_suma <- renderDT({
    datos_originales <- summary(datos_problemas()[[1]])
    datos_transformados <- summary(suma())
    tabla <- data.frame(Medida = c("Media", "Mediana", "Q3", "SD", "RIQ"),
                        Original = round(c(mean(datos_problemas()[[1]]),
                                           median(datos_problemas()[[1]]),
                                           quantile(datos_problemas()[[1]], .75),
                                           sd(datos_problemas()[[1]]),
                                           IQR(datos_problemas()[[1]])),2),
                        Transformado = round(c(mean(suma()),
                                               median(suma()),
                                               quantile(suma(), .75),
                                               sd(suma()),
                                               IQR(suma())),2))
    datatable(tabla)
  })
  
  # Calcula la multiplicación y genera el histograma para la segunda pestaña
  multiplicacion <- reactive({
    multiplicacion_result <- datos_problemas()[[1]] * input$multiplicador
    return(multiplicacion_result)
  })
  
  ambos2 <- reactive({
    df <- NULL
    df <- data.frame(datos = c(datos_problemas()[[1]], multiplicacion()))
    tmp <- rep("original", length(datos_problemas()[[1]]))
    tmp2 <- rep("transformado", length(datos_problemas()[[1]]))
    df$tipo <- c(tmp, tmp2)
    return(df)
  })
  
  output$boxplot_multiplicacion <- renderPlot({
    ggplot(ambos2(), aes(y=datos, fill=tipo)) +
      geom_boxplot(color="black", alpha=1) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  
  output$histograma_multiplicacion <- renderPlot({
    ggplot(ambos2(), aes(x=datos, fill=tipo)) +
      geom_histogram(color="white", alpha=1, position = "identity") +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  
  
  #
  # output$histograma_multiplicacion <- renderPlot({
  #   ggplot(data.frame(x = multiplicacion()), aes(x)) +
  #     geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  #     labs(title = "Histograma de la Multiplicación")
  # })
  
  output$tabla_multiplicacion <- renderDT({
    datos_originales <- summary(datos_problemas()[[1]])
    datos_transformados <- summary(multiplicacion())
    tabla <- data.frame(Medida = c("Media", "Mediana", "Q3", "SD", "RIQ"),
                        Original = round(c(mean(datos_problemas()[[1]]),
                                           median(datos_problemas()[[1]]),
                                           quantile(datos_problemas()[[1]], .75),
                                           sd(datos_problemas()[[1]]),
                                           IQR(datos_problemas()[[1]])),2),
                        Transformado = round(c(mean(multiplicacion()),
                                               median(multiplicacion()),
                                               quantile(multiplicacion(), .75),
                                               sd(multiplicacion()),
                                               IQR(multiplicacion())),2))
    datatable(tabla)
  })
  
  
  # Calcula la suma y multiplicación y genera el histograma para la tercera pestaña #VER
  suma_y_multiplicacion <- reactive({
    multiplicacion_result <-  datos_problemas()[[1]]* input$multiplicador_ambos
    suma_result <- multiplicacion_result + input$constante_ambos
    
    return(suma_result)
  })
  ambos3 <- reactive({
    df <- NULL
    df <- data.frame(datos = c(datos_problemas()[[1]], suma_y_multiplicacion()))
    tmp <- rep("original", length(datos_problemas()[[1]]))
    tmp2 <- rep("transformado", length(datos_problemas()[[1]]))
    df$tipo <- c(tmp, tmp2)
    return(df)
  })
  output$boxplot_ambos <- renderPlot({
    ggplot(ambos3(), aes(y=datos, fill=tipo)) +
      geom_boxplot(color="black", alpha=1) +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  output$histograma_ambos <- renderPlot({
    ggplot(ambos3(), aes(x=datos, fill=tipo)) +
      geom_histogram(color="white", alpha=1, position = "identity") +
      scale_fill_manual(values=c("#69b3a2", "#404080")) +
      theme_minimal() +
      labs(fill="")
  })
  
  
  output$tabla_ambos <- renderDT({
    datos_originales <- summary(datos_problemas()[[1]])
    datos_transformados <- summary(suma_y_multiplicacion())
    tabla <- data.frame(Medida = c("Media", "Mediana","Q3", "SD", "RIQ"),
                        Original = round(c(mean(datos_problemas()[[1]]),
                                           median(datos_problemas()[[1]]),
                                           quantile(datos_problemas()[[1]], .75),
                                           sd(datos_problemas()[[1]]),
                                           IQR(datos_problemas()[[1]])),),
                        Transformado = round(c(mean(suma_y_multiplicacion()),
                                               median(suma_y_multiplicacion()),
                                               quantile(suma_y_multiplicacion(), .75),
                                               sd(suma_y_multiplicacion()),
                                               IQR(suma_y_multiplicacion())),2))
    datatable(tabla)
  })
}


shinyApp(ui, server)