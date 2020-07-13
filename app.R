library(shiny)
library(jpeg)
library(magrittr)
library(scales)
library(shinybusy)

ui <- fluidPage(title = "k-means litapallettudót",
  fluidRow(
  column(4,
         h2(NULL),
         fileInput("file",  "Hér getur þú hlaðið upp .jpeg mynd",
                   accept = c(".jpg", ".jpeg")),
         sliderInput("centers", "Fjöldi lita", min = 1, max = 20, value = 8, step = 1),
         sliderInput("iterations", "Ítranir í reikniriti", min = 5, max = 30, value = 20,
                     step = 5),
         actionButton("reikna", "Reikna")),
  column(7,
         h2("Notaðu k-means klösunarreiknirit til að búa til litapallettu út frá .jpeg mynd"),
         h4("Sýndu banananum þolinmæði. Þetta getur tekið örfáar sekúndur."),
         plotOutput("litir_ut")
  ),
  column(2,
         use_busy_gif(
           src = "https://jeroen.github.io/images/banana.gif",
           height = 70, width = 70
         ))
  )
)

server <- function(input, output, session) {
  mynd_inn <- reactive({
    req(input$file)

    ext <- tools::file_ext(input$file$name)
    switch(ext,
           jpg  = jpeg::readJPEG(source = input$file$datapath),
           jpeg = jpeg::readJPEG(source = input$file$datapath),
           validate("Úps, getur verið að þetta sé hvorki .jpg né .jpeg mynd?")
    )
  })



  mynd_rgb <- reactive({

    dimension <- dim(mynd_inn())

    data.frame(
    x = rep(1:dimension[2], each = dimension[1]),
    y = rep(dimension[1]:1, dimension[2]),
    R = as.vector(mynd_inn()[,,1]), #slicing our array into three
    G = as.vector(mynd_inn()[,,2]),
    B = as.vector(mynd_inn()[,,3])
    )
  })

  kjarnar <- eventReactive(input$reikna, {
    play_gif()
    x <- kmeans(mynd_rgb()[,c("R","G","B")],
           centers = input$centers,
           iter.max = input$iterations)
    stop_gif()
    x
    })

  output$litir_ut <- renderPlot({

    show_col(rgb(kjarnar()$centers))

  })


}

shinyApp(ui, server)
