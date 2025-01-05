#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages(c("shinylive", "httpuv"))
#install.packages("reactlog")
#library(reactlog)
library(pacman)
p_load(shiny, tidyverse, terra, tidyterra, ggnewscale)
#whitebox::install_whitebox()
# library(shiny)
# library(tidyverse)
# library(terra)
# library(tidyterra)
# library(whitebox)
# library(ggnewscale)

#read in data
w3_outline <- vect("10m_shedbound.shp")
hillshade <- rast("1mdem_hillshade.tif")
soil <- rast("soilCrop.tif")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Travel distance for perched lateral flow, W3"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("uaa", "Minimum Drainage area (m^2)",
                       c("80" = 80,
                       "200" = 200,
                       "300" = 300)),
            sliderInput("KuKl",
                        "Ratio of Hydraulic Conductivities:",
                        min = 10,
                        max = 600,
                        value = 300),
            sliderInput("Cn",
                        "Impeding layer thickness (m):",
                        min = 2.8,
                        max = 4.5,
                        value = 3.2),
            sliderInput("N",
                        "Saturated thickness (m):",
                        min = 0.1,
                        max = 0.35,
                        value = 0.2),
            radioButtons("back", "Background",
                         c("none" = "none",
                           "hillshade" = "hillshade",
                           "soil model" = "soil_model"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
           
        )
    )
)

#use input$Cn etc.

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    o <- reactive(rast(paste0(input$uaa,"_downdist.tif")),
                  label = "Read downdist")
    stream <- reactive(rast(paste0(input$uaa,"_stream.tif")),
                       label = "Read stream")
    rads <- reactive(rast(paste0(input$uaa,"_flowslope.tif")) * pi/180,
                     label = "Read flowslope")
    Lt <- reactive(input$KuKl * (sin(rads()/((input$N + input$Cn)/input$Cn)) * input$N),
                   label = "Calc Lt")
    #make another reactive for next step
    r <-  reactive(ifel(o() <= Lt(), 1, NA),
                   label = "threshold")
    
    # con <-  reactive({mask(r(), r())},
    #                  label = "mask")
    #classify output
    
    # contributing <- reactive({
    #   cls <- c("contributing hillslope", "stream")
    #   df <- data.frame(id = 1:2, class=cls)
    #   levels(con()) <- df
    #   mask(con(), r())
    #   
    # })
    
    #reactive intermediate steps
    
  #make plot
    output$distPlot <- renderPlot({
      
         # if(input$uaa == 80){
         #   rads <- rast("80_flowslope.tif") * pi/180
         #   o <- rast("80_downdist.tif")
         #   streams <- rast("80_stream.tif")
         #   Lt <- input$KuKl * (sin(rads)/((input$N + input$Cn)/input$Cn)) * input$N
         #   
         # } else if(input$uaa == 200) {
         #   rads <- rast("200_flowslope.tif") * pi/180
         #   o <- rast("200_downdist.tif")
         #   streams <- rast("200_stream.tif")
         #   Lt <- input$KuKl * (sin(rads)/((input$N + input$Cn)/input$Cn)) * input$N
         #   
         # } else if(input$uaa == 300) {
         #   rads <- rast("300_flowslope.tif") * pi/180
         #   o <- rast("300_downdist.tif")
         #   streams <- rast("300_stream.tif")
         #   Lt <- input$KuKl * (sin(rads)/((input$N + input$Cn)/input$Cn)) * input$N
         #   
         #  }
        #output already in degrees
        # rads <- rast(paste0(input$uaa,"_flowslope.tif")) * pi/180
        # o <- rast(paste0(input$uaa,"_downdist.tif")) 
        # streams <- rast(paste0(input$uaa,"_stream.tif"))
        # 
  
       #Lt <- input$KuKl * (sin(rads)/((input$N + input$Cn)/input$Cn)) * input$N
       
       #x <- ifel(o <= Lt, 1, NA)
        
        #contributing <- ifel(x == streams, 2, x)
        # r <-  ifel(o() <= Lt(), 1, NA)
        # 
        # con <-  ifel(r == stream(), 2, r)
        # #classify output
       #  cls <- c("contributing hillslope", "stream")
       #  df <- data.frame(ID = 1:2, class=cls)
       # # contributing <- mask(con(), r())
       #  contributing <- categories(con(), layer=1, df)
       
      #this simple plot works
      #   plot(r(), col = "lightblue", alpha = 0.7, box = FALSE, axes = FALSE, legend= FALSE)
      # plot(stream(), add = TRUE, col = "purple", alpha = 0.7, legend = FALSE)
      # plot(w3_outline, add = TRUE)
      #add conditional background
      if(input$back == "none"){
        plot(r(), col = "lightblue", alpha = 0.7, box = TRUE, axes = FALSE, legend= FALSE)
        plot(stream(), add = TRUE, col = "purple", alpha = 0.7, legend = FALSE)
        plot(w3_outline, add = TRUE)
        
        } else if(input$back == "hillshade") {
          plot(hillshade, axes = FALSE, legend= FALSE, col = hcl.colors(20, palette = "Grays"))
          plot(r(), add = TRUE, col = "lightblue", alpha = 0.7, box = TRUE, axes = FALSE, legend= FALSE)
          plot(stream(), add = TRUE, col = "purple", alpha = 0.7, legend = FALSE)
          plot(w3_outline, add = TRUE)

        } else if(input$back == "soil_model") {
          plot(soil, axes = FALSE)
          plot(r(), add = TRUE, col = "lightblue", alpha = 0.7, box = TRUE, axes = FALSE, legend= FALSE)
          plot(stream(), add = TRUE, col = "purple", alpha = 0.7, legend = FALSE)
          plot(w3_outline, add = TRUE)

         }
        # draw the histogram with the specified number of bins
        # ggplot()+
        #   geom_spatraster(data = drop_na(con()), fill = "lightblue", alpha = 0.7)+
        #   geom_spatraster(data = drop_na(stream()), fill = "purple", alpha = 0.7)+
        #   scale_fill_manual(values = c("lightblue", "purple"),
        #                     na.translate=FALSE)+
        #   theme_void()+
        #   geom_sf(data = w3_outline, fill = NA, color = "black", alpha = 0.3, lwd = 1)+
        #   theme(rect = element_rect(fill = "transparent", color = NA),
        #         legend.title=element_blank())#+
    })
    #output$raster <- renderText(con())
}

# Run the application 
shinyApp(ui = ui, server = server)
#shinylive::export(appdir = "latFlow", destdir = "docs")

#reactlog::reactlog_enable()
#shiny::reactlogShow()

#works perfectly!!
#things to do:
# add 1 m and 3 m resolution calculations
