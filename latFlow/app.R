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
library(shiny)
library(tidyverse)
library(terra)
library(tidyterra)
library(whitebox)
library(ggnewscale)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Travel distance for perched lateral flow, W3"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
             sliderInput("ex",
                          "Minimum Drainage area (m^2):",
                          min = 80,
                          max = 300,
                          value = 80),
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
                        value = 0.2)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")#,
           #textOutput("contributingArea")
           
        )
    )
)

#use input$Cn etc.

# Define server logic required to draw a histogram
server <- function(input, output) {
  #distance to stream
  o <- eventReactive(input$ex,{
    uaa2 <- ifel(rast("hydem10mlpns_uaa_streams.tif") >= input$ex, 1, NA)
    writeRaster(uaa2, "uaa_thresholded.tif", overwrite = TRUE)
    wbt_downslope_distance_to_stream(dem = "hydem1mlpns_wsbound.tif",
                                     streams = "uaa_thresholded.tif",
                                     output = "hydem1mlpns_downdist.tif")
    rast("hydem1mlpns_downdist.tif")})

  #make plot
    output$distPlot <- renderPlot({
      #read in stuff needed for plotting
      w3_outline <- vect("10m_shedbound.shp")
      w3_shed <- "hydem1mlpns_shed.tif"
      
      
       
        #output already in degrees
        rads <- rast("hydem1mlpns_avgflowslope.tif") * pi/180
        
        #travel distance
        Lt <- input$KuKl * (sin(rads)/((input$N + input$Cn)/input$Cn)) * input$N
        
        o <- o() #rast("hydem1mlpns_downdist.tif")
        #if o is reactive, stream network does not work. 
        #If o is not reactive, contrib area does not adjust to new network
        x <- ifel(o <= Lt, 1, NA)
        uaa2 <- ifel(rast("hydem10mlpns_uaa_streams.tif") >= input$ex, 1, NA)
        
        contributing <- ifel(x == uaa2, 2, x)
        
        #classify output
        cls <- c("contributing hillslope", "stream")
        df <- data.frame(id = 1:2, class=cls)
        levels(contributing) <- df
        contributing <- mask(contributing, x)

        # draw the histogram with the specified number of bins
        ggplot()+
          geom_spatraster(data = drop_na(contributing), aes(fill = class), alpha = 0.7)+
          scale_fill_manual(values = c("lightblue", "purple"),
                            na.translate=FALSE)+
          theme_void()+
          geom_sf(data = w3_outline, fill = NA, color = "black", alpha = 0.3, lwd = 1)+
          theme(rect = element_rect(fill = "transparent", color = NA),
                legend.title=element_blank())#+
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
#shinylive::export(appdir = "latFlow", destdir = "docs")

