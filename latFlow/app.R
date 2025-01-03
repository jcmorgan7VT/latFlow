#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
#install.packages(c("shinylive", "httpuv"))

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
                          min = 8000,
                          max = 30000,
                          value = 8000),
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
                        min = 0,
                        max = 0.35,
                        value = 0.2)
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
    
    output$distPlot <- renderPlot({
      #read in stuff needed for plotting
      w3_outline <- vect("10m_shedbound.shp")
        #hill <- rast("./req/hydem1mlpns_hill.tif")
      thresh <- input$ex / 100
        
        #create stream network extent based on user input
        w3_flowacc <- "hydem1mlpns_flowacc.tif"
        w3_streams <- "hydem10mlpns_streams.tif"
        wbt_extract_streams(flow_accum = w3_flowacc,
                            output = w3_streams,
                            threshold = thresh)
        
        #calculate downslope dsitance
        bound_dem <- "hydem1mlpns_wsbound.tif"
        w3_downdist <- "hydem1mlpns_downdist.tif"
        wbt_downslope_distance_to_stream(dem = bound_dem,
                                         streams = w3_streams,
                                         output = w3_downdist)
        
        ##calculate average flowpath slope
        w3_avgflowslope <- "hydem1mlpns_avgflowslope.tif"
        #output already in degrees
        rads <- rast(w3_avgflowslope) * pi/180
        
        #calculate the travel distance
        ##calculate Lt based on slope and input values
        #input parameters
        Ku_Kl <- input$KuKl
        N <- input$N #meters
        Cn <- input$Cn
        
        #caluclate Lt, or travel distance of lateral water flux
        Lt <- Ku_Kl * (sin(rads)/((N + Cn)/Cn)) * N
        
        o <- rast(w3_downdist)
        
        
        #compare downslope distance to travel length
        x <- ifel(o <= Lt, 1, NA)
        w3_shed <- "hydem1mlpns_shed.tif"
        streams <- rast(w3_streams) %>% 
          mask(rast(w3_shed))
        activated <- ifel(x == streams, 2, x)
        
        #classify output
        cls <- c("activated hillslope", "stream")
        df <- data.frame(id = 1:2, class=cls)
        levels(activated) <- df
        activated <- mask(activated, x)

        # draw the histogram with the specified number of bins
        ggplot()+
          # geom_spatraster(data = hill)+
          # theme(legend.position = "")+
          # scale_fill_gradientn(colors = c("gray9", "gray48","lightgray", "white"), guide = 'none')+
          # new_scale_fill() +
          geom_spatraster(data = drop_na(activated), aes(fill = class), alpha = 0.7)+
          scale_fill_manual(values = c("lightblue", "purple"),
                            na.translate=FALSE)+
          theme_void()+
          geom_sf(data = w3_outline, fill = NA, color = "black", alpha = 0.3, lwd = 1)+
          theme(rect = element_rect(fill = "transparent", color = NA),
                legend.title=element_blank())
    })
}

#does not work locally, but works remotely
# Run the application 
shinyApp(ui = ui, server = server)
#shinylive::export(appdir = "latFlow", destdir = "docs")

