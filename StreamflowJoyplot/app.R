# Lauren Steely, August 2017

library(shiny)
library(dplyr)
library(dataRetrieval)
library(lubridate)
library(ggjoy)
library(RColorBrewer)
library(leaflet)
library(shinythemes)
library(WaterML)
library(tools)

# Define UI for application
ui <- fluidPage(theme=shinytheme("sandstone"),
   
   titlePanel("JoyFlow"),
   helpText("Lauren Steely, ", a("@MadreDeZanjas", href="https://twitter.com/MadreDeZanjas"), " | ",
            a("Code available on Github", href="https://github.com/codeswitching/Streamflow-Joyplot-Tool")),
   fluidRow(
     column(width=4,
            selectizeInput("sitesInput", "Enter a USGS gauging station site ID:",
                        c("Colorado R" = "09380000", "Rio Grande" = "08319000", "Columbia R" = "14105700",
                          "Sacramento R" = "11425500"), options = list(create = TRUE))
     ),
     column(width=2,
       actionButton("button", "Make joyful")
     )
   ),
   tags$style(type='text/css', "#button { width:100%; margin-top: 25px;}"),
   helpText("Examples | Rio Grande: 08319000 | Colorado R: 09380000 | Columbia R: 14105700 | Sacramento R: 11425500 | Klamath R: 11530500"),
   helpText(a("Find gauging stations", href="https://maps.waterdata.usgs.gov/mapper/index.html"), " on a map"),

   hr(),
   
   fluidRow(
    column(width=4,
     sliderInput("scaleInput", "Add joy:", 0, 11, 4, 1)
    ),
    column(width=3,
     radioButtons("radioInput", "Fill color:", choices =
                    c("by year (pretty)" = "byyear", "by total annual discharge (shows dry vs wet years)" = "bydischarge"))
    ),
    column(width=3,
     radioButtons("radioInput2", "x axis:", choices =
                c("calendar year" = "calendar", "water year" = "water"))
    )
   ),

   plotOutput("distPlot", height="700px"),

   hr(),
   
   leafletOutput("map"),
   
   hr()
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$button, {
     withProgress(message = "Downloading data from NWIS...", value=0.2, {
       water_year <- function(date) {
         ifelse(month(date) < 10, year(date)-1, year(date))
       }
       selectedSite <- input$sitesInput
       server <- "http://hydroportal.cuahsi.org/nwisdv/cuahsi_1_1.asmx?WSDL"
       sitelocation <- GetSiteInfo(server, paste0("NWISDV:", selectedSite))
       variable <- "00060" # Discharge in cfs
       COdischarge_raw <- readNWISdv(selectedSite, variable, "1901-01-01", today()) # download discharge data from USGS
       setProgress(message="Plotting...", value=0.6)
       COdischarge_raw %>%
         rename(cfs = X_00060_00003, date = Date) %>%
         mutate(year = year(date), wyear = water_year(date)) %>%
         filter(cfs >= 0) %>%
         group_by(year) %>%
         mutate(total = sum(cfs)) -> COdischarge
       numyears = max(COdischarge$year) - min(COdischarge$year)
     
     output$distPlot <- renderPlot({
       
       # Change the palette based on selecte year type
       if (input$radioInput == "byyear") { myPalette <- colorRampPalette(rev(brewer.pal(numyears, "Spectral"))) }
       else { myPalette <- colorRampPalette(brewer.pal(numyears, "RdYlBu")) }
       
       # Make a new yearvar column and fill with either calendar or water years
       COdischarge$yearvar <- switch(input$radioInput2, calendar = COdischarge$year, water = COdischarge$wyear)
       ylabel <- switch(input$radioInput2, calendar = "Year", water = "Water Year")
       xlabel <- switch(input$radioInput2, calendar = "Day of year", water = "Day of water year")
       
       # Recalculate annual flow and julian days based on selected year type
       COdischarge %>% group_by(yearvar) %>%
         mutate(total = sum(cfs), julian = row_number()) -> COdischarge
       
       # fillvar will control the color of each ridge. 
       COdischarge$fillvar <- switch(input$radioInput, byyear = COdischarge$yearvar, bydischarge = COdischarge$total)
       
       ggplot(COdischarge, aes(julian, -yearvar, height = cfs, fill=fillvar, group=yearvar)) +
         geom_joy(stat="identity", scale = input$scaleInput * 1.2, size = 0.5) +
         theme_joy() +
         theme(text=element_text(family="Arial Narrow", size=20),
               axis.text=element_text(family="Arial Narrow", size=18),
               plot.title=element_text(family="Arial Narrow", size=21),
               plot.caption=element_text(color="#999999", size=12),
               legend.position = "none") +
         scale_fill_gradientn(colors=myPalette(numyears)) +
         labs(x = xlabel, y = ylabel,
              title = paste("Annual discharge at", toTitleCase(tolower(sitelocation$SiteName))),
              subtitle = "data from USGS NWIS")
     })
     
     output$map <- renderLeaflet({
       leaflet() %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
         addMarkers(lat=sitelocation$Latitude, lng=sitelocation$Longitude,
                    label=paste(sitelocation$SiteCode, "-", sitelocation$SiteName)) %>%
         setView(lng = mean(sitelocation$Longitude), lat = mean(sitelocation$Latitude), zoom = 10)
     })
     
     setProgress(message="All done.", value=1)
     })
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

