# Lauren Steely, August 2017

library(shiny)
library(tidyverse)
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
   
   titlePanel("Streamflow Comparison Tool"),
   helpText("Lauren Steely | @MadreDeZanjas | ",
            a("Code available on Github", href="https://github.com/codeswitching/Streamflow-Joyplot-Tool")),
   textInput("sitesInput", "Enter a USGS gauging station site ID:",
             value = "09380000", width = 300, placeholder = NULL),
   helpText("Examples | Rio Grande: 08319000 | Colorado R: 09380000 | Columbia R: 14105700 | Sacramento R: 11425500 | Klamath R: 11530500"),
   helpText(a("Find gauging stations", href="https://maps.waterdata.usgs.gov/mapper/index.html"), " via NWIS"),
   actionButton("button", "Make joyful"),
   
   hr(),
   
   fluidRow(
    column(width=4,
     sliderInput("scaleInput", "Add joy:", 0, 11, 3, 1)
    ),
    column(width=6,
     radioButtons("radioInput", "Fill color:", choices =
                    c("by year (pretty)" = "byyear", "by total annual discharge (shows dry vs wet years)" = "bydischarge"))
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
     selectedSite <- input$sitesInput
     server <- "http://hydroportal.cuahsi.org/nwisdv/cuahsi_1_1.asmx?WSDL"
     sitelocation <- GetSiteInfo(server, paste0("NWISDV:", selectedSite))
     variable <- "00060" # Discharge in cfs09380000
     COdischarge_raw <- readNWISdv(selectedSite, variable, "1901-01-01", today())
     setProgress(message="Plotting...", value=0.6)
     COdischarge_raw %>%
       rename(cfs = X_00060_00003, date = Date) %>%
       mutate(year = year(date), julian = yday(date)) %>%
       filter(cfs >= 0) %>%
       group_by(year) %>%
       mutate(total = sum(cfs)) -> COdischarge
     numyears = max(COdischarge$year) - min(COdischarge$year)
     
     output$distPlot <- renderPlot({
       if (input$radioInput == "byyear") { myPalette <- colorRampPalette(rev(brewer.pal(numyears, "Spectral"))) }
       else { myPalette <- colorRampPalette(brewer.pal(numyears, "RdYlBu")) }
       
       COdischarge$fillvar <- switch(input$radioInput, byyear = COdischarge$year, bydischarge = COdischarge$total)
       ggplot(COdischarge, aes(julian, -year, height = cfs, fill=fillvar, group=year)) +
         geom_joy(stat="identity", scale = input$scaleInput * 1.2, size = 0.6) +
         theme_joy() +
         theme(text=element_text(family="Arial Narrow", size=20),
               axis.text=element_text(family="Arial Narrow", size=18),
               plot.title=element_text(family="Arial Narrow", size=21),
               plot.caption=element_text(color="#999999", size=12),
               legend.position = "none") +
         scale_fill_gradientn(colors=myPalette(numyears)) +
         labs(x = "Day of year", y = "Year",
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

