library(tidyverse)
library(WaterML)
library(dataRetrieval)
library(lubridate)
library(httr)
library(ggridges)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(leaflet)

set_config(config(ssl_verifypeer = 0L)) # must turn off SSL certificate verification, otherwise readNWISdv returns a server error
font_import() # import additional system fonts (not working)

source('../personal libraries/turbo palette.R')

# WaterML method
server <- "http://hydroportal.cuahsi.org/nwisdv/cuahsi_1_1.asmx?WSDL"
variables <- GetVariables(server)
allSites <- GetSites(server) # get a list of all sites
siteinfo <- GetSiteInfo(server, "NWISDV:09380000") # get list of variables for selected site
discharge <- GetValues(server, siteCode="NWISDV:0938000", variableCode="NWISDV:00060", daily="mean") # Doesn't work :(
discharge <- GetValues(server, siteCode="NWISDV:0938000", variableCode="NWISDV:00060/DataType=MEAN") # Doesn't work

# dataRetrieval method
site <- "09380000" # Lee's Ferry
site <- "09421500" # Below Hoover
variable <- "00060" # daily discharge
discharge <- readNWISdv(site, variable, "1910-01-01", today())
discharge %>% select(-agency_cd, -X_00060_00003_cd) %>%
  rename(cfs = X_00060_00003, date = Date) %>%
  mutate(year = year(date), julian = yday(date), month = julian/31+1) -> discharge

# Plot a single station for all years
sc <- (max(discharge$cfs) / min(discharge$cfs) * 0.000035) # intelligent scaling of mountain height
ggplot(discharge, aes(month, year, height = cfs, group = year, fill = year)) +
  geom_ridgeline(scale = 0.00007, size=0.33) +
  scale_fill_gradientn(colors = turbo(), trans='reverse') +
#  scale_fill_viridis_c(trans='reverse') +
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1920, 2025, by=10), expand=c(0,0.7)) +
  labs(title = "100 years of Colorado River flow at Lee's Ferry",
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position = 'none',
        panel.background = element_blank())

# Plot a version with fill color varying by flow instead of year
ggplot(discharge, aes(month, year, height = cfs, group = year, fill = cfs)) +
  geom_ridgeline_gradient(scale = 0.00007, size=0.33, color='black') +
  scale_fill_gradientn(colors=turbo()) +
#  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1920, 2025, by=10), expand=c(0,0.7)) +
  labs(title = "100 years of Colorado River flow at Lee's Ferry",
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position = 'none',
        panel.background = element_blank())

ggsave("LeesFerry variegated.png", width=6, height=8)

### Plot multiple stations moving downriver

# Important: List stations in upstream -> downstream order
selectedSites <- c("09026500", "09033300", "09034250", "09058000", "09070500", "09095500", "09163500", "09185600")
variable <- "00060" # Discharge in cfs
COdischarge_raw <- readNWISdv(selectedSites, variable, "1966-01-01", today())
COdischarge_raw %>% group_by(site_no) %>% summarize(FirstDate = min(Date), LastDate = max(Date)) # show date availability for ea. site
COdischarge_raw %>% select(-agency_cd, -X_00060_00003_cd) %>%
  rename(cfs = X_00060_00003, date = Date) %>%
  mutate(year = year(date), julian = yday(date), site_no = factor(site_no, levels=rev(selectedSites))) %>%
  filter(cfs >= 0, year==2017)  -> COdischarge

ggplot(COdischarge, aes(julian, site_no, height = cfs, fill = site_no)) +
  geom_joy(stat="identity", scale = 2, size = 0.5) +
  theme_joy() +
  theme(text=element_text(family="Arial Narrow", size=16),
        plot.title=element_text(family="Arial Narrow", size=20),
        plot.caption=element_text(color="#999999", size=10),
        legend.position = "none") +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Day of year", y = "USGS Gauging Station", title = "Average discharge along the Colorado River",
       subtitle="Headwaters to Lake Powell (Fraser, CO to Canyonlands, UT)",
       caption = "Lauren Steely | @MadreDeZanjas")

ggsave('downstream.png', height=6, width=9)

sitelocations <- filter(allSites, SiteCode %in% selectedSites) %>% select(SiteCode, SiteName, Latitude, Longitude)

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addMarkers(lat=sitelocations$Latitude, lng=sitelocations$Longitude,
             label=paste(sitelocations$SiteCode, "-", sitelocations$SiteName)) %>%
  setView(lng = -107.8, lat = 39.4, zoom = 8) # point the camera at Blythe
