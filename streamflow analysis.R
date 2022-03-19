# Lauren Steely, MadreDeZanjas

# This script fetches stream discharge data form the USGS NWIS API and creates ridgeline plots of the data

library(tidyverse)
library(WaterML)
library(dataRetrieval) # USGS library for accessing API water data
library(lubridate)     # tools for working with dates
library(httr)         
library(ggridges)      # create ridgeline plots in ggplot
library(RColorBrewer)
library(rayshader)     # create 3d plots in ggplot
library(magick)        # nice shadows on 3d plots

set_config(config(ssl_verifypeer = 0L)) # turn off SSL certificate verification, otherwise readNWISdv may return server error

source('../personal libraries/turbo palette.R') # import a more scientific rainbow gradient

server <- "http://hydroportal.cuahsi.org/nwisdv/cuahsi_1_1.asmx?WSDL"  # NWIS server to fetch data from

### Fetch and clean data

site <- "09380000" # Lee's Ferry
site <- "09421500" # Below Hoover
site <- "09379500" # San Juan near Bluff UT
site <- "09180500" # Colorado R near Cisco UT
site <- "09315000" # Green River at Green River UT
site <- "09152500" # Gunnison at Grand Junction

variable <- "00060" # daily discharge

discharge <- readNWISdv(site, variable, "1890-01-01", today()) # get all data within this date range

discharge <- discharge %>%
  select(-agency_cd, -X_00060_00003_cd) %>%     # remove unneeded columns
  rename(cfs = X_00060_00003, date = Date) %>%  # rename columns
  mutate(year = year(date), julian = yday(date), month = julian/31+1) # create new date variables, incl. decimalized month

### Plot data

# Plot a single station for all years
ggplot(discharge, aes(month, year, height = cfs, group = year, fill = year)) +
  geom_ridgeline(scale = 0.00007, size=0.33) +   # adjust <scale> to change scaling and spacing of years
  scale_fill_gradientn(colors = turbo(), trans='reverse') +   # add rainbow gradient
# scale_fill_viridis_c(trans='reverse') +                    # can also use viridis gradient
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +    # scale x axis
  scale_y_reverse(breaks = seq(1920, 2025, by=10), expand=c(0,0.7)) +  # scale y axis
  labs(title = "100 years of Colorado River flow at Lee's Ferry",
       caption = 'Source: USGS NWIS',
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position = 'none',
        panel.background = element_blank())

# Plot a version with fill color varying by flow instead of year
ggplot(discharge, aes(month, year, height = cfs, group = year, fill = cfs)) +   # fill = cfs for this version
  geom_ridgeline_gradient(scale = 0.00007, size=0.33, color='black') +
  scale_fill_gradientn(colors=turbo()) +
# scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1920, 2025, by=10), expand=c(0,0.7)) +
  labs(title = "100 years of Colorado River flow at Lee's Ferry",
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position = 'none',
        panel.background = element_blank())

ggsave("LeesFerry variegated.png", width=6, height=8)   # save the plot

### 3D Heatmap

myplot <- ggplot(discharge, aes(month, year, fill = cfs)) +
  geom_tile() +   # create a heatmap of the data
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1910, 2025, by=10), expand=c(0,0.7)) +
  scale_fill_gradientn(colors=turbo()) +
  labs(title = "Green River at Green River UT",
       caption = 'USGS / NWIS',
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        plot.title = element_text(vjust = 2),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        panel.background = element_blank())

plot_gg(myplot,             # ggplot to 3d-ize using rayshader library
        raytrace = T,       # use raytracing to create shadows
        multicore=T,        # use multiple cores for raytracing
        width=5, height=5,
        scale=200,          # vertical exaggeration
        sunangle=215,       # angle of light source for creating shadows
        shadow_intensity = 0.4,
        windowsize = c(1500,1000),
        zoom = 0.6,         # default zoom
        phi=30)             # default view angle

render_snapshot(file='3d flow Green.png', vignette=T) # capture image of plot


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

### Add a map of gauge location

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addMarkers(lat=sitelocations$Latitude, lng=sitelocations$Longitude,
             label=paste(sitelocations$SiteCode, "-", sitelocations$SiteName)) %>%
  setView(lng = -107.8, lat = 39.4, zoom = 8) # point the camera at Blythe
