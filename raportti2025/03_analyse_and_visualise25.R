##########################################################################'
##
## Analyze and visualise data for Metsatuhoraportti
##
##########################################################################'

library(tidyverse)
library(sf)
library(lubridate)
library(patchwork)


luke_colors <- grDevices::rgb(red=c(255,84,0,0,225,120,127),
                              green=c(130,88,181,51,0,190,63),
                              blue=c(0,90,226,160,152,32,152),
                              names=c('orange','darkgray','turqoise','darkblue','fuchsia','green','violet'),
                              maxColorValue=255)

target_year <- 2024
min_year <- 2014

# Read data ---------------------------------------------------------------

storm_centroids <- st_read(dsn="./data/stormdamage/stormCentroids_finland_2025.gpkg")
storm_centroids$date <- as.Date(as.character(storm_centroids$declarationarrivaldate))

storm_centroids <- storm_centroids %>% 
  filter(date >= as.Date(paste0(min_year, "-01-01")) & date < as.Date(paste0(target_year+1, "-01-01")))

## Storm dates from FMI list (updated in 14/3/2025)

storm_dates <- read.csv2("./data/IL_myrskyt_2025.csv", skip = 1)

storm_dates <- storm_dates %>%
  select(pvm_edit, nimi, tyyppi) %>%
  mutate(pvm_edit = as.Date(pvm_edit, format = "%d.%m.%Y")) %>%
  filter(pvm_edit > as.Date(paste0(2012+1, "-01-01")))

storm_dates_labels <- storm_dates %>%
  mutate(full_label = paste(nimi, format(pvm_edit, "%d.%m.%Y")),
         lead_full_label = paste(lead(nimi), format(lead(pvm_edit), "%d.%m.")),
         full_label = ifelse(as.numeric(pvm_edit - lead(pvm_edit), unit = "days") < 31, 
                             paste(lead_full_label, full_label, sep=" ja "), 
                             full_label),
         keep = abs(as.numeric(pvm_edit - lag(pvm_edit), unit = "days")) > 31,
         pvm_edit = if_else(as.numeric(pvm_edit - lead(pvm_edit), unit = "days") < 31, 
                               lead(pvm_edit),
                               pvm_edit)  ) %>%
  filter(keep | is.na(keep))  %>% 
  filter(pvm_edit > as.Date(paste0(min_year, "-01-01")))

# Check stats -------------------------------------------------------------
storm_centroids <- storm_centroids %>%
  mutate(year = year(date))  

annual_stats <- storm_centroids %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(n = n(),
            sum_ha = sum(area))

annual_stats

ggplot(annual_stats, aes(year, n)) + geom_bar(stat = "identity")
ggplot(annual_stats, aes(year, sum_ha)) + geom_bar(stat = "identity")


# Draw figures ------------------------------------------------------------

## Timeline ----
ymax_timeline <- 2500

storm_dates_labels$y_label <- ymax_timeline-900

polygon_focusYear <- data.frame(x = c(as.Date(paste0(target_year, '-01-01')),
                                      as.Date(paste0(target_year, '-01-01')),
                                      as.Date(paste0(target_year, '-12-31')),
                                      as.Date(paste0(target_year, '-12-31')) ),
                        y = c(0, ymax_timeline+100, ymax_timeline+100, 0))
annocol <-  luke_colors[["darkblue"]]
annolwd <- .4
annoalpha <- 1

mk_plot <- ggplot(storm_centroids, aes(date)) + 
  geom_polygon(data=polygon_focusYear, aes(x=x, y=y), 
               # fill=rgb(.5,.5,.6),
               fill = luke_colors[["green"]],
               alpha=.2)  +
  geom_histogram(binwidth=14, fill=annocol, alpha = 0.5) +
  coord_cartesian(ylim = c(0, ymax_timeline))  +
  scale_x_date(breaks=as.Date(paste0(min_year:(target_year+1), "-01-01")),
               labels=as.character(min_year:(target_year+1)), expand = c(0.01,0.01)) +
  ylab("Myrskytuhoilmoitukset (kpl)") + xlab("") +
  geom_vline(data=storm_dates_labels,
             aes(xintercept=pvm_edit),
             col=annocol,
             lwd=annolwd, 
             alpha=annoalpha) +
  geom_text(data = storm_dates_labels,
             aes(pvm_edit, y_label, label = full_label),
             angle = 90, nudge_x = -33, col = annocol, alpha=.8, size=2.5) + 
  annotate("text", x=as.Date(paste0(target_year,'-07-02')),
           y=ymax_timeline, 
           label=target_year, 
           size=3) +
    theme_bw() +
    theme(panel.grid.major = element_line(color="gray70", size=.2), 
          panel.grid.minor = element_blank(),
          text =element_text(size = 10))

mk_plot
ggsave("./outputs/storm_timeline_2025.png", width=16, height = 7, unit = "cm")


## Only 2024 ----


moreStorms2024 <- data.frame(storm = c("Lyly", "Jari"),
                             label = c("Lyly 01.11.", "Jari 20.11."),
                             pvm = c("1.11.2024", "20.11.2024")) %>%
  mutate(pvm_date = as.Date(pvm, "%d.%m.%Y")) 


moreStorms2024$y_label <- 800
ymax_timeline <- 1200


storm_centroids2024 <- storm_centroids %>%
  filter(date >= as.Date("2024-01-01")) %>%
  mutate(myrsky = case_when(
    date >= as.Date("2024-01-01") & date < as.Date("2024-06-29") ~ "1.1.-28.6.",
    date >= as.Date("2024-06-29") & date < as.Date("2024-11-01") ~ "29.6.-31.10.",
    TRUE ~ "1.11.-31.12."),
    myrsky = factor(myrsky, levels = c("1.1.-28.6.", "29.6.-31.10.", "1.11.-31.12.")))

hist_bins <- seq(from = as.Date("2024-01-01"), by = 14, length.out = 27)
# hist_bins <- seq(from = as.Date("2024-01-01"), by = 14/2, length.out = 27*2)

cols_bins <- c("1.1.-28.6." = luke_colors[["green"]],
               "29.6.-31.10." = luke_colors[["turqoise"]],
               "1.11.-31.12." = luke_colors[["darkblue"]])


mk_plot2024 <- storm_centroids2024 %>% 
  ggplot(aes(date)) +
  geom_histogram( aes(fill=myrsky), alpha = 0.4,
                 breaks = hist_bins) +
  scale_fill_manual(values = cols_bins) +
  scale_x_date(date_labels =  "%m/%y", expand = c(0.01, 0.01)) +
  coord_cartesian(ylim = c(0, ymax_timeline),
                  xlim = c(as.Date(paste0(target_year, "-01-01")), as.Date(paste0(target_year, "-12-31"))))  +
  geom_vline(data=storm_dates_labels,aes(xintercept=pvm_edit),
             col=annocol,
             lwd=annolwd, 
             alpha=annoalpha) +
  geom_vline(data=moreStorms2024,aes(xintercept=pvm_date),
             col=annocol) +
  geom_text(data = moreStorms2024,
            aes(pvm_date, y_label, label = label),
            angle = 90, nudge_x = -8, col = annocol,
            size=2.5) +
  theme_bw() +
  theme(panel.grid.major = element_line(color="gray70", size=.2), 
        panel.grid.minor = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        text = element_text(size = 10)) +
  ylab("Myrskytuhoilmoitukset (kpl)") + xlab("") +
  # ggtitle("A.") +
  guides(fill = guide_legend(title = "Ilmoituspäivämäärä")) 

mk_plot2024
# ggsave("./outputs/storm_timeline_only2024.png", width=8, height = 8, unit = "cm")

## Kartat 2024 ----
suomi <- st_read("../data_and_bits/suomi_karttapohja/suomi_aland_etrstmfin.shp")

mk_kartat24 <- ggplot(suomi) + 
  geom_sf() +
  geom_sf(data = storm_centroids2024,
          aes(col = myrsky), 
          size=0.5) + 
  facet_wrap(myrsky ~ ., ncol = 4) +
  scale_color_manual(values = cols_bins,
                     guide="none")  +
  scale_shape(guide = "none") +
  # ggtitle("B.") + 
  # guides(color = guide_legend(title = "Ilmoituspäivämäärä"),
  #        shape = guide_legend(title = "Ilmoituspäivämäärä")) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text = element_blank())

mk_kartat24

mk_plot2024 + mk_kartat24 +
  plot_layout(guides = "collect",
              ncol = 1, height = c(1, 1.2)
              # , ncol = 2, widths = c(2, 2.5)
              ) &
  theme(legend.position='none')
# ggsave("./outputs/storm2024_timeline_and_map.png", width=12, height = 11, unit = "cm")


mk_plot2024 + mk_kartat24 +
  plot_layout(guides = "collect",
              ncol = 2, width = c(1, 1.2)) &
  theme(legend.position='none')
ggsave("./outputs/storm2025_timeline_and_map_VAAKA.png", width=16, height = 6, unit = "cm")


# ggplot(suomi) +
#   geom_sf() +
#   geom_sf(data = storm_centroids2023, aes(shape = myrsky, col = myrsky), size = 1)

# kartat yksitellen
mk_kartat24_1 <- ggplot(suomi) + 
  geom_sf() +
  geom_sf(data = storm_centroids2024 %>% filter(myrsky == levels(storm_centroids2024$myrsky)[1]),
          aes(col = myrsky), 
          size=0.5, col = cols_bins[1]) + 
  ggtitle("B)", subtitle=levels(storm_centroids2024$myrsky)[1]) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text = element_blank())

mk_kartat24_2 <- ggplot(suomi) + 
  geom_sf() +
  geom_sf(data = storm_centroids2024 %>% filter(myrsky == levels(storm_centroids2024$myrsky)[2]),
          aes(col = myrsky), 
          size=0.5, col = cols_bins[2]) + 
  ggtitle("C)", subtitle = levels(storm_centroids2024$myrsky)[2]) +
  # guides(color = guide_legend(title = "Ilmoituspäivämäärä"),
  #        shape = guide_legend(title = "Ilmoituspäivämäärä")) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text = element_blank())

mk_kartat24_3 <- ggplot(suomi) + 
  geom_sf() +
  geom_sf(data = storm_centroids2024 %>% filter(myrsky == levels(storm_centroids2024$myrsky)[3]),
          aes(col = myrsky), 
          size=0.5, col = cols_bins[3]) + 
  ggtitle("D)", subtitle=levels(storm_centroids2024$myrsky)[3]) +
  # guides(color = guide_legend(title = "Ilmoituspäivämäärä"),
  #        shape = guide_legend(title = "Ilmoituspäivämäärä")) +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text = element_blank())


(mk_plot2024 +ggtitle("A)")) +
  mk_kartat24_1 +
  mk_kartat24_2 + 
  mk_kartat24_3 +
  plot_layout(guides = "collect", ncol = 4, width = c(2.5, 1, 1, 1)) &
  theme(legend.position='none')
ggsave("./outputs/storm2024_timeline_and_map_VAAKA2.png", width=16, height = 6.8, unit = "cm")




# Snow damage -------------------------------------------------------------

snow_centroids <- st_read(dsn="./data/stormdamage/snowCentroids_finland_2025.gpkg")
snow_centroids$date <- as.Date(as.character(snow_centroids$declarationarrivaldate))

snow_centroids <- snow_centroids %>%
  filter(date >= as.Date(paste0(min_year, "-01-01")) & date < as.Date(paste0(target_year+1, "-01-01")))

# Change specs for snow figures
min_year <- 2018
ymax_timeline <- 1700
annocol <-luke_colors[["darkblue"]]

# Timeline

mk_snowplot <- ggplot(snow_centroids, aes(date)) + 
  geom_polygon(data=polygon_focusYear, aes(x=x, y=y), 
               fill = luke_colors[["turqoise"]],
               alpha=.1)  +
  geom_histogram(binwidth=14, fill=annocol, alpha = 0.8) +
  coord_cartesian(ylim = c(0, ymax_timeline))  +
  scale_x_date(breaks=as.Date(paste0(min_year:(target_year+1), "-01-01")),
               labels=as.character(min_year:(target_year+1)), expand = c(0.01,0.01)) +
  ylab("Lumituhoilmoitukset (kpl)") + xlab("") +
  annotate("text", x=as.Date(paste0(target_year,'-07-02')),
           y=ymax_timeline, 
           label=target_year, 
           size=3) +
  theme_bw() +
  theme(panel.grid.major = element_line(color="gray70", size=.2), 
        panel.grid.minor = element_blank(),
        text =element_text(size = 10)) +
  ggtitle("A) Lumituhoilmoitukset 2018-2024")

mk_snowplot

snowmap24 <- ggplot(suomi) + 
  geom_sf() +
  geom_sf(data = snow_centroids %>%
            filter(date >= as.Date("2024-01-01")), 
          size=0.5, col = luke_colors[["turqoise"]]) + 
  scale_shape(guide = "none") +
  theme_minimal() +
  theme(text = element_text(size = 10),
        legend.position = "none",
        axis.text = element_blank()) +
  ggtitle("B) 2024")

mk_snowplot + snowmap24
ggsave("./outputs/snow_timeline_2025.png", width=16, height = 7, unit = "cm")


# Check stats -------------------------------------------------------------
snow_centroids <- snow_centroids %>%
  mutate(year = year(date))  

annual_stats_snow <- snow_centroids %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(n = n(),
            sum_ha = sum(area))

annual_stats_snow

ggplot(annual_stats_snow, aes(year, n)) + geom_bar(stat = "identity")
ggplot(annual_stats_snow, aes(year, sum_ha)) + geom_bar(stat = "identity")
