##########################################################################'
##
## Analyze and visualise data for Metsatuhoraportti
##
##########################################################################'

library(tidyverse)
library(sf)
library(lubridate)
library(patchwork)


target_year <- 2023
min_year <- 2012

# Read data ---------------------------------------------------------------

storm_centroids <- st_read(dsn="./data/stormdamage/stormCentroids_finland_2024.gpkg")
storm_centroids$date <- as.Date(as.character(storm_centroids$declarationarrivaldate))

storm_centroids <- storm_centroids %>% 
  filter(date >= as.Date(paste0(2012+1, "-01-01")) & date < as.Date(paste0(target_year+1, "-01-01")))

## Storm dates from FMI list
storm_dates <- read.csv2("./data/IL_myrskyt.csv", skip = 1)
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
         keep = abs(as.numeric(pvm_edit - lag(pvm_edit), unit = "days")) > 31) %>%
  filter(keep | is.na(keep))

# Storm dates from:
# https://myrskyvaroitus.com/index.php/myrskytieto/myrskyhistoria/235-2023-joitakin-myrskyjae-trombeja-sylvia-elokuussa

# moreStorms2023 <- data.frame(name = c("Sylvia", "Maanantaimyrsky", "Varpu", "Pirjo", "Otso", "Talvimyräkkä"),
#                              pvm = c("8.8.2023", "28.8.2023", "20.9.2023", "6.10.2023", "11.10.2023", "22.11.2023")) %>%
#   mutate(pvm_date = as.Date(pvm, "%d.%m.%Y")) 

# Draw figures ------------------------------------------------------------

## Timeline ----
ymax_timeline <- 2500

storm_dates_labels$y_label <- ymax_timeline-800

polygon_focusYear <- data.frame(x = c(as.Date(paste0(target_year, '-01-01')),
                                      as.Date(paste0(target_year, '-01-01')),
                                      as.Date(paste0(target_year, '-12-31')),
                                      as.Date(paste0(target_year, '-12-31')) ),
                        y = c(0, ymax_timeline+100, ymax_timeline+100, 0))
annocol <- "blue"
annolwd <- .5
annoalpha <- .45

mk_plot <- ggplot(storm_centroids, aes(date)) + 
  geom_polygon(data=polygon_focusYear, aes(x=x, y=y), fill=rgb(.5,.5,.6), alpha=.3) + 
  annotate("text", x=as.Date(paste0(target_year,'-07-02')),
           y=ymax_timeline, 
           label=target_year, 
           size=4) +
  geom_histogram(binwidth=14, fill="gray50") +
  coord_cartesian(ylim = c(0, ymax_timeline))  +
  scale_x_date(breaks=as.Date(paste0(min_year:(target_year+1), "-01-01")),
               labels=as.character(min_year:(target_year+1))) +
  ylab("Myrskytuhoilmoitukset (kpl)") + xlab("") +
  geom_vline(data=storm_dates_labels,aes(xintercept=pvm_edit),
             col=annocol,
             lwd=annolwd, 
             alpha=annoalpha) +
  geom_text(data = storm_dates_labels,
             aes(pvm_edit, y_label, label = full_label),
             angle = 90, nudge_x = -40, col = annocol, alpha=.8, size=2.5)  +
    theme_bw() +
    theme(panel.grid.major = element_line(color="gray70", size=.2), 
          panel.grid.minor = element_blank())

mk_plot
ggsave("./outputs/storm_timeline.png", width=16, height = 7, unit = "cm")


## Only 2023 ----


moreStorms2023 <- data.frame(storm = c("Sylvia", "Varpu", "Pirjo ja Otso", ""),
                             name = c("Sylvia 8.8.2023", "Varpu 20.9.2023", "Pirjo ja Otso\n6. & 11.10.2023", ""),
                             pvm = c("8.8.2023",  "20.9.2023", "6.10.2023", "11.10.2023")) %>%
  mutate(pvm_date = as.Date(pvm, "%d.%m.%Y")) 


moreStorms2023$y_label <- 350
ymax_timeline <- 400



mk_plot2023 <- storm_centroids %>% 
  filter(date >= as.Date(paste0(target_year, "-01-01"))) %>%
  ggplot(aes(date)) +

  geom_histogram(binwidth=14, fill="gray50") +
  coord_cartesian(ylim = c(0, ymax_timeline),
                  xlim = c(as.Date(paste0(target_year, "-01-01")), as.Date(paste0(target_year, "-12-31"))))  +
  geom_vline(data=storm_dates_labels,aes(xintercept=pvm_edit),
             col=annocol,
             lwd=annolwd, 
             alpha=annoalpha) +
  geom_vline(data=moreStorms2023,aes(xintercept=pvm_date),
             col=annocol, alpha = 0.7) +
  geom_text(data = moreStorms2023,
            aes(pvm_date, y_label, label = storm),
            angle = 90, nudge_x = -10, col = annocol, alpha=0.8, vjust = 1)  +
  theme_bw() +
  theme(panel.grid.major = element_line(color="gray70", size=.2), 
        panel.grid.minor = element_blank()) +
  ylab("Myrskytuhoilmoitukset (kpl)") + xlab("") +
  ggtitle("A.")

mk_plot2023
# ggsave("./outputs/storm_timeline_only2023.png", width=7, height = 7, unit = "cm")

## Kartat 2023 ----
suomi <- st_read("../data_and_bits/suomi_karttapohja/suomi_aland_etrstmfin.shp")

storm_centroids2023 <- storm_centroids %>%
  filter(date >= as.Date("2023-01-01")) %>%
  # mutate(myrsky = case_when(
  #   date >= as.Date("2023-08-08") & date < as.Date(date_sylvia2) ~ "8.8.-5.10.",
  #   date >= as.Date("2023-09-20") & date < as.Date("2023-10-06") ~ "20.9.-5.10",
  #   date >= as.Date("2023-10-06") ~ "6.10.-",
  #   TRUE ~ "1.1.-7.8."),
  #   myrsky = factor(myrsky, levels = c("1.1.-7.8.", "8.8.-5.10.", "20.9.-5.10", "6.10.-")))
  mutate(myrsky = case_when(
    date >= as.Date("2023-08-08") & date < as.Date(date_sylvia2) ~ "8.8.-20.9.",
    date >= as.Date("2023-09-20")  ~ "20.9.-",
    # date >= as.Date("2023-10-06") ~ "6.10.-",
    TRUE ~ "1.1.-7.8."),
    myrsky = factor(myrsky, levels = c("1.1.-7.8.", "8.8.-20.9.", "20.9.")))
  
ggplot(suomi) +  geom_sf() +
  geom_sf(data = storm_centroids2023, #%>% filter(myrsky %in% c("Sylvia1", "Sylvia2")),
          aes(col = myrsky, shape = myrsky), size=1) + 
  facet_wrap(myrsky ~ ., ncol = 4) +
  theme_bw()

ggplot(suomi) +
  geom_sf() +
  geom_sf(data = storm_centroids2023, aes(shape = myrsky, col = myrsky), size = 1)

