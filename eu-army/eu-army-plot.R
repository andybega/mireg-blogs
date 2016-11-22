

library("readxl")
library("readr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("countrycode")
library("stringr")
library("ggthemes")
library("raster")
library("rgeos")
library("cshapes")

eb <- read_excel("VolumeAEB843COMMstandard20151119.xls", sheet = "QA16", skip = 8)
colnames(eb)[1] <- "Variable"

responses <- c("Totally in favour", "Somewhat in favour", "DK", "Somewhat opposed", "Totally opposed")

eb <- dplyr::select(eb, -`D-E`, -`D-W`, -`UE28\nEU28`)
eb <- dplyr::filter(eb, Variable %in% responses)

eb <- tidyr::gather(eb, Country, Value, -Variable)

eb <- tidyr::spread(eb, Variable, Value)
eb <- dplyr::mutate(eb,
                    Favor = `Totally in favour` + `Somewhat in favour`,
                    Opposed = `Totally opposed` + `Somewhat opposed`)
eb <- tidyr::gather(eb, Variable, Value, -Country, -Favor, -Opposed)

df <- eb
df$Variable <- factor(df$Variable, levels = rev(responses))
df <- dplyr::arrange(df, desc(Variable))
df$country_name <- countrycode::countrycode(df$Country, "iso2c", "country.name")
df$country_name[df$Country=="UK"] <- "United Kingdom"
df$country_name[df$Country=="EL"] <- "Greece"
df$country_name  <- factor(df$country_name, levels = df$country_name[df$Variable=="Totally in favour"][rev(order(df$Favor[df$Variable=="Totally in favour"]))])

subtitle <- label_wrap_gen(120)("Thinking about the future of the EU, please tell me whether you are in favour or opposed to the following statement: the creation of an EU army.")

ggplot(df, aes(x = country_name, y = Value, fill = Variable)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer("Response", type = "div") +
  theme_minimal() +
  geom_hline(yintercept = .5, colour = "gray50", size = 0.5, 
             alpha = 0.5, linetype = "dashed") +
  labs(x = "", y = "",
       title = "Support for an EU Army",
       subtitle = subtitle,
       caption = "Data from Eurobarometer 84 (2015), http://data.europa.eu/euodp/en/data") +
  theme(plot.caption = element_text(size=8, hjust=0, margin=margin(t=1),
                                    colour = "gray30"),
        plot.subtitle = element_text(face = "italic", colour = "gray30"),
        plot.title    = element_text(size = 18, face = "bold"),
        axis.text.x   = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank())

ggsave(file = "eu-army-barplot.png", height = 5, width = 8)


eu28_spdf <- subset(cshp(as.Date("2015-06-30")), ISO1AL2 %in% c(unique(eb$Country), "GR", "GB"))
eu28_spdf@data$id <- as.character(eu28_spdf@data$ISO1AL2)
eu28_spdf@data$id[eu28_spdf@data$ISO1AL2=="GR"] <- "EL"
eu28_spdf@data$id[eu28_spdf@data$ISO1AL2=="GB"] <- "UK"

eu28_spdf <- raster::crop(eu28_spdf, extent(-16, 34.6, 31, 70.1))

eu28_df <- fortify(eu28_spdf)

world    <- raster::crop(world.spdf, bbox(eu28_spdf))
world_df <- fortify(world)

eb_country <- dplyr::group_by(eb, Country) %>%
  dplyr::summarize(Opposed = mean(Opposed), Favor = mean(Favor)) %>%
  dplyr::mutate(FmO = Favor - Opposed)

eb_country$z <- cut(
  eb_country$FmO, 
  breaks = c(-1.1, -.2, -.1, -.05, .0499, .099, .199, 1.1), 
  labels = c("Strongly opposed (20% or more)", "Opposed (10-19%)", 
             "Leaning opposed (5-9%)", "Undecided (<5% either direction)",
             "Leaning in favor (5-9%)", 
             "In favor (10-19%)", "Strongly in favor (20% or more)"))

eu28_spdf@data <- dplyr::left_join(eu28_spdf@data, eb_country, by = c("id" = "Country"))
vals <- data.frame(FmO = eu28_spdf[, "FmO"], z = eu28_spdf[, "z"], id = row.names(eu28_spdf))
eu28_df <- dplyr::left_join(eu28_df, vals, by = "id")

ggplot(eu28_df, aes(long, lat, group = group, fill = FmO)) +
  geom_polygon() +
  coord_equal() +
  scale_fill_gradient2(low = "#d8b365", high = "#5ab4ac")

ggplot(eu28_df, aes(long, lat, group = group)) +
  geom_polygon(data = world_df, aes(long, lat, group = group), colour = "white") +
  geom_polygon(aes(fill = z), colour = "white") +
  coord_equal() +
  scale_fill_brewer(type = "div") +
  theme_map() +
  theme(plot.background = element_rect(fill = hsv(.54, .25, .95)),
        panel.border = element_blank())

# EPSG:3034
proj <- "+proj=lcc +lat_1=35 +lat_2=65 +lat_0=52 +lon_0=10 +x_0=4000000 +y_0=2800000 +ellps=GRS80 +units=m +no_defs"
eu28_proj    <- spTransform(eu28_spdf, CRS(proj))
eu28_proj_df <- fortify(eu28_proj)
eu28_proj_df <- dplyr::left_join(eu28_proj_df, vals, by = "id")

exp_mat <- matrix(c(0.95, 0, 0, 1.05), nrow = 2)

world_proj    <- spTransform(cshp(as.Date("2015-06-30")), CRS(proj))
world_proj    <- rgeos::gBuffer(world_proj, byid=TRUE, width=0)
world_proj    <- raster::crop(world_proj, bbox(eu28_proj) %*% exp_mat)
world_proj_df <- fortify(world_proj)

load("ne_10m_graticules_20.rda")
grat_proj    <- raster::crop(spTransform(grat20, CRS(proj)), bbox(eu28_proj) %*% exp_mat)
grat_proj_df <- suppressMessages(fortify(grat_proj))

subtitle <- label_wrap_gen(120)("Thinking about the future of the EU, please tell me whether you are in favour or opposed to the following statement: the creation of an EU army.")

ggplot(eu28_proj_df, aes(long, lat, group = group)) +
  geom_polygon(data = world_proj_df, aes(long, lat, group = group), 
               fill = "gray80", colour = "gray60", size = 0.15) +
  geom_polygon(aes(fill = z), colour = "gray60", size = 0.15) +
  geom_path(data=grat_proj_df, aes(long, lat, fill = NULL), 
            color = "white", size = 0.2, alpha = 0.5) +
  coord_equal() +
  #scale_fill_gradient2(low = "#d8b365", high = "#5ab4ac") +
  scale_fill_brewer("Margin between % opposed and\n% in favor", type = "div") +
  theme_map() +
  theme(panel.background = element_rect(fill = hsv(.54, .25, .95), size = 0),
        legend.justification=c(1, 1),
        legend.position = c(1, 1)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Support for an EU Army",
       subtitle = subtitle,
       caption = "Margin is calculated as % in favor - % opposed, disregaring 'Don't know' responses. Minus signs omitted in legend.\nData from Eurobarometer 83.4 (2015), http://data.europa.eu/euodp/en/data, image by Andreas Beger") +
  theme(plot.caption = element_text(size=8, hjust=0, margin=margin(t=1),
                                    colour = "gray30"),
        plot.subtitle = element_text(face = "italic", colour = "gray30"),
        plot.title    = element_text(size = 18, face = "bold"))

ggsave(file = "eu-army-map.png", height = 8, width = 7)

milex <- read_csv("API_MS.MIL.XPND.GD.ZS_DS2_en_csv_v2.csv", skip = 4)
milex$eu2c <- countrycode::countrycode(milex$`Country Code`, "iso3c", "iso2c")
milex$eu2c[milex$`Country Code`=="GBR"] <- "UK"
milex$eu2c[milex$`Country Code`=="GRC"] <- "EL"

eb <- dplyr::left_join(eb, dplyr::select(milex, eu2c, `2015`), by = c("Country" = "eu2c"))
eb <- dplyr::rename(eb, milx_gdp2015 = `2015`)
eb$milx_gdp2015[eb$Country=="MT"] <- 0.55781676  # Malta take 2014

df <- eb %>% dplyr::group_by(Country) %>%
  dplyr::summarize(Opposed = mean(Opposed), Favor = mean(Favor), milx = mean(milx_gdp2015))
df <- tidyr::gather(df, Response, Value, -Country, -milx)
ggplot(df, aes(x = milx, y = Value, label = Country)) + 
  geom_text() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Response) +
  labs(x = "Military expenditure 2015, % of GDP",
       title = "Defense spending is not related to views on an EU Army",
       subtitle = label_wrap_gen(100)("Question: Thinking about the future of the EU, please tell me whether you are in favour or opposed to the following statement: the creation of an EU army. "),
       caption = "Data from World Bank World Development Indicators and Eurobarometer 83.4 (2015)") +
  theme(plot.title=element_text(margin=margin(b=15)),
        plot.caption=element_text(size=10, hjust=0, margin=margin(t=15))) 

ggsave(file = "eu-army-milexp.png", height = 5, width = 8)



