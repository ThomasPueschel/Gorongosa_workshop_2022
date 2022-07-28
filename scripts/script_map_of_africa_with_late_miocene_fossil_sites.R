library(ggmap)
library(raster)

sites_Africa <- read.csv("LateMioceneSitesAfrica.csv",
                         fileEncoding="UTF-8-BOM")

sites_Africa$fossil.sites <- factor(
  sites_Africa$fossil.sites,
  levels = c("MAMMALS", "Niger 885", "Nakalipithecus",
             "Samburupithecus", "Chororapithecus", "Sahelanthropus",
             "Lothagam", "Orrorin", "A. kadabba")
  )

colnames(sites_Africa)[4] <- "Late Miocene sites"

library(rgeos)
gnp <- data.frame(lon = c(34.5), lat = c(-19))
coordinates(gnp) <- ~ lon + lat
projection(gnp) <- "+init=epsg:4326"
gnp <- spTransform(gnp, CRS = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))

gap <- gBuffer(gnp, width = 1700000, quadsegs = 50) # width in meters thanks to projection above
gap <- spTransform(gap, CRS("+init=epsg:4326"))
gap <- fortify(gap)

afr <- c(left = -20, bottom = -36, right = 52, top = 40) # rectangle containing the african continent (I saw it quickly with Google Maps)

lbs <- c("MAMMALS", "Niger 885", expression(italic("Nakalipithecus")),
         expression(italic("Samburupithecus")), expression(italic("Chororapithecus")),
         expression(italic("Sahelanthropus")), expression(italic("Lothagam")),
         expression(italic("Orrorin")), expression(italic("A. kadabba")))

mapafrica <- ggmap(get_stamenmap(afr, zoom = 5, maptype = "terrain-background")) +
  geom_path(data = gap, aes(long, lat, group = group), color = "#f1c40f", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.4) +
  geom_hline(yintercept = 23.43667, linetype = "dotted", alpha = 0.2) +
  geom_hline(yintercept = -23.43667, linetype = "dotted", alpha = 0.2) +
  geom_point(
    data = sites_Africa,
    aes(x = lon, y = lat,
        colour = `Late Miocene sites`,
        shape = `Late Miocene sites`,
        size = `Late Miocene sites`,
        stroke = 0.8)) +
  scale_shape_manual(values = c(18, 1, 4, 2, 0, 1, 4, 2, 0), labels = lbs) +
  scale_colour_manual(values = c("#2c3e50", rep("#c0392b", 4), rep("#3498db", 4)), labels = lbs) +
  scale_size_manual(values = rep(2, 9), labels = lbs) +
  geom_rect(xmin = 33.75, xmax = 35.25, ymin = -20, ymax = -18, color = 'black', fill = '#8FD744FF', alpha = 0.1) +
  annotate("text", x = 34, y = -21.1, label = "Urema/Gorongosa", size = 3) +
  annotate("text", x = -3.5, y = -20, label = 'atop(bold("Southeast gap"))', color = "#f1c40f", parse = TRUE, size = 5.6) +
  annotate("text", x = -3.5, y = -22.5, label = 'atop(bold("radius = 1700 km"))', color = "#f1c40f", parse = TRUE, size = 4.4) +
  xlab("longitude") + ylab("latitude") + theme_minimal() + theme(legend.text.align = 0)

mapafrica

ggsave('berço_da_humanidade.png', mapafrica, device = 'png', width = 6.5, height = 5.6, scale = 1, dpi = 'retina', type = "cairo")


moz <- c(left = 33.5, bottom = -20, right = 35.5, top = -17)
ggmap(get_stamenmap(moz, zoom = 9, maptype = "terrain")) +
  theme_minimal()

ggsave('zoom_na_gorongosa.tiff', last_plot(), device = 'tiff', scale = 1, dpi = 'retina', type = "cairo")
