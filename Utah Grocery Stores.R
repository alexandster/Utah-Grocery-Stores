#install.packages("gt")
#install.packages("gtExtras")
library(sparr)
library(dplyr)
library(sf)
library(gt)
library(gtExtras)
library(raster)
library(tmap)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read Utah state boundary

geom <- st_read("UtahStateBoundary_2711924915742528563/Utah.shp") %>%
  subset(., STATE == "Utah") %>%
  st_transform(., crs = 26912) %>%
  st_coordinates(.) %>%
  as.data.frame(.)

#read table
df <- read.csv("UtahGroceryAndFoodStores_DAF_3709603099855469219.csv") %>%
  subset(., select = c(OBJECTID, TYPE, x, y))

#store type frequency
table(df$TYPE)

#classify: Supermarket
df$class <- ifelse(df$TYPE == "Supermarket", 1, 0) %>%
  as.factor(.)


#separate 
df_0 <- subset(df, class == 0, select = c(-TYPE, -class))
df_1 <- subset(df, class == 1, select = c(-TYPE, -class))

#ppp object window
w <- owin(poly=list(x=geom$X,y=geom$Y)) #window
gstores <- ppp(x = df$x, y = df$y, window = w, marks = df$class)

#ppp
df_0_ppp <- ppp(df_0$x, df_0$y, window = w)
df_1_ppp <- ppp(df_1$x, df_1$y, window = w)

#kde 0
g_tilde <- bivariate.density(pp=df_0_ppp, h0=OS(df_0_ppp)/4, adapt=FALSE, resolution=750, verbose=TRUE, parallelise = 7)

#kde 1
f_breve <- bivariate.density(pp=df_1_ppp, h0=OS(df_1_ppp)/4, adapt=FALSE, resolution=750, verbose=TRUE, parallelise = 7)

#risk
rho <- risk(f_breve, g_tilde, tolerate = TRUE)

plot(rho)

#classify points
rho.class <- tol.classify(rho, cutoff = 0.05)

#plot
plot(rho)
points(rho.class$fin,col=2)
points(rho.class$fout)

library(spatstat)
library(spatstat.utils)

#cluster report table
ID <- 1:length(rho.class[["finsplit"]])       #cluster identifier
Cases <- lengths(rho.class[["finsplit"]])     #case count
Controls <- lengths(rho.class[["ginsplit"]])  #control count
N <- Cases + Controls                         #point count
Risk <- Cases/N                               #

#contours to sf
pcpolys <- rho.class$pcpolys %>%
  lapply(., FUN = st_as_sf) %>%
  do.call(rbind, .) %>%
  st_set_crs(26912)
pcpolys$ID <- 1:length(rho.class[["finsplit"]])
#st_write(pcpolys,"pcpolys.shp")

Area <- st_area(pcpolys) #Take care of units
Case_density <- Cases/Area                    #

#style it
df_res <- data.frame(ID, N, Cases, Controls, Risk, Case_density, Area) %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Clusters of Supermarkets in Utah")
df_res

#risk surface raster
r <- raster(rho$rr)
crs(r) <- "+init=epsg:26912 +proj=utm +zone=12 +datum=NAD83 +units=m" 
#writeRaster(r, "risk", format = "GTiff", overwrite=TRUE)

#tmap
map <- tm_shape(r) +
  tm_raster() + 
  tm_shape(pcpolys,
           title = "Utah") +
  tm_borders(lwd=2) +
  tm_text("ID", xmod = 0.25, size = 0.5) +
  tm_layout(
    # legend.position = c("right", "top"),
    legend.position = c(-0.2, 0.1),
            inner.margins = c(.1,.1,.1,.1),
            frame = FALSE,
            )
map

