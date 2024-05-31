library(sparr)
library(dplyr)
library(sf)

# set workspace----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#read table
df <- read.csv("UtahGroceryAndFoodStores_DAF_3709603099855469219.csv") %>%
  subset(., select = c(OBJECTID, TYPE, x, y))

#store type frequency
table(df$TYPE)

#classify: Supermarket
df$class <- ifelse(df$TYPE == "Supermarket", 1, 0)

#df_sf <- st_as_sf(df, coords = c("x","y"))

#separate 
df_0 <- subset(df, class == 0, select = c(-TYPE, -class))
df_1 <- subset(df, class == 1, select = c(-TYPE, -class))

#ppp
df_0_ppp <- ppp(df_0$x, df_0$y, window = owin(c(min(df_0$x) - 0.5,max(df_0$x) + 0.5),c(min(df_0$y) - 0.5,max(df_0$y) + 0.5)))
df_1_ppp <- ppp(df_1$x, df_1$y, window = owin(c(min(df_0$x) - 0.5,max(df_0$x) + 0.5),c(min(df_0$y) - 0.5,max(df_0$y) + 0.5)))

#kde 0
g_tilde <- bivariate.density(pp=df_0_ppp, h0=OS(df_0_ppp)/4, adapt=FALSE, resolution=750, verbose=TRUE, parallelise = 7)

#kde 1
f_breve <- bivariate.density(pp=df_1_ppp, h0=OS(df_1_ppp)/4, adapt=FALSE, resolution=750, verbose=TRUE, parallelise = 7)

#risk
f <- risk(f_breve, g_tilde, tolerate = TRUE)

#plot
jpeg(file="Grocery Stores.jpeg")
plot(f)
dev.off()

test <- tol.classify(f, cutoff = 0.05)

plot(test$fin)
plot(test$fout)
plot(test$gin)
plot(test$gout)
