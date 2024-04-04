# # Homework 1 ############################
#
# 1. Make EDA for data (5 points).
# 2. Build an ordination of objects using NMDS methods 
#    (descriptions, samples, etc.) (5 points).
# 3. Visualise the relationship between the resulting ordination and 
#    environmental parameters with functions envfit() and ordisufr()(5 points).
# 4. Draw conclusions about the most important factors (5 points).
#
# #*Data sources**
# 
# Trees on Barro Colorado (data from Condit et al. (2002), `BCI' data, `vegan' package).

install.packages("rmarkdown")
install.packages("devtools")
devtools::install_github("gavinsimpson/ggvegan")
install.packages("broom")
install.packages("ggmap")

library(broom)
library(ggmap)
theme_set(theme_bw(base_size = 12))
library(tidyr)
library(dplyr)
library(ggvegan)
library(vegan)

data(BCI)
data(BCI.env)

#Let's check the distribution of species by total occurrence:
sums <- sort(colSums(BCI), decreasing = TRUE)
plot(sums)

cat('N of species with sums == 1:', sum(sums == 1))
cat('N of species with sums <= 1:', sum(sums <= 2))
cat('N of species with sums <= 5:', sum(sums <= 5))
cat('N of species with sums <= 10:', sum(sums <= 10))
cat('N of species with sums <= 20:', sum(sums <= 20))
cat('N of species with sums <= 50:', sum(sums <= 50))

# I wasn't sure what criteria could be used to group some species into one "rare" category, so I left it as is.

# Let's look at the environmental data:
summary(BCI.env)

# There are not very many parameters here that could be used further down in ordisurf(). I added some characteristics from Harms et al. 2001 for visualisation, and converted them into quantitative characteristics for ordisurf().

# additional <- data.frame(
#   Habitat = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"),
#   Slope_num = c(1, 0, NA, 0, NA),
#   Slope = c(">=7", "<7", "All", "<7", "All"),
#   Elevation_art_num = c(NA, 0, NA, 1, NA),
#   Elevation_art = c("All", "<152", "All", ">=152", "All"),
#   Density = c(4878.43, 5097.46, 2795.83, 4581.76, 5015.10)
# )

additional_cat <- data.frame(
  Habitat = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"),
  Slope = c(">=7", "<7", "All", "<7", "All"),
  Elevation_art = c("All", "<152", "All", ">=152", "All"),
  Density = c(4878.43, 5097.46, 2795.83, 4581.76, 5015.10)
)

additional_num <- data.frame(
  Habitat = c("OldSlope", "OldLow", "Swamp", "OldHigh", "Young"),
  Slope_num = c(1, 0, NA, 0, NA),
  Elevation_art_num = c(NA, 0, NA, 1, NA),
  Density = c(4878.43, 5097.46, 2795.83, 4581.76, 5015.10)
)

BCI.env_add_cat <- merge(BCI.env, additional_cat, by = "Habitat")
BCI.env_add_num <- merge(BCI.env, additional_num, by = "Habitat")

# I also checked to see if there was any additional data in the original article, but there was nothing extra there.
# fullmatrix <- read_excel("conditwebtable.xls", sheet = "fullmatrix")
# site_info <- read_excel("conditwebtable.xls", sheet = "site info")
# head(fullmatrix)
# head(site_info)

### Ordination ###

ord <- metaMDS(BCI)

pal_col <- c("red", "blue", "green", "orange", "purple")
pal_sh <- c(1, 2)

ordiplot(ord, type = "n", xlim = c(-1, 1), ylim = c(-0.4, 0.4))
points(ord, col = pal_col[BCI.env_add_cat$Habitat], pch = pal_sh[BCI.env_add_cat$Stream])
legend("topleft", bty = "n",
       title = "Habitat:",
       legend = levels(BCI.env_add_cat$Habitat), pch = 15, col = pal_col)
legend("topright", bty = "n",
       title = "Stream:",
       legend = levels(BCI.env_add_cat$Stream), pch = pal_sh, col = "black")

envfit_result_0 <- envfit(ord, BCI.env)
envfit_result_0

# cll columns except habitat were with p > 0.05
envfit_result_0_sel <- envfit(ord, BCI.env[, c("EnvHet", "Habitat", "Stream")])

ordiplot(ord, type = "n", xlim = c(-1, 1), ylim = c(-0.4, 0.4))
points(ord, col = pal_col[BCI.env_add_cat$Habitat], pch = pal_sh[BCI.env_add_cat$Stream])
plot(envfit_result_0_sel, p.max = 0.05)


envfit_result <- envfit(ord, BCI.env_add_cat)
envfit_result

envfit_result_sel <- envfit(ord, BCI.env_add_cat[, c("EnvHet")])
# envfit_result_sel <- envfit(ord, BCI.env_add_cat[, c("Elevation_art", "Slope", "EnvHet")])
# it's too messy
ordiplot(ord, type = "n", xlim = c(-0.5, 1), ylim = c(-0.5, 0.5))
points(ord, col = pal_col[BCI.env_add_cat$Habitat], pch = pal_sh[BCI.env_add_cat$Stream])
plot(envfit_result_sel, p.max = 0.05)

BCI.env_add_num$Habitat_num <- as.integer(BCI.env_add_num$Habitat)
BCI.env_add_num$Stream_num <- as.integer(BCI.env_add_num$Stream)-1

ordisurf_Habitat_num<- ordisurf(ord, BCI.env_add_num$Habitat_num)
summary(ordisurf_Habitat_num) # p = 4.35e-06 ***

ordisurf_Elevation_art_num<- ordisurf(ord, BCI.env_add_num$Elevation_art_num)
summary(ordisurf_Elevation_art_num) # p = 0.0138 *

ordisurf_Slope_num<- ordisurf(ord, BCI.env_add_num$Slope_num)
summary(ordisurf_Slope_num) # p = 0.000134 ***

ordisurf_EnvHet<- ordisurf(ord, BCI.env_add_num$EnvHet)
summary(ordisurf_EnvHet) # p = 0.0225 *

ordisurf_Stream_num<- ordisurf(ord, BCI.env_add_num$Stream_num)
summary(ordisurf_Stream_num) # p = 0.54

ordisurf_Density<- ordisurf(ord, BCI.env_add_num$Density)
summary(ordisurf_Density) # p = 0.601