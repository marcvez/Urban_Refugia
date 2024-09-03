###############################################################################
###############################################################################
#################                                             #################
#################           URBAN REFUGIA ANALYSIS            #################
#################                                             #################
###############################################################################
###############################################################################


### PART 0: Required packages ####

install.packages("raster")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("glmmTMB")
install.packages("performance")
install.packages("extRemes")
install.packages("bbmle")
install.packages("lme4")
install.packages("pgirmess")
install.packages("DHARMa")
install.packages("partR2")
install.packages("FactoMineR")
install.packages("vcd")
install.packages("factoextra")
install.packages("plot3D")
install.packages("sf")


library(raster)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(glmmTMB)
library(performance)
library(extRemes)
library(bbmle)
library(lme4)
library(pgirmess)
library(DHARMa)
library(partR2)
library(FactoMineR)
library(vcd)
library(factoextra)
library(plotly)
library(plot3D)
library(ggpubr)
library(EnvStats)
library(sf)
library(viridis)
library(ggpubr)


#



### PART 1: Urbanization index #####

## PART 1.1: Urbanization index calculation ####

# Map working directory 
setwd("C:/Users/marc9/Desktop/Marc/CREAF/Urban refugia")

# 10x10m raster map (https://worldcover2021.esa.int/data/docs/WorldCover_PUM_V2.0.pdf)
Eivissa_map <- raster("Eivissa2021.tif")

# Data working directory
setwd("C:/Users/marc9/Desktop/Marc/CREAF/Urban refugia")


# Data (we are going to work with a modified data frame which classifies Ibiza city as invaded even though the interpolation classifies it as non invaded)
Urban_Refugia_MVG_cens_total_net <- read_csv("UR_new_classifcation_nov_2023_ibiza_inv.csv")

# Sampling points coordinates (long/lat)
coords <- Urban_Refugia_MVG_cens_total_net %>%
  dplyr::select(Latitude, Longitude)

# Transform to correct format 
coordinates(coords) <- ~ Longitude + Latitude
# plot(coords, axes = TRUE)

# 50 metres buffer around sampling points
buff_shp <- buffer(coords, 50)
# plot(buff_shp, axes = TRUE)

# Plot (island + buffers)
plot(Eivissa_map, axes = T)
plot(buff_shp, add = TRUE)
# plot(coords, add = TRUE)

# Extract use of land information from every 10x10m cell inside the 50m buffer
landcover_eivissa <- raster::extract(Eivissa_map, coords, buffer = 50, df=TRUE)

# Convert to data frame (ID = sampling point | Map = land use category)
landcover_eivissa <- as.data.frame(landcover_eivissa)

# Summarise by sampling point and land use category (how many 10x10m cells of each type are within each buffer area)
landcover_eivissa <- landcover_eivissa %>% 
  group_by(ID, Map) %>%
  dplyr::summarise(n=n())

# Total number of 10x10m cells within each buffer
landcover_eivissa <- landcover_eivissa %>% 
  group_by(ID) %>%
  dplyr::mutate(total=sum(n))

# Percentage of cells from each category with respect to the total number of cells within each buffer
landcover_eivissa <- landcover_eivissa %>% 
  group_by(ID, Map) %>%
  dplyr::summarise(value=n/total)

# Wide format
landcover_eivissa <- spread(landcover_eivissa, Map, value)

# Name of each land use category
landcover_eivissa <- landcover_eivissa %>%
  dplyr::rename(Tree_cover="10",
                Shrubland="20",
                Grassland="30",
                Cropland="40",
                Built_up="50",
                Bare.sparse_vegetation="60",
                Permanent_water_bodies="80")

# Add Place, Site and Project columns
landcover_eivissa <- landcover_eivissa %>%
  add_column(Place = Urban_Refugia_MVG_cens_total_net$Place,
             Site = Urban_Refugia_MVG_cens_total_net$Site_ID,
             Project = Urban_Refugia_MVG_cens_total_net$Project)

# Place, Site and Project columns to the beginning of the data frame
landcover_eivissa <- landcover_eivissa %>%
  relocate(c(Place, Site, Project), .after = ID)

# Summary table without replicates
landcover_eivissa2 <- landcover_eivissa %>% 
  group_by(Place, Site) %>%
  dplyr::summarise(across(c(Tree_cover, 
                            Shrubland, 
                            Grassland, 
                            Cropland, 
                            Built_up, 
                            Bare.sparse_vegetation, 
                            Permanent_water_bodies), 
                          list(mean)))

# NA as 0
landcover_eivissa[is.na(landcover_eivissa)] <- 0

# Urbanization index (Build/Total)
landcover_eivissa$Urbanization_index <- landcover_eivissa$Built_up/
  (landcover_eivissa$Tree_cover + 
     landcover_eivissa$Shrubland + 
     landcover_eivissa$Grassland + 
     landcover_eivissa$Cropland + 
     landcover_eivissa$Bare.sparse_vegetation + 
     landcover_eivissa$Built_up)

# Comparison between R urbanization index and QGis urbanization index (linear model)
summary(lm(Urban_Refugia_MVG_cens_total_net$`Index Urb R` ~ landcover_eivissa$Urbanization_index))

plot(Urban_Refugia_MVG_cens_total_net$`Index Urb R` ~ landcover_eivissa$Urbanization_index, 
     xlab = "Urbanization index method 2", 
     ylab = "Urbanization index method 1", 
     sub = "coeff y ~ x = 0.99 | R2 = 0.8664")

abline(lm(Urban_Refugia_MVG_cens_total_net$`Index Urb R` ~ landcover_eivissa$Urbanization_index))

# Urbanization index histogram of both methodologies
par(mfrow = c(1,2))

hist(Urban_Refugia_MVG_cens_total_net$`Index Urb R`, 
     main = "QGis")

hist(landcover_eivissa$Urbanization_index, 
     main = "R")

par(mfrow = c(1,1))



  ## PART 1.2: Urbanization index integration in final data frame ####

# Creation of final data frame, only with meaningful variables
# ("Date", "Project", "Place", "Site_ID", "State", "Inital_Hour", "Latitude":"Total NA", "Population 2021":"Distance_to_invasion")
Urban_Refugia_MVG_seleccio <- Urban_Refugia_MVG_cens_total_net[, c(1, 3, 4, 6, 41, 11:26, 37, 39, 40, 42)]

# Convert factor variables to factor
Urban_Refugia_MVG_seleccio$Place <- as.factor(Urban_Refugia_MVG_seleccio$Place)
Urban_Refugia_MVG_seleccio$Project <- as.factor(Urban_Refugia_MVG_seleccio$Project)
Urban_Refugia_MVG_seleccio$Date <- as.factor(Urban_Refugia_MVG_seleccio$Date)
Urban_Refugia_MVG_seleccio$Site_ID <- as.factor(Urban_Refugia_MVG_seleccio$Site_ID)
Urban_Refugia_MVG_seleccio$Inv_state <- as.factor(Urban_Refugia_MVG_seleccio$Inv_state)
Urban_Refugia_MVG_seleccio$Classifica <- as.factor(Urban_Refugia_MVG_seleccio$Classifica)


# Total number of Podarcis per replicate
Urban_Refugia_MVG_seleccio$Suma_total <- Urban_Refugia_MVG_seleccio$`Total Male` +  
  Urban_Refugia_MVG_seleccio$`Total Fema` + 
  Urban_Refugia_MVG_seleccio$`Total Juve` + 
  Urban_Refugia_MVG_seleccio$`Total NA`

# Reorder variables
Urban_Refugia_MVG_seleccio <- Urban_Refugia_MVG_seleccio[, c(1:21, 26, 22:25)]

# Merge urbanization index to data frame
Urban_Refugia_MVG_seleccio$Urbanization_index <- landcover_eivissa$Urbanization_index

str(Urban_Refugia_MVG_seleccio)








### PART 2: Exploratory plots raw data #####

# Order of factors
Urban_Refugia_MVG_seleccio$Inv_state <- factor(Urban_Refugia_MVG_seleccio$Inv_state,
                                               levels = c("NonInvaded", "Invaded"))

# New variable that merges invasion state and habitat type
Urban_Refugia_MVG_seleccio$Code <- paste0(Urban_Refugia_MVG_seleccio$Inv_state, "_", Urban_Refugia_MVG_seleccio$Project)

xaxislab1 <- expression(paste("Total abundance of ", italic("P. pityusensis"), " per census"))

# Joint density plot (normal and logarithmic)
ggplot(data = Urban_Refugia_MVG_seleccio, 
       aes(x = (Suma_total), 
           color = Classifica, 
           fill = Classifica)) +
  scale_color_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                     labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                     name = "Sampling point status") + 
  labs(x= xaxislab1, y = "Density") + 
  scale_fill_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                    labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                    name = "Sampling point status") +
  geom_density(lwd = 1.3, 
               alpha = 0.05)

xaxislab2 <- expression(paste("log(Total abundance of ", italic("P. pityusensis"), " per census)"))

theme_set(theme_minimal() + theme(legend.position = 'bottom') + theme(text = element_text(size = 17)))

ggplot(data = Urban_Refugia_MVG_seleccio, 
       aes(x = log1p(Suma_total), 
           color = Classifica, 
           fill = Classifica)) +
  scale_color_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                     labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                     name = "Sampling point status") + 
  labs(x= xaxislab2, y = "Density") + 
  scale_fill_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                    labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                    name = "Sampling point status") +
  geom_density(lwd = 1.3, 
               alpha = 0.05)


# We create all combinations of these 3 variables
all_combinations <- expand.grid(Inv_state = unique(Urban_Refugia_MVG_seleccio$Inv_state),
                                Project = unique(Urban_Refugia_MVG_seleccio$Project),
                                Suma_total = unique(Urban_Refugia_MVG_seleccio$Suma_total))

# Count the number of times each combination appears in our data 
hist_values <- all_combinations %>%
  left_join(Urban_Refugia_MVG_seleccio %>% 
              group_by(Inv_state, Project, Suma_total) %>%
              dplyr::summarise(n = n()), 
            by = c("Inv_state", "Project", "Suma_total")) %>%
  mutate(n = ifelse(is.na(n), 0, n))


# Total nº of sightings per experimental scenario
hist_values <- hist_values %>% 
  group_by(Inv_state, Project) %>%
  dplyr::mutate(total=sum(n))

hist_values_violin <-  hist_values

# Summary table of total number of sightings
hist_values2 <- Urban_Refugia_MVG_seleccio %>% 
  group_by(Inv_state, Project) %>%
  dplyr::summarise(n=n())

# Relative abundance of sightings categories (0, 1, 2, 3, ...)
hist_values <- hist_values %>% 
  group_by(Inv_state, Project, Suma_total) %>%
  dplyr::summarise(value=n/total)

# Standarized sighting abundance histogram (normal and logarithmic)
supp.labs <- c("Non-invaded", "Invaded")
names(supp.labs) <- c("NonInvaded", "Invaded")

xaxislab <- expression(paste("Total abundance of ", italic("P. pityusensis"), " per census"))

theme_set(theme_minimal() + theme(legend.position = 'bottom') + theme(text = element_text(size = 15)))

ggplot(hist_values, 
       aes(fill = Project, 
           x = Suma_total, 
           y = value)) + 
  facet_wrap(vars(Inv_state), labeller = labeller(Inv_state = supp.labs)) +
  geom_bar(stat = "identity", 
           position=position_dodge(),
           color = "black") + 
  xlab(xaxislab) + 
  ylab("Percentage of censuses") +
  labs(fill = "Urbanisation situation") +
  scale_fill_manual(values=c("grey80", "red3"),
                    labels=c("Peri-urban", "Urban")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.5))

# Violin plot (?)

# Crear el gráfico de violín
ggplot(Urban_Refugia_MVG_seleccio, aes(x = Code, y = Suma_total)) +
  geom_violin(trim = FALSE) +
  labs(x = "Treatment", y = "Lizard abundance") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)


ggplot(Urban_Refugia_MVG_seleccio, aes(x = factor(Code, level=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban")), y = Suma_total, fill = Code)) +
  geom_violin(trim = TRUE, adjust = 1.5, width = 0.75) +  
  labs(x = "", y = "Lizard abundance") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +
  geom_jitter(shape = 16, position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.75, jitter.height = 0.2)) +  
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", fill = "white", stroke = 1, size = 4, position = position_dodge(width = 0.7)) +  
  scale_fill_manual(values = c( "grey60", "grey30", "tomato1", "red3"),
                    name = "Treatment", 
                    breaks=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban"),
                    labels = c("Non-invaded | Peri-Urban      ", "Non-invaded | Urban      ", "Invaded | Peri-urban      ", "Invaded | Urban     ")) + 
  scale_x_discrete(labels=c("NI-PU", "NI-U", "I-PU", "I-U")) +
  guides(fill=guide_legend(ncol=2)) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE)






theme_set(theme_classic() + theme(legend.position = 'bottom') + theme(text = element_text(size = 17)))


ggplot(Urban_Refugia_MVG_seleccio, aes(x = factor(Code, level=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban")), y = Suma_total, fill = Code)) +
  geom_violin(trim = TRUE, adjust = 1.5, width = 0.75) +  
  labs(x = "\nInvasion and urbanization status", y = "Lizard abundance") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5), legend.position = "none") +
  geom_jitter(shape = 16, position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.75, jitter.height = 0.2)) +  
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", fill = "white", stroke = 1, size = 4, position = position_dodge(width = 0.7)) +  
  scale_fill_manual(values = c( "grey60", "grey30", "tomato1", "red3"),
                    name = "Treatment", 
                    breaks=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban")) +
  scale_x_discrete(labels=c("Non-invaded\nPeri-urban", "Non-invaded\nUrban", "Invaded\nPeri-urban", "Invaded\nUrban")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE)


ggplot(Urban_Refugia_MVG_seleccio, aes(x = factor(Code, level=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban")), y = Suma_total, fill = Code)) +
  geom_violin(trim = TRUE, adjust = 1.5, width = 0.75) +  
  labs(x = "\nInvasion and urbanization status", y = "Lizard abundance") +
  theme(legend.position = "none", axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  geom_jitter(shape = 16, position = position_jitterdodge(jitter.width = 0.6, dodge.width = 0.75, jitter.height = 0.2)) +  
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", fill = "white", stroke = 1, size = 4, position = position_dodge(width = 0.7)) +  
  scale_fill_manual(values = c( "grey60", "grey30", "tomato1", "red3"),
                    name = "Treatment", 
                    breaks=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban")) +
  scale_x_discrete(labels=c("", "", "", "")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE)

aggregate(Suma_total ~ Inv_state + Project, Urban_Refugia_MVG_seleccio, function(x) c(mean = mean(x), sd = sd(x)))

summary(aov(Suma_total ~ Inv_state + Project, data = Urban_Refugia_MVG_seleccio))

summary(aov(Suma_total ~ Code, data = Urban_Refugia_MVG_seleccio))

TukeyHSD(aov(Suma_total ~ Inv_state + Project, data = Urban_Refugia_MVG_seleccio))

TukeyHSD(aov(Suma_total ~ Code, data = Urban_Refugia_MVG_seleccio))

ggplot(Urban_Refugia_MVG_seleccio, aes(x = factor(Inv_state, level=c("NonInvaded", "Invaded")), y = Suma_total, fill = Code)) +
  geom_violin(trim = TRUE, adjust = 1.5, width = 0.75) +  
  labs(x = "Invasion status", y = "Lizard abundance") +
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)) +  
  geom_jitter(shape = 16, position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.75, jitter.height = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", shape = 19, col = "white", size = 4, position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c( "grey60", "grey30", "tomato1", "red3"),
                    name = "Treatment", 
                    breaks=c('NonInvaded_PeriUrban', 'NonInvaded_Urban', 'Invaded_PeriUrban', "Invaded_Urban"),
                    labels = c("Non-invaded | Peri-Urban      ", "Non-invaded | Urban      ", "Invaded | Peri-urban      ", "Invaded | Urban     ")) +
  scale_x_discrete(labels=c("Non-invaded", "Invaded")) +
  guides(fill=guide_legend(ncol=2)) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE)


# Zero distribution

# We select entries with only 0.
UR_zeros <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Suma_total == 0,]

# Density of zeros based on urbanisation index. Low urbanisation index = more 0
ggplot(UR_zeros, aes(x = Urbanization_index)) + 
  geom_density()

# Density of zeros based on invasion year. More 0 in middle years. Oldest record is Santa Eulària, which still has lizards. This might be creating this phenomenon in interaction
ggplot(UR_zeros, aes(x = `3P 250 200`)) + 
  geom_density()


# Urbanization index density plot

urb_complete <- ggplot(Urban_Refugia_MVG_seleccio, aes(x = `Index Urb`)) + 
  geom_density(fill = "blue", alpha = 0.6, linewidth = 1) +
  ylab("All sites") +
  xlab("Urbanization index")

Urban_Refugia_MVG_seleccio_urban <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Project == "Urban", ]

urb_urb_complete <- ggplot(Urban_Refugia_MVG_seleccio_urban, aes(x = `Index Urb`)) + 
  geom_density(fill = "grey20", alpha = 0.6, linewidth = 1) +
  ylab("Density") +
  xlab("Urbanisation index") +
  theme(
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5)) 


Urban_Refugia_MVG_seleccio_periurban <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Project == "PeriUrban", ]

urb_periurb_complete <- ggplot(Urban_Refugia_MVG_seleccio_periurban, aes(x = `Index Urb`)) + 
  geom_density(fill = "grey80", alpha = 0.6, linewidth = 1) +
  xlim(0, 1) + 
  ylab("Density") +
  xlab("Urbanisation index") +
  theme(
    axis.title.y = element_text(vjust = 2),
    axis.title.x = element_text(vjust = -0.5))


ggarrange(urb_complete, 
          ggarrange(urb_urb_complete, urb_periurb_complete, ncol = 1, nrow = 2), 
          ncol = 2, nrow = 1)

ggarrange(urb_periurb_complete, urb_urb_complete,  ncol = 2, nrow = 1)





# 3D plot Urbanization index (color/size), year of invasion (x), abundance (y), only urban sites
# We select urban sites
Urban_data <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Project == "Urban", ]

# Size determines urbanisation index
ggplot(Urban_data, aes(x = `3P 250 200`, y = Suma_total, size = `Index Urb`)) +
  geom_point() +
  labs(x = "Invasion year", y = "Abundance", size = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()

# Color determines urbanisation index
threed_plot_urban <- ggplot(Urban_data, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_smooth(method = "lm", se = T, colour = "black") +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, colour = "black") +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()


ggplot(Urban_data, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_point(size = 5) +
  geom_smooth(method = "lm", se = T, colour = "black") +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, colour = "black") +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()

# PeriUrban data
PeriUrban_data <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Project == "PeriUrban", ]

threed_plot_periurban <- ggplot(PeriUrban_data, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_smooth(method = "lm", se = T, colour = "black") +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, colour = "black") +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()


ggarrange(threed_plot_urban, threed_plot_periurban,
          ncol = 1, nrow = 2)



# Boxplot by year breaks

inferior_limit <- min(Urban_Refugia_MVG_seleccio$`3P 250 200`)
superior_limit <- max(Urban_Refugia_MVG_seleccio$`3P 250 200`)

break_year <- (superior_limit - inferior_limit)/4

groups <- seq(inferior_limit, superior_limit, by = break_year)

for(i in 1:nrow(Urban_Refugia_MVG_seleccio)){
  
  if(Urban_Refugia_MVG_seleccio$`3P 250 200`[i] <= groups[2]){
    
    Urban_Refugia_MVG_seleccio$Year_group[i] <- "D"
    
  } else if(Urban_Refugia_MVG_seleccio$`3P 250 200`[i] > groups[2] & Urban_Refugia_MVG_seleccio$`3P 250 200`[i] <= groups[3]){
    
    Urban_Refugia_MVG_seleccio$Year_group[i] <- "C"
    
  }else if(Urban_Refugia_MVG_seleccio$`3P 250 200`[i] > groups[3] & Urban_Refugia_MVG_seleccio$`3P 250 200`[i] <= groups[4]){
    
    Urban_Refugia_MVG_seleccio$Year_group[i] <- "B"
    
  }else if(Urban_Refugia_MVG_seleccio$`3P 250 200`[i] > groups[4]){
    
    Urban_Refugia_MVG_seleccio$Year_group[i] <- "A"
    
  }
  
}


ggplot(Urban_Refugia_MVG_seleccio, aes(x = Year_group, y = Suma_total, fill = Project)) +
  geom_boxplot(alpha = 0.8, outliers = FALSE) +
  geom_jitter(shape = 16, col = "black", size = 0.8, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75, jitter.height = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", stroke = 1, size = 3, position = position_dodge(width = 0.75), alpha = 1) +
  labs(x = "Grupos", y = "Lizard abundance", fill = "Urbanisation") +
  scale_x_discrete(labels=c("Non-invaded", "0-3 years", "3-6 years", "6-9 years")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE) +
  scale_fill_manual(values = c("grey80", "grey20"))





# Boxplot by year breaks (non-invaded + 3 categories)

Urban_Refugia_MVG_seleccio_non_invaded <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Inv_state %in% c("NonInvaded"), ]

Urban_Refugia_MVG_seleccio_invaded <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Inv_state %in% c("Invaded"), ]

Urban_Refugia_MVG_seleccio_non_invaded$Year_group <- "A"



inferior_limit <- min(Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`)
superior_limit <- max(Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`)

break_year <- (superior_limit - inferior_limit)/3

groups <- seq(inferior_limit, superior_limit, by = break_year)

for(i in 1:nrow(Urban_Refugia_MVG_seleccio_invaded)){
  
  if(Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`[i] <= groups[2]){
    
    Urban_Refugia_MVG_seleccio_invaded$Year_group[i] <- "D"
    
  } else if(Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`[i] > groups[2] & Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`[i] <= groups[3]){
    
    Urban_Refugia_MVG_seleccio_invaded$Year_group[i] <- "C"
    
  }else if(Urban_Refugia_MVG_seleccio_invaded$`3P 250 200`[i] > groups[3]){
    
    Urban_Refugia_MVG_seleccio_invaded$Year_group[i] <- "B"
    
  }
  
}


Urban_Refugia_MVG_seleccio_inv_noninv_joined <- rbind(Urban_Refugia_MVG_seleccio_invaded, Urban_Refugia_MVG_seleccio_non_invaded)



ggplot(Urban_Refugia_MVG_seleccio_inv_noninv_joined, aes(x = Year_group, y = Suma_total, fill = Project)) +
  facet_wrap(~ Project, labeller = labeller(Project = supp.labs)) +
  geom_boxplot(alpha = 0.8, outliers = FALSE) +
  geom_jitter(shape = 16, col = "black", size = 0.8, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75, jitter.height = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", stroke = 1, bg = "white", size = 3, position = position_dodge(width = 0.75), alpha = 1) +
  labs(x = "Invasion status", y = "Lizard abundance", fill = "Urbanisation") +
  scale_x_discrete(labels=c("Non-\ninvaded", "2022-\n-2019", "2019-\n-2016", "2016-\n-2013")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE) +
  scale_fill_manual(values = c("grey80", "grey20"))

ggplot(Urban_Refugia_MVG_seleccio_inv_noninv_joined, aes(x = Year_group, y = Suma_total, fill = Project)) +
  geom_boxplot(alpha = 0.8, outliers = FALSE) +
  geom_jitter(shape = 16, col = "black", size = 0.8, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75, jitter.height = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", shape = 21, col = "black", stroke = 1, size = 3, position = position_dodge(width = 0.75), alpha = 1) +
  labs(x = "Invasion status", y = "Lizard abundance", fill = "Urbanisation") +
  scale_x_discrete(labels=c("Non-\ninvaded", "2022-\n-2019", "2019-\n-2016", "2016-\n-2013")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE) +
  scale_fill_manual(values = c("grey80", "grey20"))


Urban_Refugia_MVG_seleccio_inv_noninv_urban <- Urban_Refugia_MVG_seleccio_inv_noninv_joined[Urban_Refugia_MVG_seleccio_inv_noninv_joined$Project %in% c("Urban"), ]


Urban_Refugia_MVG_seleccio_inv_noninv_periurban <- Urban_Refugia_MVG_seleccio_inv_noninv_joined[Urban_Refugia_MVG_seleccio_inv_noninv_joined$Project %in% c("PeriUrban"), ]

# ANOVA urban
anova_urban <- aov(Suma_total ~ Year_group, data = Urban_Refugia_MVG_seleccio_inv_noninv_urban)

summary(anova_urban)

TukeyHSD(anova_urban, conf.level = 0.95)

plot(TukeyHSD(anova_urban, conf.level = 0.95), las = 2)


# ANOVA periurban
anova_periurban <- aov(Suma_total ~ Year_group, data = Urban_Refugia_MVG_seleccio_inv_noninv_periurban)

summary(anova_periurban)

TukeyHSD(anova_periurban, conf.level = 0.95)

  plot(TukeyHSD(anova_periurban, conf.level = 0.95), las = 2)





# Point plot joined only invaded

Urban_data_inv <- Urban_data[Urban_data$Inv_state %in% c("Invaded"), ]

PeriUrban_data_inv <- PeriUrban_data[PeriUrban_data$Inv_state %in% c("Invaded"), ]


threed_plot_urban_inv <- ggplot(Urban_data_inv, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_smooth(method = "lm", se = T, colour = "black") +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, colour = "black") +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()


threed_plot_periurban_inv <- ggplot(PeriUrban_data_inv, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_smooth(method = "lm", se = T, colour = "black") +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = T, colour = "black") +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()

ggarrange(threed_plot_urban_inv, threed_plot_periurban_inv,
          ncol = 1, nrow = 2)




# All data
ggplot(Urban_Refugia_MVG_seleccio, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`)) +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) + 
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index") +
  theme_minimal() + 
  scale_x_reverse()


ggplot(Urban_Refugia_MVG_seleccio, aes(x = `3P 250 200`, y = Suma_total, color = `Index Urb`, shape = Project)) +
  geom_point(size = 5) +
  scale_color_gradient(low = viridis(2)[2], high = viridis(2)[1]) +
  labs(x = "Invasion year", y = "Abundance", color = "Urbanization index", shape = "Project") +
  theme_minimal() +
  scale_x_reverse() +
  guides(shape = guide_legend(title = "Urbanisation category")) + # Esto es opcional para agregar un título a la leyenda de formas
  scale_shape_manual(values = c(17, 16))








# Theoretical data source-sink dynamic
abundancia_max <- 10 # Cambia este valor al máximo deseado
abundancia_media <- abundancia_max / 2

data_teóricos <- data.frame(
  x = c(0, abundancia_max),
  y = c(0, 0),
  Dynamic = c("Active dispersal dynamic", "Source-Sink dynamic")
)

data_teóricos <- rbind(data_teóricos, data.frame(x = c(0), y = c(abundancia_max), Dynamic = "Source-Sink dynamic"))
data_teóricos <- rbind(data_teóricos, data.frame(x = c(0, abundancia_max), y = c(abundancia_media, abundancia_media), Dynamic = "No Source-Sink dynamic"))
data_teóricos <- rbind(data_teóricos, data.frame(x = c(abundancia_max), y = c(abundancia_max), Dynamic = "Active dispersal dynamic"))

theme_set(theme_minimal() + theme(legend.position = 'bottom') + theme(text = element_text(size = 15)))

# Plot
ggplot(data_teóricos, aes(x = x, y = y, color = Dynamic)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = c(0, abundancia_max), labels = c("New", "Old")) +
  scale_y_continuous(breaks = c(0, abundancia_max), labels = c(0, "Max. abundance")) +
  labs(x = "Invasion status", y = "Lizard abundance") +
  theme_classic()




# 3D plot
plot_ly(data = Urban_data, x = ~`3P 250 200`, y = ~Suma_total, z = ~`Index Urb`)

# snake captures
Traps_snake <- read_delim("Traps_snake.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Proportion of no-snake traps inside city
Zones <- c(rep("Outer", 18), rep("Intermediate", 8), rep("Inner", 15))

Total <- c(11, 2, 6, 4, 9, 4, 4, 7, 3, 2, 2, 1, 5, 2, 0, 0, 0, 0, 2, 0, 0, 4, 1, 1, 3, 2, rep(0, 15))  # Por ejemplo, número de serpientes en cada zona

# Crear el data frame
Trap_eff_zones <- data.frame(Zones = Zones, Total = Total)

media_por_zona <- aggregate(Total ~ Zones, data = Traps_snake, FUN = mean)
total_por_zona <- aggregate(Total ~ Zones, data = Traps_snake, FUN = length)

colors <- c("Outer" = "grey80", "Intermediate" = "grey50", "Inner" = "grey20")

theme_set(theme_classic() + theme(legend.position = 'bottom') + theme(text = element_text(size = 15)))

Trap_eff_zones$Zones <- factor(Trap_eff_zones$Zones, levels = c("Outer", "Intermediate", "Inner"))

aggregate(. ~ Zones, Trap_eff_zones, function(x) c(mean = mean(x), sd = sd(x)))




ggplot(Trap_eff_zones, aes(x = Zones, y = Total, fill = Zones)) +
  geom_boxplot(alpha = 0.8, outliers = FALSE) +
  geom_jitter(shape = 16, size = 2, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75, jitter.height = 0.2)) +
  geom_point(data = media_por_zona, aes(y = Total), color = "black", fill = "white", size = 4, shape = 21) +
  labs(x = "",
       y = "Captured snakes per trap") +
  scale_fill_manual(values = colors) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE) +
  theme(legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(Trap_eff_zones, aes(x = Zones, y = Total, fill = Zones)) +
  geom_violin(trim = TRUE, adjust = 1.5, width = 0.75) +
  geom_jitter(shape = 16, size = 2, position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75, jitter.height = 0.2)) +
  geom_point(data = media_por_zona, aes(y = Total), color = "black", fill = "white", size = 4, shape = 21) +
  labs(x = "",
       y = "Captured snakes per trap") +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels=c("Outer area", "Intermediate area", "Inner area")) +
  stat_n_text(y.pos = NULL, color = "black", text.box = FALSE) +
  theme(legend.position = "none",
        axis.ticks.x = element_blank())


summary(aov(Total ~ factor(Zones),data = Trap_eff_zones))

TukeyHSD(aov(Total ~ factor(Zones),data = Trap_eff_zones))



### PART 3: Max abundance #####

# Here, there's an alternative way to look at the data: without replicates and only taking into account the max number of lizards observed per census.

max_number <- Urban_Refugia_MVG_seleccio %>%
  group_by(Project, Inv_state, Classifica, Latitude, Longitude, Place, Site_ID, Urbanization_index, `3P 250 200`, Distance) %>%
  summarize(max_number_lizards = max(Suma_total))


# Standarized histogram: How many times do we have each nº of sightings per experimental scenario
hist_values_max <- max_number %>% 
  group_by(Inv_state, Project, max_number_lizards) %>%
  dplyr::summarise(n=n())

# Total nº of sightings per experimental scenario
hist_values_max <- hist_values_max %>% 
  group_by(Inv_state, Project) %>%
  dplyr::mutate(total=sum(n))

# Summary table of total number of sightings
hist_values2_max <- max_number %>% 
  group_by(Inv_state, Project) %>%
  dplyr::summarise(n=n())

# Relative abundance of sightings categories (0, 1, 2, 3, ...)
hist_values_max <- hist_values_max %>% 
  group_by(Inv_state, Project, max_number_lizards) %>%
  dplyr::summarise(value=n/total)

# Standarized sighting abundance histogram (normal and logarithmic)
supp.labs <- c("Non-invaded", "Invaded")
names(supp.labs) <- c("NonInvaded", "Invaded")

xaxislab <- expression(paste("Total abundance of ", italic("P. pityusensis"), " per sampling point"))

theme_set(theme_minimal() + theme(legend.position = 'bottom') + theme(text = element_text(size = 17)))

ggplot(hist_values_max, 
       aes(fill = Project, 
           x = max_number_lizards, 
           y = value)) + 
  facet_wrap(vars(Inv_state), labeller = labeller(Inv_state = supp.labs)) +
  geom_bar(stat = "identity", 
           position=position_dodge(),
           color = "black") + 
  xlab(xaxislab) + 
  ylab("Percentage of censuses") +
  labs(fill = "Urbanisation situation") +
  scale_fill_manual(values=c("grey80", "red3"),
                    labels=c("Peri-urban", "Urban")) +
  scale_y_continuous(labels = scales::percent)



xaxislab1 <- expression(paste("Total abundance of ", italic("P. pityusensis"), " per sampling point"))

ggplot(data = max_number, 
       aes(x = (max_number_lizards), 
           color = Classifica, 
           fill = Classifica)) +
  scale_color_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                     labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                     name = "Sampling point status") + 
  labs(x= xaxislab1, y = "Density") + 
  scale_fill_manual(values=c("tomato1", "red3", "grey60", "grey30"),
                    labels=c("I-PU", "I-U", "NI-PU", "NI-U"),
                    name = "Sampling point status") +
  geom_density(lwd = 1.3, 
               alpha = 0.05)

setwd("C:/Users/marc9/Desktop/Marc/CREAF/Urban refugia")

path_csv <- file.path("C:/Users/marc9/Desktop/Marc/CREAF/Urban refugia", "max_number_lizards_ibiza_inv.csv")

write.csv2(max_number, file = path_csv, row.names = FALSE)








### PART 4: GLMM #####

# There are a lot of models we can use, with lots of justifications on which one to use and which ones not. Here, I'm going to go through the whole model selection process, with justifications on which ones are the ones we should consider.

## PART 4.1: Raw data #### 

# Do our data follow a Poisson distribution (mean = variance)
fpois((Urban_Refugia_MVG_seleccio$Suma_total))

fpois(log1p(Urban_Refugia_MVG_seleccio$Suma_total))

# Model without zeros doesn't follow a Poisson either... Truncated model wouldn't follow a Poisson... Consider Zero infalted negative binomial?
UR_no_zeros <- Urban_Refugia_MVG_seleccio[Urban_Refugia_MVG_seleccio$Suma_total != 0, ]

fpois((UR_no_zeros$Suma_total))

plot(density(UR_no_zeros$Suma_total))

hist(UR_no_zeros$Suma_total)

lines(0:14, dpois(0:14, lambda = 1), type = "l")

plot(0:14, dpois(0:14, lambda = 1), type = "l")


# variance is greater than mean, so no simple Poisson distribution is followed

# Percentage of 0 in our data (pretty high, we should consider a ZI model)
length(Urban_Refugia_MVG_seleccio$Suma_total[Urban_Refugia_MVG_seleccio$Suma_total == 0])/
  nrow(Urban_Refugia_MVG_seleccio) * 100

# To avoid different scales on variables, we are going to substract the minimum year a snake has been detected to each year. This way, the range of values for year of invasion will be from 0 to 10 aprox, instead of 2010-2020 aprox, compared to 0-1 from urbanisation index

#Urban_Refugia_MVG_seleccio$`3P 250 200` <- Urban_Refugia_MVG_seleccio$`3P 250 200` - min(Urban_Refugia_MVG_seleccio$`3P 250 200`)

# We divide by 10, in order to make it from 0 to 1, same as urbanisation index
#Urban_Refugia_MVG_seleccio$`3P 250 200` <- Urban_Refugia_MVG_seleccio$`3P 250 200`/10

#max(Urban_Refugia_MVG_seleccio$`3P 250 200`)


# Invert year of invasion variable. Substract maximum year (present) and do absolute value. Now it means years from the present, and divide by diff in years in order to obtain a 0-1 index


Urban_Refugia_MVG_seleccio_backup <- Urban_Refugia_MVG_seleccio

Urban_Refugia_MVG_seleccio <- Urban_Refugia_MVG_seleccio_backup



Urban_Refugia_MVG_seleccio$`3P 250 200` <- Urban_Refugia_MVG_seleccio$`3P 250 200` - max(Urban_Refugia_MVG_seleccio$`3P 250 200`)

diff_years <- max(Urban_Refugia_MVG_seleccio$`3P 250 200`) - min(Urban_Refugia_MVG_seleccio$`3P 250 200`)

Urban_Refugia_MVG_seleccio$`3P 250 200` <- abs(Urban_Refugia_MVG_seleccio$`3P 250 200`)

Urban_Refugia_MVG_seleccio$`3P 250 200` <- Urban_Refugia_MVG_seleccio$`3P 250 200`/diff_years





# PART 4.1.1: Negative binomial vs. Poisson distribution ####

# We might consider use a negative binomial model due to the differences between mean and variance. Even so, if the link of the model is logarithmic, we might have a Poisson distribution. Let's test which one is the best


# Negative binomial null model: Only random factors
NB_null <- glmmTMB(Suma_total ~ (1 | Place/Site_ID),  
                   family="nbinom2",
                   data = Urban_Refugia_MVG_seleccio)
summary(NB_null)

# Negative binomial model 1: Invasion state and habitat type as fixed factors
NB1 <- glmmTMB(Suma_total ~ Inv_state + Project + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB1)

# Negative binomial model 2: Invasion state and habitat type as fixed factors (interaction)
NB2 <- glmmTMB(Suma_total ~ Inv_state * Project + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB2)

# Negative binomial model 3: Invasion state and urbanization index as fixed factors
NB3 <- glmmTMB(Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB3)

# Negative binomial model 4: Invasion state and urbanization index as fixed factors (interaction)
NB4 <- glmmTMB(Suma_total ~ Inv_state * Urbanization_index + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB4)

# Negative binomial model 5: Year of invasion and urbanization index as fixed factors
NB5 <- glmmTMB(Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB5)

# Negative binomial model 6: Year of invasion and urbanization index as fixed factors (interaction)
NB6 <- glmmTMB(Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID),  
               family="nbinom2",
               data = Urban_Refugia_MVG_seleccio)
summary(NB6)

# Poisson null model: Only random factors
P_null <- glmer(Suma_total ~ (1 | Place/Site_ID), 
                family = poisson(), 
                data = Urban_Refugia_MVG_seleccio, 
                verbose = FALSE)
summary(P_null)

# Poisson model 1: Invasion state and habitat type as fixed factors
P1 <- glmer(Suma_total ~ Inv_state + Project + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P1)

# Poisson model 2: Invasion state and habitat type as fixed factors (interaction)
P2 <- glmer(Suma_total ~ Inv_state * Project + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P2)

# Poisson model 3: Invasion state and urbanization index as fixed factors
P3 <- glmer(Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P3)

# Poisson model 4: Invasion state and urbanization index as fixed factors (interaction)
P4 <- glmer(Suma_total ~ Inv_state * Urbanization_index + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P4)

# Poisson model 5: Year of invasion and urbanization index as fixed factors
P5 <- glmer(Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P5)

# Poisson model 6: Year of invasion and urbanization index as fixed factors (interaction)
P6 <- glmer(Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P6)


# Including year of invasion results in error. Related to scale? Year = 2000... , Urbanization index = 0-1. How can I rescale these variables? 

# Should I rescale both variables, or only one? Only in the case that I have two continuous variables, or also when I have one continuos and one discrete?


# Poisson vs. Negative binomial results
PvsNB_results <- sjPlot:: tab_model(P_null, P1, P2, P3, P4, P5, P6, NB_null, NB1, NB2, NB3, NB4, NB5, NB6,
                                    show.aicc = TRUE, 
                                    show.obs = FALSE, 
                                    show.ngroups = FALSE, 
                                    p.style = "stars", 
                                    show.intercept = FALSE,
                                    dv.labels = c("P null model", "P Model 1", "P Model 2", "P Model 3", "P Model 4", "P Model 5", "P Model 6", "NB null model", "NB Model 1", "NB Model 2", "NB Model 3", "NB Model 4", "NB Model 5", "NB Model 6"),
                                    string.p = "p-value",
                                    string.est = "Incidence Rate Ratios")

PvsNB_results


# AIC ranking
AICtab(P_null, P1, P2, P3, P4, P5, P6, NB_null, NB1, NB2, NB3, NB4, NB5, NB6)

# The best models are by far, the ones that include year of invasion. This variable, though, is tricky to use, so for simplicity, we are going to use state of invasion. At the end, we are going to check if state of invasion and year of invasion tell the same story or not, and then decide if its better to just use the simple one or the complete one.

# Best alternative models: NB3 and P3 (not comparable bw them)
# Also, there are "no diff" bw model 3 and 4 in both cases

# Residual vs. fitted of both models (Poisson has less dispersion)
par(mfrow = c(1,2))

plot(fitted(NB5), residuals(NB5), main = "Negative Binomial")

plot(fitted(P5), residuals(P5), main = "Poisson")

par(mfrow = c(1,1))

# Overdispersion analysis. If there were overdispersion: NB. If there is not overdispersion: P
check_overdispersion(P5)

# No overdispersion detected: we stick to a Poisson model


# PART 4.1.2: Zero Inflated models ####

# Zero inflation analysis
check_zeroinflation(P5, tolerance = 0.05)

check_zeroinflation(P5, tolerance = 0.1)
# Depending on the tolerance, we might have more zeros than expected by the model in our data. If we consider that our data is zero inflated, there are 2 ways of proceeding:

# Zero inflated model: First, we test if our data has more 0 than expected by the normal distribution (Poisson). After assesing if there are more 0 than expected or not, we evaluate our response variable with our model. The 0 is included as a possible output of our response variable.

# Hurdle model: First, we investigate the source of our 0, and see if there is any variable that adds more 0 to our data than expected by the model. Then, we evaluate the response variable with a truncated model. This means that 0 are not included in the analysis, only values <1. We consider that 0 is not a possible output of our data, and that if there is a 0, it is caused by any of the positive significative variables of the zero inflated part of the model.

# Poisson model 3: Invasion state and urbanization index as fixed factors (best model in the previous analysis)
P3 <- glmer(Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID), 
            family = poisson(), 
            data = Urban_Refugia_MVG_seleccio, 
            verbose = FALSE)
summary(P3)

# Zero Inflated Poisson (ZIP) null model : Only random variables and unidentified source of 0 as predictor variable
ZIP_null <- glmmTMB(
  Suma_total ~ (1 | Place/Site_ID),
  ziformula = ~ 1, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP_null)

# Zero Inflated Truncated Poisson (ZITP) null model : Only random variables and unidentified source of 0 as predictor variable
ZITP_null <- glmmTMB(
  Suma_total ~ (1 | Place/Site_ID),
  ziformula = ~ 1, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP_null)

# ZIP simple model: Unidentified source of 0 as predictor variable
ZIP3_1 <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ 1, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_1)

# ZITP simple model: Unidentified source of 0 as predictor variable
ZITP3_1 <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ 1, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_1)

# ZIP state model: Invasion state (snake or not snake) as predictor variable for zero inflated model
ZIP3_State <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_State)

# ZITP state model: Invasion state (snake or not snake) as predictor variable for zero inflated model
ZITP3_State <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_State)

# ZIP state model: Invasion state (snake or not snake) and urbanization index as predictor variable for zero inflated model
ZIP3_State_Urb <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state + Urbanization_index, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_State_Urb)

# ZITP state model: Invasion state (snake or not snake) and urbanization index as predictor variable for zero inflated mode
ZITP3_State_Urb <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state + Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_State_Urb)


ZITP3_State_Urb <- glmmTMB(
  Suma_total ~ Inv_state + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state + Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_State_Urb)

# Interaction
ZITP3_State_Urb_int <- glmmTMB(
  Suma_total ~ Inv_state * Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ Inv_state * Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_State_Urb_int)


# ZIP vs. ZITP summary table
PandZIP_results <- sjPlot:: tab_model(P3, ZIP_null, ZITP_null, ZIP3_1, ZITP3_1, ZIP3_State, ZITP3_State, ZIP3_State_Urb, ZITP3_State_Urb,
                                      show.aicc = TRUE, 
                                      show.obs = FALSE, 
                                      show.ngroups = FALSE, 
                                      p.style = "stars", 
                                      show.intercept = TRUE,
                                      dv.labels = c("P Model 3", "ZIP Null Model", "ZITP Null Model", "ZIP3 Simple Model", "ZITP3 Simple Model", "ZIP3 State Model", "ZITP3 State Model", "ZIP3 Complete Model", "ZITP3 Complete Model"),
                                      string.p = "p-value",
                                      string.est = "Incidence Rate Ratios")





PandZIP_results

# AIC ranking
AICtab(P3, ZIP_null, ZITP_null, ZIP3_1, ZITP3_1, ZIP3_State, ZITP3_State, ZIP3_State_Urb, ZITP3_State_Urb, ZITP3_State_Urb_int)

# Best models of each type of ZI model: ZITP3_State_Urb and ZIP3_State_Urb

# Now with year of invasion

# ZITP Year
ZITP3_Year_Urb <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_Year_Urb)

# ZITP: interaction
ZITP3_Year_Urb_int <- glmmTMB(
  Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` * Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_Year_Urb_int)

# ZITP all variables
ZITP3_Year_Urb_State <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + Inv_state + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index + Inv_state, 
  family = truncated_poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITP3_Year_Urb_State)

# ZIP Year + Urb
ZIP3_Year_Urb <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_Year_Urb)


ZIP3_Year_Urb_int <- glmmTMB(
  Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` * Urbanization_index, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_Year_Urb_int)


# Year of invasion or not
PandZIP_results <- sjPlot:: tab_model(ZITP3_State_Urb, ZITP3_Year_Urb, ZITP3_Year_Urb_int,  ZITP3_Year_Urb_State,
                                      show.aicc = TRUE, 
                                      show.obs = FALSE, 
                                      show.ngroups = FALSE, 
                                      p.style = "stars", 
                                      show.intercept = TRUE,
                                      transform = NULL,
                                      dv.labels = c("ZITP3 Complete Model", "ZIPT3 Year", "ZIPT3 Year interaction", "ZITP3 Complete Model + Year"),
                                      string.p = "p-value",
                                      string.est = "Incidence Rate Ratios")

PandZIP_results

AICtab(ZITP3_State_Urb, ZITP3_Year_Urb, ZITP3_Year_Urb_int, ZITP3_Year_Urb_State, ZIP3_Year_Urb)

# The best model is the one that includes all variables, but as year of invasion and invasion state are kind of repetitive, we are going to go for the next one. The other best model is the one with year as response variable.

# This model indicates that: intercept is significant (I don't know what it means), that year of invasion has a negative effect on the ZI part of the model (the bigger the year (more recent), the less 0 we observe), and the same for urbanization index (the higher the index, the less 0 we observe). As for the truncated poisson, only the intercept is significant (no idea what it means) and the urbanization index, with higher abundances with higher urbanization indexes. 

# The previous better model (the one with invasion state) is similar: invasion state is positive significant (snake = more 0), Urbanization is negative (more urbanization = less 0), and for the truncated Poisson, urbanization index is marginally non significant (p = 0.0585).

# We can say that both models explain the same, and that depending on the situation, we could take one or the other, depending if we want to make the explanation easier or not.

# Let's try Zero Inflated Truncated/non-Truncated Negative binomial, as truncated data don't follow a Poisson either.

# ZITNB 
ZITNB3_Year_Urb <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index, 
  family = truncated_nbinom2, 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITNB3_Year_Urb)

# ZITNB interaction
ZITNB3_Year_Urb_int <- glmmTMB(
  Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` * Urbanization_index, 
  family = truncated_nbinom2(), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZITNB3_Year_Urb_int)

# ZINB: the results aren't okay
ZINB3_Year_Urb <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index, 
  family = nbinom2, 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZINB3_Year_Urb)

ZINB3_Year_Urb_int <- glmmTMB(
  Suma_total ~ `3P 250 200` * Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` * Urbanization_index, 
  family = nbinom2, 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZINB3_Year_Urb_int)


sjPlot::tab_model(ZITP3_Year_Urb, ZITNB3_Year_Urb, ZIP3_Year_Urb, ZINB3_Year_Urb,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZITP", "ZITNB", "ZIP", "ZINB"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")


# 8 best models
sjPlot::tab_model(ZITP3_Year_Urb, ZITP3_Year_Urb_int,  ZITNB3_Year_Urb, ZITNB3_Year_Urb_int, ZIP3_Year_Urb, ZIP3_Year_Urb_int,  ZINB3_Year_Urb, ZINB3_Year_Urb_int,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZITP", "ZITP int", "ZITNB", "ZITNB int", "ZIP", "ZIP int", "ZINB", "ZINB int"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")



# So, final decision. We are going to use the ZIP with year of invasion and urbanisation index as fixed factors. Why?

# First of all, while it is true that there are possibly lizards everywhere before the arrival of the snake, I think the 0's also make sense within the counting process. By doing the Truncated Poisson, we would be saying that the 0's are only caused by the snake, when I think it could also be caused by lack of sampling or that simply, in this small sampled space they do not exist. It seems more conservative to me to assume that we may have 0's for extra reasons than snake alone, even though we know that snake is a very important factor in explaining local extirpation. Regardless of how we understand 0 in an ecological sense, the results of both models indicate the same thing: that the presence of snakes increases the number of 0 observed, that the older the invasion, the more 0 we observe, and that in addition urbanization, the fewer 0 we find.

# Second, I believe that the results that the ZIP gives us for the Poisson part are more biologically accurate than those of the ZITP model. In the ZITP model, the only cause that modified the abundance of lizards was the rate of urbanization. In contrast, in the ZIP model, the timing of snake arrival is also a factor affecting total lizard abundance. To me, this makes more sense than assuming that the only factor affecting abundances is the degree of urbanization.

# Looking at the random factors, which until now I had not looked at very much, I see that those of the ZIP model also make more sense than those of the ZITP model. The ZIP model has a between-locality variance of 0.76, while the ZITP model has a between-locality variance of 0.11. The variance between sampling sites within the same locality for the ZIP model is 0.33, and for the ZITP model it is 0.30. What does the ZIP model tell us? That between different localities the variability is higher than within a single locality, which makes sense, since populations are more different between them (invaded or non-invaded) than between sampling sites within a single locality (it may be different between urban and peri-urban, but all points will behave similarly depending on whether or not they are invaded). In the ZITP model, on the other hand, the variability between localities is less than between sampling points within the same locality, which to me doesn't make much sense, does it? I would say that this is because by removing all 0's from one part of the model (truncated Poisson), you remove the between-locality variability and homogenize the samples further, and only the differences between urban and non-urban stand out (between-sampling-point variance). As a result, it is logical to expect the overall variance of the ZIP model to be higher than that of the ZITP model (0.81 vs. 0.11, respectively).

# Finally, the marginal R2 (only explained by the fixed factors) is higher in the ZIP model than in the ZITP. The conditional (including fixed factors) is higher in the ZITP model than in the ZIP model. I consider that it is more important that that of the fixed factors is higher than the total R2.

# I have also done a Zero Inflated Truncated Negative Binomial model, since Roberto hinted us to consider it as well, but it does not convince me for the same reasons as the ZITP. Also, one of the tests you can do to choose between Poisson or Negative Binomial is an overdispersion test, and the preliminary Zero Inflated Truncated models do not have overdispersion, so we should stick with a Poisson model, not a Binomial.

# ZIP Year + Urb
ZIP3_Year_Urb <- glmmTMB(
  Suma_total ~ `3P 250 200` + Urbanization_index + (1 | Place/Site_ID),
  ziformula = ~ `3P 250 200` + Urbanization_index, 
  family = poisson(link = "log"), 
  data = Urban_Refugia_MVG_seleccio
)
summary(ZIP3_Year_Urb)

sjPlot::tab_model(ZIP3_Year_Urb,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZIP"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")


# ANOVA tests

# ZIP3_Year_Urb
glmmTMB:::Anova.glmmTMB(ZIP3_Year_Urb, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "cond")

glmmTMB:::Anova.glmmTMB(ZIP3_Year_Urb, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "zi")

# ZITP3_Year_Urb
glmmTMB:::Anova.glmmTMB(ZITP3_Year_Urb, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "cond")

glmmTMB:::Anova.glmmTMB(ZITP3_Year_Urb, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "zi")





## PART 4.2: Max abundance models ####

# We might consider use a negative binomial model due to the differences between mean and variance. Even so, if the link of the model is logarithmic, we might have a Poisson distribution. Let's test which one is the best

# PART 4.2.1: Negative binomial vs. Poisson distribution ####

# Negative binomial null model: Only random factors
NB_null_max <- glmmTMB(max_number_lizards ~ (1 | Place),  
                   family="nbinom2",
                   data = max_number)
summary(NB_null_max)

# Negative binomial model 1: Invasion state and habitat type as fixed factors
NB1_max <- glmmTMB(max_number_lizards ~ Inv_state + Project + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB1_max)

# Negative binomial model 2: Invasion state and habitat type as fixed factors (interaction)
NB2_max <- glmmTMB(max_number_lizards ~ Inv_state * Project + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB2_max)

# Negative binomial model 3: Invasion state and urbanization index as fixed factors
NB3_max <- glmmTMB(max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB3_max)

# Negative binomial model 4: Invasion state and urbanization index as fixed factors (interaction)
NB4_max <- glmmTMB(max_number_lizards ~ Inv_state * Urbanization_index + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB4_max)

# Negative binomial model 5: Year of invasion and urbanization index as fixed factors
NB5_max <- glmmTMB(max_number_lizards ~ scale(`3P 250 200`) + Urbanization_index + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB5_max)

# Negative binomial model 6: Year of invasion and urbanization index as fixed factors (interaction)
NB6_max <- glmmTMB(max_number_lizards ~ scale(`3P 250 200`) * Urbanization_index + (1 | Place),  
               family="nbinom2",
               data = max_number)
summary(NB6_max)

# Poisson null model: Only random factors
P_null_max <- glmer(max_number_lizards ~ (1 | Place), 
                family = poisson(), 
                data = max_number, 
                verbose = FALSE)
summary(P_null_max)

# Poisson model 1: Invasion state and habitat type as fixed factors
P1_max <- glmer(max_number_lizards ~ Inv_state + Project + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P1_max)

# Poisson model 2: Invasion state and habitat type as fixed factors (interaction)
P2_max <- glmer(max_number_lizards ~ Inv_state * Project + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P2_max)

# Poisson model 3: Invasion state and urbanization index as fixed factors
P3_max <- glmer(max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P3_max)

# Poisson model 4: Invasion state and urbanization index as fixed factors (interaction)
P4_max <- glmer(max_number_lizards ~ Inv_state * Urbanization_index + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P4_max)

# Poisson model 5: Year of invasion and urbanization index as fixed factors
P5_max <- glmer(max_number_lizards ~ scale(`3P 250 200`) + Urbanization_index + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P5_max)

# Poisson model 6: Year of invasion and urbanization index as fixed factors (interaction)
P6_max <- glmer(max_number_lizards ~ scale(`3P 250 200`) * Urbanization_index + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P6_max)

# Including year of invasion results in error. Related to scale? Year = 2000... , Urbanization index = 0-1. How can I rescale these variables? 

# Should I rescale both variables, or only one? Only in the case that I have two continuous variables, or also when I have one continuos and one discrete?


# Poisson vs. Negative binomial results
PvsNB_results <- sjPlot:: tab_model(P_null_max, P1_max, P2_max, P3_max, P4_max, P5_max, P6_max, NB_null_max, NB1_max, NB2_max, NB3_max, NB4_max, NB5_max, NB6_max,
                                    show.aicc = TRUE, 
                                    show.obs = FALSE, 
                                    show.ngroups = FALSE, 
                                    p.style = "stars", 
                                    show.intercept = FALSE,
                                    dv.labels = c("P null model", "P Model 1", "P Model 2", "P Model 3", "P Model 4", "P Model 5", "P Model 6", "NB null model", "NB Model 1", "NB Model 2", "NB Model 3", "NB Model 4", "NB Model 5", "NB Model 6"),
                                    string.p = "p-value",
                                    string.est = "Incidence Rate Ratios")

PvsNB_results


# AIC ranking
AICtab(P_null_max, P1_max, P2_max, P3_max, P4_max, P5_max, P6_max, NB_null_max, NB1_max, NB2_max, NB3_max, NB4_max, NB5_max, NB6_max)

# The best models are by far, the ones that include year of invasion. This variable, though, is tricky to use, so for simplicity, we are going to use state of invasion. At the end, we are going to check if state of invasion and year of invasion tell the same story or not, and then decide if its better to just use the simple one or the complete one.

# Best alternative models: NB3 and P3 (not comparable bw them)
# Also, there are "no diff" bw model 3 and 4 in both cases

# Residual vs. fitted of both models (Poisson has less dispersion)
par(mfrow = c(1,2))

plot(fitted(NB3_max), residuals(NB3_max), main = "Negative Binomial")

plot(fitted(P3_max), residuals(P3_max), main = "Poisson")

par(mfrow = c(1,1))

# Overdispersion analysis. If there were overdispersion: NB. If there is not overdispersion: P
check_overdispersion(P3_max)

# No overdispersion detected: we stick to a Poisson model








# PART 4.2.2: Zero Inflated models ####

# Zero inflation analysis
check_zeroinflation(P3_max, tolerance = 0.05)

check_zeroinflation(P3_max, tolerance = 0.1)
# Depending on the tolerance, we might have more zeros than expected by the model in our data. If we consider that our data is zero inflated, there are 2 ways of proceeding:

# Zero inflated model: First, we test if our data has more 0 than expected by the normal distribution (Poisson). After assesing if there are more 0 than expected or not, we evaluate our response variable with our model. The 0 is included as a possible output of our response variable.

# Hurdle model: First, we investigate the source of our 0, and see if there is any variable that adds more 0 to our data than expected by the model. Then, we evaluate the response variable with a truncated model. This means that 0 are not included in the analysis, only values <1. We consider that 0 is not a possible output of our data, and that if there is a 0, it is caused by any of the positive significative variables of the zero inflated part of the model.

# Poisson model 3: Invasion state and urbanization index as fixed factors (best model in the previous analysis)
P3_max <- glmer(max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place), 
            family = poisson(), 
            data = max_number, 
            verbose = FALSE)
summary(P3_max)

# Zero Inflated Poisson (ZIP) null model : Only random variables and unidentified source of 0 as predictor variable
ZIP_null_max <- glmmTMB(
  max_number_lizards ~ (1 | Place),
  ziformula = ~ 1, 
  family = poisson(link = "log"), 
  data = max_number
)
summary(ZIP_null_max)

# Zero Inflated Truncated Poisson (ZITP) null model : Only random variables and unidentified source of 0 as predictor variable
ZITP_null_max <- glmmTMB(
  max_number_lizards ~ (1 | Place),
  ziformula = ~ 1, 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP_null_max)

# ZIP simple model: Unidentified source of 0 as predictor variable
ZIP3_1_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ 1, 
  family = poisson(link = "log"), 
  data = max_number
)
summary(ZIP3_1_max)

# ZITP simple model: Unidentified source of 0 as predictor variable
ZITP3_1_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ 1, 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_1_max)

# ZIP state model: Invasion state (snake or not snake) as predictor variable for zero inflated model
ZIP3_State_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ Inv_state, 
  family = poisson(link = "log"), 
  data = max_number
)
summary(ZIP3_State_max)

# ZITP state model: Invasion state (snake or not snake) as predictor variable for zero inflated model
ZITP3_State_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ Inv_state, 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_State_max)

# ZIP state model: Invasion state (snake or not snake) and urbanization index as predictor variable for zero inflated model
ZIP3_State_Urb_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ Inv_state + Urbanization_index, 
  family = poisson(link = "log"), 
  data = max_number
)
summary(ZIP3_State_Urb_max)

# ZITP state model: Invasion state (snake or not snake) and urbanization index as predictor variable for zero inflated mode
ZITP3_State_Urb_max <- glmmTMB(
  max_number_lizards ~ Inv_state + Urbanization_index + (1 | Place),
  ziformula = ~ Inv_state + Urbanization_index, 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_State_Urb_max)


ZITP3_State_Urb_max <- glmmTMB(
  max_number_lizards ~ Inv_state + scale(Urbanization_index) + (1 | Place),
  ziformula = ~ Inv_state + scale(Urbanization_index), 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_State_Urb_max)


# ZIP vs. ZITP summary table
PandZIP_results <- sjPlot:: tab_model(P3_max, ZIP_null_max, ZITP_null_max, ZIP3_1_max, ZITP3_1_max, ZIP3_State_max, ZITP3_State_max, ZIP3_State_Urb_max, ZITP3_State_Urb_max,
                                      show.aicc = TRUE, 
                                      show.obs = FALSE, 
                                      show.ngroups = FALSE, 
                                      p.style = "stars", 
                                      show.intercept = TRUE,
                                      dv.labels = c("P Model 3", "ZIP Null Model", "ZITP Null Model", "ZIP3 Simple Model", "ZITP3 Simple Model", "ZIP3 State Model", "ZITP3 State Model", "ZIP3 Complete Model", "ZITP3 Complete Model"),
                                      string.p = "p-value",
                                      string.est = "Incidence Rate Ratios")





PandZIP_results

# AIC ranking
AICtab(P3_max, ZIP_null_max, ZITP_null_max, ZIP3_1_max, ZITP3_1_max, ZIP3_State_max, ZITP3_State_max, ZIP3_State_Urb_max, ZITP3_State_Urb_max)

# Best models of each type of ZI model: ZITP3_State_Urb and ZIP3_State_Urb

# Now with year of invasion

ZITP3_Year_Urb_max <- glmmTMB(
  max_number_lizards ~ scale(`3P 250 200`) + scale(Urbanization_index) + (1 | Place/Site_ID),
  ziformula = ~ scale(`3P 250 200`) + scale(Urbanization_index), 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_Year_Urb_max)


ZITP3_Year_Urb_State_max <- glmmTMB(
  max_number_lizards ~ scale(`3P 250 200`) + scale(Urbanization_index) + Inv_state + (1 | Place/Site_ID),
  ziformula = ~ scale(`3P 250 200`) + scale(Urbanization_index) + Inv_state, 
  family = truncated_poisson(link = "log"), 
  data = max_number
)
summary(ZITP3_Year_Urb_State_max)


# Year of invasion or not
PandZIP_results <- sjPlot:: tab_model(ZITP3_State_Urb_max, ZITP3_Year_Urb_max, ZITP3_Year_Urb_State_max,
                                      show.aicc = TRUE, 
                                      show.obs = FALSE, 
                                      show.ngroups = FALSE, 
                                      p.style = "stars", 
                                      show.intercept = TRUE,
                                      transform = NULL,
                                      dv.labels = c("ZITP3 Complete Model", "ZIPT3 Year", "ZITP3 Complete Model + Year"),
                                      string.p = "p-value",
                                      string.est = "Incidence Rate Ratios")

PandZIP_results

AICtab(ZITP3_State_Urb_max, ZITP3_Year_Urb_max, ZITP3_Year_Urb_State_max)

# The best model is the one that includes all variables, but as year of invasion and invasion state are kind of repetitive, we are going to go for the next one. The other best model is the one with year as response variable.

# This model indicates that: intercept is significant (I don't know what it means), that year of invasion has a negative effect on the ZI part of the model (the bigger the year (more recent), the less 0 we observe), and the same for urbanization index (the higher the index, the less 0 we observe). As for the truncated poisson, the intercept is significant (no idea what it means), the urbanization index (higher abundances with higher urbanization indexes) and year of invasion, meaning that the older the snakes arrive, the more abundace there are, which makes sense. 

# The previous better model (the one with invasion state) is similar: invasion state is positive significant (snake = more 0), Urbanization is negative (more urbanization = less 0), and for the truncated Poisson, urbanization index is significant.

# We can say that both models explain the same, and that depending on the situation, we could take one or the other, depending if we want to make the explanation easier or not.

# ANOVA tests

# ZITP3_State_Urb
glmmTMB:::Anova.glmmTMB(ZITP3_State_Urb_max, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "cond")

glmmTMB:::Anova.glmmTMB(ZITP3_State_Urb_max, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "zi")

# ZITP3_Year_Urb
glmmTMB:::Anova.glmmTMB(ZITP3_Year_Urb_max, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "cond")

glmmTMB:::Anova.glmmTMB(ZITP3_Year_Urb_max, 
                        type = 2, 
                        test.statistic = "Chisq", 
                        component = "zi")











### PART 5: Spatial autocorrelation ####

# ZITP3_State_Urb
# Extract residuals of model
res_ZITP3 <- as.data.frame(residuals(ZITP3_State_Urb))
colnames(res_ZITP3) <- c("residuals_P3")

# Add columns of interest
res_ZITP3 <- res_ZITP3 %>%
  add_column(Place = Urban_Refugia_MVG_cens_total_net$Place,
             Site = Urban_Refugia_MVG_cens_total_net$Site_ID,
             Project = Urban_Refugia_MVG_cens_total_net$Project,
             Latitude = Urban_Refugia_MVG_seleccio$Latitude,
             Longitude = Urban_Refugia_MVG_seleccio$Longitude)

# Move the new columns to the beginning
res_ZITP3 <- res_ZITP3 %>%
  relocate(c(Place, Site, Project, Latitude, Longitude), .before = residuals_P3)

# Delete duplications
res_ZITP3 <- res_ZITP3 %>% group_by(Place, Site, Latitude, Longitude) %>%
  dplyr::summarise(across(c(residuals_P3), list(mean)))

# Distance matrix between sampling points
dist_matrix <- dist(Urban_Refugia_MVG_seleccio[,c(7,8)], diag=T, upper=T)

dist_matrix <- as.data.frame(as.matrix(dist_matrix))

# Spatial autocorrelation plot (100 partitions or default partition)
correlog_ZITP3 <- pgirmess::correlog(res_ZITP3[,c(4,3)], res_ZITP3$residuals_P3_1, method="Moran", nbclass = 100)

correlog_ZITP3 <- pgirmess::correlog(res_ZITP3[,c(4,3)], res_ZITP3$residuals_P3_1, method="Moran", nbclass = NULL)

# Plot spatial autocorrelation analysis
plot(correlog_ZITP3)

# The function does not plot well the significance, we have to do it ourselves
correlog_ZITP3 <- as.data.frame(as.matrix(correlog_ZITP3))

# See how coefficients and p values are distributed
par(mfrow = c(1,2))

hist(correlog_ZITP3$coef,  breaks = 100)

hist(correlog_ZITP3$p.value, breaks = 100)

par(mfrow = c(1,1))

# Plot corrected plot, with correct significances
plot(correlog_ZITP3$coef ~ correlog_ZITP3$dist.class, type = "b")

for(i in 1:length(correlog_ZITP3$coef)){
  
  if(correlog_ZITP3$p.value[i] < 0.025){
    
    points(correlog_ZITP3$dist.class[i], correlog_ZITP3$coef[i], col = "red", pch = 19, cex = 1.5)
    
  } else if (correlog_ZITP3$p.value[i] > 0.975){
    
    points(correlog_ZITP3$dist.class[i], correlog_ZITP3$coef[i], col = "blue", pch = 19,  cex = 1.5)
    
  }
  
}

# Linear regression
lm_correlog_ZITP3 <- lm(coef ~ dist.class, data = correlog_ZITP3)

summary(lm_correlog_ZITP3)

abline(lm_correlog_ZITP3)

# To perform a spatial autocorrelation test, we have to make a distance matrix without duplications

dist_matrix <- dist(res_ZITP3[,c(3,4)], diag=T, upper=T)

dist_matrix <- as.data.frame(as.matrix(dist_matrix))

testSpatialAutocorrelation(res_ZITP3$residuals_P3_1,
                           distMat = dist_matrix)

# There is spatial autocorrelation. We should delete points, understand what causes the spatial autocorrelation, and try to add a new variable that explains this spatial autocorrelation (spoiler: it does not work well :(   )


# ZIP3_Year_Urb
res_ZITP3 <- as.data.frame(residuals(ZIP3_Year_Urb))
colnames(res_ZITP3) <- c("residuals_P3")

# Add columns of interest
res_ZITP3 <- res_ZITP3 %>%
  add_column(Place = Urban_Refugia_MVG_cens_total_net$Place,
             Site = Urban_Refugia_MVG_cens_total_net$Site_ID,
             Project = Urban_Refugia_MVG_cens_total_net$Project,
             Latitude = Urban_Refugia_MVG_seleccio$Latitude,
             Longitude = Urban_Refugia_MVG_seleccio$Longitude)

# Move the new columns to the beginning
res_ZITP3 <- res_ZITP3 %>%
  relocate(c(Place, Site, Project, Latitude, Longitude), .before = residuals_P3)

# Delete duplications
res_ZITP3 <- res_ZITP3 %>% group_by(Place, Site, Latitude, Longitude) %>%
  dplyr::summarise(across(c(residuals_P3), list(mean)))

# Distance matrix between sampling points
dist_matrix <- dist(Urban_Refugia_MVG_seleccio[,c(7,8)], diag=T, upper=T)

dist_matrix <- as.data.frame(as.matrix(dist_matrix))

# Spatial autocorrelation plot (100 partitions or default partition)
correlog_ZITP3 <- pgirmess::correlog(res_ZITP3[,c(4,3)], res_ZITP3$residuals_P3_1, method="Moran", nbclass = 100)

correlog_ZITP3 <- pgirmess::correlog(res_ZITP3[,c(4,3)], res_ZITP3$residuals_P3_1, method="Moran", nbclass = NULL)

# Plot spatial autocorrelation analysis
plot(correlog_ZITP3)

# The function does not plot well the significance, we have to do it ourselves
correlog_ZITP3 <- as.data.frame(as.matrix(correlog_ZITP3))

# See how coefficients and p values are distributed
par(mfrow = c(1,2))

hist(correlog_ZITP3$coef,  breaks = 100)

hist(correlog_ZITP3$p.value, breaks = 100)

par(mfrow = c(1,1))

# Plot corrected plot, with correct significances
plot(correlog_ZITP3$coef ~ correlog_ZITP3$dist.class, type = "b")

for(i in 1:length(correlog_ZITP3$coef)){
  
  if(correlog_ZITP3$p.value[i] < 0.025){
    
    points(correlog_ZITP3$dist.class[i], correlog_ZITP3$coef[i], col = "red", pch = 19, cex = 1.5)
    
  } else if (correlog_ZITP3$p.value[i] > 0.975){
    
    points(correlog_ZITP3$dist.class[i], correlog_ZITP3$coef[i], col = "blue", pch = 19,  cex = 1.5)
    
  }
  
}

# Linear regression
lm_correlog_ZITP3 <- lm(coef ~ dist.class, data = correlog_ZITP3)

summary(lm_correlog_ZITP3)

abline(lm_correlog_ZITP3)

# To perform a spatial autocorrelation test, we have to make a distance matrix without duplications

dist_matrix <- dist(res_ZITP3[,c(3,4)], diag=T, upper=T)

dist_matrix <- as.data.frame(as.matrix(dist_matrix))

testSpatialAutocorrelation(res_ZITP3$residuals_P3_1,
                           distMat = dist_matrix)


# There is no spatial autocorrelation.





### PART 6: ZITP plot ####
## PART 6.1: ZITP plot with year of invasion as variable ####

sjPlot::tab_model(ZITP3_Year_Urb,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZITP3 State Model"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")

summary(ZITP3_Year_Urb)

data_ZITP3_Year_Urb <- data.frame(
  Model  = as.factor(c("Poisson", "Poisson", "Poisson", "Zero_inf", "Zero_inf", "Zero_inf")),
  Variable = as.factor(c("Urb_index", "Year_inv", "Intercept", "Urb_index", "Year_inv", "Intercept")),
  Value = c(0.28, 0.26, 0.46, -0.56, -1.58, 1.15),
  Lower = c(0.05, -0.10, 0.06, -0.85, -1.97, 0.81),
  Upper = c(0.51, 0.63, 0.87, -0.27, -1.18, 1.49),
  pvalue = as.factor(c("< 0.5", "> 0.5", "< 0.5", "< 0.001", "< 0.001", "< 0.001")))

#define colours for dots and bars
dotCOLS = c("grey80","tomato1")
barCOLS = c("grey30","red3")


ggplot(data_ZITP3_Year_Urb, aes(x=Variable, y=Value, ymin=Lower, ymax=Upper, col=Model, fill=Model)) + 
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  geom_point(size=3, shape=21, colour="white", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS, labels = c("Truncated-Poisson", "Zero-inflated")) +
  scale_color_manual(values=dotCOLS, labels = c("Truncated-Poisson", "Zero-inflated")) +
  scale_x_discrete(name="Fixed factors", limits=rev,  labels=c('Year of invasion', 'Urbanisation index', 'Intercept')) +
  scale_y_continuous(name="Predictor value", limits = c(min(data_ZITP3_Year_Urb$Lower), max(data_ZITP3_Year_Urb$Upper))) +
  expand_limits(y=c(0.1, 50)) + 
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE)) +
  coord_flip() +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 17))





## PART 6.2: ZIP plot with year of invasion as variable ####

sjPlot::tab_model(ZIP3_Year_Urb,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZIP3 State Model"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")

summary(ZIP3_Year_Urb)

data_ZIP3_Year_Urb <- data.frame(
  Model  = as.factor(c("Poisson", "Poisson", "Poisson", "Zero_inf", "Zero_inf", "Zero_inf")),
  Variable = as.factor(c("Urb_index", "Year_inv", "Intercept", "Urb_index", "Year_inv", "Intercept")),
  Value = c(0.75, -2.68, 0.33, -1.82, 4.53, -0.91),
  Lower = c(0.06, -4.32, -0.41, -3.50, 2.29, -1.63),
  Upper = c(1.44, -0.64, 1.06, -0.14, 6.76, -0.19),
  pvalue = as.factor(c("< 0.05", "< 0.05", "> 0.05", "< 0.05", "< 0.001", "< 0.05")))

#define colours for dots and bars
dotCOLS = c("grey80","tomato1")
barCOLS = c("grey30","red3")


ggplot(data_ZIP3_Year_Urb, aes(x=Variable, y=Value, ymin=Lower, ymax=Upper, col=Model, fill=Model)) + 
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  geom_point(size=3, shape=21, colour="black", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS, labels = c("Poisson", "Zero-inflated")) +
  scale_color_manual(values=dotCOLS, labels = c("Poisson", "Zero-inflated")) +
  scale_x_discrete(name="Fixed factors", limits=rev,  labels=c('Year of\ninvasion', 'Urbanization\n index', 'Intercept')) +
  scale_y_continuous(name="Odds ratio", limits = c(min(data_ZIP3_Year_Urb$Lower), max(data_ZIP3_Year_Urb$Upper))) +
  expand_limits(y=c(0.1, 50)) + 
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE)) +
  coord_flip() +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 17),
        axis.title.y = element_text(vjust = 2))

ggplot(data_ZIP3_Year_Urb, aes(x = Variable, y = Value, ymin = Lower, ymax = Upper, col = Model, fill = Model)) + 
  geom_linerange(size = 5, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(size = 3, shape = 21, colour = "black", stroke = 0.5, position = position_dodge(width = 0.5)) +
  scale_fill_manual(values = barCOLS, labels = c("Poisson", "Zero-inflated")) +
  scale_color_manual(values = dotCOLS, labels = c("Poisson", "Zero-inflated")) +
  scale_x_discrete(name = "Fixed factors", limits = rev, labels = c('Year of\ninvasion', 'Urbanization\n index', 'Intercept')) +
  scale_y_continuous(name = "Odds ratio", limits = c(min(data_ZIP3_Year_Urb$Lower), max(data_ZIP3_Year_Urb$Upper))) +
  expand_limits(y = c(0.1, 50)) + 
  guides(color = guide_legend(reverse = TRUE), fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "bottom",
        text = element_text(size = 17),
        axis.title.y = element_text(vjust = 2),
        panel.grid.major = element_blank(),  
        panel.grid.minor = element_blank(),  
        panel.background = element_blank(),  
        axis.line = element_line(),
        axis.ticks = element_line())


## PART 6.3: ZITP plot with state of invasion as variable ####

sjPlot::tab_model(ZITP3_State_Urb,
                  show.aicc = TRUE, 
                  show.obs = FALSE, 
                  show.ngroups = FALSE, 
                  p.style = "stars",
                  transform = NULL,
                  show.intercept = TRUE,
                  dv.labels = c("ZIP3 State Model"),
                  string.p = "p-value",
                  string.est = "Incidence Rate Ratios")

summary(ZITP3_State_Urb)

data_ZITP3_State_Urb <- data.frame(
  Model  = as.factor(c("Poisson", "Poisson", "Poisson", "Zero_inf", "Zero_inf", "Zero_inf")),
  Variable = as.factor(c("Urb_index", "Inv_state", "Intercept", "Urb_index", "Inv_state", "Intercept")),
  Value = c(0.21, 0.32, 0.49, -0.39, 1.89, -0.41),
  Lower = c(-0.01, -0.24, 0.06, -0.65, 1.35, -0.82),
  Upper = c(0.44, 0.87, 0.91, -0.12, 2.43, -0.01),
  pvalue = as.factor(c("< 0.1", "> 0.5", "< 0.5", "< 0.01", "< 0.001", "< 0.05")))

#define colours for dots and bars
dotCOLS = c("grey80","tomato1")
barCOLS = c("grey30","red3")


ggplot(data_ZITP3_State_Urb, aes(x=Variable, y=Value, ymin=Lower, ymax=Upper, col=Model, fill=Model)) + 
  geom_linerange(size=5,position=position_dodge(width = 0.5)) +
  geom_hline(yintercept=0, lty=2) +
  geom_point(size=3, shape=21, colour="white", stroke = 0.5, position=position_dodge(width = 0.5)) +
  scale_fill_manual(values=barCOLS, labels = c("Truncated-Poisson", "Zero-inflated")) +
  scale_color_manual(values=dotCOLS, labels = c("Truncated-Poisson", "Zero-inflated")) +
  scale_x_discrete(name="Fixed factors", limits=rev, labels=c('Urbanisation index', 'Invasion state', 'Intercept')) +
  scale_y_continuous(name="Predictor value", limits = c(min(data_ZITP3_State_Urb$Lower), max(data_ZITP3_State_Urb$Upper))) +
  expand_limits(y=c(0.1, 50)) + 
  guides(color = guide_legend(reverse=TRUE), fill = guide_legend(reverse=TRUE)) +
  coord_flip() +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 17))





### PART 7: Map ####

Urban_Refugia_MVG_seleccio$Code <- paste0(Urban_Refugia_MVG_seleccio$Inv_state, "_", Urban_Refugia_MVG_seleccio$Project)

split_Code <- split(Urban_Refugia_MVG_seleccio, Urban_Refugia_MVG_seleccio$Code)

coords_inv_periurban <- split_Code$Invaded_PeriUrban %>%
  dplyr::select(Latitude, Longitude)

coords_inv_urban <- split_Code$Invaded_Urban %>%
  dplyr::select(Latitude, Longitude)

coords_noninv_periurban <- split_Code$NonInvaded_PeriUrban %>%
  dplyr::select(Latitude, Longitude)

coords_noninv_urban <- split_Code$NonInvaded_Urban %>%
  dplyr::select(Latitude, Longitude)

coordinates(coords_inv_periurban) <- ~ Longitude + Latitude

coordinates(coords_inv_urban) <- ~ Longitude + Latitude

coordinates(coords_noninv_periurban) <- ~ Longitude + Latitude

coordinates(coords_noninv_urban) <- ~ Longitude + Latitude


buff_inv_periurban <- buffer(coords_inv_periurban, 50, dissolve = F)

buff_inv_urban <- buffer(coords_inv_urban, 50, dissolve = F)

buff_noninv_periurban <- buffer(coords_noninv_periurban, 50, dissolve = F)

buff_noninv_urban <- buffer(coords_noninv_urban, 50, dissolve = F)

colors2 <- c("#89d2a3", "#a7dca5", "#c5ecac", "#fbdf9d", "#e58087", "#d1d1d1", "#dceef3", "#8dc8d8", "#d4bbfc")

# Plotejem everything togheter
roads <- read_sf(dsn = "C:/Users/marc9/Desktop/Marc/CREAF/Invasion snake/Final_project", layer = "carreteras_ibiza")

roads$clased <- factor(roads$clased, levels = c("Carretera convencional", "Carretera multicarril", "Urbano"))

roads_subset <- subset(roads, clased %in% c("Carretera convencional", "Carretera multicarril", "Urbano"))

traps <- read_sf(dsn = "C:/Users/marc9/Desktop/Marc/CREAF/Snake Life History/COFIB data", layer = "COFIB_2022")




op <- par(cex = 1)

plot(Eivissa_map, axes = T, col = colors2, legend = F, xlim = c(1.40, 1.46), ylim = c(38.9, 38.925), xaxt = "n", yaxt = "n")

colores <- c(rep("black", 3))

tamanios <- c(2, 2, 0.05)

for (i in 1:length(levels(roads_subset$clased))) {
  subset_roads <- roads_subset[roads_subset$clased == levels(roads_subset$clased)[i], ] 
  lines(subset_roads, lwd = tamanios[i], col = colores[i]) 
}


colors_traps <- c("black", colorRampPalette(c("#feeae1", "red"))(19))

colors_points <- colors_traps[traps$Captures + 1]


points(traps$`LONG (DD)`, traps$`LAT (DD)`, pch = 21, cex = 1.8, bg = colors_points, col = "black", lwd = 2)



op <- par(cex = 1.2)

legend(1.4412, 38.9246, legend = c("Tree cover", "Shrubland", "Grassland", "Cropland", "Built-up", "Bare/Sparse vegetation", "Snow and ice", "Permanent water bodies", "Herbaceus wetland"), fill = colors2, box.lty = 1, bg = "white", cex = 1)

legend(1.4483, 38.9052, legend = c("Outer", "Intermediate", "Inner"), fill = c("grey80", "grey50", "grey20", box.lty = 1, bg = "white", cex = 1.8))


plot(buff_inv_urban, add = TRUE, lwd = 2, col = paste0("#FF0000", "40"))

plot(buff_inv_periurban, add = TRUE, lwd = 2, col = paste0("#BFBFBE", "40"))

plot(buff_noninv_urban, add = TRUE, lwd = 2, col = paste0("#0027AD", "40"))

plot(buff_noninv_periurban, add = TRUE, lwd = 2, col = paste0("#06CD00", "40"))





# same as invasion stauts

plot(Eivissa_map, axes = T, col = colors2, legend = F, xaxt = "n", yaxt = "n")

colores <- c(rep("black", 3))

tamanios <- c(2, 2, 0.05)

for (i in 1:length(levels(roads_subset$clased))) {
  subset_roads <- roads_subset[roads_subset$clased == levels(roads_subset$clased)[i], ] 
  lines(subset_roads, lwd = tamanios[i], col = colores[i]) 
}


colors_traps <- c("black", colorRampPalette(c("#feeae1", "red"))(19))

colors_points <- colors_traps[traps$Captures + 1]


points(traps$`LONG (DD)`, traps$`LAT (DD)`, pch = 4, cex = 1.7, bg = colors_points, col = "black", lwd = 6)








# Example radius urban refugia

colors3 <- c("#89d2a3", "#a7dca5", "#c5ecac", "#fbdf9d", "#e58087",  "#d1d1d1", "#8dc8d8", "#8dc8d8")

plot(Eivissa_map, axes = T, col = colors3, legend = F, xlim = c(1.42, 1.44), ylim = c(38.902, 38.912), xaxt = "n", yaxt = "n")

for (i in 1:length(levels(roads_subset$clased))) {
  subset_roads <- roads_subset[roads_subset$clased == levels(roads_subset$clased)[i], ] 
  lines(subset_roads, lwd = tamanios[i], col = colores[i]) 
}

plot(buff_inv_urban, add = TRUE, lwd = 3)







# Map IBIZA

library(sf)
colors2 <- c("#89d2a3", "#a7dca5", "#c5ecac", "#fbdf9d", "#e58087", "#d1d1d1", "#dceef3", "#8dc8d8", "#d4bbfc")

# Plotejem everything togheter
roads <- read_sf(dsn = "C:/Users/marc9/Desktop/Marc/CREAF/Invasion snake/Final_project", layer = "carreteras_ibiza")

roads$clased <- factor(roads$clased, levels = c("Carretera convencional", "Carretera multicarril", "Urbano"))

roads_subset <- subset(roads, clased %in% c("Carretera convencional", "Carretera multicarril", "Urbano"))


plot(Eivissa_map, axes = T, col = colors2, legend = F, xaxt = "n", yaxt = "n")

colores <- c(rep("black", 3))

tamanios <- c(0.5, 0.5, 0.001)

for (i in 1:length(levels(roads_subset$clased))) {
  subset_roads <- roads_subset[roads_subset$clased == levels(roads_subset$clased)[i], ] 
  lines(subset_roads, lwd = tamanios[i], col = colores[i]) 
}

