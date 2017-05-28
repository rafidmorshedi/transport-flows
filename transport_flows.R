#mapping transport flows inspired by
#http://spatial.ly/2015/03/mapping-flows/

#load in packages
pkgs <- c("readr","tidyr","dplyr","ggplot2","rgdal", "rgeos", "maptools","tmap","ggmap","animation")
lapply(pkgs,library, character.only = TRUE)

#read in the OD data
df <- read_csv('data/2011JTW_Table19_V1.1.csv')

#read in the geographic data
shp <- readOGR("data/bts_spatial_tz_nsw_2011_shapefile",layer = "TZ_NSW_2011")

str(shp@data)

#calculate the centroids
tz2011_centroids <- gCentroid(shp,byid = TRUE)
xy <- as_data_frame(coordinates(tz2011_centroids))

#add the centroids to shp
shp@data <- as_data_frame(shp@data) %>% 
  bind_cols(xy)

#only get the required cols from each df
#cols from the spatial data
xy <- shp@data %>% select(TZ_CODE11,x,y)

#get the cols from the od matrix
OD <- df %>% 
  #only keep the starting and fininshing in the Sydey SA4's
  filter(grepl("Sydney",D_SA4_NAME11),grepl("Sydney",O_SA4_NAME11)) %>% 
  select(O_TZ11,D_TZ11,MODE9_NAME,EMPLOYED_PERSONS) %>% 
  group_by(O_TZ11,D_TZ11) %>% 
  summarise(
    n = sum(EMPLOYED_PERSONS),
    dom_mode = MODE9_NAME[which.max(EMPLOYED_PERSONS)]
  )

#add the OD coords
OD <- OD %>% 
  left_join(xy,by = c("O_TZ11"="TZ_CODE11"), suffix = c("_o","_d")) %>% 
  left_join(xy,by = c("D_TZ11"="TZ_CODE11"), suffix = c("_o","_d"))

#make groups for travel type
pt <- c("Train","Bus","Ferry/Tram")
car <- c("Vehicle driver","Vehicle passenger")
other <- c("Worked at Home or Did not go to Work","Walked only","Other mode","Mode not stated")

#filter to get rid of OD's with very few trips
OD_f <- OD %>% 
  ungroup() %>% 
  mutate(
    dom_type = ifelse(dom_mode %in% pt, "Public Transport", ifelse(dom_mode %in% car,"Car","Other"))
  )%>% 
  filter(n>15)

#start plotting
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
lquiet <- theme(legend.position = "none")
quiet<-list(xquiet, yquiet,lquiet)
#quiet<-list(xquiet, yquiet)

##plot with modes (this plot is square)
pdf("odPlot_modes.pdf", width = 35, height = 35)

#make the plot
odPlot1 <- ggplot(OD_f, aes(x_o, y_o))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=x_o, y=y_o,xend=x_d, yend=y_d, alpha=n,col = dom_type))+
  scale_color_manual(values = c("#FE7C73", "#EDAE49", "white"),guide = TRUE)+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.12, 0.42))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='#022342',colour='#022342'),
        plot.background=element_rect(fill='#022342', color='#022342'))+
  quiet+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Journeys to Work | Sydney",size = 25,col = "white", alpha = 0.8,hjust = 0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Data: Journey to Work 2011 cc-by-TPA",size = 15,col = "white", alpha = 0.8,vjust = 3,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Car",size = 15,col = "#FE7C73", alpha = 0.8,vjust = 6,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Other",size = 15,col = "#EDAE49", alpha = 0.8,vjust = 8,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Public Transport",size = 15,col = "white", alpha = 0.8,vjust = 10,hjust =0)+
  annotate("text", x = max(OD_f$x_o), y = min(OD_f$y_o), label = "Graphic: Rafid Morshedi (rafid.morshedi@gmail.com)",
           size = 15,col = "white", alpha = 0.8,hjust = 1)
  

print(odPlot1)

dev.off()


#make the plot - white (no modes) - square shape
pdf("odPlot_white.pdf", width = 35, height = 35)


odPlot2 <- ggplot(OD_f, aes(x_o, y_o))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=x_o, y=y_o,xend=x_d, yend=y_d, alpha=n),col = "white")+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.12, 0.4))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='#022342',colour='#022342'),
        plot.background=element_rect(fill='#022342', color='#022342'))+
  quiet+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Journeys to Work | Sydney",size = 25,col = "white", alpha = 0.8,hjust = 0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Data: Journey to Work 2011  cc-by-TPA",
           size = 15,col = "white", alpha = 0.8,vjust = 3.5,hjust =0)+
  annotate("text", x = max(OD_f$x_o), y = min(OD_f$y_o), label = "Graphic: Rafid Morshedi (rafid.morshedi@gmail.com)",
           size = 15,col = "white", alpha = 0.8,hjust = 1)

print(odPlot2)

dev.off()

####################modified for A2 16.53 x 23.39
#make the plot - white (no modes)
pdf("odPlot_white_A2.pdf", width = 23.39, height = 16.53)


odPlot2 <- ggplot(OD_f, aes(x_o, y_o))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=x_o, y=y_o,xend=x_d, yend=y_d, alpha=n),col = "white")+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.12, 0.4))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='#022342',colour='#022342'),
        plot.background=element_rect(fill='#022342', color='#022342'))+
  quiet+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Journeys to Work | Sydney",size = 15,col = "white", alpha = 0.8,hjust = 0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Data: Journey to Work 2011  cc-by-TPA",
           size = 9,col = "white", alpha = 0.8,vjust = 3.5,hjust =0)+
  annotate("text", x = max(OD_f$x_o), y = min(OD_f$y_o), label = "Graphic: Rafid Morshedi (rafid.morshedi@gmail.com)",
           size = 9,col = "white", alpha = 0.8,hjust = 1)

print(odPlot2)

dev.off()

##plot with modes ############ for a2
pdf("odPlot_modes_A2.pdf", width = 23.39, height = 16.53)

#make the plot
odPlot1 <- ggplot(OD_f, aes(x_o, y_o))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=x_o, y=y_o,xend=x_d, yend=y_d, alpha=n,col = dom_type))+
  scale_color_manual(values = c("#FE7C73", "#EDAE49", "white"),guide = TRUE)+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.12, 0.42))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='#022342',colour='#022342'),
        plot.background=element_rect(fill='#022342', color='#022342'))+
  quiet+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Journeys to Work | Sydney",size = 15,col = "white", alpha = 0.8,hjust = 0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Data: Journey to Work 2011 cc-by-TPA",size = 8,col = "white", alpha = 0.8,vjust = 3,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Car",size = 8,col = "#FE7C73", alpha = 0.8,vjust = 6,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Other",size = 8,col = "#EDAE49", alpha = 0.8,vjust = 8,hjust =0)+
  annotate("text", x = min(OD_f$x_o), y = max(OD_f$y_o), label = "Mode: Public Transport",size = 8,col = "white", alpha = 0.8,vjust = 10,hjust =0)+
  annotate("text", x = max(OD_f$x_o), y = min(OD_f$y_o), label = "Graphic: Rafid Morshedi (rafid.morshedi@gmail.com)",
           size = 8,col = "white", alpha = 0.8,hjust = 1)


print(odPlot1)

dev.off()

