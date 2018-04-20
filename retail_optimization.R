library(stringr)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(caret)
library(klaR)
library(standardize)

#load data----
retail <-read_csv('retail.csv')

# clean data-----
color<-read_csv('color.csv')
color <- color[2:13]

#parse string with $ to number
color <- color %>%
  mutate_if(~all(str_detect(.x,pattern = "^\\$.*")), ~parse_number(.x))

week2num <- c("M"=1,"T"=2,"W"=3,"Th"=4,"F"=5,"Sa"=6,"Su"=7)
week2num[color$`Day of the Week`]

color <- color %>%
  mutate(dow = week2num[color$`Day of the Week`])

#data normalization-----
rescale01 <- function(x){(x-min(x))/(max(x)-min(x))}
color %>% mutate_if(~is.numeric(.x),~rescale01(.x))



#correlation heatmap------
color_cor<-color[3:13]
cormat <- round(cor(color_cor),2)
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(lower_tri, na.rm = TRUE)
# Create a ggheatmap

ggheatmap<-ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# add text

ggheatmap + 
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

color%>%
  group_by(dow)%>%
  summarise(avg_profit = mean(Profit))%>%
  ggplot(aes(x =dow , y = avg_profit))+
  geom_col()

#linear model ---

model <-lm(data = color, `Profit`~ `Inventory & Shelving/unit`+`Sales`+`Items Sold`+`Average Price`+`Clearance`+`Shelf Spaces`)
summary(model)

data%>% mutate(shelf_log = log10(`Shelf Spaces`))

plot(x =`Shelf Spaces`, y = Profit, data = color, log = color$`Shelf Spaces`)

