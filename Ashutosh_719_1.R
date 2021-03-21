# IST 719 Final Project
# Ashutosh Kumar Jha 
# Suid -351773354

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

#directory 
setwd("/Users/ashutoshjha/Documents/rstudio719")

# calling libraries 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(np)

#command to display all the palettes

#Reading Data files

US <- read.csv("/Users/ashutoshjha/Downloads/IST719_Final_Project/USvideos.csv" ,sep=",", header = TRUE)

# adding categories to the us dataset 

usnew <- US

usnew <- usnew %>% 
  mutate(category = case_when(
    category_id== '1' ~ 'Film and Animation',
    category_id== '2' ~ 'Autos and Vehicles',
    category_id== '10'~ 'Music',
    category_id== '15'~ 'Pets and Animals',
    category_id== '17'~ 'Sports',
    category_id== '18'~ 'Short Movies',
    category_id== '19'~ 'Travel and Events',
    category_id== '20'~'Gaming',
    category_id== '21'~'Videoblogging',
    category_id== '22'~ 'People and Blogs',
    category_id== '23'~ 'Comedy',
    category_id== '24'~ 'Entertainment',
    category_id== '25'~ 'News and Politics',
    category_id== '26'~ 'How to and Style',
    category_id== '27'~ 'Education',
    category_id== '28'~ 'Science and Technology',
    category_id== '29'~ 'Nonprofits & Activism',
    category_id== '30'~ 'Movies',
    category_id== '31'~ 'Anime/Animation',
    category_id== '32'~ 'Action/Adventure',
    category_id== '33'~ 'Classics',
    category_id== '34'~ 'Comedy',
    category_id== '35'~ 'Documentary',
    category_id== '36'~ 'Drama',
    category_id== '37'~ 'Family',
    category_id== '38'~ 'Foreign',
    category_id== '39'~ 'Horror',
    category_id== '40'~ 'Sci-Fi/Fantasy',
    category_id== '41'~ 'Thriller',
    category_id== '42'~ 'Shorts',
    category_id== '43'~ 'Shows',
    category_id== '44'~ 'Trailers'))


# Drawing a barplot  find the top 5 video channel watched in  united sates 

usatop <- data.frame(usnew)

usatop = filter(usatop) %>%
  count(category) %>% 
  arrange(desc(n)) %>% 
            head(5)

usatop$fraction = usatop$n/ sum(usatop$n)
usatop$ymax = cumsum(usatop$fraction)
usatop$ymin = c(0, head(usatop$ymax, n=-1))
usatop$labelPosition = (usatop$ymax + usatop$ymin) / 2
usatop$label = paste0(usatop$category, "\n count: ", usatop$n)

ggplot(usatop, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Reds") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void()+ 
  theme(legend.position = "none")


#Top 5 channels across all the five categories 

# top 5 channel across comedy 

uschannel <- usnew

uscomedy <- uschannel[uschannel$category == "Comedy",]

uscomedy <- filter(uscomedy) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)
x <- ggplot(data = uscomedy, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Comedy Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))
x

x + labs( x = " Total number of views" , y = "Channels")

# Finding top 5 channel for the Entertainment

usenter <- uschannel[uschannel$category == "Entertainment",]

usenter <- filter(usenter) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usenter, aes(x = n, y = reorder(channel_title, n))) +
     geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
     ggtitle( "Top 5 Entertainment Channel" ) +
     theme(plot.title = element_text(hjust = 0.5))
  
x

x + labs( x = " Total number of views" , y = "Channels")


  
# Finding the top 5 music channel 

usmusic <- uschannel[uschannel$category == "Music",]

usmusic <- filter(usmusic) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usmusic, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 Music Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))
  
x
x + labs( x = " Total number of views" , y = "Channels")

#How to and Style top5 channel

usstyle <- uschannel[uschannel$category == "How to and Style",]
usstyle <- filter(usstyle) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usstyle, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 How to and Style Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))
x

x + labs( x = " Total number of views" , y = "Channels")

#People and Blogs


usblog <- uschannel[uschannel$category == "People and Blogs",]

usblog <- filter(usblog) %>%
  count(channel_title) %>%
  arrange(desc(n)) %>%
  head(5)

x <- ggplot(data = usblog, aes(x = n, y = reorder(channel_title, n))) +
  geom_bar(stat = "identity", fill= brewer.pal(5,"Reds"), width = 0.5) +
  ggtitle( "Top 5 People and Blogs Channel" ) +
  theme(plot.title = element_text(hjust = 0.5))

x

x + labs( x = " Total number of views" , y = "Channels")









