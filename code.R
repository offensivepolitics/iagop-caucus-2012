# must install gpclib or this fails
library(maptools)
library(ggplot2)
library(RColorBrewer)

gpclibPermit()

# map theme for nice looking plots, stolen from from Osmo Salomaa on ggplot2 google group 
theme_map = function(size=12)
{
    o = list(axis.line=theme_blank(),
             axis.text.x=theme_blank(),
             axis.text.y=theme_blank(),
             axis.ticks=theme_blank(),
             axis.ticks.length=unit(0.3, "lines"),
             axis.ticks.margin=unit(0.5, "lines"),
             axis.title.x=theme_blank(),
             axis.title.y=theme_blank(),
             legend.background=theme_rect(fill="white", colour=NA),
             legend.key=theme_rect(colour="white"),
             legend.key.size=unit(1.2, "lines"),
             legend.position="right",
             legend.text=theme_text(size=size*0.8),
             legend.title=theme_text(size=size*0.8, face="bold",
hjust=0),
             panel.background=theme_blank(),
             panel.border=theme_blank(),
             panel.grid.major=theme_blank(),
             panel.grid.minor=theme_blank(),
             panel.margin=unit(0, "lines"),
             plot.background=theme_blank(),
             plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
             plot.title=theme_text(size=size*1.2),
             strip.background=theme_rect(fill="grey90",
colour="grey50"),
             strip.text.x=theme_text(size=size*0.8),
             strip.text.y=theme_text(size=size*0.8, angle=-90))

    return(structure(o, class="options")) 
}

# load the results file
iagop <- read.csv("Iowa Caucus 2012 County Results with Shapes.csv")
# drop the Shape column
iagop <- iagop[,-3]

# precalc the vote percentages
iagop$pct.paul <- (iagop$VoteCount.Paul / iagop$NumVoters) * 100.0
iagop$pct.bachmann <- (iagop$VoteCount.Bachmann / iagop$NumVoters) * 100.0
iagop$pct.gingrich <- (iagop$VoteCount.Gingrich / iagop$NumVoters) * 100.0
iagop$pct.santorum <- (iagop$VoteCount.Santorum / iagop$NumVoters) * 100.0
iagop$pct.romney <- (iagop$VoteCount.Romney / iagop$NumVoters) * 100.0
iagop$pct.perry <- (iagop$VoteCount.Perry / iagop$NumVoters) * 100.0
# rename the iagop ID column to Name to match the shapefile
names(iagop)[1] <- "Name"


# load the shapefile
shp <- readShapeSpatial("iowa-gop-county-2012")

# convert to a dataframe so we can use ggplot
# taken almost verbatim from the ggplot2 wiki
#  https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
shp@data$id <- rownames(shp@data)
shp.points <- fortify.SpatialPolygonsDataFrame(shp,region="id")
shp.df <- join(shp.points,shp@data,by="id")
# drop the Descripto column w/ HTML in it
shp.df <- shp.df[,-7]

# join the shapefile dataframe and the results file dataframe
shp.df <- join(shp.df,iagop, by="Name")
# drop unused columns
shp.df <- shp.df[,-c(13,14,15,16,17,18,19,20,21,22,23)]

# plot a heatmap of each candidates vote percentage
p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.paul) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("paul.png")

p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.romney) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("romney.png")

p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.santorum) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("santorum.png")

p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.gingrich) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("gingrich.png")

p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.perry) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("perry.png")

p<-ggplot(shp.df) + aes(long,lat,group=group,fill=pct.bachmann) + geom_polygon() + 
                 geom_path(color="white") + coord_equal() + scale_fill_gradient(limits=c(0,100),low="white",high="red") + 
		 theme_map()
ggsave("bachmann.png")

## plot romney - santorum 
p<- ggplot(shp.df) + aes(long,lat,group=group,fill=pct.romney-pct.santorum) + geom_polygon() + 
                  geom_path(color="white") + coord_equal() + scale_fill_continuous(limits=c(-60,60),name="Romney - Santorum") + theme_map() opts(main="Romney minus Santorum IA GOP Caucus 2012")
ggsave("romney-santorum.png")
p<-qplot(pct.romney - pct.santorum,data=iagop,geom="histogram",main="Romney - Santorum Percentage by County IAGOP Caucus 2012") + geom_vline(x=0)
p<-ggsave("romney-santorum-histogram.png")

## plot romney - paul
p<- ggplot(shp.df) + aes(long,lat,group=group,fill=pct.romney-pct.paul) + geom_polygon() + 
                  geom_path(color="white") + coord_equal() + scale_fill_continuous(limits=c(-60,60),name="Romney - Paul") + theme_map() + opts(main="Romney minus Paul IA GOP Caucus 2012")
ggsave("romney-paul.png")
p<-qplot(pct.romney - pct.paul,data=iagop,geom="histogram",main="Romney - Paul Percentage by County IAGOP Caucus 2012") + geom_vline(x=0)
ggsave("romney-paul-histogram.png")

## plot santorum - paul
p<- ggplot(shp.df) + aes(long,lat,group=group,fill=pct.santorum - pct.paul) + geom_polygon() + 
                  geom_path(color="white") + coord_equal() + scale_fill_continuous(limits=c(-60,60),name="Santorum - Paul") + theme_map() + opts(main="Santorum minus Paul IA GOP Caucus 2012")
ggsave("romney-paul.png")
p<-qplot(pct.romney - pct.paul,data=iagop,geom="histogram",main="Santorum - Paul Percentage by County IAGOP Caucus 2012") + geom_vline(x=0)
p<-ggsave("santorum-paul-histogram.png")
