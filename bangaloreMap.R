library(igraph)
library(ggmap)

### CONSTRUCT GRAPH###
prelimGraph <- graph.data.frame(edges,directed = FALSE, vertices = nodes)
prelimGraph <- delete.vertices(prelimGraph,which(degree(prelimGraph)<1))
x = V(prelimGraph)$Longitude
y = V(prelimGraph)$Latitude
l2 = cbind(x, y)

getCoordinates <- function(nodeId){
  coord <- c(nodes[nodeId,'Longitude'],nodes[nodeId,'Latitude'])
  return(coord)
}

#### Diameter ####
nodes.diameter<- get.diameter(prelimGraph,weights = E(prelimGraph)$distance)
V(prelimGraph)[nodes.diameter]$color<-'red'
E(prelimGraph,path=nodes.diameter)$color<-'red'
E(prelimGraph,path=nodes.diameter)$width<- 2

### Degree ###
degree_nodes <- degree(prelimGraph,mode="all")
maxDegree <- which.max(degree_nodes)
#V(prelimGraph)[as.vector(maxDegree)]$color<-'red'
#V(prelimGraph)[maxDegree]$label<-'revgeocode(getCoordinates(maxDegree))'

### Eccentricity ###

graph.eccentricScore <- eccentricity(prelimGraph)
mostEccentric <- which.min(graph.eccentricScore)
V(prelimGraph)[as.vector(mostEccentric)]$size <- 15
V(prelimGraph)[as.vector(which.min(graph.eccentricScore))]$color <- 'yellow'

### Betweenness ###
graph.between <- betweenness(prelimGraph)
max_betweenness <- which.max(graph.between)
#V(prelimGraph)[max_betweenness]$color <- 'darkgreen'
nodesize=degree(prelimGraph)*2


### Closeness ###
graph.closeness <- closeness(prelimGraph,mode='all')
V(prelimGraph)[as.vector(which.min(graph.closeness))]$color <- 'darkgreen'

### degree distribution ###
# 
# "tiff('powerLaw.tif',res = 1000,compression = 'lzw',height = 10,width=10,units="in")
# plot(deg.distr,log="xy",
#      ylim=c(.01,10),
#      bg="black",pch=21,
#      xlab="Degree",
#      ylab="Cumulative Frequency")
# prelimGraph$layout <- l2
# dev.off()"


### PLOTTING STARTS###
tiff('bangalore_Map_new.tif',res = 1000,compression = 'lzw',height = 10,width=10,units="in")
plot.igraph(prelimGraph,layout = l2,
            vertex.size=2, edge.color="white", vertex.frame.color="gray",  
            edge.arrow.size=0.1,edge.width=1, rescale=TRUE, vertex.label.dist=0.0,
            vertex.label.cex=0.01, add=FALSE,   vertex.label.font=.001
            
)
dev.off()