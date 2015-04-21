library('geosphere')
### Reading Nodess ###
df <- read.csv("nodes_2.csv", header=TRUE)

###Deleting Duplicates###
uniqueNodes <- df[!duplicated(df),]
a.vec<-seq(1,nrow(uniqueNodes),1)
uniqueNodes$nodeId <- a.vec

###Get the nodes in the required Format ###
uniqueNodes <- data.frame(nodeId = uniqueNodes[,3],Longitude = uniqueNodes[,1], Latitude = uniqueNodes[,2])

### Construct the edges and map their layout###
road_edges<-read.csv('result2.csv',header=TRUE)
nodes <- uniqueNodes
comb1 <- merge(road_edges,nodes,by.x=c("pointA_Longitude","pointA_Latitude"),by.y=c("Longitude","Latitude"))
comb2 <- data.frame(startId = comb1[,6], pointB_Longitude=comb1[,4], pointB_Latitude = comb1[,5])
comb3 <- merge(comb2,nodes,by.x=c("pointB_Longitude","pointB_Latitude"),by.y=c("Longitude","Latitude"))
edges <- data.frame(startId = comb3[,3], endId = comb3[,4], edgeId = seq(1,nrow(comb3),1),distance = 100000000)

### FUnction to get Coordinates of a node by its nodeId###
getCoordinates <- function(nodeId){
  coord <- c(nodes[nodeId,'Longitude'],nodes[nodeId,'Latitude'])
  return(coord)
}

###Function to compute distance between two points on earth ###
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

### Calculate distance vector of each edge###
for(i in 1:nrow(edges))
{
  edges[i,'distance'] <- earth.dist(getCoordinates(edges[i,'startId'])[1],getCoordinates(edges[i,'startId'])[2],getCoordinates(edges[i,'endId'])[1],getCoordinates(edges[i,'endId'])[2])
}