
library(readr)
library(igraph)
library(cartography)

# upload the edge table
a <- read_tsv("flows.tsv")

# transform the edge table into an igraph object
g <- graph_from_data_frame(a, directed = FALSE)

# set the graphical parameters

# color of the links, the alpha parameter allow to fix the transparency
E(g)$color <- adjustcolor("grey30", alpha = .6)

# color of the nodes
V(g)$color <- "#0055A5" #EPSA logo color acc. to Mozilla color Pipette

summary(E(g)$weight) # statistical summary

# measure the degree
V(g)$degree <- degree(g)

# only keep the nodes with a degree different from zero
# g <- induced.subgraph(g,v = V(g)$degree > 0)

# plot the graph
plot(g, vertex.label = NA)

fr <- layout_with_fr(g, weight =  NULL) #ldh, layout_nicely layout_with_kk, layout_with_fr, layout_with_gem, layout_with_mds
plot(g, vertex.label = NA, layout = fr)  #fr

## -------- if you wanna modify the position of certain nodes
#library("tkrplot")
#tkid <- tkplot(g)         # tkid is the id of the output of tkplot
#l2 <- tkplot.getcoords(tkid) # to get the resulting coordinates and save them
#plot(g, layout = l2, vertex.label = NA)

#---------------------- IGRAPH - CARTOGRAPHY MIX = CARTIGRAPH -----------------------------------------------

# set the size parameter (the size of the nodes will depend on the value of this parameter)
# increase this value if you want bigger nodes
inches <- 0.03

# choose the variable to which the size of the circles should be proportional
var <- strength(g)

# the following operations (line 53-62) allow to represent circles whose surface is proportional to a given variable
# by default, the igraph.plot function varies the radius of the circles, not their surface

# fix the surface of the largest circle
smax <- inches * inches * pi

# fix the radius of the circles
  # nb: we multiply by 200 because the igraph package divides, by default, radius value per 200 (version 1.2.4.2)
siz <- sqrt((var * smax/max(var))/pi)
size <- siz*200

# set the size of the circles
V(g)$size <- size

# map
plot(g, vertex.label = NA)

# select the values of the 4 sizes of circles that will be represented in the legend
varvect <- seq(max(var), min(var), length.out = 4)

# set the width of the largest flow
maxsize <- 10 # width of the largest link

# set the link thickness
E(g)$width <-((E(g)$weight/max(E(g)$weight))*maxsize) 

# set the thresholds and link thicknesses that will be represented in the legend

# 1. select 5 values between min. and max. val.
breaks.edge <- seq(max(E(g)$weight), min(E(g)$weight), length.out = 5) 

# 2. list of thicknesses of the links appearing in the legend
lwd.edge <- ((breaks.edge[1:4]/max(E(g)$weight))*maxsize) 

# save the original graphic settings
opar <- par()

# activate the following line if you want a pdf or svg output
svg(file = "EPSA_network.svg.svg", width = 8, height = 6)

# generate a new plot
plot.new()

# normalize the space in which the vertices are positioned 
ln <- norm_coords(fr, ymin = -1, ymax = 1, xmin = -1, xmax = 1)

# setting new graphic parameters
par(mar = c(0,0,1.2,0), usr = c(-1.3,1.3,-1.3,1.3)) ## xmin, xmax, ymin, et ymax

# adjust the size parameter of the legend circles taking into account the selected graphic parameters
  # nb: it is a necessary step to avoid a mismatch between the size of the nodes in the {Cartography} legend
  # and the size of the nodes in the {igraph} plot
xinch <- diff(par("usr")[1L:2])/par("pin")[1L]
sizevect <- inches/xinch

# plot the network and labels of the nodes
plot(g, edge.curved = .1, rescale = F, layout = ln, add = T, main = "", edge.width = E(g)$width,
     vertex.frame.color = "grey",
     vertex.label.family = "sans", # labels' font
     vertex.label.color = "black", # labels' color
     vertex.label.degree = -pi/2, # label positioning angle
     vertex.label.dist = sqrt(V(g)$size)/pi +0.2, # distance of labels
     vertex.label.cex = 0.5) # labels' size

# add the legend indicating the size of the circles
legendCirclesSymbols(pos = "topright", title.txt ="Normalised nb of collab. \n
                                                   per academic orgs",
                     title.cex = 0.6, values.cex = 0.5,
                     var = varvect, inches = sizevect, #circles' size
                     col = "white", frame = F, 
                     values.rnd = 1, # number of digits after the decimal point
                     style = "e") # choice of "extended" style for better readability

# add the legend indicating the thickness of the links
legendGradLines(pos = "topleft", # legend position
                title.txt = "Normalised nb of co-authored papers", # legend's title
                title.cex = 0.6, values.cex = 0.6, # font size
                breaks = breaks.edge, # intervals of the displayed values
                lwd = lwd.edge, # link thickness
                col = "gray80", # link color
                values.rnd = 1, # number of digits after the decimal point
                frame = F) # legend's frame

# specify the title, source and author of the graphic
layoutLayer(title = "Scientific co-affiliation network of EPSA participants 2020-06", coltitle = "white",
            sources = "Source: EPSA website(https://www.epsanet.org/programme). Data & script: https://github.com/briatte/epsa2020), June, 6 2020. \n Co-authorship ties between academic orgs, based on papers presented at #EPSA2020.",
            # possibility to add North and scale if necessary
            scale = NULL, north = F, frame = TRUE, col = "#0055A5",
            # author = "F. Briatte & M. Maisonobe, 2020",  
)

# activate the following line if you want to clean up the graphics window or complete the file export
dev.off()

# -------------------------------- BONUS --------------------------------------------------

## keep only the largest connected component and explore the graph interactively with VisNetwork
  # check here for the extended script: https://framagit.org/MarionMai/data-shs

library("visNetwork")

# plot the component's list

clusters(g)

# keep only the largest component

gtop<-induced.subgraph(g, clusters(g)$membership == 1)

---------------------------------------------------------------
  
  # to VisNetwork
  
  data <- toVisNetworkData(gtop)

# VizNetwork plot

visNetwork(nodes = data$nodes, edges = data$edges)


