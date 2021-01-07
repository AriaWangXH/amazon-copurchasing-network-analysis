# Lab 2:
# Exponential Random Graph Models (ERGMs)
#
# Install packages below if you do not have them:
# -------------------------------------------------
# install.packages("statnet")
# install.packages("igraph")
# install.packages("texreg")

library(statnet)
?statnet
# -------------------------------------------------------------------------------------------------
######################## Read Data ########################
# -------------------------------------------------------------------------------------------------

#Datalist <- read.csv("buyInFromYouEdgelist.csv")
Datalist <- read.csv("Amazon0302_SUBSET.csv")
#Datalist$FromNodeId = Datalist$FromNodeId+1
#Datalist$ToNodeId = Datalist$ToNodeId+1
head(Datalist)
DataNetwork <- as.network.matrix(Datalist, matrix.type = "edgelist") 

set.vertex.attribute(DataNetwork, "Group",read.csv("ID_INFO_SUBSET.csv")$Group) 
set.vertex.attribute(DataNetwork, "Categories",read.csv("ID_INFO_SUBSET.csv")$Categories)


summary(DataNetwork)                              # summarize
network.size(DataNetwork)                         # print out the network size
betweenness(DataNetwork)                          # calculate betweenness for the network
isolates(DataNetwork) 

# ----------------------------------------------------------------------------------------------------
######################## PART I: Building and Visualizing the Networks ########################
# ----------------------------------------------------------------------------------------------------

library('igraph')

# Set default plot options
igraph_options(vertex.size = 6, vertex.color = 'grey', # vertex.size changes the size of nodes; vertex.color changes the color of nodes
               edge.color='gray80', edge.arrow.size=.1, # edge.color changes the color of ties; edge.arrow.size changes the size of tie arrow heads
               vertex.label = NA)                       # vertex.label = NA specifies not to display vertex labels in the plot

# Plot the network
DataNetwork_igraph <- graph.adjacency(as.matrix.network(DataNetwork)) # make an igraph network object from statnet network object
net_layout <- layout_with_fr(DataNetwork_igraph) # spring-embedded layout
plot(DataNetwork_igraph, layout=net_layout, edge.color='black', vertex.label = V(DataNetwork_igraph))

# -------------------------------------------------------------------------------------------------
######################## PART II: Build the ERGM models ########################
# -------------------------------------------------------------------------------------------------

# Remove the 'igraph' package from your environment. 
detach(package:igraph)
library(statnet)
options(ergm.loglik.warn_dyads=FALSE) #Whether or not a warning should be issued when sample space constraints render the observed number of dyads ill-defined

summary (DataNetwork ~ edges + mutual)
summary (DataNetwork ~ odegree(0:5))  
summary (DataNetwork ~ idegree(0:65)) 

model1 <- ergm(DataNetwork ~ edges   
               + mutual 
               , constraints =~ bd(maxout=5) 
) 
summary(model1) 

model2 <- ergm(DataNetwork ~ edges                          # This model will be slower to estimate than model 1
               # Structural patterns
               + mutual
               + gwidegree(1.06, fixed = T)                 # Inverted preferential attachment (indegree)
               + gwodegree(log(2), fixed = T)               # Inverted preferential attachment (outdegree)
               + dgwesp(log(2), type = "OTP", fixed = T)    # A modified version of Outgoing Two Path(i->j + i->k->j) structures. Geometrically weighted version of transitivity
               + dgwdsp(log(2), type = "RTP", fixed = T)    # A modified version of Reciprocated Two Path(i<->k<->j)structures. Geometrically weighted version of co-occurence of both transitivity + cyclicality
               # Node attribute effects
               + nodematch("Group") 
               + nodematch("Categories"), 
               nr.maxit = 100)                
) 
summary(model2) 

# -------------------------------------------------------------------------------------------------
# Goodness of fit test
# Check how well the estimated model captures certain features of the observed network, for example triangles in the network.
# -------------------------------------------------------------------------------------------------
# This first command simulates 100 networks.
# These networks, if we use sufficient burnin steps in the markov chain used to generate them,
# may be thought of as random samples from the joint probability distribution that is our fitted ERGM.
sim <- simulate(model2, burnin=100000, interval=100000, nsim=100, verbose=T)  # Uses the ergm model to simulate a null model

# Plot the first of the simulated networks
sim1_net <- igraph::graph.adjacency(as.matrix.network(sim[[1]]))
igraph::plot.igraph(sim1_net,layout=net_layout,edge.color="brown",  
                    vertex.color = 'grey',edge.arrow.size=.4)                                                               

# Plot the 10th simulated network
sim10_net <- igraph::graph.adjacency(as.matrix.network(sim[[10]]))
igraph::plot.igraph(sim10_net,layout=net_layout,edge.color="purple",  
                    vertex.color = 'grey',edge.arrow.size=.4)                                                                 

# -------------------------------------------------------------------------------------------------
# Extract the number of triangles from each of the 100 samples and
# compare the distribution of triangles in the sampled networks with the observed network
# -------------------------------------------------------------------------------------------------
model.tridist <- sapply(1:100, function(x) summary(sim[[x]] ~triangle)) # Extracts the tiangle data from the simulated networks
hist(model.tridist,xlim=c(8000,10000),breaks=100)                             # Plots that triangle distribution as a histogram, change xlim to change the x-axis range if necessary
DataNetwork.tri <- summary(DataNetwork ~ triangle)                                  # Saves the CRIeq triangle data from the summary to the CRI.eq variable
DataNetwork.tri
arrows(DataNetwork.tri,20, DataNetwork.tri, 5, col="red", lwd=3)                    # Adds an arrow to the plotted histogram
c(obs=DataNetwork.tri,mean=mean(model.tridist),sd=sd(model.tridist),
  tstat=abs(mean(model.tridist)-DataNetwork.tri)/sd(model.tridist))

# -------------------------------------------------------------------------------------------------
# Test the goodness of fit of the model
# Compiles statistics for these simulations as well as the observed network, and calculates p-values 
# -------------------------------------------------------------------------------------------------
# This first command runs goodness of fit testing
# It may take a second for this command to run.
gof <- gof(model2 ~ idegree + odegree + espartners + distance, verbose=T, burnin=1e+5, interval=1e+5, control = control.gof.ergm(nsim = 200))
# If you run below and then wouldn't see the plot, try par(mar=c(2,2,2,2))
plot(gof)           # Plot the goodness of fit
# Note: this should produce five separate plots that you should look through.
gof           
