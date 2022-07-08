#####################################################################################################
### INTRO TO TREES ###
#####################################################################################################

#For first time use only, first install packages 
#Then, for each time you start a session, load the packages
library(ape)
library(geiger)
library(phytools)
library(adephylo)
library(phangorn)
library(evomap)


#load data
data<-read.table("Triquetrum.txt",header=T,sep="\t",row.names=1)
#load a tree
tree<-read.nexus("Primates.nex")
tree_all<-tree
plot(tree)
#Align data and tree
tree<-treedata(tree,data,sort=T,warnings=T)$phy
data<-treedata(tree,data,sort=T,warnings=T)$data
plot(tree)
#write.nexus(tree,file="Workspace/C01_tree_used.nex")

#Working with trees, info on taxa, taxon names, and taxon synonyms
http://animaldiversity.ummz.umich.edu/
  http://10ktrees.nunn-lab.org/Primates/downloadTrees.php
http://www.planet-mammiferes.org/drupal/en/node/37?taxon=1


#Accessing the tree structure
summary(tree)
str(tree)
tree$edge.length
mean(tree$edge.length)
summary(tree$edge.length)
hist(tree$edge.length)


#####################################################################################
#EXERCISE 3
#Load the data 'Capitate_Extants'
data_wrist<-read.table("Capitate.txt",header=T,sep="\t",row.names=1)
#Prune the tree to match this data set
tree_wrist<-read.nexus("Primates.nex")
tree_wrist<-treedata(tree_wrist,data_wrist,sort=T,warnings=T)$phy
#Plot the tree
plot(tree_wrist)
#How many branches does the tree have?
str(tree_wrist)
####################################################

#Let's continue to work with 'tree_wrist'
tree<-tree_wrist
#Class "phylo"
plot(tree)
tree$edge
tree$tip.label
tree$Nnode
tree$root.edge
tree$root.edge<-5
#Nodes, tips, and edges
tiplabels()
tiplabels(cex=3)
tiplabels(cex=1,col="green")
tiplabels(cex=2,col="green",bg="black")
nodelabels(cex=2,col="yellow",bg="red")
plot(tree)
nodelabels()
nodelabels(cex=1,srt=90)
plot(tree); tiplabels(); nodelabels()
#Note that the root is tiplabels + 1 (n+1)

identify(tree,tips=T)
?identify.phylo      
identify(tree,nodes=T)
plot(tree)

edgelabels()

#Different 'orders' of the tree
plot(tree); tiplabels(); nodelabels(); edgelabels()
str(tree)
tree_postorder<-reorder.phylo(tree,order="postorder")
plot(tree_postorder); tiplabels(); nodelabels(); edgelabels()
str(tree_postorder)

#Note how the edge numbers are different.
#Cladewise is the default for most analyses. 
#A much used alternative is postorder


#Depth of the tree
branching.times(tree)
#oops... ?branching.times says it assumes our tree is ultrametric. Is it?
is.ultrametric(tree)
tree<-drop.tip(tree,"Homo_sapiens_neanderthalensis")
#nope....
distRoot(tree)
summary(distRoot(tree))
tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE) 
#Only use when non-ultrametricity is due to rounding errors
#Note that the tree is now in 'postorder'
plot(tree) 
is.ultrametric(tree) #okay then...

branching.times(tree)
names(branching.times(tree))
branching.times(tree)[which(names(branching.times(tree))==29)]
nodeHeights(tree)
cbind(tree$edge,nodeHeights(tree))


#Accessing the tree
plot(tree); tiplabels(); nodelabels()
getDescendants(tree,39)
getTips(tree,39)
getEdges(tree,39)
plot(tree); edgelabels()

plot(tree); nodelabels()
getSisters(tree,39)
getTips(tree,getSisters(tree,39))
tree$tip.label[getTips(tree,getSisters(tree,39))]

findMRCA(tree,c("Pongo_pygmaeus","Hylobates_lar"))
findMRCA(tree,c("Pongo_pygmaeus","Hylobates_lar"),type="height")
branching.times(tree)[which(names(branching.times(tree))==findMRCA(tree,c("Pongo_pygmaeus","Hylobates_lar")))]
branching.times(tree)[1]

plot(tree); axisPhylo()
axisPhylo(cex=2,col="red",col.axis="green",side=1)
axisPhylo(cex=2,col="red",col.axis="green",side=3)

#####################################################################################
#EXERCISE 4
#Load the data 'Lunate.txt'
data_Lunate<-read.table("Lunate.txt",header=T,sep="\t",row.names=1)
#Prune the tree to match this data set
tree_Lunate<-read.nexus("Primates.nex")
tree_Lunate<-treedata(tree_Lunate,data_Lunate,sort=T,warnings=T)$phy
#Indicate the internal node numbers
plot(tree_Lunate)
nodelabels()
#Which node is the MRCA of Macaca mulatta and Hylobates lar?
targetNode<-findMRCA(tree_Lunate,c("Macaca_mulatta","Hylobates_lar"))
targetNode
#How old is this node?
branching.times(tree_Lunate)[which(names(branching.times(tree_Lunate))==targetNode)]
branching.times(tree_Lunate)[[which(names(branching.times(tree_Lunate))==targetNode)]]
#Which is its sister node?
sisterNode<-getSisters(tree_Lunate,targetNode)
sisterNode
#How old is this sister node?
branching.times(tree_Lunate)[[which(names(branching.times(tree_Lunate))==sisterNode)]]
#####################################################################################


#Plotting trees
plot(tree)
plot.phylo(tree)
?plot.phylo
plot(tree,label.offset=4)
plot(tree,dir="u")
axisPhylo(cex=2,col="red",col.axis="green",side=2)
par(mar = c(5, 4, 4, 2))
?par
par(mar = c(1, 4, 1, 2))
plot(tree,dir="u")
axisPhylo(cex=2,col="red",col.axis="green",side=2)
par(mar = c(5, 4, 4, 2))
plot(tree,dir="d")
plot(tree,dir="d")$x.lim
plot(tree,dir="d")$y.lim
plot(tree,dir="d",x.lim=c(1,19))
plot(tree,dir="d",y.lim=c(-30,45),label.offset=3,cex=0.6)
par(mar = c(1, 4, 1, 2))
plot(tree,dir="d",y.lim=c(-30,45),label.offset=3,cex=0.6)
nodelabels(cex=0.6,srt=-90)
par(mar = c(5, 4, 4, 2))
plot(tree,dir="l")
plot(tree,type="phylogram")
plot(tree,type="cladogram")
plot(tree,type="unrooted")
plot(tree,type="unrooted",cex=0.5)
plot(tree,type="fan")
plot(tree,type="radial")
plot(tree)
tree2<-ladderize(tree)
plot(tree2)
plot(tree)
nodelabels(pch=25,col="red",bg="yellow")
plot(tree)
thermo_values<-runif(tree$Nnode,min=0,max=1)
nodelabels(thermo=thermo_values)
plot(tree)
nodelabels(thermo=thermo_values,piecol=c("red","yellow"))
plot(tree)
nodelabels(pie=thermo_values)
plot(tree)
nodelabels(pie=thermo_values,piecol=c("red","yellow"))
plot(tree)
nodelabels("Interesting node",39,frame="r",col="White", bg="black")
plot(tree,show.tip.label=F)
tiplabels(tree$tip.label,adj=0,col="White", bg="black")
par(mar = c(5, 4, 4, 6))
plot(tree,show.tip.label=F)
tiplabels(tree$tip.label,adj=0,col="White", bg="black")
plot(tree,show.tip.label=F)$x.lim
plot(tree,show.tip.label=F,x.lim=c(0,80))
tiplabels(tree$tip.label,adj=0,col="White", bg="black",cex=0.8)
plot(tree,show.tip.label=F,x.lim=c(0,80))
bg_col<-rep("black",length(tree$tip.label))
bg_col[10]<-"red"
tiplabels(tree$tip.label,adj=0,col="White", bg=bg_col,cex=0.8)
par(mar = c(5, 4, 4, 2))
plot(tree)
mtext("This is a really nice tree",side=1)
mtext("Indeed it is",side=2)
text(locator(1),"wow")
text(locator(1),"root",srt=90)
arrows(8,5,16,9)
text(locator(1),"the wow node")

#Highlighting branches/tips etc
#Through adding text
plot(tree,cex=0.7)
plot(tree,cex=0.7)$x.lim
plot(tree,cex=0.7)$y.lim
segments(55,0.5,55,8.5)
text(57,5,"NW monkeys",srt=270)
#Through adding rectangles
plot(tree,cex=0.7)
plot(tree,cex=0.7)$x.lim
plot(tree,cex=0.7)$y.lim
rect(20,0.5,55,8.5,col=rgb(1,0,0,0.5,maxColorValue=1))
#Through changing colors
plot(tree,edge.width=4)
edge_width<-abs(rnorm(length(tree$edge.length),mean=3,sd=2))
plot(tree,edge.width=edge_width)
edge_width<-rep(1,length(tree$edge.length))
edgelabels()
edge_width[getEdges(tree,findMRCA(tree,c("Presbytis_comata","Cercopithecus_mitis")))]<-5
plot(tree,edge.width=edge_width)
plot(tree,edge.width=edge_width,edge.col="red")
edge_col<-rep("red",length(tree$edge.length))
edge_col[getEdges(tree,findMRCA(tree,c("Presbytis_comata","Cercopithecus_mitis")))]<-"green"
plot(tree,edge.width=edge_width,edge.col=edge_col)
tip_col<-rep("red",length(tree$tip.label))
tip_col[getTips(tree,findMRCA(tree,c("Presbytis_comata","Cercopithecus_mitis")))]<-"green"
plot(tree,edge.width=edge_width,edge.col=edge_col,tip.col=tip_col)
edge_lty<-rep(2,length(tree$edge.length))
edge_lty[getEdges(tree,findMRCA(tree,c("Presbytis_comata","Cercopithecus_mitis")))]<-1
plot(tree,edge.width=edge_width,edge.col=edge_col,tip.col=tip_col,edge.lty=edge_lty)

#####################################################################################
#EXERCISE 5
#Load the data 'Capitate.txt'
data_wrist<-read.table("./Data/Capitate.txt",header=T,sep="\t",row.names=1)
#Prune the tree to match this data set
tree_wrist<-read.nexus("./Data/Primates.nex")
tree_wrist<-treedata(tree_wrist,data_wrist,sort=T,warnings=T)$phy
#Highlight the Gorillas using a blue-filled rectangle
plot(tree_wrist)
plot(tree_wrist,cex=0.7)$x.lim
plot(tree_wrist,cex=0.7)$y.lim
rect(35,10.5,55,13.5,col=rgb(0,0,1,0.5,maxColorValue=1))
#Highlight the Gorillas by giving their tiplabels a different color
plot(tree_wrist)
tip_col<-rep("red",length(tree_wrist$tip.label))
tip_col[getTips(tree_wrist,findMRCA(tree_wrist,c("Gorilla_gorilla_gorilla","Gorilla_beringei")))]<-"blue"
plot(tree_wrist,tip.col=tip_col)
#Highlight the Gorillas by giving their edgelabels a different color                                                                                
plot(tree_wrist)
edge_col<-rep("red",length(tree_wrist$edge.length))
edgelabels()
edge_col[getEdges(tree_wrist,findMRCA(tree_wrist,c("Gorilla_gorilla_gorilla","Gorilla_beringei")))]<-"blue"
plot(tree_wrist,edge.col=edge_col)
#####################################################################################

#Use data and tree from wrist example
data<-data_wrist
tree<-tree_wrist
#Combining plots
#Combining plots with data
data_phylo<-phylo4d(tree,data)
table.phylo4d(data_phylo,box=F)
#Combining multiple trees
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(tree,cex=0.5)
plot(tree,cex=0.5)
dev.off()
#Plotting large trees
tree_all<-read.nexus("./Data/Primates.nex")
plot(tree_all)
pdf("./Workspace/D01_S01_Plot tree primates.pdf",height=60,width=30)
plot(tree_all,label.offset=2)
tiplabels()
dev.off()
zoom(tree_all,1:30,cex=0.7,col="red",no.margin=T,subtree=T)
zoom(tree_all,list(1:30,174:198),cex=0.7,col=c("red","blue"),no.margin=T,subtree=T)
dev.off()

#Manipulating the tree structure
#Adding/removing tips/fossils/clades
tree2<-tree
str(tree)
tree2<-reorder.phylo(tree,order="cladewise") #make sure the tree is order cladewise; note that our tree was in postorder.

plot(tree2)
tiplabels()
?drop.tip
tree3<-drop.tip(tree2,10)
tree3<-ape::drop.tip(tree2,10)
plot(tree3)
tree3<-ape::drop.tip(tree2,"Homo_sapiens")
plot(tree3)




#load data
data<-read.table("Data/Triquetrum.txt",header=T,sep="\t",row.names=1)
#load a tree
tree<-read.nexus("Data/Primates.nex")
tree_all<-tree
plot(tree)
#Align data and tree
tree<-treedata(tree,data,sort=T,warnings=T)$phy
data<-treedata(tree,data,sort=T,warnings=T)$data
plot(tree)

tree2<-tree
plot(tree2)
tiplabels()
nodelabels()
?bind.tip
tree4<-bind.tip(tree2,"Big_Foot",edge.length=2,where=37,position=4)
plot(tree4)
edgelabels()
tree4$edge.length[23]
tree4$edge.length[24]
tree4$edge.length[29]
plot(tree2)
edgelabels()
tree2$edge.length[22]

#####################################################################################
#EXERCISE 6
#Continue with tree4
#Add a missing link 4 My older than the common ancestor of humans and gorillas. 
#Give this missing link a branch length of 2My
plot(tree4)
tiplabels()
nodelabels()
targetNode<-findMRCA(tree4,c("Homo_sapiens","Gorilla_gorilla_gorilla"))
tree4<-bind.tip(tree4,"Missing_Link",where=targetNode,position=4,edge.length=2)
plot(tree4)
edgelabels()
tree4$edge.length
#Add a 'very' missing link 4 My older than the root of the tree which survived for 2My
tree4<-bind.tip(tree4,"So_Missing_Link_It_Says_Bigly",where="root",position=4,edge.length=2)
str(tree4)     
tree4$root.edge<-5
str(tree4)     
plot(tree4)
#####################################################################################

#Extracting clades
tree2<-tree
plot(tree2)
nodelabels()
tree3<-extract.clade(tree2,findMRCA(tree2,c("Gorilla_beringei","Gorilla_gorilla_gorilla")))
plot(tree3)
tree4<-extract.clade(tree2,findMRCA(tree2,c("Pan_troglodytes_troglodytes","Homo_sapiens")))
plot(tree4)
#Binding trees
plot(tree3)
str(tree3)     
tree3$root.edge<-10
str(tree3)     
plot(tree4)
str(tree4)     
tree4$root.edge<-10
str(tree4)     
tree5<-bind.tree(tree3,tree4,where="root",position=10)
plot(tree5)
tree5$edge.length
tree5<-bind.tree(tree3,tree4,where="root",position=2)
plot(tree5)
tree5$edge.length
#Extracting clades by removing all others
Keep<-tree$tip.label[c(getTips(tree,findMRCA(tree,c("Gorilla_gorilla_gorilla","Pan_troglodytes_troglodytes"))),getTips(tree,findMRCA(tree,c("Ateles_paniscus","Lagothrix_lagotricha"))))]
Remove<-setdiff(tree$tip.label,Keep)
tree6<-drop.tip(tree2,Remove)
plot(tree6)


#####################################################################################
#EXERCISE 7
#Take the primate tree, and derive a tree that consists only of the apes and the strepsirrhines
Apes
"Hylobates_lar"
"Homo_sapiens"
#Take the tree of all primates
tree_all<-read.nexus("./Data/Primates.nex")
pdf("./Workspace/D01_S01_Plot tree primates.pdf",height=60,width=30)
plot(tree_all,label.offset=2)
nodelabels(); tiplabels()
dev.off()

#By pruning
#Extract the apes 
rootApes<-findMRCA(tree_all,c("Homo_sapiens","Hylobates_lar"))
rootStrep<-findMRCA(tree_all,c("Allocebus_trichotis","Galagoides_demidoff"))
targetSpecies<-tree_all$tip.label[c(getTips(tree_all,rootApes),getTips(tree_all,rootStrep))]
removeSpecies<-setdiff(tree_all$tip.label,targetSpecies)
tree_new02<-drop.tip(tree_all,removeSpecies)
plot(tree_new02,cex=0.7)
#####################################################################################

#####################################################################################################
### Basic Tree inference ###
#####################################################################################################


library("phangorn")
library("ape")

# read the file into the object 'primates':
primates <- read.phyDat("chars2.txt", format="phylip", type="DNA")  

# The next step is to provide the package with a starter tree to begin the optimization process. 
# To do this, you might use a distance based approach.
# First, create a distance matrix using the 'phangorn' functions 'dist.dna' and 'as.DNAbin':

dm <- dist.dna(as.DNAbin(primates))    

# Next, create two trees, one using UPGMA and another using Neighbor Joining,
# both of which are available as functions in 'ape':

# trees based on distance

treeUPGMA <- upgma(dm)                    

treeNJ <- NJ(dm)                         

layout(matrix(c(1,2)), height=c(1,1.25)) # plot window dimensions 

par(mar = c(.1,.1,.1,.1))  # adjust margins

plot(treeUPGMA, main="UPGMA", cex = 0.8)  # rooted tree on top; cex adjusts text size

plot(treeNJ, "unrooted", main="NJ", cex = 0.5) # unrooted tree on bottom

# We can now obtain data on the parsimony score (i.e., the number of steps) for the two trees:

parsimony(treeUPGMA, primates)

parsimony(treeNJ, primates)

# The most parsimonious tree is the one with the lowest score. In this case, it is the neighbor joining tree with a score of 302.

#This is great, but what we really want to do is find the most parsimonious tree. For this, we can use the function 'optim.parsimony()', as follows, with our rooted tree:

optParsUPGMA <- optim.parsimony(treeUPGMA, primates)

# with the unrooted tree

optParsNJ <- optim.parsimony(treeNJ, primates)


plot(optParsUPGMA, main="UPGMA", cex = 0.8) # rooted tree on top

plot(optParsNJ, "unrooted", main="NJ", cex = 0.5) # unrooted tree on bottom

#Leaving aside the quality of our tree with this small dataset (what's the deal with Lemur, Pongo and Tarsius?), 
#we can export our tree in newick format using this function:

write.tree(optParsUPGMA, file="optParsUPGMA.nex")

#  Inferring Phylogeny using Maximum Likelihood in R (phangorn)

# Maximum likelihood (ML) is based upon calculating the probability of observed data given a hypothesis.
# The alternative hypotheses in phylogenetic inference are all the various trees that can be drawn for a set of taxa. 
# In an ML search, we aim to find the tree that, given our evolutionary model, results in the highest likelihood of obtaining the data we observe.
# Thus, we are maximizing the likelihood of the data under the model for evolution that we choose, 

fit_treeUPGMA <- pml(unroot(treeUPGMA), data=primates)

 # We can then optimize the branch lengths under this simple model of evolution (the Jukes-Cantor model, where all changes are equally likely),
# and compare the two trees graphically:

fit_treeUPGMA_opt1 <- optim.pml(fit_treeUPGMA)
layout(matrix(c(1,2)), height=c(1,1))
par(mar = c(.1,.1,.1,.1))
plot(fit_treeUPGMA, main="default branches", cex = 0.8)   # top = default branch lengths
plot(fit_treeUPGMA_opt1, main="optimized branches", cex = 0.8)   # bottom = optimized branch lengths

#In the graphics window, the tree on top shows the default branches, while the tree on the bottom shows the branches after optimization.
# Longer branches indicate that more molecular change has occurred along a given branch. You will find that the likelihood of the data under optimized branch lengths has increased considerably - from 1573.4 to 1555.
# We can compare these models using the 'AIC' function and we find unsurprisingly that the AIC is substantially lower for the tree with branches optimized.

AIC(fit_treeUPGMA, fit_treeUPGMA_opt1)


# We can also search for a better tree by re-arranging the branches,
# i.e., by setting the parameter 'optNni' to 'TRUE', which causes the function 'optim.pml' to optimize tree topology in addition to branch lengths:

fit_treeUPGMA_opt2 = optim.pml(fit_treeUPGMA, optNni=TRUE)

layout(matrix(c(1,2)), height=c(1,1))

plot(fit_treeUPGMA_opt1, cex = 0.8)  # top = original topology with optimized branch lengths

plot(fit_treeUPGMA_opt2, cex = 0.8)    # bottom = optimized topology AND branch lengths

AIC(fit_treeUPGMA_opt1, fit_treeUPGMA_opt2)
