#load packages
library(jpeg)
library(png)
library(scatterplot3d)
library(Morpho)
library(geomorph)


#2D
#reading an image and interacting with the image
oo<-readPNG("pusheen.png")
plot(c(1, dim(oo)[2]), c(1, dim(oo)[1]), type = "n", xlab = "", ylab = "", asp=1)
a<-rasterImage(oo, 0, 0, dim(oo)[2], dim(oo)[1])
#getting coordinates from the image
coord<-locator(5,type="p",bg=5, pch=21)
coord

# basic data interaction
dat<-scan("TURTLE.DTA", skip=8, what="character")
turt1<-matrix(as.numeric(dat[1:(74*3)]), 74,3, byrow=T)
#use scatterplot3d to draw the configuration
scatterplot3d(turt1, scale.y=1) 
plot(turt1[,1], turt1[,2], asp=1)
#another way to visualize x, y and z dimensions
pairs(turt1, asp=1)
link<-c((1:25)*2-1,1,NA,(1:25)*2,2,NA,1,2,NA,3,4,NA,5,6,NA,7,8,NA,9,10,NA,11,12,NA,13,14,NA,15,16,NA,17,18,NA,19,20,NA,21,22,NA,23,24,NA,25,26,NA,27,28,NA,29,30,NA,31, 32,NA,33,34,NA,35,36,NA,37,38,NA,39,40,NA,41,42,NA,43,44,NA,45,46,NA,47,48,NA,49,50, NA,51,61:74,60,51,NA,61,74,NA,63,72,NA,70,65,NA,67,24,NA,28,68,NA,66,54,NA,64,53,NA,62, 52,NA,73,59,NA,71,58,NA,69,57)
plot(turt1[link,1:2],type="b", asp=1)   
scatterplot3d(turt1[link,1], turt1[link,2], turt1[link,3], type="b", angle=120)
#the rgl library
rglwidget()
plot3d(turt1[,1], turt1[,2], turt1[,3], aspect="iso") 
#draw spheres instead of points
plot3d(turt1[,1], turt1[,2], turt1[,3], aspect="iso",type="s",col="purple", box=F, axes=F)
#adjust the size of spheres
plot3d(turt1[,1], turt1[,2], turt1[,3], aspect="iso",type="s",size=0.6,col="red", box=F, axes=F)
#add links
lines3d(turt1[link,1], turt1[link,2], turt1[link,3],col="blue", lwd=3)
#identify a landmark in 3d
identify3d(turt1, buttons="right", n=1, adj=2)
# Another example
#read PLY files and plot mesh files
#load the geomorph library
adapis<-read.ply("Adapis_parisiensis_left_ear.ply")
#clear the rgl scene
rgl.clear()
#use two colors for vertices
shade3d(adapis, color=c("magenta", "cyan",'yellow'))
# another example
data(nose)
shade3d(shortnose.mesh, color="pink")

#
#digitise 3d landmarks  #https://rdrr.io/cran/geomorph/man/digitsurface.html
## .ply has to be in ASCII format not binary
x <-read.ply("surface.for.specimen1.ply", ShowSpecimen=TRUE,addNormals = F) #read ply file and generate individual data
colors='green'
x$material=list(color=colors)
digit.fixed(x, fixed=5, index = FALSE, ptsize =1, center = TRUE) # landmark collection


# some examples
#getting familiar with GM data
data(plethodon)
#
oo<-readPNG("plethodon.png")
plot(c(1, dim(oo)[2]), c(1, dim(oo)[1]), type = "n", xlab = "", ylab = "", asp=1)
a<-rasterImage(oo, 0, 0, dim(oo)[2], dim(oo)[1])
# check coordinates 1st individual
plethodon$land[,,1] 
# check covariates
plethodon$species
# Procrustes
Y <- gpagen(plethodon$land)
plot(Y)
plotAllSpecimens(Y$coords,links=plethodon$links)
Y$coords #shape variables
Csize<-Y$Csize # Centroid size
#
#VISUALIZATION
ref <- mshape(Y$coords) #creates a reference from the average of the GPA specimens
#Principal Components Analysis
PCA<-plotTangentSpace(Y$coords, groups=as.factor(paste(plethodon$species, plethodon$site)))
#
#plot shape differences between a reference and target specimen
plotRefToTarget(ref, Y$coords[,,39], method="TPS")
#can magnify the differences seen using mag=#
plotRefToTarget(ref, Y$coords[,,39], mag=3, method="TPS") #magnifies by 3
#by changing the method to "vector" instead of "TPS" can see vector displacements rather than a thin plate spline
#a similar change is induced by switching the method to "points" to show reference and landmark points
plotRefToTarget(ref, Y$coords[,,39], mag=3, method="points", links=plethodon$links) 
#

#3d
#for a "fun diversion", see how to use a 3D dataset with semilandmarks
#load in the scallop data and check out the landmarks and semilandmarks. This dataset has semilandmarks on both curves and surfaces, differing from the demo dataset using salamanders (only landmarks)
data(scallops)
scallops$curvslide # Matrix defining which points are semilandmarks (middle column) and in which directions they slide (columns 1 [before] vs. 3 [after])
scallops$surfslide # Matrix (1 column) defining which points are semilandmarks to slide over the surface
#Procrustes Distance for the semilandmarks
Sc <- gpagen(A=scallops$coorddata, curves=scallops$curvslide, surfaces=scallops$surfslide)
#thin plate spline
scallopref <- mshape(Sc$coords)
plotRefToTarget(scallopref, Sc$coords[,,1], method="TPS", mag=3)
#plot 3D specimen with landmarks and semilandmarks
data(scallopPLY)
ply <- scallopPLY$ply
digitdat <- scallopPLY$coords
plotspec(spec=ply, digitspec=digitdat, fixed=16, centered=T) 
rglwidget()
#PCA
PCA_Sc<-plotTangentSpace(Sc$coords)
# plot PC extremes
PC1max<-plotRefToTarget(scallopref,PCA_Sc$pc.shapes$PC1max, mesh = ply, method="surface", mag = 3)
PC1min<-plotRefToTarget(scallopref,PCA_Sc$pc.shapes$PC1min, mesh = ply, method="surface", mag = 3)



# Colecting GM data


##### 1: Digitizing in StereoMorph
install.packages("StereoMorph")
library(StereoMorph)
library(geomorph)

# Landmark digtizing: see Tutorial for additional instructions
digitizeImages(image.file='Images', shapes.file='Shapes',
               landmarks.ref=paste("LM", c(1:5), sep=""))

# Scaling: See Tutorial for instructions

# Read Data

shapes <- readShapes("Shapes") # Steromorph function.  Turns image txt files into a list
shapesGM <- readland.shapes(shapes) # geomorph conversion function

# GPA (more below)

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

# Digitizing a curve (add to existing LMs)

digitizeImages(image.file='Images', shapes.file='Shapes',
               landmarks.ref=paste("LM", c(1:5), sep=""),
               curves.ref = "example.curves.txt")

# Read Data

shapes <- readShapes("Shapes") # Steromorph function.  Turns image txt files into a list
shapesGM <- readland.shapes(shapes, nCurvePts = 10) # geomorph conversion function

# GPA

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

#### 2 Read Data

# Working with digitized specimens (StereoMorph)
# These 30 specimens have 11 fixed landmarks and 6 curves

shapes <- readShapes("example.digitized")

# curves include eye, head, tail1, tail end, tail2, anal fin, 
# pectoral fin, opercle, pre-opercle
curves.list <- c(20, 24, 24, 16, 12, 8, 8, 24, 16)
shapesGM <- readland.shapes(shapes, nCurvePts = curves.list)

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

# Too many semilandmarks?

shapesGM <- readland.shapes(shapes, nCurvePts = 0.5 * curves.list)

Y.gpa <- gpagen(shapesGM)
plot(Y.gpa)

# Attributes

class(shapesGM)

attributes(shapesGM)

shapesGM$fixed
shapesGM$curves
shapesGM$landmarks[[1]] # specimen 1
names(shapesGM$landmarks)


# Read TPS File
mydata <- readland.tps("Data/salamanders.tps")
str(mydata)
dim(mydata)  # file contains 287 specimens with 12 landmarks each of 2D data
mydata[,,1]

mydata <- readland.tps("Data/salamanders.tps", specID="imageID") # Specify specimen labels
str(mydata)
mydata[,,1:2]

# Read NTS File
mydata <- readland.nts("Data/RATS.nts")
str(mydata)
mydata[,,1]

# Read Morphologika
mydata <- read.morphologika("Data/mophologikaexample.txt")
str(mydata)
dim(mydata$coords)
mydata$coords[,,1]

# Read PLY
new <- read.ply("Data/Mandible.ply")
str(new)

# Read CSV
food <- read.csv("Data/food.csv", header=TRUE, row.names=1)
str(food)
food[1:2,]

# Read Phylogeny
library(ape)
mytree <- read.tree("Data/plethtree.tre")
plot(mytree)

##### 3: Data Pre-Processing

# Check for Outliers
data(plethodon)
Y <- gpagen(newland, print.progress = FALSE) 

plotOutliers(Y$coords, inspect.outliers = T)

# Fixed Angle
jaw.fixed <- fixed.angle(Y$coords,
                         art.pt=1, angle.pts.1 = 5, 
                         angle.pts.2 = 6, rot.pts = c(2,3,4,5))

gpa.fixed <- gpagen(jaw.fixed, print.progress = FALSE)
plotAllSpecimens(gpa.fixed$coords, links = plethodon$links)

# Estimate Missing Landmarks

#### build some missing data (EXAMPLE ONLY)
data(plethodon)
plethland<-plethodon$land
plethland[3,,2]<-plethland[8,,2]<-NA  #create missing landmarks
plethland[3,,5]<-plethland[8,,5]<-plethland[9,,5]<-NA  
plethland[3,,10]<-NA  

# Estimate via TPS or Regression
estimate.missing(plethland,method="TPS")
estimate.missing(plethland,method="Reg")

##### 4: Generalized Procrustes Analysis: GPA

data(plethodon)
pleth.gpa <- gpagen(plethodon$land, print.progress = F)
summary(pleth.gpa)

plot(pleth.gpa)
plotAllSpecimens(pleth.gpa$coords, links = plethodon$links)


# part2 -------------------------------------------------------------------

library(geomorph)

##### 1: GPA with semilandmarks

# Fixed points only
data(plethodon)
pleth.gpa <- gpagen(plethodon$land, print.progress = F)
summary(pleth.gpa)

plot(pleth.gpa)
plotAllSpecimens(pleth.gpa$coords, links = plethodon$links)

# Points and Curve points
data(hummingbirds)
hummingbirds$curvepts   
gpa.BE <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=FALSE, print.progress = F)
plot(gpa.BE)

gpa.procD <- gpagen(hummingbirds$land, curves=hummingbirds$curvepts, ProcD=TRUE, print.progress = F)
plot(gpa.procD)

# Points, Curves, and Surfaces
data(scallops)
scallops$surfslide  
gpa.scallop <- gpagen(A=scallops$coorddata, curves=scallops$curvslide, surfaces=scallops$surfslide, print.progress = F)
plot(gpa.scallop)

# Points and curves via readland.shapes

library(StereoMorph)

shapes <- readShapes("example.digitized")
shapesGM <- readland.shapes(shapes, 
                            nCurvePts = c(12, 12, 12, 8, 6, 6, 6, 12, 10))

shapesGM$curves
gpa.pupfish <- gpagen(shapesGM)
plot(gpa.pupfish)

##### 2: Visualizing Shape Differences and PCA

# Plotting all specimens
data(plethodon)
Y.gpa <- gpagen(plethodon$land, print.progress = F)    # GPA-alignment

par(mfrow=c(1,2)) 
plotAllSpecimens(plethodon$land, links=plethodon$links)  # Raw data
mtext("Raw Data")
plotAllSpecimens(Y.gpa$coords, links=plethodon$links)    # GPA-aligned data
mtext("GPA-Aligned Specimens")
par(mfrow=c(1,1)) 

# Types of deformations

ref <- mshape(Y.gpa$coords)
par(mfrow=c(3,2))
plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links)
mtext("TPS")
plotRefToTarget(ref,Y.gpa$coords[,,39],mag=2.5, links=plethodon$links)
mtext("TPS: 2.5X magnification")

plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links,method="vector",mag=3)
mtext("Vector Displacements")
plotRefToTarget(ref,Y.gpa$coords[,,39], links=plethodon$links,gridPars=gridPar(pt.bg="red", link.col="green", pt.size = 1),
                method="vector",mag=3)
mtext("Vector Displacements: Other Options")

plotRefToTarget(ref,Y.gpa$coords[,,39],mag=2,outline=plethodon$outline)  
mtext("Outline Deformation")
plotRefToTarget(ref,Y.gpa$coords[,,39],method="points",outline=plethodon$outline)
mtext("OUtline Deformations Ref (gray) & and Tar (black)")
par(mfrow=c(1,1))

# Shape Predictions

# PCA-based
M <- mshape(Y.gpa$coords)
PCA <- gm.prcomp(Y.gpa$coords)
PC <- PCA$x[,1]
preds <- shape.predictor(Y.gpa$coords, x= PC, Intercept = FALSE, 
                         pred1 = min(PC), pred2 = max(PC)) # PC 1 extremes, more technically
plotRefToTarget(M, preds$pred1, links = plethodon$links)
mtext("PC1 - Min.")
plotRefToTarget(M, preds$pred2, links = plethodon$links)
mtext("PC1 - Max.")

# Regression-based
gdf <- geomorph.data.frame(Y.gpa)
plethAllometry <- procD.lm(coords ~ log(Csize), data=gdf, print.progress = FALSE)
allom.plot <- plot(plethAllometry, 
                   type = "regression", 
                   predictor = log(gdf$Csize),
                   reg.type ="PredLine") # make sure to have a predictor 

preds <- shape.predictor(plethAllometry$GM$fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
plotRefToTarget(M, preds$predmin, mag=3, links = plethodon$links)
plotRefToTarget(M, preds$predmax, mag=3, links = plethodon$links)

# via picknplot.shape (more detail below)

picknplot.shape(allom.plot) 

# Group difference-based
gdf <- geomorph.data.frame(Y.gpa, species = plethodon$species, site = plethodon$site)
pleth.anova <- procD.lm(coords ~ species*site, data=gdf, print.progress = FALSE)
X <- pleth.anova$X
X # includes intercept; remove for better functioning 
X <- X[,-1]
symJord <- c(0,1,0) # design for P. Jordani in sympatry
alloJord <- c(0,0,0) # design for P. Jordani in allopatry
preds <- shape.predictor(pleth.anova$GM$fitted, x = X, Intercept = TRUE, 
                         symJord=symJord, alloJord=alloJord)
plotRefToTarget(M, preds$symJord, links = plethodon$links, mag=2)
plotRefToTarget(M, preds$alloJord, links = plethodon$links, mag=2)

# via picknplot.shape (more detail below)

plot.anova <- plot(pleth.anova, type = "PC", pch = 21, 
                   bg = interaction(gdf$species, gdf$site), 
                   asp = 1)

picknplot.shape(plot.anova) 

##### 3: Principal Components Analysis (PCA)

plotTangentSpace(Y.gpa$coords, groups = interaction(plethodon$species, plethodon$site))

pleth.raw <- gm.prcomp(Y.gpa$coords)

gps <- as.factor(paste(plethodon$species, plethodon$site))
plot(pleth.raw)
par(mar=c(2, 2, 2, 2))
plot(pleth.raw, pch=22, cex = 1.5, bg = gps) 
#  Add things as desired using standard R plotting
text(par()$usr[1], 0.1*par()$usr[3], labels = "PC1 - 45.64%", pos = 4, font = 2)
text(0, 0.95*par()$usr[4], labels = "PC2 - 18.80%", pos = 4, font = 2)
legend("topleft", pch=22, pt.bg = unique(gps), legend = levels(gps))

##### 4: PickNPlot Shapes in Real Time (more detail here)

data(plethodon) 
Y.gpa <- gpagen(plethodon$land)
pleth.pca <- gm.prcomp(Y.gpa$coords)

pleth.pca.plot <- plot(pleth.pca)
picknplot.shape(pleth.pca.plot) 

picknplot.shape(plot(pleth.pca), method = "points", mag = 3, links=plethodon$links)

##### 5: 3D Warping

scallops <- readland.tps("Data/scallops for viz.tps", specID = "ID")
ref <- mshape(scallops)
refmesh <- warpRefMesh(read.ply("Data/glyp02L.ply"), 
                       scallops[,,1], ref, color=NULL, centered=T)
plotTangentSpace(scallops, axis1 = 1, axis2 = 2, warpgrids=T, mesh= refmesh)

##### 6: Two-Block Partial Least Squares (PLS)

data(pupfish)
Y.gpa <- gpagen(pupfish$coords, print.progress = FALSE)
plotAllSpecimens(Y.gpa$coords)
shape <- Y.gpa$coords
headland <- c(4, 10:17, 39:56)

PLS <- two.b.pls(shape[headland,,],shape[-headland,,], iter=999, print.progress = FALSE)
summary(PLS)
pls.plot <- plot(PLS)

## PLS shape predictions
preds <- shape.predictor(shape[headland,,], two.d.array(shape[-headland,,]), Intercept = FALSE,
                         method = "PLS", pred1 = -0.2, pred2 = 0.2) # using PLS plot as a guide

M <- mshape(shape[headland,,])
plotRefToTarget(M, -1*preds$pred1, mag=3)
plotRefToTarget(M, preds$pred2, mag=3)

# via picknplot.shape (more detail above)

picknplot.shape(pls.plot, mag = 3) 

##### 7: Regression

pupfish$logSize <- log(pupfish$CS)  #add logCS to geomorph data frame
fit <- procD.lm(coords ~ logSize, data = pupfish, print.progress = FALSE)
anova(fit)

plot(fit)
plot(fit, type = "regression", reg.type = "PredLine", predictor = pupfish$logSize, pch=21, bg="red")
plot(fit, type = "regression", reg.type = "RegScore", predictor = pupfish$logSize, pch=21, bg="red")

## Regression predictions
allom.plot <- plot(fit, 
                   type = "regression", 
                   predictor = pupfish$logSize,
                   reg.type ="RegScore") # make sure to have a predictor 

preds <- shape.predictor(allom.plot$GM$fitted, x= allom.plot$RegScore, Intercept = FALSE, 
                         predmin = min(allom.plot$RegScore), 
                         predmax = max(allom.plot$RegScore)) 
M <- mshape(pupfish$coords)
plotRefToTarget(M, preds$predmin, mag=3)
plotRefToTarget(M, preds$predmax, mag=3)


# via picknplot.shape (more detail above)

picknplot.shape(allom.plot, mag = 3) 


