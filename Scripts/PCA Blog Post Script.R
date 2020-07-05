

library(FactoMineR)

# NOTE YOU NEED TO SPECIFY THE DIRECTORY
# WHERE YOU SAVED THE DATA!
in_dir <- 'D:\\Directory\\'
feelings <- read.csv(paste0(in_dir, 'feelings.csv'))

#make the row names of conditions the beverage names
feelings <- as.data.frame(feelings)
rownames(feelings) <- feelings$Name

table(feelings$Category)

#####create palette of colors
# http://tools.medialab.sciences-po.fr/iwanthue/
colors_3_cats <- c("#8e7600",
                   "#8245cc",
                   "#018d69")


###### PCA analysis of the chosen variables

head(feelings)

# make the row names of conditions the beverage names
# useful if you want to plot the beverage names
# on the PCA plot
rownames(feelings) <- feelings$Name

# compute the PCA analysis
# we scale the variables and indicate
# the "Category" variable as a "supplementary"
# variable. 
PCA_feelings <- PCA(feelings[, c(2, 5:ncol(feelings))], 
                    scale.unit = TRUE, ncp = 5,
                    quali.sup = 1, graph = FALSE, axes = c(1,2))

###get scree plots, percentage of the variance explained
# make a nice barplot
# http://www.sthda.com/english/wiki/factominer-and-factoextra-principal-component-analysis-visualization-r-software-and-data-mining

#### Eigenvalues

# plot the eigenvalues from the PCA analysis
# using the base plotting system
barplot(PCA_feelings$eig[,1], names.arg=1:nrow(PCA_feelings$eig), 
        main = "Scree Plot",
        xlab = "Principal Component",
        ylab = "Eigenvalues",
        col = "steelblue")
abline(h=1,lty=2,col="red")


#### % of variance explained
barplot(PCA_feelings$eig[, 2], names.arg=1:nrow(PCA_feelings$eig), 
        main = "% of Variance Explained",
        xlab = "Principal Component",
        ylab = "% of variance Explained",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(PCA_feelings$eig), PCA_feelings$eig[, 2], 
      type="b", pch=19, col = "red")



# bi-plot for variables using ggplot2
# adapted from:
# http://stackoverflow.com/questions/10252639/pca-factominer-plot-data  

# first, create the dataset for the variables
vPC1 <- PCA_feelings$var$coord[,1]
vPC2 <- PCA_feelings$var$coord[,2]
vlabs <- rownames(PCA_feelings$var$coord)
vPCs <- data.frame(cbind(vPC1, vPC2))
rownames(vPCs) <- vlabs
colnames(vPCs) <- c('PC1', 'PC2')

str(vPCs)

# now make the bi-plot
library(ggplot2)

# set up theme for plot
pv <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# put a faint circle there, as is customary
angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
pv <- pv + geom_path(aes(x, y), data = df, colour="grey70") 
# add on arrows and variable labels
pv <- pv + geom_text(data = vPCs, aes_string(x = vPC1, y = vPC2),
              label=rownames(vPCs), cex= 2) + xlab('PC1 (62.49%)') + 
              ylab('PC2 (13.61%)')
# define end points for arrows    
pv <- pv + geom_segment(data=vPCs, aes_string(x = 0, y = 0, xend = vPC1*0.9, 
              yend = vPC2*0.9), arrow = arrow(length = unit(1/2, 'picas')), 
              color = "grey30")
# specify format for the axis titles and text
pv <- pv + theme(axis.title.y = element_text(size = rel(.65), angle = 90))
pv <- pv + theme(axis.title.x = element_text(size = rel(.65), angle = 00))
pv <- pv + theme(axis.text.y = element_text(angle = 90, size=13))
pv <- pv + theme(axis.text.x = element_text(angle = 00, size=13))
# show plot
pv 

# display the correlations between
# the variables and the principal components
dimdesc(PCA_feelings, proba = 1)



# create the dataset for the individuals (beverages)
PC1 <- PCA_feelings$ind$coord[,1]
PC2 <- PCA_feelings$ind$coord[,2]
Category <- as.character(feelings$Category)
labs <- rownames(PCA_feelings$ind$coord)
PCs <- data.frame(cbind(PC1,PC2,Category))
rownames(PCs) <- labs

# create the dataset for qualitative supplementary variables
cPC1 <- PCA_feelings$quali.sup$coor[,1]
cPC2 <- PCA_feelings$quali.sup$coor[,2]
clabs <- rownames(PCA_feelings$quali.sup$coor)
cPCs <- data.frame(cbind(cPC1,cPC2,clabs))
rownames(cPCs) <- clabs
colnames(cPCs) <- c('PC1', 'PC2', 'Category')

# plot the individuals (beverages)
# and quali supp variable

# define the plot basics - individuals
g <- ggplot(PCs, aes_string(x = PC1, y = PC2, color = "Category")) + geom_point()
# define the colors, axis limits & labels
g <- g + scale_color_manual(values=c("#8e7600", "#8245cc", "#018d69")) + 
  xlab('PC1 (62.49%)') + ylab('PC2 (13.61%)') +
  coord_cartesian(xlim = c(-5.5, 5), ylim = c(-5, 2.5)) #+
# add the quali supplemental variables
g <- g + geom_point(data = cPCs, aes(x=cPC1,y=cPC2), size = 8)
# show the plot
g

