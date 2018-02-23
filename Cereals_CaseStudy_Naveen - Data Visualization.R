############# Cereal Case Study For Practice ############## 
# Under the Guidance of Dr. Vinod

# Which Cereal Breakfast is Healthy?

library("readxl")
#Reading the cereal data
cereal_data<- read_excel("E:/MACHINE LEARNING WITH R/DR. Vinod/fwdacadgildnotification/cereals_practice.xlsx", sheet = 1)
head(cereal_data)
str(cereal_data)
summary(cereal_data)

dim(cereal_data)

# Checking for any missing value
sum(is.na(cereal_data))

# List record with missing data
cereal_data[ apply(is.na(cereal_data), 1, any),]

# Removing the missing values
# cereal_data[cereal_data == -1] <- NA
cereal_data=na.omit(cereal_data)
View(cereal_data)

#Finding Correlation between these variables
library(corrplot)
col_numeric <- sapply(cereal_data, is.numeric)
numericalVars <- cereal_data[,col_numeric]
corrMat <- cor(numericalVars)
corrMat

# Correlation Plot
corrplot(corrMat, method= "number")
corrplot(corrMat, method= "pie")

col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "blue", "#00007F"))
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col3 <- colorRampPalette(c("red", "white", "blue")) 
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                           "cyan", "#007FFF", "blue", "#00007F"))
whiteblack <- c("white", "black")


corrplot(corrMat, order = "AOE")
corrplot(corrMat, order = "hclust")
corrplot(corrMat, order = "FPC")
corrplot(corrMat, order = "alphabet")
corrplot(corrMat, order = "hclust", addrect = 2)
corrplot(corrMat, order = "hclust", addrect = 3,method= "number")
corrplot(corrMat, order = "hclust", addrect = 3)
corrplot(corrMat, order = "hclust", addrect = 2, col = whiteblack, bg = "gold2")


# install.packages("RColorBrewer")
library(RColorBrewer)

corrplot(corrMat, type = "lower", order = "hclust", method= "number",
         col = brewer.pal(n = 8, name = "RdBu"))

corrplot(corrMat, type = "lower", method= "number", order = "alphabet",
         col = brewer.pal(n = 8, name = "PuOr"))

corrplot(corrMat, type = "lower", method= "number", order = "hclust", tl.col = "black", tl.srt = 45)


# Understanding Calories vs Rating
summary(cereal_data$calories)
# install.packages("Hmisc")
library("Hmisc")
describe(cereal_data$calories)
# stem(cereal_data$calories, scale=1)
# boxplot(cereal_data$calories,data=cereal_data)

boxplot(cereal_data$calories, horizontal=TRUE, main="Understanding the density of Calories")
boxplot(cereal_data$rating, horizontal=TRUE, main="Understanding the density of ratings")

par(mfrow=c(3,3))
hist(cereal_data$calories, right=FALSE, col="yellow", main="Histogram of Calories", xlab="Calories")
hist(cereal_data$protein, right=FALSE, col="Green", main="Histogram of Protein", xlab="Protein")
hist(cereal_data$fat, right=FALSE, col="orangered", main="Histogram of fat", xlab="fat")
hist(cereal_data$sodium, right=FALSE, col="Blue", main="Histogram of Sodium", xlab="sodium")
hist(cereal_data$fiber, right=FALSE, col="cyan", main="Histogram of Fiber", xlab="fiber")
hist(cereal_data$carbo, right=FALSE, col="Violet", main="Histogram of Carbo", xlab="carbo")
hist(cereal_data$sugars, right=FALSE, col="olivedrab1", main="Histogram of Sugars", xlab="sugars")
hist(cereal_data$potass, right=FALSE, col="chocolate1", main="Histogram of Potass", xlab="potass")
hist(cereal_data$rating,col="seagreen1", main = "Histogram of Ratings", xlab = "rating") 
par(mfrow=c(1,1))

# install.packages("reshape2")
# install.packages("ggcorrplot")
# install.packages("lattice")

library(reshape2)
library(ggcorrplot)
library(lattice)

res <- as.data.frame(cor(corrMat))
res$Nutrients <- rownames(res)
df1 <- melt(res,"Nutrients")
g1 <- ggplot(df1[ which(df1$variable=='rating' & df1$Nutrients != 'rating'),], aes(x = Nutrients, y=value)) +
  geom_bar( position = "identity", stat = "identity", alpha = .3 ,aes(fill = Nutrients))
g1

# Bar Chart of Manufacture
ggplot(cereal_data, aes(mfr)) +
  geom_bar() +
  labs(title = "Manufacture of Cereal")

# Correlogram of cereals Nutrients
ggcorrplot(corrMat, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of cereals Nutrients", 
           ggtheme=theme_bw)

# correlation between the rating and other variables 
res <- as.data.frame(cor(corrMat))
res$Nutrients <- rownames(res)
df1 <- melt(res,"Nutrients")
g1 <- ggplot(df1[ which(df1$variable=='rating' & df1$Nutrients != 'rating'),], aes(x = Nutrients, y=value)) +
 geom_bar( position = "identity", stat = "identity", alpha = .3 ,aes(fill = Nutrients))
g1

# Density plot for Manufacturer Vs Ratings
g <- ggplot(cereal_data, aes(rating))
g + geom_density(aes(fill=factor(mfr)), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Lets understand the manufacturer with the best ratings",
       caption="Density plot for Manufacturer Vs Ratings",
       x="Rating",
       fill="# Manufacturer") + coord_cartesian(ylim = c(0, 0.08)) 

# Density plot for Manufacturer Vs Fiber
g <- ggplot(cereal_data, aes(fiber))
g + geom_density(aes(fill=factor(mfr)), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Lets understand the manufacturer with the best fiber",
       caption="Density plot for Manufacturer Vs Fiber",
       x="Fiber",
       fill="# Manufacturer") 

# Density plot for Manufacturer Vs protein
g <- ggplot(cereal_data, aes(protein))
g + geom_density(aes(fill=factor(mfr)), alpha=0.5) + 
  labs(title="Density plot", 
       subtitle="Lets understand the manufacturer with the best protein",
       caption="Density plot for Manufacturer Vs protein",
       x="Protein",
       fill="# Manufacturer") 

# Divergence of ratings for a manufacturer
cereal.ordered <- cereal_data
cereal.ordered$`Manufacturer name` <- cereal.ordered$mfr
cereal.ordered$rating_z <- round((cereal.ordered$rating - mean(cereal.ordered$rating))/sd(cereal.ordered$rating), 2)  # compute normalized ratings
cereal.ordered$rating_type <- ifelse(cereal.ordered$rating_z < 0, "below", "above")  # above / below avg flag
cereal.ordered <- cereal.ordered[order(cereal.ordered$rating_z), ]  # sort

# Diverging Barcharts
ggplot(cereal.ordered, aes(x=`Manufacturer name`, y=rating_z, label=rating_z)) + 
  geom_bar(stat='identity', aes(fill=rating_type), width=.5)  +
  scale_fill_manual(name="Rating", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised Ratings for Manufacturers", 
       title= "Diverging Bars (Ratings)") + 
  coord_flip()

# Divergence of fiber for a manufacturer
cereal.ordered <- cereal_data
cereal.ordered$`Manufacturer name` <- cereal.ordered$mfr
cereal.ordered$fiber_z <- round((cereal.ordered$fiber - mean(cereal.ordered$fiber))/sd(cereal.ordered$fiber), 2)  # compute normalized fiber content
cereal.ordered$fiber_type <- ifelse(cereal.ordered$fiber_z < 0, "below", "above")  # above / below avg flag
cereal.ordered <- cereal.ordered[order(cereal.ordered$fiber_z), ]  # sort

# Diverging Barcharts
ggplot(cereal.ordered, aes(x=`Manufacturer name`, y=fiber_z, label=fiber_z)) + 
  geom_bar(stat='identity', aes(fill=fiber_type), width=.5)  +
  scale_fill_manual(name="Fiber", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised fiber for Manufacturers", 
       title= "Diverging Bars (Fiber)") + 
  coord_flip()

# Divergence of protein for a manufacturer
cereal.ordered <- cereal_data
cereal.ordered$`Manufacturer name` <- cereal.ordered$mfr
cereal.ordered$protein_z <- round((cereal.ordered$protein - mean(cereal.ordered$protein))/sd(cereal.ordered$protein), 2)  # compute normalized protein content
cereal.ordered$protein_type <- ifelse(cereal.ordered$protein_z < 0, "below", "above")  # above / below avg flag
cereal.ordered <- cereal.ordered[order(cereal.ordered$protein_z), ]  # sort

# Diverging Barcharts
ggplot(cereal.ordered, aes(x=`Manufacturer name`, y=protein_z, label=protein_z)) + 
  geom_bar(stat='identity', aes(fill=protein_type), width=.5)  +
  scale_fill_manual(name="Protein", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised protein for Manufacturers", 
       title= "Diverging Bars (Protein") + 
  coord_flip()

library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(calories, aes(mfr))
g + geom_bar(aes(fill=factor(mfr)), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes")

g <- ggplot(calories, aes(fiber))
g + geom_bar(aes(fill=factor(mfr)), width=0.5) + 
  labs(title="Density plot", 
       subtitle="Lets understand the manufacturer with the best fiber",
       caption="Density plot for Manufacturer Vs Fiber",
       x="Fiber",
       fill="# Manufacturer") 

g + geom_histogram(aes(fill=mfr), 
                   bins=5, 
                   col="black", 
                   size=.1) +   # change number of bins
  labs(title="Histogram with Fixed Bins", 
       subtitle="Engine Displacement across Vehicle Classes") 


