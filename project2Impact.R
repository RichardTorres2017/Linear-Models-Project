library(ggplot2)
data <- read.table("table142.txt", header=T, sep=";")
data

# Points of  Lot: lot of insulating material 
# vs strength: impact strength 
ggplot(data, aes(Lot, Strength)) +
   geom_point()

# Boxplot of  Lot: lot of insulating material 
#vs strength: impact strength 
# in foot-pounds.
ggplot(data, aes( Lot , Strength)) +
   geom_boxplot()

# Points of Cut: cut lengthwise (length) or crosswise (cross)  
# vs strength: impact strength in foot-pounds.
ggplot(data, aes(Cut, Strength)) +
   geom_point()

# Boxplot of Cut: cut lengthwise (length) or crosswise (cross)  
# vs strength: impact strength in foot-pounds.
ggplot(data, aes(Cut, Strength)) +
   geom_boxplot()

## The model Strength=Lot*Cut
mod1 <- lm(Strength~Lot*Cut, data=data, x=T)
mod1
# Summary of our model
summary(mod1)
# Anova table of our model
anova(mod1)

