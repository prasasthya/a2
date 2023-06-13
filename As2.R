library(imager)
library(tverse)
library(rverse)

#part a 
data(iris)
boxplot(Sepal.Length ~ Species, data = iris, col = "lightblue")
boxplot(Petal.Length ~ Species, data = iris, col = "lightgreen")
plot(Sepal.Length, Petal.Length, data = iris, col = iris$Species, pch = 16)

#part b
flip <- function(image) {
  
  img_dims <- dim(image)
  height <- img_dims[1]
  width <- img_dims[2]
  
  
  flipped_image <- imager(height = height, width = width)
  
  
  for (y in 1:height) {
    for (x in 1:width) {
      
      pixel_color <- image[y, x, ]
      
      
      flipped_x <- width - x + 1
      
      
      flipped_image[y, flipped_x, ] <- pixel_color
    }
  }
  
  
  return(flipped_image)
}

#part c
library(MASS)
?ships
data(ships)


plot_accidents <- barplot(table(ships$type), 
                          main = "Number of Accidents by Ship Type",
                          xlab = "Ship Type",
                          ylab = "Number of Accidents",
                          col = "skyblue",
                          ylim = c(0,max(table(ships$type)) + 2))


text(plot_accidents, table(ships$type), table(ships$type), pos = 3)
#part e


value = 0
value[1:1000] = 0


for(i in 1:10000){
  k = 1
  p = 1
  while(k){
    k = sample(c(0,1),size = 1,prob = c(1-p,p))
    value[i] = value[i] + 1
    p = p - (1/100)
  }
}

ans = mean(t)