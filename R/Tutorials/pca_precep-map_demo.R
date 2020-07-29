
library(plfm)
data(car)

#look at data
str(mtcars) #small sample so its easy to see
summary(mtcars)

#look at data --we have many features/attributes for each car, but many are probably related (HP, engine size, fuel eff)
car$freq1

#get numeric cols only
data_subset = mtcars[,c(1:7,10,11)]
str(data_subset)

#applt pca
pca = prcomp(data_subset, center = TRUE,scale. = TRUE)
summary(pca)

#summary(mtcars.pca)

#plot top 2 components on a perceptual map
#library(fs)
#library(devtools)
#on mac might need to enable xcode (reminder miigh tneed to sudo xcodebuild -license)
#install_github("vqv/ggbiplot");

library(ggbiplot)

#plot
ggbiplot(mtcars.pca)
mtcars.pca