# 1 Question 

d$Title<-regmatches(as.character(d$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(d$Name)))
d$Title<-unlist(lapply(d$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(d$Title)


d$Title[which(d$Title %in% c("Mme", "Mlle"))] <- "Miss"
d$Title[which(d$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
d$Title[which(d$Title=="Dr" & d$Sex=="female")] <- "Mrs"
d$Title[which(d$Title=="Dr" & d$Sex=="male")] <- "Mr"
d$Title[which(d$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
d$Title<-as.factor(d$Title)


ggplot(d, aes(Title, fill = Title)) +
  geom_bar(position = 'fill') +
  ggtitle('Titles Represent families') + 
  labs(y = '%')



# 2 Question 

familySize <- d$SibSp + d$Parch + 1
familySizeClass = array(dim = length(familySize))
familySizeClass[familySize == 1] = 'Small'
familySizeClass[familySize >= 2 & familySize <= 4] = 'Medium'
familySizeClass[familySize > 4] = 'Big'

d$FamilySize <- as.factor(familySizeClass)

library(ggplot2)
ggplot(d, aes(FamilySize, fill = Survived)) +
  geom_bar(position = 'fill') +
  ggtitle('Family Size Impact on Survival') + 
  labs(y = '%')


# 3 Question

hist(d$Age, xlab = 'Age Standardized', main = 'Age Histogram Before ')


table(is.na(d$Age))
d$Age[is.na(d$Age)] <-   mean(d$Age,na.rm = T)

hist(d$Age, xlab = 'Age Standardized', main = 'Age Histogram After ')

