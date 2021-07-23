#reading the data sets
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#adding a 'survived' column to test frame to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
#above line creates a data frame test.suvived 
#and adds a new column 'survived' and repeats the value none for nrows(test) number of times
# and adds the test df to it

#combining the data sets
data.combined <- rbind(train, test.survived)
#rbind adds the two dfs and appends the rows

#this line reflects all the datatypes of the variables
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
#the above two lines of code turn pclass and survival variable of the 
#data set into factor datatype for data analysis reasons

#take a look at survival rates
table(data.combined$Survived)
#this will display a table about who perished 
#and who survived

#distribution across classes ie percentage of classes
table(data.combined$Pclass)

#loading up ggplot2 for data visualisation
library(ggplot2)

#hypothesis - rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
#truned Pclass of train data set to factor
ggplot(train, aes(x = Pclass, fill = factor(Survived)))+
  geom_bar(width = 0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")
#first arg is data we need to plot, 
#second is aesthetics, ie what x axis would be
#3rd arg would be color grading the plot according to Survived values which has been 
#typecasted to factor
#then we add the type of plot and the width and the labels
#and how we assign fill to survived
#from the geom_bar the hypothesis is kinda proved to be true

#examine the few names in the training data set
head(as.character(train$Name))
#head() gives first few of the top

#how many unique names are there across both datasets
length(unique(as.character(data.combined$Name)))
#unique returns a vector of unique elements
#length would be the length of that array

#this stores the duplicate names as an array
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
#duplicated() determines which elements of a vector 
#or data frame are duplicates of elements with smaller subscripts,
#and returns a logical vector indicating which elements (rows) are duplicates
#which() would assigning TRUE value for the ones satisfying and those values would be taken in the vector

#now we check the combined data set for the duplicate names
data.combined[which(data.combined$Name %in% dup.names),]
#the second arg is left vacant cause it will show all data if name matches with duplicate
#we checked and found they are just same names of diff persons
#%in% checks whether the iterator is present in second df or not

#another library is loaded
library(stringr)

#trying to derive data from name titles
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]#shows first 5 rows and all columns so col is left blank
#the above code will extract all data in which the string detect function will see Miss

#same for Mrs.
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

#same for men but acc to sex
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]


#expand upon the relationship b/w 'Survived'
#and Pclass by adding the new title variable to the 
#data set and then explore a potential 3d relationship

#create a utility func to help with title extraction
extracTitle <- function(name){
  name <- as.character(name)
  
  if(length(grep("Miss.", name))>0){
    return ("Miss.")
  }
  else if(length(grep("Master", name))>0){
    return ("Master")
  }else if(length(grep("Mrs.", name))>0){
    return ("Mrs.")
  }else if(length(grep("Mr.", name))>0){
    return ("Mr.")
  }else{
    return ("Other")
  }
  
}
#grep is a pattern matching function which finds the arg passed patter

titles <- NULL
for(i in 1:nrow(data.combined)){
  titles <- c(titles, extracTitle(data.combined[i, "Name"]))
}
#c is for combining, whatever returns is added to titles
#first arg is the array and second is wahtever is being returned by the function on 
#passing the names
data.combined$titles <- as.factor(titles)
#new column to data.combined is added as factor datatype

ggplot(data.combined[1:891,], aes(x = titles, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill = "Survived")

