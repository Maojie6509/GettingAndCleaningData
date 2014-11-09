
###############################
## Be sure to set your working directory  and give the correct path when reading them into memory

##load the x and y files for Test and Train
xTrain = read.table("X_train.txt", sep="")
yTrain = read.table("y_train.txt", sep="")
xTest = read.table( "X_test.txt", sep="")
yTest = read.table( "y_test.txt", sep="")

## load  the feature and label files
reNames = read.table("features.txt", sep=""); reNames = reNames[,2];
activityLabels = read.table("activity_labels.txt", sep="");

##Rename the columns
names(xTrain) <- reNames
names(xTest) <- reNames

## Merge Test and Train
xMatrix = rbind(xTest,xTrain)
yVector = rbind(yTest,yTrain) 

##corresponding 
whichMeans = grep("[m]ean", reNames)
xMatrixReduced = xMatrix[,whichMeans]

## write 
write.csv(xMatrixReduced,file="xMatrixReduced.csv")

yVectorLabels=vector(mode="character", length=length(yVector))

##each activity label put in a factor variable with the name of the activity
for(a in 1:6){
  inds = which(yVector==a)
  yVectorLabels[inds] <- as.character(activityLabels[a,2])
}
# #loading subject and renaming IDs
subjectTrain = read.table( "subject_train.txt", sep="")
subjectTest = read.table("subject_test.txt", sep="")

##create a function to add the word "Subject" to the number ID and every element
subjectify = function(x){return(paste("Subject", as.character(x), sep=""))}
subjectNamesTest= sapply(subjectTest, FUN=subjectify)
subjectNamesTrain= sapply(subjectTrain, FUN=subjectify)

##merge
subjectNamesVector = rbind(subjectNamesTrain, subjectNamesTest)

##creating a new dataset that has the means of each variable for each subject
newData = matrix(ncol=length(names(xMatrixReduced)), nrow=length(unique(subjectNamesVector)))
rownames(newData) = unique(subjectNamesVector); 
colnames(newData) = names(xMatrixReduced);

##calculate the mean of all variables for each subject 
for(s in unique(subjectNamesVector)){
    w = which(subjectNamesVector == s)
    cm = colMeans(xMatrixReduced[w,])
    newData[s,] = cm
}
##write 
 write.table(newData,file="newDataset.txt",row.name=FALSE, sep="")
