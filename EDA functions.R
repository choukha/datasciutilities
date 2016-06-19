#-------------------------------------------#
# Custom Data Summary Function
#-------------------------------------------#
# Function to See the Data Summary of numeric columns in a Tabular format.
data_summary <- function(dataset)
{
  #First Find the columns which are numeric
  nums <- sapply(dataset, is.numeric)
  numcols <-names(which(nums==T))
  
  means <-colMeans(dataset[,numcols],na.rm=T)
  mins <-apply(dataset[,numcols],2,function(x) min(x,na.rm=T))
  maxs <-apply(dataset[,numcols],2,function(x) max(x,na.rm=T))
  medians <-apply(dataset[,numcols],2,function(x) median(x,na.rm=T))
  quantiles <-apply(dataset[,numcols],2,function(x) quantile(x,na.rm=T))
  datasumm<-cbind(means,medians,mins,maxs,t(quantiles))
  print("Data Summary")
  return(datasumm)
}

#------------------------------------------------------#
# Custom Function for Finding & Analysing Missing data
#------------------------------------------------------#
find_missing <-function(dataset)
{
  #Find How many missing values are there in the given dataset
  totalmissing <-sum(is.na(dataset))
  print('Total missing values')
  print(totalmissing)
  
  #Find How many values are missing in each column which have missing valeus
  colmiss <-apply(dataset,2, function(x) sum(is.na(x)))
  # Note here 2 means columnwise, for row wise put 1
  columnwisemissing <-colmiss[colmiss>0]
  print("Column wise missing values")
  print(columnwisemissing)
  print("How many such columns")
  print(length(columnwisemissing))
  
  #To find the columns with all values missing
  allmisscols <- apply(dataset,2, function(x)all(is.na(x)))
  colswithallmiss <-names(allmisscols[allmisscols>0])
  print("the columns with all values missing")
  print(colswithallmiss) 
  
  # To find the rows having missing values complete.cases() function can be used.
  #The function complete.cases() returns a logical vector indicating which cases are complete.
  
  #the number of complete non missing observations 
  complete_obs <-dataset[complete.cases(dataset),]
  print("the number of complete non missing observations (Complete Cases)") 
  print(nrow(complete_obs))
  }

#-------------------------------------------------------------#
# Custom function to plot Histograms and Bar Charts of columns
#-------------------------------------------------------------#

plot_hist_bars <- function(dataset,bins)
{
  #First Find the columns which are numeric
  nums <- sapply(dataset, is.numeric)
  numcols <-names(which(nums==T))
  
  colsingraph=2
  png("hists.png") 
  par(mfrow=c(4,4))
  
  for( i in 1:length(numcols))
  {
    
    hist(dataset[,numcols[i]], xlab=numcols[i], main=paste("Histogram of ",numcols[i]),breaks=bins,col="pink")
    
    
  }
  dev.off()
  #First Find the columns which are factors or categorical
  cats <- sapply(dataset, is.character)
  catcols <-names(which(cats==T))
  
  for(i in 1:length(catcols))
  {
    png(paste(catcols[i],".png"))
    barplot(table(dataset[,catcols[i]]), xlab=catcols[i], main=paste("Barplot of ",catcols[i]),col="blue")
    dev.off()
  }
  
}

# For Example
plot_hist_bars(df,20)
