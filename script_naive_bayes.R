source('utility_naive_bayes.r')

#select dataset 
dataset_name = "project3_dataset4"

#options(stringsAsFactors = TRUE)
dataset = read.csv(file = paste0(dataset_name, ".txt"), header = FALSE, sep = "\t")
colnames(dataset) <- c( paste0("attr", 1:(ncol(dataset)- 1)),"label")


#if the project3_dataset2 is selected then we need to convert the fifth column 
#(original file has values 'Present'and 'Absent') to 0 and 1 
#this is because in Knn we need to find the distance between data points for classifying new points

if(dataset_name=='project3_dataset2'){
  column_num=as.list(sapply(dataset, class))
  column_num=which(column_num=='character')
  column_name=paste0("attr",eval(parse(text="column_num")))
  
  
  for(i in 1:nrow(dataset)){
    if(dataset[[i,eval(parse(text="column_name"))]]=='Present'){
      dataset[[i,eval(parse(text="column_name"))]] <- 1
    }
    else{
      dataset[[i,eval(parse(text="column_name"))]] <- 0
    }
    
  }
  
  dataset= data.frame(lapply(dataset,as.numeric))  
  
}


#dataset=dataset[1:20,]

#k-fold cross validation (implementing 10 - fold cross validation)
#---------------------------------------------------------------------
#computing number of rows in each dataset
#default value of 'k' given for this project is 10
# set the value of 'k' here 

k=10
number_of_rows=round(nrow(dataset)/k)

#dividing the dataset into k parts

previous_last_row=1
for(l in 1:k){
  #if number of rows in dataset is not multiple of k then as we taking number_of_rows=round(nrow(dataset/k))
  #assigning all the remaining rows to last dataset
  if(l==k){
    print("here")
    print(previous_last_row)
    assign(paste("dataset",l,sep=""),data.frame(dataset[(previous_last_row+1):nrow(dataset), ]))
  }
  else if(l==1){
    assign(paste("dataset",l,sep=""),data.frame(dataset[(previous_last_row):(l*number_of_rows), ]))
    previous_last_row = l*number_of_rows
  }
  else{
    assign(paste("dataset",l,sep=""),data.frame(dataset[(previous_last_row+1):(l*number_of_rows), ]))
    previous_last_row = l*number_of_rows
  }
  
}

accuracy_values=numeric()
precision_values=numeric()
recall_values=numeric()
f1_measure_values=numeric()

#looping through the k-datasets to run the KNN algorithm with k-cross validation


for(i in 1:k){
  true_positive=0
  true_negative=0
  false_positive=0
  false_negative=0
  accuracy=0.0
  precision=0.0
  recall=0.0
  f1_measure=0.0
  test_data <- dataset[0,]
  training_data <- dataset[0,]
  
  #select a dataset as test_data 
  
  test_data <- rbind(test_data,eval(as.name(paste("dataset",i,sep=""))))
  
  #selecting all other datasets (excluding dataset selected as test_data) as training_data
  
  for(m in 1:k){
    if(m != i)
      training_data <- rbind(training_data,eval(as.name(paste("dataset",m,sep=""))))
  }
  
  #training data labels
  training_label <- training_data$label
  
  #our naive-bayes implementation
  predicted_label <- naive_bayes(training_data,test_data)
  
  
  true_label <- as.list(test_data$label)
  
  #evaluating the results of the KNN function 
  
  for(j in 1:length(predicted_label)){
    
    if(predicted_label[j]==true_label[j] & predicted_label[j]==1){
      true_positive = true_positive + 1
    }
    else if(predicted_label[j]==true_label[j] & predicted_label[j]==0){
      true_negative = true_negative + 1
    }
    else if(predicted_label[j]!=true_label[j] & predicted_label[j]==0){
      false_negative = false_negative + 1
    }
    else{
      false_positive = false_positive + 1
    }
  }
  print("printing tp,tn,fp,fn")
  print(true_positive)
  print(true_negative)
  print(false_positive)
  print(false_negative)
  #computing accuracy, precision, recall and f1_measure
  accuracy=(true_positive+true_negative)/(true_positive+true_negative+false_positive+false_negative)
  precision=true_positive/(true_positive+false_positive)
  recall=true_positive/(true_positive+false_negative)
  f1_measure=(2*true_positive)/((2*true_positive)+false_negative+false_positive)
  
  #storing the values of this iteration ( accuracy, precision, recall and f1_measure ) 
  accuracy_values=c(accuracy_values,accuracy)
  precision_values=c(precision_values,precision)
  recall_values=c(recall_values,recall)
  f1_measure_values=c(f1_measure_values,f1_measure)
  
  print('===========================')
  print('iteration')
  print(i)
  
  print('Accuracy')
  print(accuracy)
  
  print('Precision')
  print(precision)
  
  print('Recall')
  print(recall)
  
  print('f1_measure')
  print(f1_measure)
  
}



#computing average value of accuracy, precision, recall and f1_measure
a=mean(accuracy_values)
b=mean(precision_values)
c=mean(recall_values)
d=mean(f1_measure_values)
print('-----------------------------')
print("average values")
print("average accuracy")
print(a)

print("average precision")
print(b)

print("average recall")
print(c)

print("average f1_meansure")
print(d)


