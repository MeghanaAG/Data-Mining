naive_bayes <- function(train_data,test_data){
  #first dividing the train_data records based on labels, separate records labelled 0 and 1
  train_data_label_one <- train_data[train_data$label==1,]
  train_data_label_zero <- train_data[train_data$label==0,]
  
  #considering all columns except the last one with labels
  train_data_label_one <- train_data_label_one[,1:ncol(train_data_label_one)-1]
  train_data_label_zero <- train_data_label_zero[,1:ncol(train_data_label_zero)-1]
  
  print("number of columns")
  print(ncol(train_data_label_one))
  
  prior_probability_one = nrow(train_data_label_one)/nrow(train_data)
  prior_probabality_zero = nrow(train_data_label_zero)/nrow(train_data)
  
  
  #print("prior probabilities")
  #print(prior_probability_one)
  #print(prior_probabality_zero)
  
  #caluculating the mean and standard deviation of every attribute (column ) in train_data_label_one and zero
  prob_dist_1 <- list()
  prob_dist_0 <- list()
  for (i in 1:ncol(train_data_label_one)) {
    # check the data type of the column
    if (class(train_data_label_one[, i]) == 'factor') {
      prob_dist_1[[i]] <- table(train_data_label_one[, i])
    } else {
      prob_dist_1[[i]] <- c(mean(train_data_label_one[,i]), sd(train_data_label_one[,i]))
    }
  }
  
  for (i in 1:ncol(train_data_label_zero)) {
    # check the data type of the column
    if (class(train_data_label_zero[, i]) == 'factor') {
      prob_dist_0[[i]] <- table(train_data_label_zero[, i])
    } else {
      prob_dist_0[[i]] <- c(mean(train_data_label_zero[,i]), sd(train_data_label_zero[,i]))
    }
  }
  
  

  
  
  predicted_label=numeric()
  
  #loop through every record in test_data and predict label
  #prediction is done based on pdf of gaussian using the mean and standard deviation computed on training data
  for(i in 1:nrow(test_data)){
    probability_one = numeric((ncol(test_data)-1))
    probability_zero = numeric((ncol(test_data)-1))
    final_probability_one=0.0
    final_probability_zero=0.0

    
    record <- test_data[i,]
    for(j in 1:(ncol(test_data)-1)){
      if (class(train_data[, j]) == 'factor') {
        probability_one[j] <- prob_dist_1[[j]][which(names(prob_dist_1[[j]]) == record[[j]])] / 
                                  sum(prob_dist_1[[j]])
        probability_zero[j] <- prob_dist_0[[j]][which(names(prob_dist_0[[j]]) == record[[j]])] / 
          sum(prob_dist_0[[j]])
      } else {
        probability_one[j] = pdf_gaussian(prob_dist_1[[j]][1],prob_dist_1[[j]][2],record[[j]])
        probability_zero[j]= pdf_gaussian(prob_dist_0[[j]][1],prob_dist_0[[j]][2],record[[j]])
      }
    }
    
    #print("printing probabilities")
    #print(probability_one)
    #print(probability_zero)

    
    #computing probability based on counts
    final_probability_one = prior_probability_one * (prod(probability_one))
    final_probability_zero = prior_probabality_zero * (prod(probability_zero))
    
    
    
    if(final_probability_one > final_probability_zero){
      predicted_label[i]=1
    }
    else{
      predicted_label[i]=0
    }
    
  }
  
  #print(predicted_label)
  return(predicted_label)
  
}

pdf_gaussian <- function(mean,standard_dev,x){
  probability = (1/sqrt(2*(standard_dev)^2*pi))*(exp(-1*((x-mean)^2)/(2*(standard_dev)^2)))
  return(probability)
  
}

