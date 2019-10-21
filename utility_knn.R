#my_knn

myknn <- function (train_data,test_data,k)
{ 
  #print(nrow(train_data))
  #print(nrow(test_data))
  #predicted labels
  pred_label=numeric()
  #computing distance to all other data points
  for(m in 1:nrow(test_data)){
    new_data_point <- test_data[m,]
  
    distances = numeric()
    for(i in 1:nrow(train_data))
      {
      
      distances = c(distances, sqrt(sum((train_data[i,1:(ncol(train_data)-1)] - new_data_point[1:length(new_data_point)-1]) ^ 2)))
      
    }
    
    sorted = sort(distances,index.return=TRUE)
    sorted_val = sorted$x
    sorted_indices = sorted$ix
    count_ones=0
    count_zeros=0
    #print("distances to nearest neighbors")
    for(j in 1:k){
      #print(sorted_val[j])
      label_of_nearest_pt = train_data[sorted_indices[j],ncol(train_data)]
      if(label_of_nearest_pt == 1)
        count_ones = count_ones + 1
      else
        count_zeros = count_zeros + 1
    }
    
    if( count_ones > count_zeros ){
      new_label = 1
    }
      
    else if(count_zeros > count_ones){
      new_label = 0
    }
    else{
      new_label=sample(0:1,1)
    }
      
    pred_label=c(pred_label,new_label)
    
  }

 return(pred_label)
}
