decision_tree <- function(dataset, threshold = 0) {
  root <- Node$new("myroot")
  root$data <- dataset
  root$candidate <- setdiff(colnames(dataset), "label")
  leaf_list <- c(root)
  while (length(leaf_list) != 0) {
    node <- leaf_list[[1]]
    # check the impurity in this node
    node_impurity <- gini_index(node$data$label)
    if (node_impurity > threshold) {
      if (length(node$candidate) == 0) {
        count <- table(node$data[, "label"])
        node$name <- paste0(names(count)[which.max(count)], " (impure)")
      } else {
        impurity_list <- rep(Inf, length(node$candidate))
        formula_list <- rep("", length(node$candidate))
        value_list <- rep(-1, length(node$candidate))
        # find correct formula for this node
        for (k in 1:length(node$candidate)) {
          var <- node$candidate[[k]]
          if (class(node$data[, var]) != "factor") {
            min_val <- Inf
            min_position <- -1
            #values <- sort(unique(node$data[, var]))
            values <- quantile(node$data[, var])
            # try all possible formulas
            for (j in 1:(length(values)-1)) { 
              val <- values[[j]]
              nleft <- length(node$data[node$data[, var] <= val, "label"])
              nright <- length(node$data[node$data[, var] > val, "label"])
              impurity_left <- gini_index(node$data[node$data[, var] <= val, "label"])
              impurity_right <- gini_index(node$data[node$data[, var] > val, "label"])
              impurity_new <- (nleft / nrow(node$data)) * impurity_left +  
                (nright / nrow(node$data)) * impurity_right
              if (impurity_new < min_val) {
                min_val <- impurity_new
                min_position <- j
                value_list[[k]] <- val
              }
            }
            formula_list[[k]] <- paste0(var, "<=", round(value_list[[k]], 4))
            impurity_list[[k]] <- min_val
          } else {
            formula <- var
            impurity <- 0
            for (level in levels(node$data[, var])) {
              impurity <- impurity + gini_index(node$data[node$data[, var] == level, "label"])
            }
            impurity_list[[k]] <- impurity
            formula_list[[k]] <- var
          }
        }
        # determine which variable to split using impurity found for each level
        best_impurity <- min(impurity_list)
        best_position <- which(impurity_list == best_impurity)[1]
        best_var <- node$candidate[[best_position]]
        best_formula <- formula_list[[best_position]]
        node$name <- best_formula
        node$variable <- best_var
        node$value <- value_list[[best_position]]
        if (class(node$data[, best_var]) != "factor") {
          left <- node$AddChild("left")
          left$data <- node$data[node$data[, best_var] <= value_list[[best_position]], ]
          left$candidate <- node$candidate#setdiff(node$candidate, best_var)
          leaf_list <- c(leaf_list, left)
          
          right <- node$AddChild("right")
          right$data <- node$data[node$data[, best_var] > value_list[[best_position]], ]
          right$candidate <- node$candidate#setdiff(node$candidate, best_var)
          leaf_list <- c(leaf_list, right)  
        } else {
          for (level in levels(node$data[,best_var])) {
            newChild <- node$AddChild(paste0("newChild", level))
            newChild$data <- node$data[node$data[, best_var] == level, ]
            newChild$candidate <- node$candidate#setdiff(node$candidate, best_var)
            leaf_list <- c(leaf_list, newChild)
          }
        }  
      }
    } else {
      node$name <- node$data[1, "label"]
    }
    if (length(leaf_list) >= 2) {
      leaf_list <- leaf_list[2:length(leaf_list)]
    } else {
      leaf_list <- c()
    }
  }
  root
}

gini_index <- function(values) {
  1 - sum((table(values) / length(values))^2)
}

entropy <- function(values) {
  prob <- table(values) / length(values)
  -sum(prob * log(prob))
}

tree.classify <- function(dataset, root) {
  predict_label <- rep(-1, nrow(dataset))
  for (i in 1:nrow(dataset)) {
    predict_label[[i]] <- tree.classify.row(dataset[i, ], root)
  }
  predict_label
}

tree.classify.row <- function(data_row, root) {
  # traverse the tree until we got the leaf
  node <- root
  while (node$isLeaf != TRUE) {
    var <- node$variable
    if (class(data_row[[var]]) != "factor") {
      if (data_row[[var]] <= node$value) {
        node <- node$children[[1]]
      } else {
        node <- node$children[[2]]
      }
    } else {
      for (j in 1:length(node$children)) {
        if (node$children[[j]]$data[1, var] == data_row[[var]]) {
          node <- node$children[[j]]
          break
        }
      }
    }
  }
  node$data[1, "label"]
}


adaboost<- function(train_data,test_data,bootstrap_sample_size,number_of_classifiers) {
  
  weights=numeric(nrow(train_data))
  
  #initializing weights to 1/n, where n is the number of rows in train_data
  for(i in 1:length(weights)){
    weights[i]=1.0/nrow(train_data)
  }
  
  
  classifier <- list()
  classifier_importance=numeric(number_of_classifiers)
  
  #initialzing variable for controlling while loop
  x=0
  
  l=1
  
  while(x==0){
    #initializing error to zero
    error=0.0
    temp_sum=0.0
    
    
    #randomly selecting a sample of records from train_data
    #the size of this set using the argument boostrap_sample_size
    index=sample.int(nrow(train_data),size=bootstrap_sample_size,replace=FALSE)
    bootstrap_sample <- data.frame(train_data[index,])
    
    #building a decision tree using bootstrap sampe records
    classifier[[l]] <- decision_tree(bootstrap_sample)
    
    #test classifier[l] with all the records in train_data
    train_predicted_label <- (as.integer(tree.classify(train_data, classifier[[l]])))
    
    #compare the predicted labels with true labels and compute the accuracy
    true_train_label <- as.integer(train_data$label)
    
    #computing the error based using the weights of misclassified weights
    for(m in 1:length(train_predicted_label)){
      if(train_predicted_label[m] != true_train_label[m]){
        temp_sum = temp_sum + weights[m]
      }
    }
    
    error=temp_sum/sum(weights)
    
    #if error is greater than 0.50 restart from the begining
    if(error > 0.50){
      #reintializing weights to 1/n
      for(i in 1:length(weights)){
        weights[i]=1.0/nrow(train_data)
      }
      
      #resetting l
      l=0
      
      next
      
    }
    
    classifier_importance[l]=0.5*(log10((1-error)/error))
    
    alpha=classifier_importance[l]
    
    #updating the weights
    for(i in 1:length(weights)){
      weights[i]=weights[i]*(exp(-1*(alpha*train_predicted_label[i]*true_train_label[i])))
    }
    
    sum_of_new_weights=sum(weights)
    #re-normalizing the wieghts
    for(i in 1:length(weights)){
      weights[i]=weights[i]/sum_of_new_weights
    }
    #check if l==number_of_classifiers to break the loop
    if(l==number_of_classifiers){
      #setting x=1 will end the loop
      x=1
    }
    else{
      l=l+1
    }
  }
  
  #predicted label for this iteration or result of adaboost on test_data
  result=numeric(nrow(test_data))
  
  
  #predict labels for test data using the classifier
  for(m in 1:nrow(test_data)){
    temp_res=numeric(length(classifier))
    test_record = test_data[m,]
    probability_zero=0.0
    probability_one=0.0
    
    #predicting the label for test_record by passing to every weak classifier in the list classifier
    for(n in 1:length(classifier)){
      temp_res[n]=(as.integer(tree.classify.row(test_record, classifier[[n]])))
    }
    
    #classify test_record
    for(i in 1:length(temp_res)){
      if(temp_res[i]==0){
        probability_zero=probability_zero+classifier_importance[i]
      }
      else{
        probability_one=probability_one+classifier_importance[i]
      }
    }
    
    #predict the class of test_recod based probability_one and probability_zero
    if(probability_one > probability_zero){
      result[m]=1
    }
    else{
      result[m]=0
    }
    
     
  }
  
  
  return(result)
  
  
}