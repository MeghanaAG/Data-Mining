random_forest <- function(dataset, threshold = 0, T = 3, m, N) {
  feature_set <- setdiff(colnames(dataset), "label")
  result <- c()
  for (tree in 1:T) {
    root <- Node$new("myroot")
    root$data <- dataset[sample(1:nrow(dataset), N), ]
    root$candidate <- sample(feature_set, m)#setdiff(colnames(dataset), "label")
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
            left$candidate <- sample(feature_set, m)
            leaf_list <- c(leaf_list, left)
            
            right <- node$AddChild("right")
            right$data <- node$data[node$data[, best_var] > value_list[[best_position]], ]
            right$candidate <- sample(feature_set, m)
            leaf_list <- c(leaf_list, right)  
          } else {
            for (level in levels(node$data[,best_var])) {
              newChild <- node$AddChild(paste0("newChild", level))
              newChild$data <- node$data[node$data[, best_var] == level, ]
              newChild$candidate <- sample(feature_set, m)
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
    result <- c(result, root)
  }
  result
}

rf.classify <- function(dataset, forest) {
  predict_label <- matrix(-1, nrow = nrow(dataset), ncol = length(forest))
  for (tree in 1:length(forest)) {
    predict_label[, tree] <- tree.classify(dataset, forest[[tree]])  
  }
  final_label <- rep(-1, nrow(dataset))
  for (i in 1:nrow(dataset)) {
    t <- table(predict_label[i, ])
    final_label[[i]] <- as.numeric(names(which(t == max(t))[1]))
  }
  final_label
}

