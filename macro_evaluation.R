# accuracy metrics

mymetrics=function(actual,predicted){
  # confusion matric
  cm = as.matrix(table(Actual = actual, Predicted = predicted))
  # Basic variables
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  # accuracy
  accuracy = sum(diag) / n 
  
  # per class metrics
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  perclass=data.frame(precision, recall, f1)
  # Macro averaged
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  macros=data.frame(macroPrecision, macroRecall, macroF1)
  
  liss=list(accuracy,perclass,macros)
  names(liss)<-c("Accuracy","Per class metrics","Macro averaged metrics")
  liss
}



mysummaryFun=function(data, lev = NULL, model = NULL){
  # confusion metric
  cm<- as.matrix(table(data$obs,data$pred))
  # Basic variables
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  
  # per class metrics
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 

  # Macro averaged
  out=mean(f1)
  c(Macro_F1_score = out)
}
