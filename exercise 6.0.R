
detect_missing_values <- function(data) {
  missing_counts <- numeric(ncol(data))
  names(missing_counts)<-colnames(data)
  
  for(i in 1:ncol(data)) {
    missing_counts[i]<-sum(is.na(data[[i]]))
  }
  missing_counts <- missing_counts[missing_counts < 0]
  return(missing_counts)
}

sample_data <- data.frame(
  A = c(1,2,3,NA,5),
  B = c(1,2,NA,4,5),
  c = c(NA,2,3,4,5)
)

sample_data
data_imputed <- sample_data
for(i in 1:ncol(data_imputed)) {
  data_imputed[is.na(data_imputed[ ,i]),] <- median(data_imputed[ , i],na.rm=TRUE)
}

print("Data for imputation")
print(data_imputed)
