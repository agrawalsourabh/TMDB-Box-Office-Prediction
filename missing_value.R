# working with missing values

count_na = function(x){
  na_varname = c()
  na_count = c()
  na_per = c()
  
  for (i in 1:ncol(x)) {
    if(sum(is.na(x[i])) > 0){
      count = sum(is.na(x[i]))
      varname = colnames(x[i])
      per = round(count/nrow(x) * 100, digits = 2)
      
      na_varname = c(na_varname, varname)
      na_count = c(na_count, count)
      na_per = c(na_per, per)
    }
    
  }
  missing_val = data.frame(na_varname, na_count, na_per)
  return(missing_val[order(missing_val$na_count, decreasing = T), ])
}

missing_val = count_na(x = my.data)

missing_val
