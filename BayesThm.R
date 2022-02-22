bayesthm <- function(pos_rate = .5, true_pos_rate = .5, true_neg_rate = .5, number=1000){
  positive <- number * pos_rate
  negative <- number * (1 - pos_rate)
  true_pos_test <- positive * true_pos_rate
  false_neg_test <- positive * (1 - true_pos_rate)
  true_neg_test <- negative * true_neg_rate
  false_pos_test <- negative * (1 - true_neg_rate)
  posterior_pos <- true_pos_test / (true_pos_test + false_pos_test)
  posterior_neg <- true_neg_test / (true_neg_test + false_neg_test)
  
  print(paste("The total number tested is", number))
  print(paste("The total number positive is", round(positive)))
  print(paste("Of which", round(true_pos_test), "correctly test positive,"))
  print(paste("and", round(false_neg_test), "incorrectly test negative."))
  print(paste("The total number negative is", round(negative)))
  print(paste("Of which", round(true_neg_test), "correctly test positive,"))
  print(paste("and", round(false_pos_test), "incorrectly test positive."))
  print(paste("Given a positive test, the probability that the patient truly is positive is", round(posterior_pos,3)))
  print(paste("Given a negative test, the probability that the patient truly is negative is", round(posterior_neg,3)))
}

### Using Tables

bayesthm2 <- function(pos_rate = .5, true_pos_rate = .5, true_neg_rate = .5, number=1000){
  positive <- round(number * pos_rate)
  negative <- round(number * (1 - pos_rate))
  true_pos_test <- round(positive * true_pos_rate)
  false_neg_test <- positive - true_pos_test
  true_neg_test <- round(negative * true_neg_rate)
  false_pos_test <- negative - true_neg_test
  posterior_pos <- true_pos_test / (true_pos_test + false_pos_test)
  posterior_neg <- true_neg_test / (true_neg_test + false_neg_test)
  
  true_status <- c(rep("Positive", positive), rep("Negative", negative))
  test_status <- c(rep("Positive test", true_pos_test),
                   rep("Negative test", false_neg_test),
                   rep("Positive test", false_pos_test),
                   rep("Negative test", true_neg_test)
                   )
  expected_df <- data.frame(true_status,test_status)
  expected_table <- table(expected_df)
  print(expected_table)
  prop.table(expected_table, margin = 2)
}

### Probabilistic Version

pos_rate = 0.01
true_pos_rate = 0.631
true_neg_rate = 0.991
number=10000
true_status <- character(number)
test_status <- character(number)

for(i in 1:number){
  true_status[i] <- sample(c("Positive", "Negative"), 1, prob = c(pos_rate, 1-pos_rate))
  if(true_status[i] == "Positive"){
    test_status[i] <- sample(c("Positive test", "Negative test"), 1, prob = c(true_pos_rate, 1-true_pos_rate))
  }
  else{
    test_status[i] <- sample(c("Negative test", "Positive test"), 1, prob = c(true_neg_rate, 1-true_neg_rate))
  }
}

expected_df <- data.frame(true_status,test_status)
expected_table <- table(expected_df)
print(expected_table)
prop.table(expected_table, margin = 2)

bayesthm3 <- function(pos_rate = 0.01,
                      true_pos_rate = 0.631,
                      true_neg_rate = 0.991,
                      number=10000
                      ){
true_status <- character(number)
test_status <- character(number)

for(i in 1:number){
  true_status[i] <- sample(c("Positive", "Negative"), 1, prob = c(pos_rate, 1-pos_rate))
  if(true_status[i] == "Positive"){
    test_status[i] <- sample(c("Positive test", "Negative test"), 1, prob = c(true_pos_rate, 1-true_pos_rate))
  }
  else{
    test_status[i] <- sample(c("Negative test", "Positive test"), 1, prob = c(true_neg_rate, 1-true_neg_rate))
  }
}

expected_df <- data.frame(true_status,test_status)
expected_table <- table(expected_df)
print(expected_table)
prop.table(expected_table, margin = 2)
}
