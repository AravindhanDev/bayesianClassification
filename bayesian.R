PERSON_DATASET <- data.frame(RID = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
AGE = c('Y', 'Y', 'M', 'S', 'S', 'S', 'M', 'Y', 'Y', 'S', 'Y', 'M', 'M', 'S'),
INCOME = c('HIGH', 'HIGH', 'HIGH', 'MEDIUM', 'LOW', 'LOW', 'LOW', 'MEDIUM', 'LOW', 'MEDIUM', 'MEDIUM', 'MEDIUM', 'HIGH', 'MEDIUM'),
EMPLOYMENT_TYPE = c('SALARIED', 'SALARIED', 'SALARIED', 'SALARIED', 'CONSULTANT', 'CONSULTANT', 'CONSULTANT', 'SALARIED', 'CONSULTANT', 'CONSULTANT', 'CONSULTANT', 'SALARIED', 'CONSULTANT', 'SALARIED'),
LOAN_IN_BANK_GT_5L = c('YES', 'NO', 'YES', 'YES', 'YES', 'NO', 'NO', 'YES', 'YES', 'YES', 'NO', 'NO', 'YES', 'NO'),
CLASS_BUYS_CAR = c('NO', 'NO', 'YES', 'YES', 'YES', 'NO', 'YES', 'NO', 'YES', 'YES', 'YES', 'YES', 'YES', 'NO')
)

n <- length(PERSON_DATASET$RID)

prob <- function(key1, value1, value2) {
    key_to_find <- names(PERSON_DATASET)[names(PERSON_DATASET) == key1]
    key1List <- PERSON_DATASET[[key_to_find]]
    key2List <- PERSON_DATASET$CLASS_BUYS_CAR 
    keyProb <- 0
    for (i in 1:n) {
      if (key1List[i] == value1 & key2List[i] == value2) {
        keyProb <- keyProb + 1
      }
    }
    return (keyProb / length(key2List[key2List == value2]))
}

bayesianClassification <- function(myTuple) {
  yesCount <- 0
  noCount <- 0
  yesPblt <- 0
  noPblt <- 0
  
  for (i in PERSON_DATASET$CLASS_BUYS_CAR) {
    if (i == 'YES') yesCount <- yesCount + 1 
    if (i == 'NO') noCount <- noCount + 1
  }
  
  yesPblt <- yesCount / n
  noPblt <- noCount / n
  
  yesResult <- 1
  noResult <- 1
  
  for (item in colnames(myTuple)) {
    yesResult <- yesResult * prob(item, myTuple[[item]], "YES")
    noResult <- noResult * prob(item, myTuple[[item]], "NO")
  }
  
  if (yesResult * yesPblt > noResult * noPblt) {
    print("You need to buy car")
  } else {
    print("Don't buy")
  }
}

myTuple <- data.frame(AGE="Y", INCOME="MEDIUM", EMPLOYMENT_TYPE="CONSULTANT", LOAN_IN_BANK_GT_5L="YES")
bayesianClassification(myTuple)
