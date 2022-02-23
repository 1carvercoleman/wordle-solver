# This function returns the result (black, green, or yellow for each letter) 
# given a guess and an answer 
get_result <- function(g, a) {
  r <- "bbbbb"
  for (i in 1:5) {
    if (substr(g, i, i) == substr(a, i, i)) {
      substr(r, i, i) <- "g"
    }
  }
  
  unique_chars_g <- unique(strsplit(g, "")[[1]])
  for(i in 1:length(unique_chars_g)) {
    occurances_in_a <- unlist(gregexpr(unique_chars_g[i], a))
    occurances_in_g <- unlist(gregexpr(unique_chars_g[i], g))
    remove_g <- occurances_in_g %in% occurances_in_a
    remove_a <- occurances_in_a %in% occurances_in_g
    g_new <- occurances_in_g[!remove_g]
    a_new <- occurances_in_a[!remove_a]
    if (length(a_new) != 0 && length(g_new) != 0) {
      if (a_new[1] != -1) {
        for (j in 1:length(a_new)) {
          substr(r, g_new[j], g_new[j]) <- "y"
          if (length(g_new) == j) {
            break
          }
        }
      }
    }
  }
  return(r)
}

# This function removes all words that wouldn't return the same result that 
# the provided guess returned
subset_data <- function(g, r, mydata) {
  rs <- as.vector(sapply(mydata[,1], get_result, g = g))
  mydata <- mydata[rs == r,]
  return(mydata)
}

# This function is the main solver. Given a subsetted dataset, it returns a 
# vector of "entropies" for each possible guess. The entropy is a measure
# of how well the guess partitions the sample space.
solver <- function(mydata) {
  entropy <- c()
  for (i in 1:nrow(mydata)) {
    entropy[i] <- 0
    guess_test <- mydata[i,1]
    for (j in 1:nrow(mydata)) {
      if (i != j) {
        answer_test <- mydata[j,1]
        data_test <- subset_data(guess_test, get_result(guess_test, answer_test), mydata)
        entropy[i] <- entropy[i] + nrow(data_test)
      }
    }
  }
  return(entropy)
}

# Test a random sample of n answers
n <- 3
words <- read.table("possible_words.txt")
for (i in 1:5) {
  words[,i+1] <- substr(words[,1], i, i)
}
test_of_words <- words[sample(1:nrow(words), n),1]

runner <- c()
for (k in 1:length(test_of_words)) {
  counter <- 1
  words <- read.table("possible_words.txt")
  for (i in 1:5) {
    words[,i+1] <- substr(words[,1], i, i)
  }
  first_guess <- "share"

  while(TRUE) {
    result <- get_result(first_guess, test_of_words[k])
    if (result == "ggggg") {
      break
    }
    words <- subset_data(first_guess, result, words)
    entropy <- solver(words)
    first_guess <- words[,1][which.min(entropy)]
    counter <- counter + 1
    if (first_guess == test_of_words[k]) {
      break
    }
  }
  print(counter)
  runner[k] <- counter
}

hist(runner)

