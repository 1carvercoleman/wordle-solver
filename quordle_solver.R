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

# These are all the possible answers for wordle
words <- read.table("possible_words.txt")
for (i in 1:5) {
  words[,i+1] <- substr(words[,1], i, i)
}

# My first guess is usually share
first_guess <- "share"
result <- c("b", "b", "b", "b")
words <- list(words, words, words, words)
directions <- c("Top Left", "Top Right", "Bottom Left", "Bottom Right")


while(TRUE) {
  
  if (first_guess == "share") {
    writeLines("\n\n\n\n\nInstructions:
      1. Guess share for your first guess
      2. Input the response from Quordle as
          b=black/gray depending on browser
          y=yellow
          g=green
         For example, if the answer were 'shape' in the top left
         quadrant, you would type 'gggbg' in the console after
         'Top left: '
      3. The program will work it out and print a word to guess
      4. Rinse and Repeat!")
  }
  
  for (i in 1:4) {
    if (result[i] != "ggggg") {
      if (nrow(words[[i]]) == 1) {
        result[i] <- get_result(first_guess, words[[i]][1,1])
      } else {
        result[i] <- readline(prompt=paste0(directions[i], ": "))
      }
    }
    
    if (nrow(words[[i]] != 1)) {
      words[[i]] <- subset_data(first_guess, result[i], words[[i]])
    }
  }
  
  possible_guesses <- c()
  for (k in 1:4) {
    if (result[k] == "ggggg" | nrow(words[[k]]) == 1) {
      next
    }
    
    if (first_guess == "share" & result[k] == "bbbbb") {
      possible_guesses[k] <- "unlit"
    } else if (first_guess == "share" & result[k] == "bybbb") {
      possible_guesses[k] <- "touch"
    } else if (first_guess == "share" & result[k] == "bbybb") {
      possible_guesses[k] <- "talon"
    } else if (first_guess == "share" & result[k] == "bbbyb") {
      possible_guesses[k] <- "droit"
    } else if (first_guess == "share" & result[k] == "bbbby") {
      possible_guesses[k] <- "olden"
    } else if (first_guess == "share" & result[k] == "gbbbb") {
      possible_guesses[k] <- "stink"
    } else if (first_guess == "share" & result[k] == "bbbbg") {
      possible_guesses[k] <- "guile"
    } else if (first_guess == "share" & result[k] == "bbbyy") {
      possible_guesses[k] <- "rider"
    } else if (first_guess == "share" & result[k] == "bbyyy") {
      possible_guesses[k] <- "alter"
    } else if (first_guess == "share" & result[k] == "bbyyb") {
      possible_guesses[k] <- "carol"
    } else if (first_guess == "share" & result[k] == "bbyby") {
      possible_guesses[k] <- "penal"
    } else if (first_guess == "share" & result[k] == "bbbyg") {
      possible_guesses[k] <- "trope"
    } else {
      entropy <- solver(words[[k]])
      possible_guesses[k] <- words[[k]][,1][which.min(entropy)]
      #first_guess <- words[,1][entropy == min(entropy)]
    }
  }

  is_answer <- FALSE
  
  for (i in 1:4) {
    if (nrow(words[[i]]) == 1 & result[i] != "ggggg") {
      is_answer <- TRUE
      first_guess <- words[[i]][1,1]
      result[i] <- "ggggg"
      break
    }
  }
  
  if (!is_answer) {
    entropies <- c()
    for (k in 1:4) { # Which guess
      if (result[k] == "ggggg") {
        entropies[k] <- 1000000
        next
      }
      entropy <- 0
      guess_test <- possible_guesses[k]
      for (i in 1:4) { # Which dataset
        if (result[i] == "ggggg") {
          next
        }
        for (j in 1:nrow(words[[i]])) {
          answer_test <- words[[i]][j,1]
          data_test <- subset_data(guess_test, get_result(guess_test, answer_test), words[[i]])
          entropy <- entropy + nrow(data_test)
        }
      }
      entropies[k] <- entropy
    }
    first_guess <- possible_guesses[which.min(entropies)]
  }
  
  if (is.null(first_guess)) {
    break
  }
  print(first_guess)
}
