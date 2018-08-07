suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
cards <- c("Ace", "Deuce", "Three", "Four","Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")

deck <- matrix(apply(X = as.matrix(expand.grid(cards, suits, stringsAsFactors = F)), 1, paste, sep = "", collapse=" "))

riffle<-function(x,...){
  
  deck_left <- matrix(x[1:(nrow(x)/2),])
  
  deck_right <- matrix(x[((nrow(x)/2)+1):nrow(x),])
  
  matrix(
    apply(X = matrix(c(1:nrow(deck_left))), 
          MARGIN = 1, 
          FUN = function(x){return(rbind(deck_left[x,],deck_right[x,]))})
  )
  
}

setNames(do.call(cbind,Reduce(f = 'riffle', x = c(1:8), init = deck, accumulate = T)),c(NULL))
