##BeeStats project Part 1 - single hive stats
#Stay tuned for the Shiny App and multiple hive stats

#load data
BeeStats <- data.frame(read.csv("BeeStats.csv",sep=",",strip.white=TRUE))
head(BeeStats)
colnames(BeeStats)

#single hive stats
hive_stats <- function() {
  input <- readline("Input date (dd/mm/yy): ")                                  #select data
  date1 <- as.Date(input,format = "%d/%m/%y")
  date2 <- format(date1, format = "X%d.%b.%y")
  if (substr(date2,2,2) == 0) {date2 <- paste(substr(date2,1,1),substr(date2,3,nchar(date2)),sep="")}
  if ((date2 %in% colnames(BeeStats)) == FALSE) stop("Data not available for selected date") #end function if data not available
  hive <- BeeStats[,date2]
  names(hive) <- NULL
  date3 <- format(date1,format = "%B %d, %Y")
  
  empty.rows <- which(hive == "")
  if (all(is.na(hive)==TRUE)) stop("Data not available for selected date")    #end function if data not available
  if (any(hive == "") == TRUE) hive <- hive[-empty.rows]                      #remove empty rows

  num.words <- length(hive)                                                     #calculate number of words
  
  length.words=c()                                                              #calculate word length stats
  for (word in hive) {
    length.word <- nchar(word)
    length.words <- c(length.words,length.word)} 
  mean.length <- mean(length.words)
  ind.max <- c(which(length.words == max(length.words)))
  longest <- paste(hive[ind.max], collapse = ", ")
  
  chars <- c()                                                                  #find unique letters
  for (word in hive) {  
    for (i in 1:nchar(word)) {
      char <- substr(word,i,i)
      chars <- c(chars,char)
      todays.letters <- sort(unique(chars))
    }
  }
  
  start.letters <- c()
  bingo <- FALSE
  for (word in hive) {  
    for (i in 1:nchar(word)) {                                                  #find centre letter and if Bingo
      word.char <- substr(word,i,i)
      start.letters <- c(start.letters,word.char[1])
      if (length(sort(unique(start.letters))) == 7) {bingo <- TRUE} 
      for (letter in todays.letters) {
        ifelse((letter %in% word.char) == FALSE, break, centre <- letter)
      }
    }
  }


  pangrams <-c()                                                                #find pangrams and pangrams data (lengths, perfect)
  length.pangrams <-c()
  perfect <- FALSE
  perfect.pangrams <- c()
  
  for (word in hive) {
    word.letters <- c()
    for (i in 1:nchar(word)) {
      word.letter <- substr(word,i,i)
      word.letters <- c(word.letters, word.letter)}

    count = 0

    for (letter in todays.letters) {
      {if ((letter %in% word.letters) == FALSE) {(next)} else {count = count +1}
      }
      if (count == 7) {
        if (nchar(word)==7) {perfect<-TRUE
        perfect.pangrams <- c(perfect.pangrams,word)}
        pangrams <- c(pangrams,word)
        length.pangrams <-c(length.pangrams,nchar(word))}
    }
  }
  
  cat("Spelling Bee stats for", weekdays(date1), date3,"\n",fill=TRUE)               #print output
  cat("Number of words: ",num.words, (if (bingo == TRUE) {"(Bingo)"}), fill=TRUE)                
  cat("The longest word(s) is/are: ", longest," (",max(length.words), " letters)", sep="",fill=TRUE)
  cat("Average word length: ", round(mean(length.words),digits = 2), "letters",fill=TRUE)
  cat("Todays's letters are: ",todays.letters,fill=TRUE)
  cat("The centre letter is: ", centre,fill=TRUE)
  cat("Today's pangram(s) is/are: ", paste(pangrams,collapse=", "), " (",length(pangrams), " pangram(s), ", length(perfect.pangrams), " perfect)", sep="",fill=TRUE)
  cat("\n Word list for ", weekdays(date1), date3,":", fill=TRUE)
  hive.df <- as.data.frame(hive)
  names(hive.df) <-NULL
  print(hive.df)
  
  todays.data <- c(date2,num.words,bingo,max(length.words),round(mean(length.words),digits=2),centre,length(pangrams),length(perfect.pangrams),todays.letters)
  return(todays.data)
  }
  
hive_stats()


