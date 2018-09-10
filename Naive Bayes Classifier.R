# To read in data from the directories:
# Partially based on code from C. Shalizi
rm(list=ls())
read.directory <- function(dirname) {
  # Store the emails in a list
  emails = list();
  # Get a list of filenames in the directory
  filenames = dir(dirname,full.names=TRUE);
  for (i in 1:length(filenames)){
    emails[[i]] = scan(filenames[i],what="",quiet=TRUE);
  }
  return(emails)
}
ham.test <-read.directory("~/Downloads/kag-ham-test")
spam.test <-read.directory("~/Downloads/kag-spam-test")
ham.train <-read.directory("~/Downloads/kag-ham-train")
spam.train <-read.directory("~/Downloads/kag-spam-train")

length(ham.test)
length(spam.test)
length(ham.train)
length(spam.train)
ham.test[1]

make.sorted.dictionary.df <- function(emails){
  # This returns a dataframe that is sorted by the number of times 
  # a word appears
  
  # List of vectors to one big vetor
  dictionary.full <- unlist(emails) 
  # Tabulates the full dictionary
  tabulate.dic <- tabulate(factor(dictionary.full)) 
  # Find unique values
  dictionary <- unique(dictionary.full) 
  # Sort them alphabetically
  dictionary <- sort(dictionary)
  dictionary.df <- data.frame(word = dictionary, count = tabulate.dic)
  sort.dictionary.df <- dictionary.df[order(dictionary.df$count,decreasing=TRUE),];
  return(sort.dictionary.df)
}

all.emails <- c(ham.test, spam.test, ham.train, spam.train)
dictionary <- make.sorted.dictionary.df(all.emails)

dim(dictionary)
head(dictionary)

make.document.term.matrix <- function(emails,dictionary){
  # This takes the email and dictionary objects from above and outputs a 
  # document term matrix
  num.emails <- length(emails);
  num.words <- length(dictionary);
  # Instantiate a matrix where rows are documents and columns are words
  dtm <- mat.or.vec(num.emails,num.words); # A matrix filled with zeros
  for (i in 1:num.emails){
    num.words.email <- length(emails[[i]]);
    email.temp <- emails[[i]];
    for (j in 1:num.words.email){
      ind <- which(dictionary == email.temp[j]);
      dtm[i,ind] <- dtm[i,ind] + 1;
    }
  }
  return(dtm);
}
dtm.ham.train <- make.document.term.matrix(ham.train,dictionary$word)
dtm.ham.test <- make.document.term.matrix(ham.test,dictionary$word)
dtm.spam.train <- make.document.term.matrix(spam.train,dictionary$word)
dtm.spam.test <- make.document.term.matrix(spam.test,dictionary$word)

make.log.pvec <- function(dtm,mu){
  # Sum up the number of instances per word
  pvec.no.mu <- colSums(dtm)
  # Sum up number of words
  n.words <- sum(pvec.no.mu)
  # Get dictionary size
  dic.len <- length(pvec.no.mu)
  # Incorporate mu and normalize
  log.pvec <- log(pvec.no.mu + mu) - log(mu*dic.len + n.words)
  return(log.pvec)
}

#Question1
naive.bayes<-function(log.pvec.spam,log.pvec.ham,log.prior.spam,log.prior.ham,dtm.test){
  bspam.test <- vector()
  for (i in 1:nrow(dtm.test)){
    log.p.spam <- log.prior.spam + sum(dtm.test[i,]*log.pvec.spam)
    log.p.ham <- log.prior.ham + sum(dtm.test[i,]*log.pvec.ham)
    bspam.test <- c(bspam.test,ifelse((log.p.spam-log.p.ham)>0,1,0))
  }
  return(bspam.test)
}

#Question2
index<-dictionary[1:10,1]
dic.ham <- make.sorted.dictionary.df(ham.train)
dic.spam <- make.sorted.dictionary.df(spam.train)
dic.ham[dic.ham$word %in% index,]
dic.spam[dic.spam$word %in% index,]

#Question3
D <- nrow(dictionary)
mu <- 1/abs(D)
log.pvec.spam.train <- make.log.pvec(dtm.spam.train,mu)
log.pvec.ham.train <- make.log.pvec(dtm.ham.train,mu)
prior.spam<- length(spam.train)/(length(ham.train)+length(spam.train))
prior.ham<- length(ham.train)/(length(ham.train)+length(spam.train))
log.prior.spam <- log(prior.spam)
log.prior.ham <-log(prior.ham)


dtm.test<-rbind(dtm.ham.test,dtm.spam.test)
pred <-naive.bayes(log.pvec.spam.train,log.pvec.ham.train,log.prior.spam,log.prior.ham, dtm.test)
real<-rep(c(0,1),each=150)
sum(real==pred)/300
table(real,pred)
