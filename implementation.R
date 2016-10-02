setwd("E:\\Codes\\hackerearthCodes")

line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
strng <- unlist(strsplit(t[[2]],""))
hacker <- c("h","a","c","k","e","r","e","a","r","t","h")
strngs <- strng %in% hacker
string2 <- strng[strngs]
u <- sapply(letters, function(x) x<-sum(x==string2))
v <- sapply(letters, function(x) x<-sum(x==hacker))
count <- 0
w <- u-v
while(sum(w<0)<1){
  count <- count +1
  w <- w-v
}
cat(count)


#Pshycic
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")

boolzero <- grepl("000000", t[[1]])
boolone <- grepl("111111", t[[1]])

if(boolone == TRUE | boolzero == TRUE){
  cat("Sorry, sorry!")
}else{
  cat("Good luck!")
}


#Final Destination
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
strng<- unlist(strsplit(t[[1]], ""))
lef <- (sum(strng == "L")*(-1) + sum(strng == "R"))
upp <- (sum(strng == "D")*(-1) + sum(strng == "U"))
cat(paste0(lef, " ", upp))

#String Sum
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
strng<- unlist(strsplit(t[[1]], ""))
u <- sapply(letters, function(x) x<-sum(x==strng))
cat(sum(u*c(1:26)))

#Count Occurances
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
strng<- as.numeric(unlist(strsplit(t[[1]], "")))
num <- c(0:9)
u <- sapply(num, function(x) x<-sum(x==strng))
for(i in 1:length(num)){
  cat(paste0(num[[i]]," ",u[[i]]),"\n")
}


#Acronym
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
nlines_dict <- as.numeric(t[[1]])
nlines_sent <- as.numeric(t[[nlines_dict+2]])

WordVecDict <- t[c(2:(nlines_dict+1))]
WordVecSent <- t[c((nlines_dict+3):(nlines_dict+2+nlines_sent))]
WordVecMain <- WordVecSent[!(WordVecSent %in% WordVecDict)]

cat(paste0(toupper(lapply(WordVecMain, function(x) substr(x,1,1) )), collapse = "."))


#Palindromic Ciphers
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
ncases <- as.numeric(t[[1]])
for(i in 2:(ncases+1)){
  u <- t[[i]]
  revu <- paste(rev(substring(u,1:nchar(u),1:nchar(u))),collapse="")
  if(u == revu){
    cat("Palindrome","\n")
  }else{
    u <- unlist(strsplit(u,""))
    WordCount <- sapply(letters, function(x) x<-sum(x==u))
    WordCount <- c(1:26) ^ WordCount
    cat((prod(WordCount)%% 1e13),"\n")
  }
}

#Illegible String
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
max_string <- gsub("w","vv", t[[2]])
min_string <- gsub("vv","w", max_string)
cat(paste0(nchar(min_string)," ",nchar(max_string)))

#The art of verification
#link: https://www.hackerearth.com/practice/basic-programming/implementation
#/basics-of-implementation/practice-problems/algorithm/the-art-of-verification/
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")

lis <- list()

lis[1] <- substr(t[[1]],
                 gregexpr(pattern ='username',t[[1]])[[1]],
                 (gregexpr(pattern ='pwd',t[[1]])[[1]])-2 )

lis[2] <- substr(t[[1]],
                 gregexpr(pattern ='pwd',t[[1]])[[1]],
                 (gregexpr(pattern ='profile',t[[1]])[[1]]) -2)

lis[3] <- substr(t[[1]],
                 gregexpr(pattern ='profile',t[[1]])[[1]],
                 (gregexpr(pattern ='role',t[[1]])[[1]])-2)
lis[4] <- substr(t[[1]],
                 gregexpr(pattern ='role',t[[1]])[[1]],
                 (gregexpr(pattern ='key',t[[1]])[[1]])-2)

lis[5] <- substr(t[[1]],
                 gregexpr(pattern ='key',t[[1]])[[1]],
                 nchar(t[[1]]))

strng <- sub("=",": ", lis)
for(i in 1:length(strng)){
  cat(strng[[i]],"\n")
}


#Car Names
#Link: https://www.hackerearth.com/practice/basic-programming/implementation
#/basics-of-implementation/practice-problems/algorithm/car-names-4/

line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
nlines <- as.numeric(t[[1]])
for(i in 2:(nlines+1)){
  strng <- unlist(strsplit(t[[i]],""))
  if(length(unique(strng)) !=3){
    cat("Not OK", "\n")
  }else{
    #Check for consecutiveness
    fac <- sum(sapply(letters, function(x) x<-sum(x==strng)))
    if(fac%%3 !=0){
      cat("Not OK", "\n")
    }else{
      facMain <- (fac/3)
      subStrng1 <- strng[1:facMain]
      subStrng2 <- strng[(facMain+1):(2*facMain)]
      subStrng2 <- strng[(2*facMain+1):(3*facMain)]
      if(length(unique(subStrng1)) == 1 & length(unique(subStrng2)) == 1 & length(unique(subStrng2)) == 1 ){
        cat("OK","\n")
      }else{
        cat("Not OK", "\n")
      }
    }
  }  
}


#What is the string made of?
line <- readLines('sample.txt', warn=FALSE)
t <- scan(text =line , what = " ")
strng<- as.numeric(unlist(strsplit(t[[1]], "")))
nums <- c(0:9)
counts <- sapply(nums, function(x) x <- sum(x == strng))
wts <- c(6,2,5,5,4,5,6,3,7,6)
counts <- counts * wts
cat(sum(counts))


#Fun at Cake Shop
TotPans <- function(x){
  y <- (x* (x+1)*(x+2))/3
  return(y)
}
line <- readLines('sample.txt', warn=FALSE)
t <- as.numeric(scan(text =line , what = " "))
n <- t[[1]]
i <- 0
while(n >0){
  i <- i+1
  n <- n - (i+i^2)
}
PansRem <- t[[1]] - TotPans(i-1)
if((PansRem - i)<=0){
  cat("Darshak")
}else{
  cat("Chandan")
}


#Complete String
line <- readLines('sample.txt', warn=FALSE)
t <- scan(text =line , what = " ")
nlines <- as.numeric(t[[1]])

for(i in 2:(nlines+1)){
  strng <- unlist(strsplit(t[[i]],""))
  if(length(unique(strng)) == 26){
    cat("YES","\n")
  }else{
    cat("NO","\n")
  }
}


#Recursive Function
fxy <- function(x,y){
  print(paste0(x," ",y))
  if(x==0){
    print("hereonce")
    return(y+1)
  }
  else if(x >0 & y==0){
    print("hereaswell")
    fxy(x-1,1)
  }
  else{
    y2 <- fxy(x,y-1)
    fxy(x-1, y2)
  }
}

line <- readLines('sample.txt', warn=FALSE)
t <- as.numeric(scan(text =line , what = " "))
fxy(t[[1]],t[[2]])










