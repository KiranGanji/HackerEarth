setwd("E:\\Codes\\hackerearthCodes")

#First Solution
u <- read.table("stdin", header= FALSE)
cat(as.integer(levels(u$V1)[1])*2)
cat("\n")
cat(levels(u$V1)[2])

#Manipulate Strings
u <- suppressWarnings(read.table("sample.txt", header= FALSE))
#u <- read.table("stdin", header= FALSE)
StringReq <- levels(u$V1)[1]

StringReq <- strsplit(StringReq, "")[[1]]

for(i in 1:length(StringReq)){
  if(StringReq[i] %in% letters){
    StringReq[i] <- toupper(StringReq[i])
  }else{
    StringReq[i]<-tolower(StringReq[i])
  }
}

cat(paste(StringReq, collapse=""))

#Palindromic Sequences
ReverseChars <- function(string)
{
  string_split = strsplit(string, split = "")
  reversed_chars = string_split[[1]][nchar(string):1]
  paste(reversed_chars, collapse="")
}

#u <- suppressWarnings(read.table("sample.txt", header= FALSE))
u <- read.table("stdin", header= FALSE)
StringReq <- levels(u$V1)[1]
StringReq2 <- ReverseChars(StringReq)

if(StringReq == StringReq2){
  cat("YES")
}else{
  cat("NO")
}


#Factorial
factorialM <- function(n){
  factorialn <- 1
  if(n == 0){
    return(1)
  }else{
    for(i in 1:n){
      factorialn <- factorialn*i
    }
    return(factorialn)
  }
}

u <- read.table("stdin", header= FALSE)
NUmReq <- as.integer(u$V1[1])
cat(factorialM(NUmReq))

#Best Solutions for Factorial
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
cat(factorial(as.numeric(t[2])))

#Divisiors
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
l <- as.numeric(t[1]) 
r <- as.numeric(t[2])
k <- as.numeric(t[3])
count <- 0
for(i in l:r){
  if(i%%k == 0){
    count <- count+1
  }
}

cat(count)

#Life, universe and everything
line <- readLines('sample.txt', warn=FALSE)
t=scan(text =line , what = " ")
len <- length(t)
i <- 1
while(i < len){
  u <- as.numeric(t[i])
  if(u == 42){break}
  else{
    cat(u)
    cat("\n")
  }
  i <- i+1
}

#Roy and profile
line <- readLines('sample.txt', warn=FALSE)
t=as.numeric(scan(text =line , what = " "))
l <- t[1]
n <- t[2]

i <- 0
j <- 3
while(i <n){
  w <- t[i+j]
  h <- t[i+j+1]
  
  if(w <l | h <l){cat("UPLOAD ANOTHER","\n")}
  else if(w ==h & w>=l & h>=l){cat("ACCEPTED","\n")}
  else{cat("CROP IT","\n")}
  
  j <- j+1
  i <- i+1
}

#Find Product
line <- readLines('sample.txt', warn=FALSE)
t=as.numeric(scan(text =line , what = " "))

n <- t[1]
count <- t[2]
for(i in 2:n){
  count <- (count*t[i]) %% (1e9 +7)
}

cat(count)







































