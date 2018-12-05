### binary-blind.R file ###

source("blind.R") # load the blind search methods

# read D bits from integer x:
binint <- function(x, D) {
  x <- rev(intToBits(x)[1:D]) # get D bits
  # remove extra 0s from raw type:
  as.numeric(unlist(strsplit(as.character(x), ""))[(1:D) * 2])
}

# convert binary vector into integer: code inspired in:
# http://stackoverflow.com/questions/12892348/in-r-how-to-convert-binary-string-to-binary-or-decimal-value
intbin <- function(x) sum(2^(which(rev(x == 1)) - 1))

# define problem (search space) ------------------------------------------------
D <- 8              # number of dimensions
x <- 0:(2^D - 1)    # integer search space

# set full search space in solutions x D:
search <- t(sapply(x, binint, D = D))

# set the domain values (D binary variables):
domain <- vector("list", D)
for (i in 1:D) domain[[i]] = c(0, 1) # bits

# sum of bits ------------------------------------------------------------------
# evaluation function:
sumbin <- function(x) sum(as.numeric(x))    # sum a raw binary object x

# full search
S1 <- fsearch(search, sumbin, "max")
cat("fsearch best s:", S1$sol, "--> f:", S1$eval, "\n")

# depth-first search
S2 <- dfsearch(domain = domain, FUN = sumbin, type = "max")
cat("dfsearch best s:", S2$sol, "--> f:", S2$eval, "\n")

# max sin ----------------------------------------------------------------------
# evaluation function:
maxsin <- function(x, Dim) sin(pi * (intbin(x)) / (2^Dim))  # max sin of binary raw object x

# full search
S3 <- fsearch(search, maxsin, "max", Dim = 8)
cat("fsearch best s:", S3$sol, "--> f:", S3$eval, "\n")

# depth-first search
S4 <- dfsearch(domain = domain, FUN = maxsin, type = "max", Dim = 8)
cat("dfsearch best s:", S4$sol, "--> f:", S4$eval, "\n")
