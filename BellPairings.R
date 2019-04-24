require(gdata)
set.seed(04242019)


active <- read.xls("/Users/amenarde/Dropbox/Penn/Around the Bell/inputs/active.xlsx", sheet = 1, header = FALSE)
history <- read.xls("/Users/amenarde/Dropbox/Penn/Around the Bell/inputs/history.xlsx", sheet = 1, header = FALSE)

active$V1 <- as.character(active$V1)
history$V1 <- as.character(history$V1)
history$V2 <- as.character(history$V2)

n = length(active[,1])

pairing.matrix <- matrix(rep(TRUE, n*n), ncol = n)
colnames(pairing.matrix) <- active$V1
rownames(pairing.matrix) <- active$V1

for (i in 1:n) {
  pairing.matrix[i, i] <- FALSE
}

# row are cars, columns are passengers
for (i in 1:length(history$V1)) {
  if (history$V1[i] %in% active$V1 & history$V2[i] %in% active$V1) {
    pairing.matrix[history$V1[i], history$V2[i]] <- FALSE
    pairing.matrix[history$V2[i], history$V1[i]] <- FALSE 
  }
}

i <- ncol(pairing.matrix)
while(i > 1) {
  cat("\n")
  found <- FALSE
  while(!found) {
    allocation <- sample(1:i, 1)
    if (pairing.matrix[1,allocation]) {
      found = TRUE
      i <- i - 2
      
      cat(rownames(pairing.matrix)[1],
          "and",
          colnames(pairing.matrix)[allocation])
      
      pairing.matrix <- pairing.matrix[rownames(pairing.matrix) != rownames(pairing.matrix)[allocation] & 
                                       rownames(pairing.matrix) != rownames(pairing.matrix)[1], 
                                       colnames(pairing.matrix) != colnames(pairing.matrix)[allocation] & 
                                       colnames(pairing.matrix) != colnames(pairing.matrix)[1],
                                       drop = FALSE]
    }
  }
}

if (i == 1) {
  cat(" and", colnames(pairing.matrix), 
      "\n", "Please check that the last allocation is valid")
}

