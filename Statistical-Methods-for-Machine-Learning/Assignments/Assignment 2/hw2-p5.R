require(Matrix)

# Change to the working directory where the data files are located.
# TODO: You should change the following ... to your working directory
setwd("/home/Moontae/Dropbox/Class/UIC/2020sp/IDS575/hw2")

# Read all individual lines in a text file.
# m = the number of training examples
dataFile <- file("articles.train", "r")
dataLines <- readLines(dataFile)
m <- length(dataLines)
close(dataFile)

# Split every string element by tokenizing space and colon.
dataTokens = strsplit(dataLines, "[: ]")

# Extract every first token from each line as a vector of numbers, which is the class label.
Y = sapply(dataTokens, function(example) {as.numeric(example[1])})

# Extract the rest of tokens from each line as a list of matrices (one matrix for each line)
# where each row consists of two columns: (feature number, its occurrences)
X_list = lapply(dataTokens, function(example) {n = length(example) - 1; matrix(as.numeric(example[2:(n+1)]), ncol=2, byrow=T)})

# Add one column that indicates the example number at the left
X_list = mapply(cbind, x=1:length(X_list), y=X_list)

# Merge a list of different examples vertcially into a matrix
X_data = do.call('rbind', X_list)

# Get a sparse data matrix X (rows: training exmaples, columns: # of occurrences for each of features)
X = sparseMatrix(x=X_data[,3], i=X_data[,1], j=X_data[,2])
