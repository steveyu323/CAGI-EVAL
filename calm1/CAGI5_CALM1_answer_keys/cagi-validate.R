#!/usr/bin/Rscript

args <- commandArgs(TRUE)
#check if file argument was provided
if (length(args) < 1) stop("Usage: Rscript cagi-validate.R <file>")


infile <- args[[1]]
#check if file exists
if (!file.exists(infile)) stop("File ",infile," does not exist!")
# and if file can be read
if (file.access(infile,mode=4) < 0) stop("No permission to read file ",infile,)

#read the lines in the file
lines <- scan(infile,what=character(),sep="\n")
if (length(lines) != 1814) stop("File does not have the required 1814 lines")

#break lines into data fields
fields <- strsplit(lines,"\t")
#check if each line has four fields
if (any(sapply(fields,length) != 4)) stop("Must have 4 tab-delimited columns!")

#check if the header is valid
header <- fields[[1]]
valid.header <- c("allele","score","sd","comment")
if (any(header != valid.header)) stop("Invalid table headers!")

#extract data columns for the remaining lines
body <- fields[-1]
body.cols <- lapply(1:4,function(i) sapply(body,`[[`,i))

#check if the first column contains valid allele descriptors
if (any(regexpr("^\\w\\d+\\w(,\\w\\d+\\w)*$",body.cols[[1]]) != 1)) {
	stop("First column must contain valid alleles!")
}

#check if the second column is numerical values or stars
if (!all(sapply(body.cols[[2]],function(x) x == "*" || !is.na(as.numeric(x))))) {
	stop("Second column must be numeric or \"*\"!")
}

#check if the third column is numerical values or stars
if (!all(sapply(body.cols[[3]],function(x) x == "*" || (!is.na(as.numeric(x))) ))) {
	stop("Third column must be numeric or \"*\"!")
}

#check if the fourth column is not empty
if (!all(nchar(body.cols[[4]]) > 0)) {
	stop("Fourth column must not be empty!")
}

cat("The file is valid!\n")

