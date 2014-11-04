# Read in countries
cou <- readLines("./countries.txt")
# Remove useless cruft
delMatch <- c("\f","INDEX","CONTENT","Foreword","Foundation")
del <- grepl(paste(delMatch,collapse="|"), cou, ignore.case=TRUE)
cou <- cou[!del]
cou1 <- strsplit(cou, split="[.]+\ ")
# Make a data frame listing the countries and their page numbers
countries <- data.frame(do.call(rbind, cou1))
colnames(countries) <- c("country", "page")

# Read in the index
#ind <- readLines("./index.txt")
# Remove useless cruft
#ind <- gsub("\f", "", ind)
#ind <- gsub("-", "", ind)
#delMatch2 <- c("INDEX")
#del2 <- grepl(paste(delMatch2,collapse="|"), ind, ignore.case=TRUE)
#ind <- ind[!del2]
#Find lines that end with text and \n

#multiline <- !grepl("[0-9]", ind)
#singleline <- grepl("[0-9]", ind)

#ind <- data.frame(cbind(ind,multiline))

#ind$num <- rownames(ind)
#ind$ind <- as.character(ind$ind)
#mul <- as.numeric(ind[ind$multiline==TRUE,]$num)

#paste(ind[mul,]$ind, ind[mul+1,]$ind) -> ind[mul+1,]$ind

#ind <- ind[-mul,]

#ind2 <- gsub("[.]", "", ind)
#ind3 <- gsub("[0-9]", "", ind2)
#kwds <- gsub("^\\s+|\\s+$", "", ind3)

# Read in the data
directory <- readLines("./dir.txt")

# Field Identification names
missing.vars <- c("Name","Address")
trash.vars <- c("Tel","Fax","Email","Head Librarian")
vars <- c("Library","Website","Holdings","Subjects","books","journal","BREAK")

# Remove unwanted fields
rubbish <- grepl(paste(trash.vars,collapse="|"), directory)
directory <- directory[!rubbish]
not.rubbish <- grepl(paste(vars,collapse="|"), directory) | grepl(paste(countries$country, collapse="|"), directory)
directory <- directory[not.rubbish]

# Show head of directory vector
#head(directory, 50)

# Remove cruft
directory <- gsub("\f", "", directory)

# Identify country breaks in readLines vector
country.name <- grepl(paste(countries$country,collapse="|"), directory)
directory <- data.frame(cbind(directory,country.name))
directory$num <- rownames(directory)

breaks <- as.numeric(directory[directory$country.name==TRUE,]$num)
breaks.beg <- breaks+1
breaks.end <- breaks-1
eof <- length(directory$directory)

count.cou <- length(breaks)

breaks.end <- c(breaks.end[2:count.cou],eof)

countries <- data.frame(countries,breaks.beg,breaks.end)

#c <- sort(unique(countries$country))

# Extract country blocks to nested list
extract.country <- function(x) {
  assign(paste(countries[x,1]), as.character(directory[countries[x,]$breaks.beg:countries[x,]$breaks.end,1]))
}

# Save list separated by country
list.data <- sapply(1:20, extract.country) 

# Morph list.data
morph.list <- function(x) {
  # Find breaks in data
  tmp.brk <- as.data.frame(grepl("BREAK", list.data[[x]]))
  colnames(tmp.brk) <- "brk"
  tmp.brk$num <- rownames(tmp.brk)
  
  brks <- as.numeric(tmp.brk[tmp.brk$brk==TRUE,]$num)
  count.row <- length(brks)-1
  brks.beg <- brks+1
  brks.beg <- brks.beg[1:count.row]
  brks.end <- brks-1
  brks.end <- brks.end[2:(count.row+1)]
  
  brk <- data.frame(brks.beg,brks.end)  

# Extract individual records
  extract.record <- function(y) {
    list.data[[x]][brks.beg[y]:brks.end[y]]
  }

  tmp <- sapply(1:count.row, extract.record) 

#  assignFields <- function(z){
#    out <- character(5)
#    # Get names
#    i <- grepl("[Library:]",as.character(z))
#    out[1] <- as.character(z[i])
#    # Assign country
#    out[2] <- as.character(countries$country[x])
#    # Get subjects
#    i <- which(grepl("[Subjects:]",as.character(z)))
#    out[3] <- ifelse(i==TRUE, as.character(z[i]), NA)
#    # Get Holdings
#    i <- which(grepl("[Holdings:]",as.character(z)))
#    out[4] <- ifelse(i==TRUE, as.character(z[i]), NA)
#    # Get Website
#    i <- which(grepl("[Website:]",as.character(z)))
#    out[5] <- ifelse(i==TRUE, as.character(z[i]), NA)
#    out
#  }

#  tmp <- lapply(tmp, assignFields)
  
  return(tmp)
}

Egypt <- morph.list(3)

#protocode to be incorporated into morph.list

assignFields <- function(x){
  out <- character(5)
  # get names
  i <- grepl("Library: ",x)
  out[1] <- x[i]
  i <- which(grepl("Subjects",x)==TRUE)
  out[2] <- ifelse(is.integer(i)==FALSE, NA, x[i])
  i <- which(grepl("Holdings",x)==TRUE)
  out[3] <- ifelse(is.integer(i)==FALSE, NA, x[i])
  i <- which(grepl("Website",x)==TRUE)
  out[4] <- ifelse(is.integer(i)==FALSE, NA, x[i])
  out[5] <- paste(as.character(countries$country[3]))
  out
}

standardEgypt <- lapply(Egypt, assignFields)

standardEgypt

(M <- matrix(
  unlist(standardEgypt)
  , nrow=length(standardEgypt)
  , byrow=TRUE))
