rm(list = ls());
library(lattice);
library(ggplot2);
library(jpeg);

#read files
files <- list.files("./images", full.name = TRUE);
n <- length(files);

#generate color histogram 
histogram <- function(img, u1, v1, u2, v2, channel) {
  h <- 0 * c(1 : 16);
  for (i in u1 : u2) {
    for (j in v1 : v2) {
      x <- floor(255 * img[i, j, channel] / 16.0) + 1;
      h[x] <- h[x] + 1;
    }
  }
  return(h);
}

#create dataset
D <- rbind();
for (i in 1 : n) {
  img <- readJPEG(files[i]);
  m <- nrow(img[ , , 1]);
  n <- ncol(img[ , , 1]);
  m1 <- floor(m / 2);
  n1 <- floor(n / 2);
  x <- c();
  for (channel in 1 : 3) {
    x <- c(x, histogram(img, 1, 1, m, n, channel));
    x <- c(x, histogram(img, 1, 1, m1, n1, channel));
    x <- c(x, histogram(img, 1, n1 + 1, m1, n, channel));
    x <- c(x, histogram(img, m1 + 1, 1, m, n1, channel));
    x <- c(x, histogram(img, m1 + 1, n1 + 1, m, n, channel));
  }
  D <- rbind(D, x);
}

#init k, steps, seed
k <- 8;
max_iterations <- 20;
set.seed(80);

#cluster using k-means
result <- kmeans(D, k, max_iterations);
labelList <- as.integer(result$cluster);

#output
sink(file = "./output.html", type = "output");
n <- length(labelList);
for (i in 1 : k) {
  cat ("<h1> Group ", i, "<h1>");
  for (j in 1 : n) {
    if (labelList[j] == i) {
      cat ("<img src=", files[j], ">");
    }
  }
  cat ("<hr>");
}
