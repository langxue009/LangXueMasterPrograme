
library(RISmed)
library(tm)
library(quanteda.textmodels) # for dfm
library(quanteda) # for corpus

library("arulesSequences")
library(tm)
library(quanteda.textmodels) # for dfm
library(quanteda)

### query 100 article information:

# 50 articles included term obesity
res <- EUtilsSummary(query = "obesity", type="esearch", db="pubmed", datetype='pdat', retmax=220)
fetch <- EUtilsGet(res) # get the query results
title_all <- ArticleTitle(fetch) # extract Title
title_has_obesity <- grepl("obesity", tolower(title_all))
title_obesity <- title_all[title_has_obesity][1:50]

# 50 articles included term cancer
res <- EUtilsSummary(query = "cancer", type="esearch", db="pubmed", datetype='pdat', retmax=220)
fetch <- EUtilsGet(res) # get the query results
title_all <- ArticleTitle(fetch) # extract Title
title_has_cancer <- grepl("cancer", tolower(title_all))
title_cancer <- title_all[title_has_cancer][1:50]

# combine both results
article_title <- c(title_obesity, title_cancer)

# using dfm() to create document term matrix
article_title1 <- removePunctuation(article_title, preserve_intra_word_dashes = T) # remove punctuation
article_title2 <- gsub("[0-9]", " ", article_title1) # remove numbers
corp <- corpus(article_title2)
DTM <- dfm(corp, tolower = T, remove = stopwords("english"), verbose = T) # document term matrix
DTM_matrix <- as(DTM, "matrix")

#write.csv(DTM_matrix, "DTM_matrix.csv", row.names = FALSE)

### Setting
n_components = 2 # reduced dimension
K = 2

### PCA 
# pr = prcomp(t(DTM_matrix))
# pcs = pr$x # all PCs
# t(pcs)[1:10,1:3]
# loading = pr$rotation
# reduced_data <- pcs[,1:K] %*% t(loading[,1:K])
# reduced_with_center <- scale(reduced_data, center = -1*pr$center, scale = FALSE)

DTM.center = scale(DTM_matrix, center = TRUE, scale = FALSE)
cov_matrix = cov(DTM.center, use = "pair")
E <- svd(cov_matrix)
A <- DTM.center %*% E$u # PC before scaled
A[1:10,1:n_components]
# eigen_part <- eigen(diag(E$d))
# A_scale = A %*% with(eigen_part, vectors %*% (values^(-0.5) * t(vectors))) # scaled principal components 
dim(A[,1:n_components])
# visualize the result
DTM_PCA = data.frame(class = c(rep('obesity', 50), rep('cancer', 50)), component1 = A[,1], component2 = A[,2])
plot(DTM_PCA$component1, DTM_PCA$component2, xlab = "First component", ylab = "Second component", 
     col = c("red", 'blue')[factor(DTM_PCA$class)], pch = 19, main = 'PCA result')
levels(factor(DTM_PCA$class))
legend('topleft', levels(factor(DTM_PCA$class)), col = c("red", "blue"), pch = 19)

### tSNE
# BiocManager::install("M3C")
library(M3C)
library(Rtsne)
set.seed(2020)
tsne_out <- Rtsne(DTM_matrix,dims=n_components,
                  pca=FALSE,perplexity=30,theta=0.0,
                  check_duplicates=FALSE) # Run TSNE
head(tsne_out$Y)
dim(tsne_out$Y)

# visualize the result
DTM_tSNE = data.frame(class = c(rep('obesity', 50), rep('cancer', 50)), component1 = tsne_out$Y[,1], component2 = tsne_out$Y[,2])
plot(DTM_tSNE$component1, DTM_tSNE$component2, xlab = "First component", ylab = "Second component", 
     col = c("red", 'blue')[factor(DTM_tSNE$class)], pch = 19, main = 'tSNE result')
levels(factor(DTM_tSNE$class))
legend('topleft', levels(factor(DTM_tSNE$class)), col = c("red", "blue"), pch = 19)

#library("plot3D")
#scatter3D(x = tsne_out$Y[,1], y = tsne_out$Y[,2], z = tsne_out$Y[,3])

### UMAP
library(umap)

# configuration used for umap (currently just use the default values)
umap.defaults
custom.config = umap.defaults
custom.config$random_state = 2020

# fit
DTM.umap = umap(DTM_matrix, config = custom.config)

# visualize the result
DTM_UMAP = data.frame(class = c(rep('obesity', 50), rep('cancer', 50)), component1 = DTM.umap$layout[,1], component2 = DTM.umap$layout[,2])
plot(DTM_UMAP$component1, DTM_UMAP$component2, xlab = "First component", ylab = "Second component", 
     col = c("red", 'blue')[factor(DTM_UMAP$class)], pch = 19, main = 'UMAP result')
levels(factor(DTM_UMAP$class))
legend('topleft', levels(factor(DTM_UMAP$class)), col = c("red", "blue"), pch = 19)



