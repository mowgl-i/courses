
#packages to install
install.packages("tm" )
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("qgraph")
install.packages("lsa")
install.packages("stringr")
install.packages("mclust")

#Duvidas: 
#documentos vazios e 'line abuse' pode afetar os resultados? 
# Como remover palavras pouco ou muito frequentes (proporcionalmente).
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(qgraph)
library(lsa)
library(stringr)
library(mclust)

#Carregando os dados com os textos originais
#setwd('F:/WAGNER 14.03')
text <- readLines("C:/Users/macia/Documents/MSIA-19/Git/courses/unstructured_data/exercise_textmining.csv")
text <- text[-(1)] #removendo a primeira linha 'history'
View(text)

# #removendo casos de 'line abuse' e 'null'
# text <- Filter(function (x) str_detect(x, "line abuse", negate=TRUE), text)
# text <- Filter(function (x) str_detect(x, "null", negate=TRUE), text)
# View(text)

#Corpus
df <- data.frame(text, stringsAsFactors=FALSE)

#View(df)
corpus <- Corpus(VectorSource(df$text))
corpus

corpus[[1]]$content

#Limpando os dados

# apenas minusculas
corpus <- tm_map(corpus, tolower)
# removendo pontua??es
corpus <- tm_map(corpus, removePunctuation)
# removendo "stopwords"
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus <- tm_map(corpus, stemDocument, language = "english")
#removendo n?meros
corpus <- tm_map(corpus, removeNumbers)
#remover espa?o em branco entre as palavras 
corpus <- tm_map(corpus, stripWhitespace)


# verificando o corpus
writeLines(as.character(corpus))


# gerando matrizes de TD e DT
tdm <- TermDocumentMatrix(corpus)
dtm <- DocumentTermMatrix(corpus)

# freq de palavras
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
head(freq, 15)

wf <- data.frame(word=names(freq), freq=freq)   
head(wf)  


p <- ggplot(subset(wf, freq>5), aes(x = reorder(word, -freq), y = freq)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x=element_text(angle=45, hjust=1))
p   

set.seed(142)   
wordcloud(names(freq), freq, min.freq=4) 


# Escalonamento Multidimensional com LSA
td.mat <- as.matrix(tdm)

# Ponderando os termos em peso local (lw) e peso global (gw)

td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) 

#View(td.mat.lsa)
# criando o espa?o latente (M = T S t(D); a matriz M ? o produto das matrizes de termos "T", documentos "D" e a diagonal "S"
# de valores singulares
lsaSpace <- lsa(td.mat.lsa)
lsaSpace$dk
View(lsaSpace)

# calculando as dist?ncias
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) 
dist.mat.lsa

# Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)

# gerando o gr?fico
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(df)))


#Gerando classificacoes com mclust

#Model Based Clustering


#Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
#fit2 <- Mclust(points)
#plot(points[,1],points[,2],col= fit2$classification)

#numero de classifica??es
#summary(fit2)
#class <- fit2$classification

library(skmeans)

#Cluster analysis
set.seed(123456)
clust<-kmeans(points,10) #dividir estes dados em 3 clusters 
clust
plot(points[,1],points[,2],col= clust$cluster)
text(points[,1],points[,2],row.names(df))
# rect.hclust(clust, k=5, border="red")

# fit2 <- Mclust(points)

#Plot das classifica??es com base em Escalonamento Multidimensional com LSA, duas dimens?es (k=2)
# plot(points[,1],points[,2],col= fit2$classification)

# #numero de classifica??es
print(clust)

class <- clust$cluster

#Tabela final com as classifica??es 
table_final = data.frame(originalText = text,
                         # PointX  = fit$points[,1],
                         # PointY  = fit$points[,2],
                         class =  clust$cluster)

#write.csv2(table_final,"table_final.csv",row.names = F)

#associações 
findAssocs(dtm,"pain",corlimit = 0.3)
#dtm2<-removeSparseTerms(dtm,0.99)
cor_t<- cor(as.matrix(dtm),method = "spearman")
#View(cor_t)
qgraph(cor_t,layout="spring",labels=colnames(cor_t),threshold=0.3)

################################################################################
#By Cluster Analysis - Cluster 1 Instruments
################################################################################
# Análise por cluster
#Corpus 
df_c1 <- data.frame(table_final$originalText[which(table_final$class==1)], stringsAsFactors=FALSE)
# View(df_c1)
corpus_c1 <- VCorpus(VectorSource(df_c1))

# apenas minusculas
corpus_c1 <- tm_map(corpus_c1, content_transformer(tolower))
# removendo pontua??es
corpus_c1 <- tm_map(corpus_c1, content_transformer(removePunctuation))
# removendo "stopwords"
corpus_c1 <- tm_map(corpus_c1, function(x) removeWords(x, stopwords("english")))
# stemiza??o (manter a raiz)
corpus_c1 <- tm_map(corpus_c1, stemDocument, language = "english")
#removendo n?meros
corpus_c1 <- tm_map(corpus_c1, content_transformer(removeNumbers))
#remover espa?o em branco entre as palavras 
corpus_c1 <- tm_map(corpus_c1, content_transformer(stripWhitespace))

writeLines(as.character(corpus_c1))

#criando uma matrix de palavras e frequencia de palavras
dtm_c1 <- DocumentTermMatrix(corpus_c1)

# descritivos por cluster
freq_c1 <- sort(colSums(as.matrix(dtm_c1)), decreasing=TRUE)   
head(freq_c1, 20)

wf_c1 <- data.frame(word=names(freq_c1), freq=freq_c1)   
# head(wf_c1)  

p_c1 <- ggplot(subset(wf_c1, freq>3), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_freqs.eps",
#           width = 8, height = 8)
p_c1  
#dev.off()

# set.seed(142)   
# wordcloud(names(freq_c1), freq_c1, min.freq=20)

dtm_c1_2<-removeSparseTerms(dtm_c1,0.90)
cor_c1 <- cor(as.matrix(dtm_c1_2),method = "spearman")
cor_c1 <- ifelse(cor_c1<0,0,cor_c1)
colnames(cor_c1)

#Ploting network
#setEPS()
# tiff("/Users/joaovissoci/Desktop/depression_sr_network.tiff", width = 16, height = 8, units='in',compression = 'rle', res = 300)
#postscript("C:/Users/rapiduser/Desktop/figure_cluster1_network.eps",
#           width = 8, height = 8)
rede1<-qgraph(cor_c1,
                  layout="spring",
                  labels=colnames(cor_c1),
                  threshold=0.1,
                  vsize=5,
                  label.scale=FALSE,
                  # grey=T,
                  color="lightblue",
                  borders = FALSE,
                  posCol = "grey",
                  label.cex=1.2)

#dev.off()
