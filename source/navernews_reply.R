library(N2H4) # 네이버 뉴스 크롤링 패키지
library(dplyr) # 전처리
library(stringr) # 텍스트 전처리
library(wordcloud) # 워드클라우드
library(rJava)
library(RColorBrewer) 
library(NIADic) # 텍스트
library(KoNLP) # 텍스트
extrafont::loadfonts(device="win") # Font 문제
library(qgraph) # 연관성분석
library(tm) # 연관성분석

# 댓글 가져오기 (패키지 )
comments01 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=413&aid=0000048047") %>% select(contents)
comments02 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=208&aid=0000001226") %>% select(contents)
comments03 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=139&aid=0002091163") %>% select(contents)
comments04 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=001&aid=0010157297") %>% select(contents)
comments05 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=076&aid=0003309778") %>% select(contents)
comments06 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=001&aid=0010584849") %>% select(contents)
comments07 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=020&aid=0003226987") %>% select(contents)
comments08 <- getAllComment("https://sports.news.naver.com/news.nhn?oid=139&aid=0002121236") %>% select(contents)


# 댓글 데이터만 추출
i <- 1
for(i in 1:8){
  con <- c(get(paste0("comments0", i))) %>% unlist()
  assign(paste0("reply_0", i), con) %>%
    str_replace_all("\\W", " ")
}

# 텍스트 전처리

useNIADic()

## Function
text_data <- function(text_data){
  data_review <- str_replace_all(text_data, "\\W", " ") %>%
    str_replace_all("[ㄱ-ㅎ]+","") %>%
    str_replace_all("\\[|\\]","") %>%
    as.character()
  noun <- sapply(data_review, extractNoun, USE.NAMES = FALSE) %>% unlist()
  noun2 <- Filter(function(x){nchar(x) >= 2}, noun) #2단어 이상의 명사만 추출
  noun2 <- gsub("이승우","",noun2) # 이승우 본인 이름 제거
  noun2 <- gsub("승우","",noun2) # 이승우 본인 이름 제거
  wordFreq <- table(noun2)
  noundta <- sort(wordFreq, decreasing = TRUE, 1000)
  return(noundta)
}

## 명사 데이터

noundta_01 <- text_data(reply_01)
noundta_02 <- text_data(reply_02)
noundta_03 <- text_data(reply_03)
noundta_04 <- text_data(reply_04)
noundta_05 <- text_data(reply_05)
noundta_06 <- text_data(reply_06)
noundta_07 <- text_data(reply_07)
noundta_08 <- text_data(reply_08)

str(noundta_01)


# 워드클라우드

pal2 <- brewer.pal(8, "Dark2")
pal <- brewer.pal(12,'Set3')
pal <- pal[-c(1:2)]

i <- 1
for (i in 1:8){
  png(paste0("plot/wordcloud_0",i,".png"), width = 800, height = 600)
  wordcloud(names(get(paste0("noundta_0",i))), freq = get(paste0("noundta_0",i)), min.freq = 6, random.order = FALSE,
            rot.per = 0, col = pal, encoding = "UTF-8")
  dev.off()
}

# 명사 상관성 분석

## Function
matrix_data <- function(matrix_data){
  data_review <- str_replace_all(matrix_data, "\\W", " ") %>%
    str_replace_all("[ㄱ-ㅎ]+","") %>%
    str_replace_all("\\[|\\]","") %>%
    as.character()
  tt <- paste(unlist(SimplePos22(data_review)))
  #명사만 가져오기
  alldta <- str_match_all(tt, "[가-힣]+/[N][C]|[가-힣]+/[N][Q]+") %>% unlist()
  #명사로 추출된 단어들의 분류표인 /NC, /NQ 등을 제거
  N <- str_replace_all(alldta, "/[N][C]","") %>%
    str_replace_all("/[N][Q]","") %>% unlist()
  N <- N[!str_detect(N,"이승우")]
  N <- N[!str_detect(N,"승우")]
  # 명사들 상관성 분석
  DtaCorpusNC <- Corpus(VectorSource(N))
  myTdmNC <- TermDocumentMatrix(DtaCorpusNC, control = list(wordLengths=c(4,10),
                                                          removePunctuation = T,
                                                          removeNumbers = T,
                                                          weighting = weightBin))
  Encoding(myTdmNC$dimnames$Terms) = "UTF-8"
  findFreqTerms(myTdmNC, lowfreq = 10)
  mtNC <- as.matrix(myTdmNC) # 행렬로 변환
  mtrowNC <- rowSums(mtNC)
  mtNC.order <- mtrowNC[order(mtrowNC, decreasing = TRUE)]
  freq.wordsNC <- mtNC.order[mtNC.order>15]
  freq.wordsNC <- as.matrix(freq.wordsNC)
  co.matrix <- freq.wordsNC %*% t(freq.wordsNC)
  return(co.matrix)
}
  
# Co.matrix
  co.matrix_01 <- matrix_data(reply_01)
  co.matrix_02 <- matrix_data(reply_02)
  co.matrix_03 <- matrix_data(reply_03)
  co.matrix_04 <- matrix_data(reply_04)
  co.matrix_05 <- matrix_data(reply_05)
  co.matrix_06 <- matrix_data(reply_06)
  co.matrix_07 <- matrix_data(reply_07)
  co.matrix_08 <- matrix_data(reply_08)
  
# qgraph draw
  i <- 1
  for (i in 1:8){
    png(paste0("plot/qgraph_0",i,".png"), width = 800, height = 600)
    windowsFonts(Times=windowsFont("TT Times New Roman"))
    qgraph(get(paste0("co.matrix_0",i)),
           labels= row.names(get(paste0("co.matrix_0",i))),
           diag= FALSE,
           layout= "spring",
           vsize= log(diag(get(paste0("co.matrix_0",i)))*2))
    dev.off()
  }
