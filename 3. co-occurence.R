library(KoNLP)
library(tidytext)
library(rJava)
library(multilinguer)
useNIADic()

comments_tb <- read_csv('comments_2021.07.03-08.11.csv', show_col_types = FALSE)


# 3. 기본적인 전처리------------------------------------------------------
## 기호 or 영어 삭제하고 공백(스페이스)으로 변경
comments_tb$comments <- gsub("[[:punct:]]|[a-zA-Z]", " ", comments_tb$comments)
## 숫자
comments_tb$comments <- gsub("[[:digit:]]", " ", comments_tb$comments)
## 공백(스페이스) 두 칸 이상은 한 칸으로 수정
comments_tb$comments <- gsub("[[:space:]][[:space:]]+", " ", comments_tb$comments)
## ㅋㅋㅋㅋㅋ / ㅠㅠ 와 같은 자음, 모음 삭제 
comments_tb$comments <- gsub('[ㄱ-ㅎ]+','',comments_tb$comments)
comments_tb$comments <- gsub('[ㅏ-ㅣ]+','',comments_tb$comments)
comments_tb$comments <- gsub('(ㅜ|ㅠ)+','',comments_tb$comments)
# comments_tb$comments <- gsub('[^가-힣]', '',comments_tb$comments)
# comments_tb$comments <- gsub("\\d+","",comments_tb$comments)
## 특수 문자 제거
comments_tb$comments <- gsub('[~!@#$%^&*()_+=?]<>','',comments_tb$comments)




## 줄임말 통일
comments_tb %>%
  mutate(comments = gsub("여자|여자들|여성들", "여성", comments)) %>%
  mutate(comments = gsub("남자|남자들|남성들", "남성", comments)) %>%
  mutate(comments = gsub("여성가족부|여가부", "여성가족부", comments)) %>% 
  mutate(comments = gsub("여성혐오|여성 혐오", "여혐", comments)) %>% 
  mutate(comments = gsub("남성혐오|남성 혐오", "남혐", comments))  %>% 
  mutate(comments = gsub("임금|시급|월급", "임금", comments)) %>% 
  mutate(comments = gsub("양육|육아", "양육", comments)) %>% 
  mutate(comments = gsub("출생|출산", "출생", comments)) %>% 
  mutate(comments = gsub("혼인|결혼", "결혼", comments))-> comments_tb


head(comments_tb)


# 토큰화 하기 ------------------------------------------------------------------


head(comments_tb)

View(comments_tb)
head(comment_pos)
comment_pos <- comments_tb %>%
  unnest_tokens(input = comments,
                output = pos,
                token = SimplePos22,
                drop = F)


head(comment_pos)


# 품사 분리하여 행 구성하기 ----------------------------------------------------------


library(tidyr)
comment_pos  <- comment_pos  %>%
  separate_rows(pos, sep = "[+]")




# 명사, 동사, 형용사를 한 번에 추출하기 --------------------------------------------------


comment_new <- comment_pos %>%
  separate_rows(pos, sep = "[+]") %>%
  filter(str_detect(pos, "/n")) %>%
  mutate(pos = str_remove(pos, "/.*$") %>% 
  filter(str_count(pos) >= 2)

  
  
comment_new <- comment_pos %>%
  filter(str_detect(pos, "/n")) %>%
  mutate(pos = str_remove(pos, "/.*$")) %>% 
  filter(str_count(pos) >= 2)
  



head(comment_new)

# 단어 동시 출현 빈도 구하기 ---------------------------------------------------------


# install.packages("widyr")
library(widyr)
# pairwise_count(
#   tbl = 대상 데이터,
#   item = 갯수를 새어야 할 컬럼,
#   feature = 함께 출현했다고 판단할 단위 그룹,
#   sort = 출현 횟수 단위로 정렬할지
# )

pair <- comment_new %>%
  pairwise_count(item = pos,
                 feature = id,
                 sort = T)
pair


# 특정 단어와 자주 함께 사용된 단어 살펴보기
pair %>% filter(item1 == "여성가족부")
pair %>% filter(item1 == "비혼")



# 동시 출현 네트워크 (Co-occurrence network) --------------------------------------
# 네트워크 그래프 데이터 만들기
# install.packages("tidygraph")
# install.packages('ggraph')
library(ggraph)
library(tidygraph)

graph_comment <- pair %>%
  filter(n >= 50) %>%
  as_tbl_graph()
graph_comment


# 네트워크가 너무 복잡하지 않도록 25회 이상 사용된 단어 추출해 생성

par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))  



# 네트워크 그래프 함수 만들기
word_network <- function(x) {            
  ggraph(graph_comment, layout = "fr") +   # 레이아웃
    geom_edge_link(color = "gray50",        # 엣지 색깔
                   alpha = 0.5) +           # 엣지 명암
    geom_node_point(color = "lightcoral",   # 노드 색깔
                    size = 5) +              # 노드 크기
    geom_node_text(aes(label = name),       # 텍스트 표시
                   repel = T,                # 노드밖 표시
                   size = 5,                 # 텍스트 크기
                   family = "AppleGothic") +   # 폰트
    theme_graph()                           # 배경 삭제
}
set.seed(1234)
word_network(graph_comment)


# 유의어 처리하기 ----------------------------------------------------------------

# 유의어 처리하기

comment_new %>%
  mutate(pos = gsub("오르다", "올리다", pos)) %>% 
  mutate(pos = gsub('그러다', '그렇다', pos)) %>% 
  mutate(pos = gsub('쓰다', '사다', pos)) -> comments_new

# 단어 동시 출현 빈도 구하기
pair <- comment_new %>%
  pairwise_count(item = pos,
                 feature = id,
                 sort = T)

# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 30) %>%
  as_tbl_graph()

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment) 


# 네트워크 그래프 데이터에 연결 중심성, 커뮤니티 변수 추가하기 --------------------------------------
set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 50) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), # 연결 중심성
         group = as.factor(group_infomap())) # 커뮤니티


# 네트워크 그래프에 연결 중심성, 커뮤티니 표현하기 ---------------------------------------------


set.seed(1234)

ggraph(graph_comment, layout = "fr") +    # 레이아웃
  geom_edge_link(color = "gray50",         # 엣지 색깔
                 alpha = 0.5) +            # 엣지 명암
  geom_node_point(aes(size = centrality,   # 노드 크기
                      color = group),      # 노드 색깔
                  show.legend = F) +        # 범례 삭제
  scale_size(range = c(2, 10)) +           # 노드 크기 범위
  geom_node_text(aes(label = name),        # 텍스트 표시
                 repel = T,                # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "AppleGothic") + # 폰트
  theme_graph()                            # 배경 삭제



# 네트워크의 주요 단어 살펴보기 --------------------------------------------------------

graph_comment %>%
  filter(name == "여성가족부")


# 같은 커뮤니티로 분류된 단어 살펴보기 ----------------------------------------------------

graph_comment %>%
  filter(group == 1) %>%
  arrange(-centrality) %>%
  data.frame()

library(tidyverse)
graph_comment %>% 
  group_by(group) %>% 
  arrange(-centrality)

graph_comment %>%
  arrange(-centrality) %>%
  data.frame() -> graph_comment_centrality

unique(graph_comment_centrality$group)
# 연결 중심성이 높은 주요 단어 살펴보기 ---------------------------------------------------

graph_comment %>%
  arrange(-centrality)


# 연결 중심성이 높은 주요 단어 살펴보기 ---------------------------------------------------


# 2번 커뮤니티로 분류된 단어
for (i in 1:4){
graph_comment %>%
  filter(group == i) %>%
  arrange(-centrality) %>%
  data.frame() %>% 
  print()}


# 주요 단어가 사용된 원문 살펴보기 ------------------------------------------------------


# 최저임금 과 인상 두 단어가 쓰인 원문 보기

comments_new %>%
  filter(str_detect(pos, "최저임금") & str_detect(pos, "인상")) %>%
  select(pos)




# 단어 간 상관 분석 (Phi coefficient) --------------------------------------------
# 파이 계수(phi coefficient)
#두 단어가 함께 사용되는 경우가 각각 사용되는 경우에 비해 얼마나 많은지 나타낸 지표
#상대적으로 관련성이 큰 단어 파악하는데 활용
#어떤 단어와 자주 함께 사용되지만 다른 단어와는 자주 함께 사용되지 않는 단어
#+1에 가까울수록 두 단어가 자주 함께 사용되어 관련성이 크다는 의미
#-1에 가까울수록 함께 사용되는 경우가 드물어 관련성이 작다는 의미

#widyr::pairwise_cor()
#item : 단어
#feature : 텍스트 구분 기준
#sort = T : 파이 계수 높은순 정렬


# 파이 계수 구하기 ---------------------------------------------------------------



word_cors <- comment_new %>%
  add_count(pos) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = pos,
               feature = id,
               sort = T)


# 특정 단어와 관련성이 큰 단어 살펴 보기 
word_cors %>%
  filter(item1 == "비혼")



# 파이 계수로 막대 그래프 만들기 -------------------------------------------------------

# 관심 단어 목록 생성
target <- c("비혼", "출생", "양육", "해체", "평등", "여성가족부")

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

# 그래프 순서 정하기
top_cors$item1 <- factor(top_cors$item1, levels = target)

library(ggplot2)
par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))  


ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "AppleGothic"))


# 파이 계수로 네트워크 그래프 만들기 -----------------------------------------------------

# 네트워크 그래프 데이터 만들기. 연결 중심성과 커뮤니티 추가하기

set.seed(1234)

graph_cors <- word_cors %>%
  filter(correlation >= 0.25) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

# 네트워크 그래프 만들기

set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation,              # 엣지 명암
                     edge_width = correlation),                 # 엣지 두께
                 show.legend = F) +                         # 범례 삭제
  scale_edge_width(range = c(1, 4)) +        # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "AppleGothic") +
  theme_graph()



# 동시 출현 빈도, 파이계수로 만든 네트워크 그래프의 차이 -----------------------------------------


#동시 출현 빈도를 이용한 네트워크 그래프
#여러 단어와 자주 함께 사용된 단어쌍 중심으로 네트워크 형성
#노드 대부분이 서로 연결되어 구조가 복잡하고 군집이 잘 드러나지 않음
#자주 사용된 단어를 파악할 때 활용

#파이 계수를 이용한 네트워크 그래프
#다른 단어에 비해 상대적으로 자주 함께 사용된 단어쌍 중심으로 네트워크 형성
#관련성이 큰 단어끼리만 연결되어 단어 군집이 명확하게 드러남
#밀접하게 관련된 단어쌍 파악할 때 활용



# 연이어 사용된 단어쌍 분석 (n-gram) -------------------------------------------------
# 동시 출현 빈도와 파이 계수의 한계: 단어가 함께 사용된 횟수만 고려 단어가 연결될 때 생기는 의미 무시 이해하기 어려운 단어쌍 등장
# 한 댓글이 하나의 행이 되도록 결합하기
line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(pos, collapse = " "))
line_comment


# 바이그램으로 토큰화하기 ------------------------------------------------------------



# 연이어 사용된 단어쌍 빈도 구하기 ------------------------------------------------------
# 바이그램 분리하기
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_seprated

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()

pair_bigram

# 동시 출현 단어쌍
pair %>%
  filter(item1 == "비혼")

# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "출생")



# 엔그램으로 네트워크 그래프 만들기 ------------------------------------------------------
# 네트워크 그래프 데이터 만들기
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph()
# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_bigram)


# 유의어 통일하고 네트워크 그래프 다시 만들기 ------------------------------------------------

#bigram_seprated의 유의어 통일, 같은 단어 연속 단어쌍 제거
#단어쌍 빈도 구하고 결측치 제거

# 유의어 처리
bigram_seprated$word1

bigram_seprated <- bigram_seprated %>%
  mutate(word1 = ifelse(str_detect(word1, "오르다"), "올리다", word1),
         word2 = ifelse(str_detect(word2, "오르다"), "올리다", word2),
         word1 = ifelse(str_detect(word1, "인상"), "올리다", word1),
         word2 = ifelse(str_detect(word2, "인상"), "올리다", word2)) %>%
  # 같은 단어 연속 제거
  filter(word1 != word2)

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit() 

# 네트워크 그래프 데이터 만들기
set.seed(1234)

graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap()))      # 커뮤니티

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") +   # 레이아웃
  geom_edge_link(color = "gray50",      # 엣지 색깔
                 alpha = 0.5) +         # 엣지 명암
  geom_node_point(aes(size = centrality,  # 노드 크기
                      color = group),       # 노드 색깔
                  show.legend = F) +       # 범례 삭제
  scale_size(range = c(5, 10)) +           # 노드 크기 범위
  geom_node_text(aes(label = name),       # 텍스트 표시
                 repel = T,                              # 노드밖 표시
                 size = 5,                               # 텍스트 크기
                 family = "AppleGothic") +               # 폰트
  theme_graph()                           # 배경 삭제 




