# 0. library 설치 ---------------------------------------------
getwd()
setwd('~/Google Drive/R_workspace/KHU_textmining')

# install.packages('tidytext')
# install.packages("multilinguer")
# install.packages('rJava')
# install.packages('tm')
# install.packages("KoNLP", repos = c("https://forkonlp.r-universe.dev","https://cloud.r-project.org"), INSTALL_opts = c("–no-multiarch"))

# KoSpacing 관련 패키지 설치
# library(installr) # install.packages('installr')
# install.conda()
# 
# reticulate::conda_version()
# reticulate::conda_list()
# # install.packages('devtools')
# library(devtools)
# devtools::install_github('haven-jeon/KoSpacing', force = T)
# remotes::install_github("mrchypark/KoSpacing")
# library(KoSpacing)
# set_env()

# install.packages('devtools')
# library(devtools)
# install_github("junhewk/RcppMeCab")
# library(RcppMeCab)

library(tidyverse)
library(tidytext)
library(rJava)
library(multilinguer)
library(KoNLP)

useNIADic()
library(tm)

library(RmecabKo)

pos('비혼출생')


# 1. 데이터(댓글, 사전, 한글 불용어) 불러오기 ---------------------------------------------


# 1) 댓글 데이터 ---------------------------------------------------------------

 
comments <- read_csv('final_df.csv')

comments_tb <- comments$comments %>%
  str_remove_all("\\['") %>%
  str_remove_all("]") %>%
  str_remove_all("',") %>%
  strsplit("'") %>% 
  unlist() %>% 
  as_tibble_col(column_name = 'comments') %>% 
  mutate(id = 1:n()) %>% 
  select(id, comments)


# 3) 한글 불용어 ---------------------------------------------------------------

stopwords_ko <- read.table("stopwords_ko.txt", sep ='\t',
                           header = T) %>% 
  as.tibble() %>% 
  add_row(word = c('하다', '있다'))



# 2. 기본적인 전처리------------------------------------------------------
head(comments_tb)
comments_tb$comments <- gsub("[[:punct:]]|[a-zA-Z]", " ", comments_tb$comments)
comments_tb$comments <- gsub("[[:digit:]]", " ", comments_tb$comments)
comments_tb$comments <- gsub("[[:space:]][[:space:]]+", " ", comments_tb$comments)
## ㅋㅋㅋㅋㅋ / ㅠㅠ 와 같은 자음, 모음 삭제 
comments_tb$comments <- gsub('[ㄱ-ㅎ]+','',comments_tb$comments)
comments_tb$comments <- gsub('[ㅏ-ㅣ]+','',comments_tb$comments)
# comments_tb$comments <- gsub('[^가-힣]', '',comments_tb$comments)
# comments_tb$comments <- gsub("\\d+","",comments_tb$comments)
## 특수 문자 제거
comments_tb$comments <- gsub('[~!@#$%^&*()_+=?ㆍ]<>','',comments_tb$comments)


## 줄임말 통일

# 2-1. 추가 전처리 ---------------------------------------------------------------

#> 상위 빈도 단어들 참고해서 더 수정할 것: 임금/시급/최저임금/최저시급, 등등

comments_tb %>%
  mutate(comments = gsub("여자|여자들|여성들", "여성", comments)) %>%
  mutate(comments = gsub("남자|남자들|남성들", "남성", comments)) %>%
  mutate(comments = gsub("여성가족부|여가부", "여성가족부", comments)) %>% 
  mutate(comments = gsub("여성혐오|여성 혐오", "여혐", comments)) %>% 
  mutate(comments = gsub("남성혐오|남성 혐오", "남혐", comments))  %>% 
  mutate(comments = gsub("임금|시급|월급", "임금", comments)) %>% 
  mutate(comments = gsub("양육|육아", "양육", comments)) %>% 
  mutate(comments = gsub("출생|출산", "출생", comments)) %>% 
  mutate(comments = gsub("혼인|결혼|결혼해서", "결혼", comments))%>% 
  mutate(comments = gsub("사유+\\w|사유", "사유리", comments)) %>%
  mutate(comments = gsub("가정", "가족", comments)) %>% 
  mutate(comments = gsub("가족 형태", "가족형태", comments)) %>% 
  mutate(comments = gsub("가족해체|가족 해체|가정 해체|가정해체", "가족해체", comments)) %>% 
  mutate(comments = gsub("아빠|아버지|애비", "아빠", comments)) %>%
  mutate(comments = gsub("엄마|어머니", "엄마", comments)) %>% 
  mutate(comments = gsub("아이|아이들|아기|자식|애", "아이", comments)) %>%
  mutate(comments = gsub("슈퍼맨이 돌아왔다|슈퍼맨이돌아왔다|슈퍼맨+\\w|슈돌+\\w", "슈돌", comments)) %>% 
  mutate(comments = gsub("우리 나라|우리나라|대한민국|남의 나라|남의나라|이나라|이 나라|한국에서|한국 사회|한국사회", "한국", comments)) %>% 
  mutate(comments = gsub('한국는|한국은|한국에|한국의', "한국", comments)) %>% 
  mutate(comments = gsub("문재앙|문재인", "문재인", comments)) %>% 
  mutate(comments = gsub("일본|일본에|니네 나라|니네나라", "일본", comments)) %>% 
  mutate(comments = gsub("국가|정부|행정부", "정부", comments)) %>% 
  mutate(comments = gsub("정상적 가족|정상 가족", "정상가족", comments)) %>% 
  mutate(comments = gsub("국민들", "국민", comments)) %>% 
  mutate(comments = gsub("본인|자신|개인", "자기", comments)) %>% 
  mutate(comments = gsub("정자 기증", "정자기증", comments)) %>% 
  mutate(comments = gsub("정자 쇼핑", "정자쇼핑", comments)) %>% 
  mutate(comments = gsub("정자 은행", "정자은행", comments)) %>% 
  mutate(comments = gsub("응원+\\w", "응원", comments)) %>%  
  mutate(comments = gsub("사회적", "사회", comments)) %>% 
  mutate(comments = gsub("비혼모+\\w", "비혼모", comments)) %>% 
  mutate(comments = gsub("비혼출생+\\w|비혼 출생+\\w", "비혼출생", comments)) %>% 
  mutate(comments = gsub("프로그램", "방송", comments)) %>% 
  mutate(comments = gsub("오르다", "올리다", comments)) %>% 
  mutate(comments = gsub("바꾸다|바뀌다|변화하다", "변화하다", comments)) %>% 
  mutate(comments = gsub("이기적", "이기", comments)) %>% 
  mutate(id = 1:n()) %>% 
  select(id, comments)-> comments_tb



# 3. 댓글 토큰화------------------------------------------------------
library(RmecabKo)

comments_tb %>%
  unnest_tokens(output = pos, 
                input = comments,
                token = pos,
                drop = F) %>%
  mutate(pos_order = 1:n()) -> comments_pos


# 4. 총 형태소 분석 데이터프레임 ----------------------------------------------------

# nng, nnp -> 명사
# vv -> 동사
# va, xr -> 형용사

# 1) 명사/ 동사/ 형용사만 출력---------------------------------------------

# 명사 
comments_pos %>%
  filter(str_detect(pos, "/nng|/nnp")) %>%
  mutate(word = str_remove(pos, "/.*$")) -> n_df


# 명사 빈도확인 
n_df %>% 
  filter(nchar(word) >= 2) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) -> n_freq

View(n_freq)


# 데이터 저장
# write_excel_csv(n_freq, 'noun_freq.csv')

# 동사
comments_pos %>%
  filter(str_detect(pos, "/vv+")) %>% 
 mutate(word = str_replace(pos, "/.*$", "다")) -> v_df



# 동사 빈도확인 
v_df %>% 
  filter(nchar(word) >= 2) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) -> v_freq


View(v_freq)

# 데이터 저장
# write_excel_csv(p_freq, 'v_freq.csv')


# 형용사 
comments_pos %>%
  filter(str_detect(pos, "/va|/xr")) %>%
  mutate(word = str_replace(pos, "/.*$", ""))-> adj_df


# 형용사 빈도확인 
adj_df %>% 
  filter(nchar(word) >= 2) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) -> adj_freq


View(adj_freq)



# 명사, 동사, 형용사 데이터 합치기 
# 1) 빈도 분석/ 동시출현 네트워크용 데이터 프레임 -------------------------------------------------------

library(tibble)

bind_rows(n_df, v_df, adj_df)%>% 
  group_by(id, comments) %>% 
  select(id, comments, pos, pos_order, word) -> freq_comment


freq_comment %>% 
  anti_join(stopwords_ko) -> freq_comment



# write_excel_csv(freq_comment, 'comments_mecab.csv')


# 2) 감성 분석용 데이터 프레임(동사, 형용사) -------------------------------------------------------

bind_rows(v_df, adj_df)%>% 
  group_by(id, comments) %>% 
  select(id, comments, pos, pos_order, word) -> senti_comment


# 6. 감성분석 전체 데이터셋: 댓글 데이터 + 감정 부여------------------------------------------------------

# senti_comment %>% 
#   filter(word == '함부로')
# 
# dic %>% 
#   filter(word == '함부로')

# 저장
# write_excel_csv(senti_comment, '~/Desktop/senti_comment2.csv')
