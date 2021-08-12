
library(tidyverse)
getwd()

# 0. 데이터 불러오기 ---------------------------------------------
comments_tb <- read_csv('comments_2021.07.03-08.11.csv', show_col_types = FALSE)
head(comments_tb)


# 1. library 설치 ---------------------------------------------
# install.packages('tidytext')
# install.packages("multilinguer")
# install.packages('rJava')
# install.packages('tm')

# install.packages("KoNLP", repos = c("https://forkonlp.r-universe.dev","https://cloud.r-project.org"), INSTALL_opts = c("–no-multiarch"))

library(tidytext)
library(rJava)
library(multilinguer)
library(KoNLP)

useNIADic()

library(tm)


# 2. 텍스트 전처리---------------------------
## 기호 or 영어 삭제하고 공백(스페이스)으로 변경
comments_tb$comments <- gsub("[[:punct:]]|[a-zA-Z]", " ", comments_tb$comments)
## 공백(스페이스) 두 칸 이상은 한 칸으로 수정
comments_tb$comments <- gsub("[[:space:]][[:space:]]+", " ", comments_tb$comments)
## ㅋㅋㅋㅋㅋ / ㅠㅠ 와 같은 자음, 모음 삭제 
comments_tb$comments <- gsub('[ㄱ-ㅎ]+','',comments_tb$comments)
comments_tb$comments <- gsub('(ㅜㅠ)+','',comments_tb$comments)
## 특수 문자 제거
comments_tb$comments <- gsub('[~!@#$%^&*()_+=?]<>','',comments_tb$comments)

## 줄임말 
comments_tb %>%
  mutate(comments = gsub("여자", "여성", comments)) %>%
  mutate(comments = gsub("남자", "남성", comments)) %>%
  mutate(comments = gsub("여성가족부|여가부", "여성가족부", comments)) -> comments_tb


## 결측치 제거
comments_tb$comments <- na.omit(comments_tb$comments)
## 확인 
head(comments_tb)

# 한글 띄어쓰기 오류 행 있음 


# 불용어 처리


# 형태소 중심의 unnest_tokens ---------------------------
comments_tb %>%
  unnest_tokens(pos, 'comments', SimplePos09) %>%
  mutate(pos_order = 1:n()) -> pos_data  # 결과물의 순서보장을 위해서

# 확인 
head(pos_data)

# 불용어 제거
# 1) 명사
pos_data %>%
  filter(str_detect(pos, "/n")) %>% # 체언만 찾기
  mutate(pos_done = str_remove(pos, "/.*$")) %>% 
  select(-pos)-> pos_result

# 두글자 이상 단어만 추출 
pos_result %>% 
  filter(nchar(pos_done) >= 2) -> pos_result

# 2) 용언
# pos_data %>%
#  filter(str_detect(pos, "/p")) %>%
#  mutate(pos_done = str_replace_all(pos, "/.*$", "다")) -> p_done

# p_done

# 명사와 용언 데이터 합치기 

#bind_rows(n_done,p_done) %>%
#  filter(nchar(pos_done) > 1) %>% 
#  select(id, pos, pos_done) %>% 
#  group_by(id) -> pos_result # 명사, 동사, 형용사만 남음

View(pos_result)

# 3. 시각화 ---------------------------
# 1) 단어 빈도 수 

pos_result_count <- pos_result %>%
  ungroup() %>% # 사용자별 그룹 해제
  count(pos_done, sort = T) # sort = T는 내림차순
  

