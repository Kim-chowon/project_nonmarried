# 비혼출산에 대한 대중인식- 네이버 댓글을 중심으로

### 0. 패키지 불러오기 및 라이브러리 설치 ---------------------------------------------

```r
# install.packages('tidytext')
# install.packages("multilinguer")
# install.packages('rJava')
# install.packages('tm')
# install.packages("KoNLP", repos = c("https://forkonlp.r-universe.dev","https://cloud.r-project.org"), INSTALL_opts = c("–no-multiarch"))

# install.packages('devtools')
# library(devtools)
# install_github("junhewk/RcppMeCab")
# library(RcppMeCab)

# install.packages("widyr")
# install.packages("tidygraph")
# install.packages('ggraph')
```
```r
library(tidyverse)
library(tidytext)
library(tibble)
library(rJava)
library(multilinguer)
library(KoNLP)
useNIADic()

library(tm)
library(RmecabKo)
library(widyr)

library(ggraph)
library(tidygraph)
```

### 1. 댓글데이터 불러오기 ---------------------------------------------

```r
comments <- read_csv('6조_rawdata.csv')

comments_tb <- comments$comments %>%
  str_remove_all("\\['") %>%
  str_remove_all("]") %>%
  str_remove_all("',") %>%
  strsplit("'") %>% 
  unlist() %>% 
  as_tibble_col(column_name = 'comments') %>% 
  mutate(id = 1:n()) %>% 
  select(id, comments)
```


### 2. 기본적인 전처리------------------------------------------------------
```r
comments_tb$comments <- gsub("[[:punct:]]|[a-zA-Z]", " ", comments_tb$comments)
comments_tb$comments <- gsub("[[:digit:]]", " ", comments_tb$comments)
comments_tb$comments <- gsub("[[:space:]][[:space:]]+", " ", comments_tb$comments)
## ㅋㅋㅋㅋㅋ / ㅠㅠ 와 같은 자음, 모음 삭제 
comments_tb$comments <- gsub('[ㄱ-ㅎ]+','',comments_tb$comments)
comments_tb$comments <- gsub('[ㅏ-ㅣ]+','',comments_tb$comments)
## 특수 문자 제거
comments_tb$comments <- gsub('[~!@#$%^&*()_+=?ㆍ]<>','',comments_tb$comments)
```

### 2-1. 추가 전처리 ---------------------------------------------------------------
#### 상위 빈도 단어들 참고해서 수정: 유사어 통일, 어근 통일화 등
```r
comments_tb %>%
  mutate(comments = gsub("여자", "여성", comments)) %>%
  mutate(comments = gsub("남자", "남성", comments)) %>%
  mutate(comments = gsub("여성가족부|여가부|여성부", "여성가족부", comments)) %>% 
  mutate(comments = gsub("여성혐오|여성 혐오", "여혐", comments)) %>% 
  mutate(comments = gsub("남성혐오|남성 혐오", "남혐", comments))  %>% 
  mutate(comments = str_replace(comments, '사유.*$', '사유리')) %>% 
  mutate(comments = gsub("가족 형태", "가족형태", comments)) %>% 
  mutate(comments = gsub("가족해체|가족 해체|가정 해체|가정해체", "가족해체", comments)) %>% 
  mutate(comments = gsub("애완동물|애완 동물|반려견", "반려동물", comments)) %>%
  mutate(comments = str_replace(comments, '응원.*$', '응원')) %>% 
  mutate(comments = str_replace(comments, '슈퍼맨.*$', '슈돌')) %>% 
  mutate(comments = str_replace(comments, '슈돌.*$', '슈돌')) %>%
  mutate(comments = gsub("우리 나라|우리나라|대한민국|남의 나라|남의나라|이나라|이 나라|한국 사회|한국사회", "한국", comments)) %>% 
  mutate(comments = gsub("문재앙", "문재인", comments)) %>% 
  mutate(comments = gsub("니네 나라|니네나라", "일본", comments)) %>% 
  mutate(comments = gsub("국가|행정부", "정부", comments)) %>% 
  mutate(comments = gsub("정상적 가족|정상 가족", "정상가족", comments)) %>% 
  mutate(comments = gsub("정자 기증", "정자기증", comments)) %>% 
  mutate(comments = gsub("정자 쇼핑", "정자쇼핑", comments)) %>% 
  mutate(comments = gsub("정자 은행", "정자은행", comments)) %>% 
  mutate(comments = gsub("공중파|공영방송|공영 방송|프로그램", "방송", comments)) %>% 
  mutate(comments = gsub("오르다", "올리다", comments)) %>% 
  mutate(comments = gsub("고정 관념", "고정관념", comments)) %>% 
  mutate(comments = gsub("비혼주의자", "비혼주의", comments)) %>% 
  mutate(comments = gsub("혼외자식", "혼외자", comments)) %>% 
  mutate(comments = gsub("더불어 민주당|더민당", "더불어민주당", comments))%>%
  mutate(id = 1:n()) %>% 
  select(id, comments) -> comments_tb
```

### 3. 댓글 토큰화------------------------------------------------------
```r
library(RmecabKo)

comments_tb %>%
  unnest_tokens(output = pos, 
                input = comments,
                token = pos, 
                drop = F) %>% 
  mutate(pos_order = 1:n()) -> comments_pos
```

### 4. 총 형태소 분석 데이터프레임 ----------------------------------------------------
##### nng, nnp -> 명사
##### vv -> 동사
##### va, xr -> 형용사

### 1) 명사/ 동사/ 형용사만 출력---------------------------------------------

#### 명사 
```r
comments_pos %>%
  filter(str_detect(pos, "/nng|/nnp")) %>%
  mutate(word = str_remove(pos, "/.*$")) %>% 
  mutate(word = gsub("본인|자신|개인", "자기", word)) %>%
  mutate(word = gsub("일자기", "일본인", word)) %>% 
  mutate(word = gsub("자기주의", "개인주의", word)) %>%
  mutate(word = gsub("이기|이기주의|이기주의자", "이기심", word)) %>%
  mutate(word = str_replace(word, '이기심.*$', '이기심')) %>%
  mutate(word = gsub("정자기", "정자기증", word)) %>%
  mutate(word = str_replace(word, '정자기증.*$', '정자기증')) %>%
  mutate(word = gsub("아기|자식|애기", "아이", word)) %>%
  mutate(word = gsub("장아이", "장애인", word)) %>%
  mutate(word = gsub("혼인", "결혼", word)) %>%
  mutate(word = gsub("양육", "육아", word)) %>% 
  mutate(word = str_replace(word, '레즈.*$', '레즈비언')) %>% 
  mutate(word = gsub("월급", "임금", word)) %>% 
  mutate(word = gsub("가정", "가족", word)) %>% 
  mutate(word = gsub("출산", "출생", word)) %>% 
  mutate(word = gsub("혼인", "결혼", word))%>% 
  mutate(word = gsub("아버지|애비|아비|아이비", "아빠", word)) %>%
  mutate(word = gsub("독박", "독박육아", word)) %>%
  mutate(word = gsub("애완견|애완용|애견", "반려동물", word)) %>%
  mutate(word = gsub("엄마|어머니", "엄마", word)) -> n_df


# 한글자 명사 중 유의미한 단어만 추출 
n_df%>% 
  filter(word == '돈|애')-> n_df_money

# 나머지 명사들
n_df %>% 
  filter(nchar(word) >= 2) -> n_df

# 합치기
bind_rows(n_df,n_df_money) %>% 
  group_by(id, comments) %>% 
  select(id, comments, pos, pos_order, word) -> n_df


library(stringr)

# 명사 빈도확인 
n_df %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) -> n_freq

View(n_freq)

# 감정사전 포함 단어 유무 확인
n_freq %>% 
  left_join(dic, 'word') %>% 
  mutate(score = ifelse(is.na(score), 0, score)) %>% 
  mutate(sentiment = ifelse(score >=1, "pos",
                            ifelse(score <= -1, "neg", "neu"))) -> n_freq
# 데이터 저장
# write_excel_csv(n_freq, 'noun_freq_Final.csv')

```

#### 동사
```r
comments_pos %>%
  filter(str_detect(pos, "/vv")) %>% 
  mutate(word = str_replace(pos, "/.*$", "다")) %>% 
  mutate(word = str_replace(word, '다.*$', '다')) %>% 
  anti_join(stop_words_ko, by = 'word') -> v_df

v_df %>% 
  mutate(word = gsub("키울다|키워다|키워다|키운다는다|키워도다|키운다고다|키워야다|키우다면다|키워라다|키워서다", "키우다", word)) %>% 
  mutate(word = gsub("태어난다|태어났다|태어날다|태어나다면다|태어나다는다", "태어나다", word)) %>% 
  mutate(word = gsub("커서다|자랄다|자란다|자라나다|자라날다", "자라다", word)) %>%
  mutate(word = gsub("멋지다|멋있다", "멋진", word)) %>%
  mutate(word = gsub("만드다|만든다|만든다|만드세요다", "만들다", word)) %>%
  mutate(word = gsub("가진다|가질다", "가지다", word)) %>%
  mutate(word = gsub("보여다|보인다|본다|봐도다", "보다", word)) %>%
  mutate(word = gsub("바랍니다|바래다|바란다|바랄다|바래요다|바라다", "바라다", word)) %>%
  mutate(word = gsub("바꿔다|바뀔다|바꾸다|바꿀다|바꿔야다|바꿔라다|바꿔서다", "바뀌다", word)) %>% 
  mutate(word = gsub("버려다", "버리다", word)) %>% 
  mutate(word = gsub("책임질다|책임져야다|책임져다", "책임지다", word)) %>% 
  mutate(word = gsub("해서다|해도다|해야다|해라다|해다|할다|했다|한다", "하다", word)) %>% 
  mutate(word = gsub("된다고다|됩니다|되다|된다|된다는다", "되다", word)) %>% 
  mutate(word = gsub("위한다|위해다", "위하다", word)) %>% 
  mutate(word = gsub("살아가다|살아갈다|사세요다", "살다", word)) %>% 
  mutate(word = gsub("죽다|죽일다", "죽이다", word)) %>% 
  mutate(word = gsub("부추긴다|부추겨다|부추긴다는다|부추길다|부추긴다|부추겨서다|부추키다|부추김다|부추겼다|부추겨야다", "부추기다", word)) %>% 
  mutate(word = gsub("원치다", "원치않다", word)) %>% 
  mutate(word = gsub("미친다고다|미친다는다|미친다|미친다|미쳐다|미쳤다", "미치다", word)) %>% 
  mutate(word = gsub("올린다|올린다고다|올린다는다|올린다", "올리다", word)) %>% 
  mutate(word = gsub("골라서다|골라다|골랐다", "고르다", word)) %>% 
  mutate(word = gsub("피해다", "피해주다", word))  %>% 
  mutate(word = gsub("망치다", "망하다", word)) %>%
  mutate(word = gsub("모를다|모른다|모른다|모를까다|모른다는다|모름니다|몰라도다", "모르다", word)) %>%
  mutate(word = gsub("한다는다|합니다|한다면다", "한다", word)) %>% 
  mutate(word = gsub("알려다|알리다|안다", "알다", word)) %>%
  mutate(word = gsub("키우다면다|키워라다|키워서다", "키우다", word)) %>%
  mutate(word = gsub("가지다", "갖다", word)) %>%
  mutate(word = gsub("나올다|나와다|나와서다|나왔다|나온다", "나오다", word)) %>%
  mutate(word = gsub("대한다", "대하다", word)) %>%
  mutate(word = gsub("죽일다", "죽이다", word)) %>%
  mutate(word = gsub("써다|쓸다", "쓰다", word)) %>%
  mutate(word = gsub("그럴다|그랬다|그래다|그래야다|그럼다", "그러다", word)) %>%
  mutate(word = gsub("버다", "벌다", word)) %>%
  mutate(word = gsub("만날다|만났다", "만나다", word)) %>%
  mutate(word = gsub("따라다|따라가다|따른다|따를다", "따르다", word)) %>%
  mutate(word = gsub("와다|와서다|올다", "오다", word)) %>%
  mutate(word = gsub("생길다|생긴다|생겼다", "생기다", word)) %>%
  mutate(word = gsub("길러다", "기르다", word)) %>%
  mutate(word = gsub("돌아간다", "돌아가다", word)) %>% 
  mutate(word = gsub("물다|물으면다|물어볼다", "묻다", word)) %>%
  mutate(word = gsub("어쩔다", "어쩌다", word)) %>%
  mutate(word = gsub("마시다|마라다|마세요다", "말다", word)) %>%
  mutate(word = gsub("웃다|웃긴다|웃긴다|웃겨다", "웃기다", word)) %>%
  mutate(word = gsub("줄어들다|줄어드다", "줄다", word)) %>%
  mutate(word = gsub("통한다|통해다", "통하다", word)) %>%
  mutate(word = gsub("도와다", "돕다", word)) %>%
  mutate(word = gsub("따져다", "따지다", word))  %>%
  mutate(word = gsub("인해다", "인하다", word))  %>%
  mutate(word = gsub("받아들일다|받아들여야다", "받아들이다", word)) %>%
  mutate(word = gsub("차려라다", "차리다", word)) %>%
  mutate(word = gsub("빠지다", "빼다", word))  %>%
  mutate(word = gsub("늘어나다|늘어날다", "늘다", word)) %>%
  mutate(word = gsub("늘리다|늘려다|늘려야다", "늘리다", word)) %>% 
  mutate(word = gsub("지난다", "지나다", word)) %>%
  mutate(word = gsub("떨어진다|떨어져다|떨어진다|떨어질다", "떨어지다", word)) %>%
  mutate(word = gsub("지켜다", "지키다", word)) %>% 
  mutate(word = gsub("그래도다", "그래도", word))-> v_df

# 동사 빈도확인 
v_df %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) -> v_freq
# 감정사전 포함 단어 유무 확인
v_freq %>% 
  left_join(dic, by = 'word') -> v_freq


# 데이터 저장
# write_excel_csv(v_freq, 'v_freq_Final.csv')
```

#### 형용사 
```r
comments_pos %>%
  filter(str_detect(pos, "/va|/xr")) %>%
  mutate(word = str_replace(pos, "/.*$", "")) -> adj_df

adj_df %>%   
  mutate(word = gsub("힘들|힘든데|힘드|힘든지", "힘든", word)) %>% 
  mutate(word = gsub("나쁘", "나쁜", word)) %>% 
  mutate(word = gsub("어렵", "어려운", word)) %>% 
  mutate(word = gsub("멋져요", "멋진", word)) %>% 
  mutate(word = gsub("다를|다름|달라|다릅니다|다르", "다른", word)) %>% 
  mutate(word = gsub("이쁜", "예쁜", word)) %>% 
  mutate(word = gsub("강하", "강한", word)) %>% 
  mutate(word = gsub("부끄럽", "부끄러운", word)) %>% 
  mutate(word = gsub("더럽|드럽", "더러운", word)) %>%
  mutate(word = gsub("아름다운다운|아름다운답|아름답", "아름다운", word)) %>% 
  mutate(word = gsub("올바른르|올바른른|올바르|올올바른", "올바른", word)) %>% 
  mutate(word = gsub("고맙|고마워|고마워요", "고마운", word))  %>%
  mutate(word = gsub("없", "없는", word)) %>% 
  mutate(word = gsub("좋", "좋은", word)) %>%
  mutate(word = gsub("같", "같은", word)) %>%
  mutate(word = gsub("많", "많은", word)) %>%
  mutate(word = gsub("싫", "싫은", word)) %>%
  mutate(word = gsub("낫", "나은", word)) %>%
  mutate(word = gsub("쉽", "쉬운", word)) %>%
  mutate(word = gsub("크|클|커|컸|클|큰", "크다", word)) %>%
  mutate(word = gsub("옳", "옳은", word)) %>%
  mutate(word = gsub("높", "높은", word)) %>%
  mutate(word = gsub("낮", "낮은", word)) %>%
  mutate(word = gsub("젊", "젊은", word)) %>%
  mutate(word = gsub("멀", "멀다", word)) %>% 
  mutate(word = gsub("멀다쩡", "멀쩡", word)) %>% 
  mutate(word = gsub("고르", "고르다", word)) %>% 
  mutate(word = gsub("귀엽|귀여워", "귀여운", word)) %>% 
  mutate(word = gsub("귀하", "귀한", word)) %>%
  mutate(word = gsub("급하", "급한", word)) %>%
  mutate(word = gsub("나쁠", "나쁜", word)) %>%
  mutate(word = gsub("나을", "나은", word)) %>%
  mutate(word = gsub("다른가", "다른", word)) %>%
  mutate(word = gsub("두렵", "두려운", word)) %>%
  mutate(word = gsub("무섭", "무서운", word)) %>%
  mutate(word = gsub("부러우|부러워|부럽", "부러운", word)) %>%
  mutate(word = gsub("슬프", "슬픈", word)) %>%
  mutate(word = gsub("심하", "심한", word)) %>%
  mutate(word = gsub("아파|아프", "아픈", word)) %>%
  mutate(word = gsub("아타깝", "안타까운", word)) %>%
  mutate(word = gsub("어때서", "어때", word)) %>%
  mutate(word = gsub("어떠|어떡|어떤지|어떨까|어떨지|어떻", "어떤", word)) %>%
  mutate(word = gsub("어려울", "어려운", word)) %>%
  mutate(word = gsub("어렸|어릴", "어린", word)) %>%
  mutate(word = gsub("역겹", "역겨운", word)) %>%
  mutate(word = gsub("이래|이러|이렇", "이럴", word)) %>%
  mutate(word = gsub("진정", "진정한", word)) %>%
  mutate(word = gsub("진정한한", "진정한", word)) %>%
  mutate(word = gsub("큽니다", "크다", word)) %>%
  mutate(word = gsub("편하", "편한", word)) %>%
  mutate(word = gsub("흔하", "흔한", word)) %>%
  mutate(word = gsub("희안", "희한", word)) %>%
  mutate(word = gsub("힘듭니다", "힘든", word)) %>% 
  mutate(word = gsub("씁쓸", "씁쓸하다", word)) %>% 
  mutate(word = gsub("지겹", "지겹다", word)) %>% 
  mutate(word = gsub("간단", "간단한", word)) %>% 
  mutate(word = gsub("귀찮", "귀찮다", word)) %>%
  mutate(word = gsub("황당", "황당한", word)) %>%
  mutate(word = gsub("이쁘|예쁘", "예쁜", word)) %>%
  mutate(word = gsub("멋지|멋있", "멋진", word)) %>%
  mutate(word = gsub("아무렇", "아무렇게", word)) -> adj_df


# 형용사 빈도확인 
adj_df %>% 
  filter(nchar(word) >= 2) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(-n) -> adj_freq

# 감정사전 포함 단어 유무 확인
adj_freq %>% 
  left_join(dic, 'word') -> adj_freq


# 데이터 저장
# write_excel_csv(adj_freq, 'adj_freq_Final.csv')
```


### 1) 빈도 분석/ 동시출현 네트워크용 데이터 프레임-------------------------------------------------------
```r
library(tibble)

bind_rows(n_df, v_df, adj_df)%>% 
  group_by(id, comments) %>% 
  anti_join(stop_words_ko, by = 'word') %>% 
  select(id, comments, pos, pos_order, word) -> freq_comment

write_excel_csv(freq_comment, 'comments_df_Final.csv')
```

### tf-idf 구하기 --------------------------------------------------------------
```
# 총 tf-idf
text_tf_idf <- freq_comment %>% 
  count(id, word, sort = T) %>% #count에서는 doc이 먼저
  bind_tf_idf(word, id, n) #tf-idf는 word가 먼저


text_count <- text_tf_idf %>% 
  group_by(word) %>% 
  summarize(n= sum(n, na.rm=T),
            tf_idf = sum(tf_idf, na.rm=T)) %>% 
  arrange(desc(n)) %>% 
  ungroup()
```
## II. 감성분석 --------------------------------------------------------------------

### 시기별 감성분석 ----------------------------------------------------------------
##### 시기별 감성분석은 전체 시기 감성분석 진행 후 같은 코드로 데이터프레임만 바꿔서 진행했습니다. 
```r
library(tidyverse)
senti_comment_all %>%
  filter(id <= 9666) -> senti_comment_이슈기

senti_comment_all %>%
  filter(id > 9666 & id <= 29951) -> senti_comment_정책의제1기

senti_comment_all %>%
  filter(id > 29951 & id <= 39912) -> senti_comment_정책의제2기

senti_comment_all %>%
  filter(id > 39912) -> senti_comment_입법시도기
```

### 1) 감성사전 데이터  ------------------------------------------------------------
```r
dic <- read_csv(file = '6조_senti_lexicon_final.csv') 

dic %>% 
  rename(c('no' = '...1', 'score' = 'polarity')) %>% 
  mutate(sentiment = ifelse(score >=1, "pos",
                            ifelse(score <= -1, "neg", "neu"))) %>% 
  select(word, score, sentiment) %>% 
  unique()-> dic

# 최근 단어 수정 포함 여부 확인 

dic %>% 
  filter(word == '없는')

# 최종 댓글 데이터 프레임 : senti_comment_all  ------------------------------------------

freq_comment %>% 
  left_join(dic, "word") %>% 
  mutate(score = ifelse(is.na(score), 0, score)) %>% 
  mutate(sentiment = ifelse(is.na(sentiment), "neu", sentiment)) -> senti_comment_all


# 불용어 ---------------------------------------------------------------------

stop_words_ko <- c('하다', '되다', '있다', '보다', '가다', '해다','하다', '될다', '맞다')

stop_words_ko <- as_tibble(stop_words_ko) %>% 
  rename(word = 'value')%>% 
  mutate(no = 1:n()) %>% 
  select(no, word)


senti_comment_all %>% 
  anti_join(stop_words_ko, by = 'word') -> senti_comment_all

write_excel_csv(senti_comment_all, "~/Desktop/khuproject/6조최종전처리데이터프레임.csv")

```

###  댓글별 감정 점수  -------------------------------------------------------------
```r
comment_score_all <- senti_comment_all %>%
  group_by(id, comments) %>%
  summarise(score = sum(score)) %>%
  ungroup()

# 내림차순 정렬
comment_score_all %>%
  select(id, comments, score) %>%
  arrange(-score) 


# 1점 기준으로 긍정 중립 부정 분류
comment_score_all <- comment_score_all %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                            ifelse(score <= -1, "neg", "neu"))) %>% 
  arrange(-score) 


# 감정 분류 백분율 
comment_score_all %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)
```

### 6. 중립단어 제외, 긍정단어와 부정단어 막대그래프 그리기------------------------------------------------------
```r
par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))  


top10_sentiment_all <- senti_comment_all  %>%
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 8)


top10_sentiment_all %>%
  ggplot(aes(
    x = reorder(word, n),
    y = n,
    fill = sentiment
  )) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  facet_wrap( ~ sentiment, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = NULL)
```


### 7. 댓글 별 감정 점수 구하기------------------------------------------------------
```r
score_comment <- senti_comment_all %>%
  group_by(id, comments) %>%
  summarise(score = sum(score)) %>%
  ungroup()

score_comment %>% 
  select(score, comments)


# View(score_comment)
```

### 8. 감정분류------------------------------------------------------
```r
# 출력해서 살펴보기 
frequency_score_all <- comment_score_all %>%
  count(sentiment) %>%
  mutate(ratio = round(n/sum(n)*100, 2))

frequency_score_all

# 막대그래프 그리기
frequency_score_all %>%
  ggplot(aes(x = sentiment, y = ratio, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = ratio), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))
```

### 9. 감정 범주 별 빈도 구하기 ------------------------------------------------------
```r
frequency_word <- senti_comment_all %>% 
  filter(str_count(word) >= 2) %>% 
  count(sentiment, word, sort = T)

frequency_word %>% 
  filter(sentiment == 'pos')

frequency_word %>% 
  filter(sentiment == 'neg')
```

## III. 시기별 네트워크 분석 --------------------------------------------------------
##### 시기별 감성분석은 전체 시기 감성분석 진행 후 같은 코드로 데이터프레임만 바꿔서 진행했습니다. 
```r
senti_comment_all %>%
  filter(id <= 9666) -> senti_comment_이슈기

senti_comment_all %>%
  filter(id > 9666 & id <= 29951) -> senti_comment_정책의제1기

senti_comment_all %>%
  filter(id > 29951 & id <= 39912) -> senti_comment_정책의제2기

senti_comment_all %>%
  filter(id > 39912) -> senti_comment_입법시도기
```

### 1. 동시 출현 네트워크 (Co-occurrence network) --------------------------------------
```r
# pairwise_count(
#   tbl = 대상 데이터,
#   item = 갯수를 새어야 할 컬럼,
#   feature = 함께 출현했다고 판단할 단위 그룹,
#   sort = 출현 횟수 단위로 정렬할지
# )


# 특정 단어와 자주 함께 사용된 단어 살펴보기
# pair %>% filter(item1 %in% c("여성가족부", "정부"))
# pair %>% filter(item1 == "비혼")
# pair %>% filter(item1 == "다양")
# pair %>% filter(item2 == "여성가족부")


# 네트워크 그래프 데이터 만들기

library(ggraph)
library(tidygraph)
senti_comment_이슈기

senti_comment_정책의제1기 <- as.tibble(senti_comment_정책의제1기)
pair_정책의제1기 <- senti_comment_정책의제1기 %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)


pair_정책의제1기 %>% 
  filter(item1 == '대리모') 


par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))  



graph_comment_정책의제1기 <- pair_정책의제1기 %>%
  filter(n >= 100) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))


# 네트워크 그래프 함수 만들기
word_network <- function(x) {            
  ggraph(graph_comment_정책의제1기, layout = "fr") +   # 레이아웃
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
word_network(graph_comment_정책의제1기)
```


### 3. 네트워크 그래프 데이터에 연결 중심성, 커뮤니티 변수 추가하기 --------------------------------------
```r
set.seed(1234)
graph_comment_정책의제1기 <- pair_정책의제1기 %>%
  filter(n >= 150) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree() , # 연결 중심성/ eigen
         group = as.factor(group_infomap())) # 커뮤니티
```


### 3-1. 네트워크 그래프에 연결 중심성, 커뮤티니 표현하기 ---------------------------------------------
```r
set.seed(1234)

ggraph(graph_comment_정책의제1기, layout = "fr") +    # 레이아웃
  geom_edge_link(color = "gray50",         # 엣지 색깔
                 alpha = 0.5) +            # 엣지 명암
  geom_node_point(aes(size = centrality,   # 노드 크기 = centrality  
                      color = as.factor(group)),      # 노드 색깔
                  show.legend = F) +        # 범례 삭제
  scale_size(range = c(3, 8)) +           # 노드 크기 범위
  geom_node_text(aes(label = name),        # 텍스트 표시
                 repel = T,                # 노드밖 표시
                 size = 5,                  # 텍스트 크기
                 family = "AppleGothic") + # 폰트
  theme_graph()                            # 배경 삭제

```

### 4. 네트워크의 주요 단어 살펴보기 --------------------------------------------------------
```r
graph_comment_정책의제1기 %>%
  filter(name == "아이")
```

### 5. 같은 커뮤니티로 분류된 단어 살펴보기 ----------------------------------------------------
```r
graph_comment_정책의제1기 %>%
  filter(group == 1) %>%
  arrange(-centrality) %>%
  data.frame()

graph_comment_정책의제1기 %>% 
  group_by(group) %>% 
  arrange(-centrality)

graph_comment_정책의제1기 %>%
  arrange(-centrality) %>%
  data.frame() -> graph_comment_정책의제1기_centrality

unique(graph_comment_정책의제1기_centrality$group)
```

### 6. 연결 중심성이 높은 주요 단어 살펴보기 ---------------------------------------------------
```r
graph_comment_정책의제1기 %>%
  arrange(-centrality)
```

### 6-1. 연결 중심성이 높은 주요 단어 살펴보기 ---------------------------------------------------
```r
# 2번 커뮤니티로 분류된 단어
for (i in 1:13){
  graph_comment_정책의제1기 %>%
    filter(group == i) %>%
    arrange(-centrality) %>%
    data.frame() %>% 
    print()}
```

### 7. 주요 단어가 사용된 원문 살펴보기 ------------------------------------------------------
```r
# 다양한 과 형태 두 단어가 쓰인 원문 보기
senti_comment %>%
  filter(str_detect(comments, "아이") & str_detect(comments, "경제")) %>%
  select(comments)
```


### 단어 간 상관 분석 (Phi coefficient) --------------------------------------------
```r
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
```

### 8. 파이 계수 구하기 ---------------------------------------------------------------
```r
word_cors <- pos_comment %>% 
  add_count(word) %>% 
  filter(n >= 25) %>% 
  pairwise_cor(item = word,
               feature = id,
               sort = T)

```

### 8-1 특정 단어와 관련성이 큰 단어 살펴 보기 
```r
word_cors %>%
  filter(item1 == "다양") 
word_cors %>%
  filter(item1 == "사유리") 


# 파이 계수로 막대 그래프 만들기 -------------------------------------------------------

# 관심 단어 목록 생성
target <- c("비혼", "사유리")


top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 10)

word_cors %>% 
  arrange(-correlation) 
```

### 8-2. 파이 계수로 막대 그래프 만들기 -------------------------------------------------------
```r
library(ggplot2)

top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 5)

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
```

### 8-3. 파이 계수로 네트워크 그래프 만들기 -----------------------------------------------------
```r
# 네트워크 그래프 데이터 만들기. 연결 중심성과 커뮤니티 추가하기

set.seed(1234)

graph_cors <- word_cors %>%
  filter(correlation >= 0.2) %>%
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

```








