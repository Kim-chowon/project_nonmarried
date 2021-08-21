# II. 감성분석 --------------------------------------------------------------------

library(tidyverse)

# NULL 값이 word 확인 
# senti_comment %>%  
#   group_by(word, sentiment) %>% 
#   count(word) %>% 
#   arrange(-n) %>% 
#   select(word, sentiment)-> senti_null

# View(senti_null)


# NULL 값 데이터프레임 불러오기 

# senti_null <- read_csv(file = 'word_dic_join.csv')


# 감정사전 수정하기 ---------------------------------------------------------------

# '소름'이 사용된 댓글
# score_comment %>% 
#   filter(str_detect(comments, '그냥')) %>% 
#   select(comments)
# 
# score_comment %>% 
#   filter(str_detect(reply, '미친')) %>% 
#   select(reply)

# 감정사전 살펴보기
# dic %>% 
#   filter(word %in% c('소름', '소름이', '미친'))



# 최종 댓글 데이터 프레임 : senti_comment  ------------------------------------------

# 감정사전 수정(neg)

# 신조어 생성하기
newword <- tibble(word = c("쩐다", "핵노잼"),
                  polarity = c(2, -2))

# 사전에 신조어 추가하기
newword_dic <- bind_rows(new_dic, newword)

# 신조어 추가된 감정사전으로 감정점수 보기
word_comment_all %>%
  inner_join(dic, by = c('pos' = 'word')) %>% 
  unique()

%>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  group_by(sentence) %>% 
  summarise(score = sum(polarity)) %>%
  ungroup()
sessionInfo()

library(rJava)
install.packages('Rmecabko')


new_dic <- dic %>% 
  mutate(score = ifelse(word %in% c('바꾸다', '바뀌다', '변하다', '그르다', '따지다', '정하다', '오히려', '차라리', '충분히'), 2, score)) %>% 
  mutate(score = ifelse(word %in% c('이런', '무슨', '얼마나', '없이', '제발', '아직', '굳이'), -2, score)) %>% 
  mutate(score = ifelse(is.na(score), 0, score)) %>% 
  mutate(sentiment = ifelse(score >=1, "pos",
                            ifelse(score <= -1, "neg", "neu"))) %>% 
  select(word, score, sentiment) 


new_dic %>% 
  filter(word == '바꾸다')


sum(is.na(new_dic$sentiment))

# 수정한 사전으로 감정 점수 부여
new_word_comment <- senti_comment %>%
  left_join(new_dic, by = "word") 

# 댓글별 감정 점수 
new_score_comment <- new_word_comment %>%
  group_by(id, reply) %>%
  summarise(score = sum(polarity)) %>%
  ungroup()

new_score_comment %>%
  select(score, reply) %>%
  arrange(-score)

# 1점 기준으로 긍정 중립 부정 분류
new_score_comment <- new_score_comment %>%
  mutate(sentiment = ifelse(score >=  1, "pos",
                            ifelse(score <= -1, "neg", "neu")))


# 수정한 감정 사전 활용
new_score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)



# 신조어 감정사전 부여 -------------------------------------------------------------
df <- tibble(sentence = c("이번 에피소드 쩐다", "이 영화 핵노잼")) %>% 
  unnest_tokens(input = sentence,
                output = word, 
                token = "words",
                drop = F)

df %>% 
  left_join(dic, by = 'word') %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  summarise(score = sum(polarity))
# 아직 사전에 신조어가 없이 때문에 점수가 0으로 뜬다

# 신조어 생성하기
newword <- tibble(word = c("쩐다", "핵노잼"),
                  polarity = c(2, -2))

# 사전에 신조어 추가하기
newword_dic <- bind_rows(new_dic, newword)

# 신조어 추가된 감정사전으로 감정점수 보기
df %>%
  left_join(newword_dic, by = 'word') %>%
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>%
  group_by(sentence) %>% 
  summarise(score = sum(polarity)) %>%
  ungroup()




# 6. 자주 사용된 감정 단어 보기------------------------------------------------------

senti_comment %>%
  group_by(id, comments) %>% 
  count(sentiment)


# 7. 중립단어 제외, 긍정단어와 부정단어 막대그래프 그리기------------------------------------------------------
par(family = "AppleGothic")
theme_set(theme_gray(base_family = 'AppleGothic'))  


top10_sentiment <- senti_comment %>%
  filter(sentiment != "neu") %>% 
  count(sentiment, word) %>% 
  group_by(sentiment) %>% 
  slice_max(n, n = 10)

top10_sentiment %>%
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

# 8. 댓글 별 감정 점수 구하기------------------------------------------------------

score_comment <- senti_comment %>%
  group_by(id, comments) %>%
  summarise(score = sum(score)) %>%
  ungroup()

score_comment %>% 
  select(score, comments)


# View(score_comment)

# 9. 감정분류------------------------------------------------------


# 출력해서 살펴보기 
frequency_score <- score_comment %>%
  count(sentiment) %>%
  mutate(ratio = n/sum(n)*100)

frequency_score

# 막대그래프 그리기
frequency_score %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  scale_x_discrete(limits = c("pos", "neu", "neg"))


# 11. 감정 범주 별 빈도 구하기 ------------------------------------------------------

# 토큰화하고 두 글자 이상 한글단어만 생성
# comment <- score_comment %>% 
#   unnest_tokens(input = comments,
#                 output = word,
#                 token = 'words',
#                 drop = F) %>% 
#   filter(str_detect(word, "[가-힣]") &  # 한글 추출
#            str_count(word) >= 2)          # 두 글자 이상 추출
# ))

frequency_word <- senti_comment %>% 
  filter(str_count(word) >= 2) %>% 
  count(sentiment, word, sort = T)

frequency_word %>% 
  filter(sentiment == 'pos')

frequency_word %>% 
  filter(sentiment == 'neg')

# 자주 사용된 단어 비교 - 로그오즈비
library(tidyr)
comment_wide <- frequency_word %>%
  filter(sentiment != "neu") %>%  # 중립 제외
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = list(n = 0))

comment_wide

# 로그 오즈비 구하기
comment_wide <- comment_wide %>%
  mutate(log_odds_ratio = log(((pos + 1) / (sum(pos + 1))) /
                                ((neg + 1) / (sum(neg + 1)))))

comment_wide


