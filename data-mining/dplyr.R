library(dplyr)
install.packages("nycflights13")
library(nycflights13)
library(ggplot2)

dim(flights) # 336776 19


# row select
dplyr::filter(flights,month == 1,day ==1) 
flights[flights$month == 1 & flights$day == 1, ] # 와 같음

arrange(flights, year, month, day) # 정렬
arrange(flights, dep_delay) # 정렬 - 원 데이터는 변경 X


arrange(flights, desc(arr_delay)) # to order a column in descending order:


# col select
select(flights, year, month, day)
select(flights, year:day) # python 문과 동일
select(flights, -(year:day)) # 제외
?select

# select와의 연계
iris <- as_tibble(iris) # so it prints a little nicer
# 옵션에 들어가는 글 중 start with 앞자리로 배치 
select(iris, starts_with("Petal")) 
# 옵션에 들어가는 글 중 end with 뒷자리로 배치 
select(iris, ends_with("Width"))

# Move Species variable to the front
select(iris, Species, everything())

df <- as.data.frame(matrix(runif(100), nrow = 10))
df <- as_tibble(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])

select(df, V4:V6) # 위치 기반
select(df, num_range("V", 4:6)) # 변수 네이밍의 숫자 기반

# select & rename
select(iris, petal_length = Petal.Length)
# rename
rename(iris, petal_length = Petal.Length)

##########
# 바꿀 이름 = 이전 이름
select(flights, tail_num = tailnum)

########################################
########################################
## Add new columns with mutate()
View(mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
))

# mutate()는 만든 col 바로 사용 가능
select(mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
), gain,gain_per_hour)

# 새 변수만 유지
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

########################################
########################################
## Summarise values with summarise()
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

sample_n(flights, 10) # sample _ obs
sample_frac(flights, 0.01) # sample 비율
# option replace = T


by_tailnum <- group_by(flights, tailnum)
View(by_tailnum)
View(arrange(by_tailnum,tailnum))

by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
delay
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

# n(): the number of observations in the current group

# n_distinct(x):the number of unique values in x.

# first(x), last(x) and nth(x, n) 
# these work similarly to x[1], x[length(x)], and x[n] 
# but give you more control over the result if the value is missing.
# 결측값 다루기 좀 더 수월 

destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

# dest 그룹 내 > 비행기 종류 수 >  총 비행기 수

########################################
########################################
## multiple gruop
daily <- group_by(flights, year, month, day) 
(per_day   <- summarise(daily, flights = n())) # roll-up
(per_month <- summarise(per_day, flights = sum(flights)))  # roll-up
(per_year  <- summarise(per_month, flights = sum(flights)))  # roll-up

########################################
########################################
## indexing
select(flights, year)
select(flights, 1)

year <- 5
select(flights, year) # 외부 입력 값이 아닌 tibble의 데이터 변수가 적용 됨
select(flights, year:day)

year <- "dep"
select(flights, starts_with(year)) # option 에서는 외부 입력값 사용

rm(year)
select(flights, starts_with(year)) # 외부 입력값 없는 경우 오류

year <- 5
select(flights, year, identity(year)) # 
select(flights, year, 5) # 

vars <- c("year", "month")
select(flights, vars, "day")

# explicit method we use the dplyr !! verb
flights$vars <- flights$year
View(flights) # vars 변수 추가
select(flights, !! vars) # !! vars의 외부 입력값으로 인식

# vignette("programming") # custom funciton 관련 더 자세한 내용 

df <- select(flights, year:dep_time)
mutate(df, "year", 2) # mutate 는 변수 생성 & expect the col vectors

mutate(df, year + 10) # year : vector로 인식

var <- seq(1, nrow(df))
mutate(df, new = var)

dplyr::arrange(group_by(df, dep_time),desc(day))

########################################
## group_by 는 mutate와 유사 = 변수를 추가 및 변경한다.
group_by(df, month = as.factor(month))
group_by(df, day_binned = cut(day, 3)) # cut : 나누기 - 3 구간으로 나눈 범주로 할당


group_by(df, "month") # month 를 만들어 버림


########################################
## group_by + select 성질 : group_by_at
df
dplyr::group_by_at(df, vars(year:day))
dplyr::group_by_at(df, vars(year))


########################################
########################################
## Piping
a1 <- group_by(flights, year, month, day)
a2 <- select(a1, arr_delay, dep_delay)
a3 <- summarise(a2,
                arr = mean(arr_delay, na.rm = TRUE),
                dep = mean(dep_delay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)
a4

filter(
  summarise(
    select(
      group_by(flights, year, month, day),
      arr_delay, dep_delay
    ),
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ),
  arr > 30 | dep > 30
)


# use pipline %>%
flights %>%
  group_by(year, month, day) %>%
  select(arr_delay, dep_delay) %>%
  summarise(
    arr = mean(arr_delay, na.rm = TRUE),
    dep = mean(dep_delay, na.rm = TRUE)
  ) %>%
  filter(arr > 30 | dep > 30)


################################################################################
################################################################################
################################################################################
################################################################################
## tidyr

install.packages("tidyverse")
library(tidyr)
## https://tidyr.tidyverse.org/
stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
#####################################
#####################################
## gather : wide data를 tidy data로 변환
#  takes multiple columns, and gathers them into key-value pairs: it makes “wide” data longer.

gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)

mini_iris <- iris[c(1, 51, 101), ]
gather(mini_iris, key = flower_att, value = measurement,
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
# same result but less verbose
gather(mini_iris, key = flower_att, value = measurement, -Species)
gather(mini_iris, key = flower_att, value = measurement)
# 제외를 시켜야 해당 변수를 기준으로 사용

mini_iris <-
  iris %>%
  group_by(Species) %>%
  slice(1) # 해당 index의 obs 추출 -> 그룹 내의 첫 번째 obs 추

#####################################
#####################################
## spread : tidy data를 wide data로 변환
#   takes two columns (key & value), and spreads into multiple columns: it makes “long” data wider.
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks
stocksm <- stocks %>% gather(stock, price, -time) # time 기준 , 변수 stock, 값 price
stocksm
stocksm %>% spread(stock, price) # 변수 stock, 값 price > wide data로 변환 
stocksm %>% spread(time, price)
