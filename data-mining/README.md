# Data-Mining
이 폴더에는 크게 두 가지 분류로 나누어진다.
1. Time-series 관련 분석
2. Classification


## Time-series 분석
Rolling Window Reg을 적용한 분석방법에 대한 예시와
ARIMA 모형에 대한 예시가 있다.

### ARIMA 모형
ARIMA 모형같은 경우 R 통계 프로그램에서는 auto.arima 라는 라이브러리 패키지가 존재하는데, 본 코드에서는 auto.arima가 실제로 적절한 AR, MA, differencing의 최적의 order를 찾아주는지에 대한 분석이다.

auto.arima 함수와 Grid Search 기반에 arima 추정 모델을 비교해 보았으며, 그 결과 auto.arima 방법은 Grid Search 방식 훨씬 낮은 Performance를 보였다.

### heatmap(visualization).R 
코드로 해당 Performance를 시각화 해보았으며, 빨강색 일수록 좋은 Performance, 파랑색일수록 낮은 Performance를 가지는 hyperparameter 조합 지역이다. 

![heatmap]("https://github.com/MAKU315/practice-github/heatmap.PNG")
Format: ![https://github.com/MAKU315/practice-github/heatmap.PNG](url)
