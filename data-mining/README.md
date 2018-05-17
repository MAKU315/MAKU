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

<p align="center">
<img height="600" src="https://github.com/MAKU315/Master-project/blob/master/data-mining/heatmap.PNG" />
</p>

### Rolling Window Regression

지정된 window만큼 regression을 실행하였다.
일정한 시간 간격마다의 변화를 확인할 수 있는 기본적인 rolling window regression 방법을 구현해 보았다. 



## Classification 분석

### data-mining SOVAL
data-mining SOVAL 분석은 multi-class logistic regression과 Softmax함수를 결합한 방법이다. 해당 방법을 R프로그램으로 구현하였다.

### multi-class data-mining
이 코드에서는 다양한 데이터 마이닝 기법을 적용하였으며, Random Sampling 방법으로 100번의 시뮬레이션을 거친 결과이다.

비교 한 모델은 다음과 같다.
#### 1. regression model : 로지스틱선형회귀모델, LASSO, Ridge, Elastic Net

Lasso와 Ridge, Elastic Net과 같은 penalty regression 같은 경우 hyperparameter가 있으며, 적절한 hyperparemeter를 찾기위해 10-fold Cross-validation을 진행하였다.

#### 2. Naive Bayes

베이즈 정리를 적용한 간단한 classifier이다. e1071 library를 사용해 구현하였다.

#### 3. Decision tree 

Decision tree 모델은 많은 라이브러리가 있으며 cart 알고리즘 이후 많은 변형이 존재한다. 사용하는 평가지수가 gini 계수, 에트로피 지수인지. 다지분류가 되는지. 등에 따라 많은 패키지가 존재하며, 지금 여 코드에는 rpart 패키지와 C50패키지를 사용했다. 적절한 깊이, 가지수 등의 hyperparameter를 설정하기 위해서 10-fold Cross-validation을 진행하였다.
 
#### 4. Random Forest

Decision tree의 앙상블을 Random Forest라고 한다.
tree의 개수, 변수의 활용 개수에 따라 Random Forest의 Performance가 달라진다.
최적의 조합을 찾기위해 Grid Search를 수행했다.


#### 5. KNN(K-Nearest Neighbors algrithm)

Clustering을 위해서 많이 사용되는 방법이지만, Classification을 위해서도 사용할 수 있다. 여기에서 k는 hyperparameter로써 몇 개까지 인접한 이웃을 하나의 Cluster로 볼건지 정해야하며 매우 중요한  hyperparameter이다. 이를 위해 최적의 K를 Cross-validation을 통해 찾았다

#### 6. SVM(Support Vector Machine)

SVM의 Overfitting을 막기위해서나, Support vector의 엄격함 정도를 설정하기 위해 여러 hyperparameter를 설정해야하며, Grid Search를 통해서 최적의 hyperparameter 조합을 찾는 것이 중요하나, 다른 모델에 비해서 계산량이 많다. 


### 모델 평가
데이터 마이닝에서 많이 사용되는 모델들을 다루었다. 
위의 모델들을 비교하기 위해서  100번의 시뮬레이션을 시행하고, 동시에 평가를 진행하기위해 RMSE(or Accuracy)값을 간단한 boxplot을 통해 시각화하였다.

다음 그림은 이 코드를 적용할 시 생성되는 결과이다.

<p align="center">
<img height="600" src="https://github.com/MAKU315/Master-project/blob/master/data-mining/boxplot.PNG" />
</p>
