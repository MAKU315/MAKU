# Text-mining

텍스트 마이닝에 관련해서 텍스트전처리와 감성분석 코드를 정리해 두었다.

##### 1. Text-mining 여행지 추천
유랑(여행 커뮤니티 국내사이트)와 트립어드바이저(여행 커뮤니티 해외사이트)에서 여행지 리뷰와 별점을 크롤링하여 
데이터로 활용하였다. 

각 리뷰의 Corpus를 Doctowordmatrix로 변환하기 위하여, 
Singular Value Decomposition방법(Latent Semantic Analysis)을 활용한 matrix를 구성하였다.

Hierarchical clustering을 활용하여, 각 여행지 리뷰들을 군집한 후 각 군집에서의 감성분석을 진행하였다.
이를 통해 우리는 여행지별 주요 키워드를 추출한 동시에 그 키워드 마다의 긍정, 부정의 요소까지도 잡아 낼 수 있었다.


##### 2. R프로그램을 활용한 Corpus(문장)을 음절로 변환

R프로그램을 활용해서 영어문장이 주어졌을때, 음절 별로 나눌 수 있도록 코드를 구성하였다.
