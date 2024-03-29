# 피파온라인 선수 댓글 감성 분석 및 선수 추천 시스템 개발
일주일 동안 진행된 딥러닝 관련 프로젝트에서 LSTM 모델을 사용하여 피파온라인 게임내의 선수들에 대한 댓글을 감성 분석하고, 감성 점수를 기반으로 사용자에게 관련 선수를 추천해주는
시스템을 개발했다.

[프로젝트 내용 설명 영상](https://drive.google.com/file/d/1uLdRgUxn478hcxcuxs0mMADmAdti4osv/view?usp=sharing)

## 프로젝트 개요 및 필요성
- 오늘날에는 SNS나 쇼핑 플랫폼이 매우 발달해서 이미지 데이터, 자연어 데이터를 활용해 다양한 분석을 할 수 있다. 특히, 자연어 관점에서는 사용자들이 제품이나 기능에 대해 평가한 댓글 데이터를 감성 분석함으로써 소비자의 선호도를 파악할 수 있고, 파악한 정보를 기반으로 상품을 보완하고, 기능을 업데이트 시킬 수 있다. 이번 프로젝트에서는 넥슨의 축구 게임인 피파 온라인의 댓글 데이터를 감성 분석함으로써 선수를 평가한 댓글이 긍정적인지, 부정적인지를 평가하고 감성 점수를 기반으로 관련 선수를 추천해주는 시스템을 개발했다. 

- 게임회사의 관점에서는 댓글 데이터만 가지고도 선수의 선호도를 평가할 수 있어, 추후 선수를 만들 때 선수의 스탯을 더 현실적으로 보완할 수 있고, 사용자의 관점에서는 비슷하거나 관련 있는 유형의 선수를 추천받아 게임 내에서 자신의 경제적인 수준에 맞춰 선수를 구매할 수 있다는 장점이 생길 것으로 기대된다.

## 프로젝트에서 사용한 가설
1. 긍정적인 댓글과 부정적인 댓글을 이루는 단어들은 차이가 있을 것이다. 

2. 선수의 포지션에 따라 자주 사용되는 단어들은 차이가 있을 것이다.

## 프로젝트 파이프라인

1. 피파인벤 홈페이지에서 선수들의 댓글 데이터를 수집하고 csv 형태로 저장한다. (약 20만개)

2. 수집한 데이터는 R 언어를 사용하여 토큰화하고, KNU 감성사전을 기반으로 각각의 댓글마다 감성점수를 도출한다.

3. 댓글에서 도출된 감성점수가 0보다 크면 긍정, 0보다 작거나 같으면 부정으로 라벨링 한다.

4. 1과 0으로 라벨링된 변수를 종속변수(예측할 변수), 댓글 변수를 독립변수로 하여 Tensorflow에서 제공하는 LSTM 모델로 학습한다.

5. 도출된 감성 점수를 기존의 평점 점수와 결합하여 새로운 감정 점수를 만들고, 이를 기반으로 추천 시스템을 개발한다.

![image](https://user-images.githubusercontent.com/97672187/178266824-6d1fece3-59f8-4224-89a1-b28c485dcad0.png)

**기존에 존재하는 평점 점수를 사용하지 않고 감성사전을 사용하여 감성 점수를 새로 계산한 이유는?**

  -> 사용자마자 평점을 주는 기준이 매우 다르기 때문에 보다 객관적인 점수를 반영하기 위해 군산대에서 개발한 KNU 한국어 감성사전 모델을 사용하여 각 댓글에 감성점수를 매겼다. 감성 사전에는 밑의 사진처럼 단어마다 감성 점수가 저장되어 있고, 편의상 신조어와 같이 사전에 없는 단어는 모두 감성 점수를 0으로 했다. 한 댓글의 감성 점수는 댓글을 이루는 단어들의 감성 점수를 모두 합쳐서 도출했다.

<br>

<p align = 'center'>
<img src = 'https://user-images.githubusercontent.com/97672187/178268547-ce671b2d-0d05-4fa0-a10b-1a2e2379479d.png' width = '50%'>
</p>

<br>

## 데이터 설명
프로젝트에서 사용한 데이터는 피파인벤 홈페이지의 선수 댓글 카테고리에서 크롤링 했고, 20만개의 행과 사용자 ID, 선수 이름, 포지션, 댓글, 평점 점수라는 5개의 열로 이루어져있다.

<br>

<p align = 'center'>
<img src = 'https://user-images.githubusercontent.com/97672187/178269560-d559d2dd-7e90-4af4-b5da-7c633a023199.png'>
</p>

<br>

수집한 데이터를 감성 사전으로 도출한 감성 점수를 기반으로 긍정/부정으로 나눴고, 중립을 나타내는 0의 점수는 편의상 부정으로 라벨링 했다. 따라서 부정인 댓글이 긍정인 댓글보다
약 3배 이상 많은 불균형 데이터가 되었다.

<br>

<p align = 'center'>
<img src = 'https://user-images.githubusercontent.com/97672187/178269952-1a8e8a76-8f6e-4703-82d2-a3e9c15dc9df.png' width = '50%'>
</p>

<br>


## 모델링
사용한 모델은 RNN 기반 모델 중 하나인 LSTM 모델이다. LSTM은 Sequential한 Data를 처리하기에 좋은 모델로, 여기서 Sequential한 Data란 순서 정보가 보장되어야 의미가 있는 데이터이다. 문장도 단어의 순서로 이루어진 데이터이고, 단어의 순서가 바뀐다면 문법이나 의미가 달라질 수 있다. 즉, 문장은 단어들의 순서가 지켜져야하는 Sequential Data이기 때문에 순서정보를 기억하면서 데이터를 학습하는 LSTM 모델이 해당 task에 적합하다고 판단했다. LSTM은 3개의 게이트에서 이전층의 정보와 현재층의 정보, 출력층의 정보를 얼만큼 기억할 것인지 정하며 학습한다는 특징이 있다.

데이터는 토큰화 이후 ['을', '를', '구매합니다', '올렸습니다', '$', '팔렸나요', .......]와 같은 특수문자나 접속사, 거래 관련 단어, 불용어를 제거하고, 정수 인코딩, 패딩의 과정을 거쳐 LSTM 모델에 input으로 사용되었다.

1) Basic 모델

케라스에서 제공하는 Embedding 벡터를 활용하여 LSTM과 출력층만 쌓은 모델을 가지고 학습을 진행했고 정확도를 평가지표로 사용했을 때 테스트 데이터에서 약 0.88의 성능을 기록했다.

2) Randomized Search CV 이후 모델

성능을 좀 더 높이기 위해 임베딩층과 LSTM층 사이에 은닉층을 하나 더 추가했고, Randomized Search CV를 진행해서 하이퍼파라미터를 조정한 결과 테스트 데이터의 성능이 0.89로 향상되었다. 

3) Grid Search CV 이후 모델

Randomized Search CV로 하이퍼파라미터 튜닝을 위한 범위를 어느 정도 잡고, 이를 참고하여 더 세부적으로 튜닝했다. 튜닝결과 가장 좋은 성능인 0.90의 정확도를 가진 모델이 개발됐다.

-> 기존 데이터는 0의 비율이 78%였는데 이렇게 모두 0으로 예측했을 때의 경우보다 더 높은 성능을 내는 LSTM 모델(0.88)을 만들었고, Randomized Search CV(0.89)와 Grid search CV(0.90) 후에는 좀 더 개선된 모델이 만들어진것을 확인할 수 있다. 


## 선수 추천시스템
만약, 사용자가 원하는 선수를 입력했을 때 해당 선수와 같은 팀에서 뛰어본 경험이 있거나, 플레이 스타일이 유사한 선수를 추천받을 수 있다면 관련 선수를 활용함으로써 능력치 향상 또는 유저 수준에 맞는 선수를 사용할 수 있게 된다.

1. 추천시스템에 사용할 평점 데이터는 기존의 평점 데이터에 감성 점수를 반영하기 위해 먼저 감성 점수를 scaling 해주었고, 긍정단어에는 1을 더하고, 부정단어에는 1에서 scaling된 점수를 뺀 후 기존의 평점과 곱해 감성 점수가 반영된 새로운 평점 변수를 생성했다.

![image](https://user-images.githubusercontent.com/97672187/178272551-42b83218-1469-4925-ae43-44f0214d61c4.png)

2. 아이템이 되는 선수이름을 행, 사용자의 이름을 열, 새로운 평점 점수가 값이 되도록 데이터를 만들고, KNN 모델을 사용해 아이템 기반 협업 필터링을 진행했다. 아이템 간의 유사도는 코사인 유사도를 활용했고, 사용자가 입력한 선수 본인을 포함한 가장 유사도가 높은 Top 5 선수를 추천해주는 함수를 만들었다.

![image](https://user-images.githubusercontent.com/97672187/178272762-5ddac1a3-fe58-4488-8556-45c105fb8d20.png)

한국 선수인 박지성 선수를 입력했을 때는 한국 선수가, 리오넬 메시 선수를 입력했을 때는 메시와 뛰고 있거나, 뛰어본 적이 있는 선수가 추천된 것을 확인할 수 있었다.

## 결론
결론 부분에서는 발표 도입부에 세웠던 가설을 검증했다.

- 첫번째 가설인 긍정과 부정 댓글들을 이루는 단어의 차이를 WordCloud와 가장 많이 사용된 Top5 단어를 활용하여 비교해보니 차이가 존재하는 것을 확인할 수 있었고 첫번째 가설이 참이라고 할 수 있다.

![image](https://user-images.githubusercontent.com/97672187/178275807-f40b5782-6068-4f44-925c-6b83f10a1b2f.png)

- 두번째 가설인 포지션에 따라 자주 사용된 단어들의 차이도 첫번째 가설과 같은 방법으로 검정해보았다. 포지션마다 “진짜” 라는 단어가 자주 사용되었는데 만약 “진짜”라는 단어를 제외하고 비교해본다면 더 명확한 차이가 보였을 것 같다. 가장 많이 사용된 단어가 포지션마다 다른 것을 보아 두번째 가설도 참이라고 할 수 있다.

![image](https://user-images.githubusercontent.com/97672187/178276004-e5918dcb-5400-4b47-a03b-dcf80491a587.png)

## 한계점 및 해결방안
1. 같은 선수라도 시즌별로 스탯이 다르기 때문에 다양한 선수처럼 존재할 수 있는데 선수를 추천해줄 때 시즌을 고려하지 않고, 모든 시즌을 하나의 선수로 취급했다. 

    -> 시간이 좀 더 주어졌다면 시즌별로 선수를 세분화하여 추천시스템 모델을 개발함으로써 해결할 수 있을 것이다. 

2. 감성 점수를 기반으로 한 새로운 평점 점수를 만들었는데 이 점수가 논리적으로 문제가 없는지 타당성을 입증하기 힘들었다. 

    -> 감성 분석과 추천 시스템이 결합된 관련 연구를 더 찾아본다면 보다 설득력 있는 점수를 만들 수 있다. 

3. 댓글에서는 다양한 신조어들이 많이 등장하는데 감성점수를 계산한 KNU 한국어 감성 사전에 존재하지 않는 신조어들은 모두 0으로 계산해서 신조어들의 특징을 살릴 수 없었다. 

    -> 빈도수가 높은 신조어들을 10개, 20개 정도라도 감성 사전에 추가했다면 좀 더 신조어의 감성점수를 반영한 모델을 만들 수 있다.

## 개발환경
![image](https://user-images.githubusercontent.com/97672187/178277682-c5bd3670-8259-4c70-bfde-da12cb3c043b.png)

