#Interpreter 정리

## 1장
* 초보자가 인터프리터를 작성하려면 작은사양으로 만드는게 좋다
* 정석에서 배운다
* 이해하기 쉬운 코드로 만든다
* 전체 구성부터 코딩하고 조금씩 상세화 한다

#### 이책에서 가정한 실행 환경
- 방식은 가상 스택머신이고 각각에 적합한 메모리 배열을 선언해 환경을 표현한다

## 2장 문법표현
1. 가장 먼저 해야 할 일은 **어떠한 사양으로 할 것인지** 생각 하는 일이다  

이런 표현에는 구문도표를 사용한다.(그림으로 표현한 로직)
구문도표 표현은 이해하기 쉽지만 그림이므로 수정이 까다롭고 텍스트 파일로 저장할 수가 없다 그래서 보통 문자로만 이론을 표현하는 방법이 고안되었고 그것이 바로 BNF(Backup-Naur Form)이다.

[구문도표, BNF](http://booolean.tistory.com/295)

* 확장 BNF 표기법
{} 는 0번 이상 반복을 의미한다

BNF표기법
<식별자> -> <영문자>|<식별자><영문자>|<식별자><숫자>
확장 BNF
<식별자> -> <영문자>{ <영문자> | <숫자> }

새로운 언어를 개발할 때 언어 사양을 BNF표기법이나 구문 도표로 새로 설계하는 것은 부담스럽다 기능이 풍부한 구문 설계는 시간이 걸리고 구문에 모순이 없는지 검증하기도 쉽지 않다 그런 때는 이미 잘 알려진 언어 사양을 활용하는 것이 편리하다 구체적인 예로 C언어 방식이 좋을 때는 C를 모델로 하고 Basic 방식이 좋을때는 Basic을 모델로 하는게 현실적이다
* C의 언어사양을 그대로 따른다
* C에서 불필요한 기능을 삭제하고 독자적인 편리한 기능을 추가한다

## 3장 어휘분석
#### 단어와 토큰
인터프리터는 소스 프로그램을 읽어들이면 우선 구문으로써 의미가 있는 최소 구성 단위로 분할한다. 이 의미가 있는 최소 단위를 가리켜 단어 또는 토큰이라고 한다 토큰에는 다음과 같은 종류가 있다
* 키워드       - Int for while 등
* 식별자       - 변수명, 함수명 등
* 상수        - 100 23.45 등
* 문자열 리터럴  - "abcd" 등
* 연산자       - + - < <= 등
* 분리자       - [] () 등

소스 프로그램의 문자 요소를 단어로 분할하는 것을 어휘 분석이라고 하고 어휘 분석을 실행하는 것을 어휘 분석 루틴이라고 한다

```scala
siz = bias + (dt1 - dt2) * 30
```
위 코드는 다음 토큰으로 분할된다 공백이 들어갈 수 있는 위치라면 공백은 몇 개가 들어 있어도 마찬가지다
```kotlin
siz, =, bais, +, (, dt1, -, dt2, ), *, 30
```
이 토큰과 단어라는 용어는 영어 표현과 번역 표환 관계에 있지만 채겡 따라서는 구분해 사용 할 때도 있다 그런 때 **토큰은 생성된 종류별 정보 전체 단어는 규칙과 일치하는 문자열 그 자체(의미값)** 로 사용하는 것이 보통이다
예를들어 토콘의 종류로 If, While, VarName, IntNum이 있다고 하자 이를 이용하면 다음과 같이 구별된다
```kotlin
If      if
while   while
VarName siz, dt1
IntNum  100, 200
```

#### 어휘 분석 루틴의 역할
어휘 분석 루틴은 토큰을 추출할 뿐 아니라 다음 역할도 하고 있다
1. 주석제거
2. 행 바꿈을 인식해서 행 수를 관리한다.(오류 메시지의 행 번호 지정에 이용한다.)
3. 구분 역할이 끝난 공백 문자를 삭제한다
4. 토큰을 추출하고 종류 등의 정보를 설정한다

컴파일러의 어휘분석 루틴은 일반적으로 다음 순서와 같다
```kotlin
statement() 
expression() 
nextTkn()
```
statement(), expression()은 구문분석을 의미하고 nextTkn()은 어휘 분석을 의미한다

이 책에서 만드는 것은 인터프리터다 인터프리터도 앞의 그림처럼 처리할 수 있지만, statement을 실핼할 때마다 소스 문자열을 어휘 분석하는 것은 효율적이지 않다. 그래서 인터프리터일 때는 다음처럼 구성하는 것이 효율적이다
1. 소스 프로그램을 읽어들인다
2. 어휘 분석을 하고 단축 기호화한 내부 코드로 변환해둔다
3. 내부 코드에서 토큰을 추출하면서 구문 분석과 실핼을 한다

토큰을 추출한다는 의미에서 2와 3은 같은 작업이지만 2에서 단축 기호화하고 처리에 필요한 관련 정보를 심어둠으로써 3단계에서의 토큰 추출이 빨라지게 되는 것이다
```kotlin
//소스 프로그램에서 토큰 추출
convert()     - 선두에 출현하는 토큰을 분석
convert_rest()- 나머지 토큰을 분석
nextTkn()     - 소스 프로그램에서 다음 토큰을 추출한다

// 내부 코드에서 토큰 추출
statement()
expression()
nextCode()
```

#### 주석 제거
이 책에서는 소스 프로그램의 어휘 분석은 nextTkn()함수에서 한다 이 함수 안에서는 한 문자씩 판정하면서 토큰을 조립한다. 

#### 문자 종류표 설정
배열을 준비해 문자의 종류 정보를 저장해두면 편리하다.
```C
TknKind ctype[256]
```
이렇게 문자 종류표를 설정해두면 
```java
switch (ctype[ch]) {
    case Letter : // 영문자나 언더바
    ...
    case Digit :  // 숫자
}
```

isxxx를 함수가 편리하다면 문자 종류표를 만들지 않고 사용해도 상관없다. 표현력 정도에 따라 구분해 사용하자

#### 토큰 추출
적절한 단어를 추출하면 그 문자열과 토큰의 종류를 정숫값으로 설정해둔다 또한 수치일 때는 그 값도 저장해 둔다 다음은 토큰 정보를 표현하는 구조체의 예다
```C
struct Token {
    TknKind kind
    String  text
    double  dvlVal
}
```

실제 토큰이 어떻게 koten 구조체에 저장되는 예를 들어보자.

|Token 내용| 키워드 | 키워드 | 식별자 | 리터럴 | 리터럴 |리터럴 |
|---------|------|------|-------|------|-------|-----|
|ㅇ | while | <= | data1 | 1234 | 45.67 | "abced"|

