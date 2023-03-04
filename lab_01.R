set.seed(123)

x <- round(rnorm(20, 0, 1), 2)

### zad. 1.1 ###
x[((x >- 2 & x < -1) | (x > 1 & x < 2))]

### zad. 1.2 ###
liczba_liczb_nieujemnych <- length(x[x >= 0])
liczba_wszystkich_liczb <- length(x)
odpowiedz_1_2 <- liczba_liczb_nieujemnych/liczba_wszystkich_liczb
odpowiedz_1_2

### zad. 1.3 ###
odpowiedz_1_3 <- mean(abs(x))

### zad. 1.4 ###
odpowiedz_1_4 <- c(x[abs(x)==min(abs(x))], x[abs(x)==max(abs(x))]) #wartosc minimalna, maksymalna
odpowiedz_1_4 <- c(x[which.min(abs(x))], x[which.max(abs(x))]) # lepszy sposób

### zad. 1.5 ###
odpowiedz_1_5 <- c(x[which.min(abs(x-2))], x[which.max(abs(x-2))])

### zad. 1.6 ###
odpowiedz_1_6 <- x-min(x) #zmieni liczby tak, ze beda w przedziale (0, +inf)
odpowiedz_1_6 <- odpowiedz_1_6 / max(odpowiedz_1_6)

### zad. 1.7 ###
odpowiedz_1_7 <- character(length(x))
odpowiedz_1_7[x==x] <- "ujemna"
odpowiedz_1_7[x>=0] <- "nieujemna"


### zad. 1.8 ###
odpowiedz_1_8 <- character(length(x))
odpowiedz_1_8 <- rep("duży", length(x))
odpowiedz_1_8[x < -1] <- "mały"
odpowiedz_1_8[x > -1 & x < 1] <- "średni"

### zad. 1.9 ###
odpowiedz_1_9 <- floor(x) + 1/2



### zad. 2 ###
r <- function(x, y){
  (1/(length(x)-1)) * sum((x-mean(x))/(sd(x)) * (y-mean(y))/(sd(y)) )
}

test_1_zad_2 <- r(temp<-rnorm(20, 0, 1), 10*temp+2)
test_2_zad_2 <- r(temp<-rnorm(20, 0, 1), -4*temp+1)
test_3_zad_2 <- r(rnorm(2000, 0, 1), rnorm(2000, 5, 2))