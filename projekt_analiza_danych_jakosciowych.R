#ładuje potrzebne pakiety
library(tidyr)
library(dplyr)
library(ggplot2)
library("aod")
library(pROC)

airline<-read.csv("C:/Users/pompa/OneDrive/Pulpit/ANALIZA DANYCH JAKOŚCIOWYCH/projekt/Invistico_Airline.csv")
#wstępny przegląd danych
summary(airline)
str(airline)
head(airline)

#zmieniam zmienne jakościowe na factor (oprócz objaśnianej satisfaction)
airline$satisfaction <- as.factor(airline$satisfaction)
airline$Gender <- as.factor(airline$Gender)
airline$Customer.Type <- as.factor(airline$Customer.Type)
airline$Type.of.Travel <- as.factor(airline$Type.of.Travel)
airline$Class <- as.factor(airline$Class)
str(airline)
##
summary(airline)

airline <- airline %>% filter(!is.na(Arrival.Delay.in.Minutes))
#rysujemy histogram
hist <- airline %>%
  select(all_of(c("Age", "Flight.Distance", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes"))) %>%
  mutate_all(as.numeric) %>%
  pivot_longer(cols = !ends_with('Satisfied'), names_to = 'Cechy') %>%
  ggplot(aes(x = value, fill = Cechy)) +
  geom_histogram(bins = 20, color = 'white', alpha = 0.7) +
  facet_wrap(~Cechy, scales = "free", ncol = 2) +  # Ustawienie facet_wrap z dwiema kolumnami
  theme_minimal() +
  labs(title = 'Rozkłady zmiennych ciągłych',
       x = 'Value',
       y = 'Count') +
  theme(legend.position = "top")

print(hist)

#usuwanie wartości NA's oraz O dla kolumn dot. odpowiedzi z ankiety (wartości które nie mają żadnego znaczenia)
airline <- airline %>% filter(Seat.comfort != 0)
airline <- airline %>% filter(Departure.Arrival.time.convenient != 0)
airline <- airline %>% filter(Food.and.drink != 0)
airline <- airline %>% filter(Gate.location != 0)
airline <- airline %>% filter(Inflight.wifi.service != 0)
airline <- airline %>% filter(Inflight.entertainment != 0)
airline <- airline %>% filter(Online.support != 0)
airline <- airline %>% filter(Ease.of.Online.booking != 0)
airline <- airline %>% filter(On.board.service != 0)
airline <- airline %>% filter(Leg.room.service != 0)
airline <- airline %>% filter(Checkin.service != 0)
airline <- airline %>% filter(Cleanliness != 0)
airline <- airline %>% filter(Online.boarding != 0)
airline <- airline %>% filter(!is.na(Arrival.Delay.in.Minutes))
#mamy 119255 obserwacji i 23 kolumny


#widzimy, że wartości zmiennych gdzie zostały uwzględnione wyniki ankiety
#zostały zakodowane jako zm. liczbowe; ponieważ nie mają one sensu liczbowego,
#potraktujemy je jako kategorie, dokonamy konwersji typu na czynnikowy
airline$Seat.comfort <- factor(airline$Seat.comfort)
airline$Food.and.drink <- factor(airline$Food.and.drink)
airline$Gate.location <- factor(airline$Gate.location)
airline$Inflight.wifi.service <- factor(airline$Inflight.wifi.service)
airline$Inflight.entertainment <- factor(airline$Inflight.entertainment)
airline$Online.support <- factor(airline$Online.support)
airline$Ease.of.Online.booking <- factor(airline$Ease.of.Online.booking)
airline$On.board.service <- factor(airline$On.board.service)
airline$Leg.room.service <- factor(airline$Leg.room.service)
airline$Baggage.handling <- factor(airline$Baggage.handling)
airline$Checkin.service <- factor(airline$Checkin.service)
airline$Cleanliness <- factor(airline$Cleanliness)
airline$Online.boarding <- factor(airline$Online.boarding)
str(airline)

#korelacje dla zm. ciągłych
helpdata <- data.frame(
  airline$Age,
  airline$Flight.Distance,
  airline$Departure.Delay.in.Minutes,
  airline$Arrival.Delay.in.Minutes
)
cor_matrix <- cor(helpdata)
print(cor_matrix)
#widzimy, że Arrival.Delay.in.Minutes and Departure.Delay.in.Minutes są ze sobą mocno powiązane, dlatego usuwamy jedna z nich

airline <- airline[, -which(names(airline) == "Arrival.Delay.in.Minutes")]

#ale usuwamy też Departure.Delay.in.Minutes ponieważ uważam, że raczej nie powinny wpływać na zmienną objaśnianą
airline <- airline[, -which(names(airline) == "Departure.Delay.in.Minutes")]

summary(airline)
#tworzymy indykator zadowolenia
airline$IsSatisfied <- ifelse(airline$satisfaction == "satisfied", 1, 0) #jeśli klient zadowolony to 1, w p.p. 0
airline <- subset(airline, select = -satisfaction)

summary(airline)

#proporcję naszej zmiennej objaśnianej
prop.table(table(airline$IsSatisfied))

#losowo wybieram 1000 obserwacji do zbioru uczącego, pozostałe do zbioru testowego
set.seed(123)
x=sample(c(1:nrow(airline)),100000)
airline_train=airline[x,]
airline_test=airline[-x,]

prop.table(table(airline_train$IsSatisfied));prop.table(table(airline_test$IsSatisfied))
#zbiory reprezentatywne


attach(airline)

#REGRESJA LOGISTYCZNA

#glm z wszystkimy zmiennymi
model1_glm=glm(IsSatisfied ~ ., family='binomial', airline_train)
s1<-summary(model1_glm)
s1
#wszystkie zmienna są istotne
#AIC: 48653


#usuwamy jedna zmienną
model2_glm=glm(IsSatisfied ~ .-Inflight.wifi.service, family='binomial', airline_train)
s2<-summary(model2_glm)
s2
#AIC: 48705
#odrobinę gorzej

##
names(airline)
model_mini_glm=glm(IsSatisfied ~ Flight.Distance+Baggage.handling+Inflight.entertainment+On.board.service+Checkin.service+Leg.room.service+Online.boarding+IsSatisfied+Inflight.wifi.service+Gate.location+Food.and.drink+Departure.Arrival.time.convenient+Seat.comfort+Flight.Distance+Class+Gender+Customer.Type+Age+Type.of.Travel, family='binomial', airline_train)
s_mini<-summary(model_mini_glm)
s_mini
names(airline)
#AIC za każdym razem gorzej





##
s<-xtabs(~IsSatisfied+Gender, data=airline)
s
ps<-prop.table(s,2)
ps
#obliczenia procentowe wskazują, że zadowolonych jest 65% kobiet oraz 43% mężczyzn
ns<-ps/(1-ps);ns # szanse zadowolenia
ns[,1]/ns[,2]    # ilorazy szans zadowolenia
#obliczony iloraz szans = 0.4 wskazuje, że szansa z bycia zadowolonym
#wśród mężczyzn (0.76) jest 0.4 razy mniejsza niż wśród kobiet (1.83)

c<-xtabs(~IsSatisfied+Class, data=airline)
pc<-prop.table(c,2)
pc
nc <- pc/(1-pc);nc # szanse zadowolenia
nc[,1]/nc[,2]      # porównanie szans Business klasy z Eco klasą
nc[,2]/nc[,3]      # porównanie szans Eco z Eco Plus klasą
##

#testujemy jeszcze inne modele, ale wartość dla AIC się powiększają

#próbujemy z interakcjami
frm2<-IsSatisfied~Class+Gender+Customer.Type+Age+Flight.Distance+
  Inflight.entertainment+Seat.comfort+Ease.of.Online.booking+Online.support+
  On.board.service+Online.boarding+Leg.room.service+Baggage.handling+
  Cleanliness+Checkin.service+Food.and.drink+Gender*Customer.Type+Gender*Age+
  Gender*Class+Class*Flight.Distance

glm(formula = frm2, family = "binomial", data = airline_train)
#AIC: 51420

#stepwise function (regresja krokowa)
s3<-step(model1_glm,direction='backward')
s3
#AIC: 48650
#podobnie jak model1

#testowanie
summary(model1_glm)
#Residual deviance:  48531
logLik(model1_glm)
#'log Lik.' -24265.35 (df=61)
#mnożymy razy -2, otrzymamy dewiancję, która pozwala ocenić istotność zmiennych uwzgl w modelu
-2*logLik(model1_glm)
#'log Lik.' 48530.7 (df=61)

with(model1_glm, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#wynik 0

# test logarytmu wiarygodności:
lmtest::lrtest(model1_glm)

#porównanie który lepszy
#oblicza statystyke testu jako różnice dewiancji obu modeli
anova(model1_glm,model2_glm,test = "Chisq")
#p-value>5%, tak jak się spodziewaliśmy 1model lepszy

# test reszt deviance:
anova(model1_glm, test="Chisq")

#ilorazy wiarygodności na podstawie oszacowanych parametrów modelu
#cbind(OR=exp(coef(model1_glm)))

#test wald'a
for( i in 1:21){
  for (j in i:21){
    w=wald.test(b = coef(model1_glm), Sigma = vcov(model1_glm),Terms=i:j)
    if (w$result$chi2[3]>0.05){
      print(w$result$chi2[3])
      print(i)
      print(j)
    }
  }
}
#funkcja nie zwróciła żadnej wartości, co oznacza, że wszystkie zmienne w modelu 1 są istotne



#po estymacji kilku modeli, który z nich wybrać? 
#takiego wyboru możemy dokonać np. na podstawie kryterium informacyjnego AIC.
#kryterium informacyjne Akaike, sugeruje żebyśmy wybrali model pierwszy z wszystkimi zmiennymi
#ponieważ charakteryzuje się najmiejszą wartością AIC
s1$aic;s2$aic;s3$aic


#przewidujemy poziomy satysfakcji za pomocą funkcji przewidywania przy użyciu testowej ramki danych
pred=predict(model1_glm,airline_test,type='response')
#pred
#zmieniamy przewidywane wartości z zakresu od 0 do 1 na dwie możliwe wartości 0 lub 1
pred_glm=ifelse(pred>0.5,1,0)
#pred_glm

#obliczymy dokładność i precyzję modelu
tab=table(airline_test$IsSatisfied,pred_glm)
tab
print(paste("The accuracy = ",(tab[1,1]+tab[2,2])/sum(tab)))
#"The accuracy =  0.897896650220722"
print(paste("The precision = ",tab[2,2]/(tab[1,2]+tab[2,2])))
#"The precision =  0.909329957148422"

#Wynik prognozy pokazuje, że dokładność i precyzja modelu wynosi około 90%,
#co jest wystarczająco wysokie, żeby zaakceptować model

#Krzywa ROC to wykres true positive rate (TPR) w stosunku do false positive rate (FPR)
#dla zbioru uczącego
train_prob = predict(model1_glm, newdata = airline_train, type = "response")
par(pty="s")
train_roc = roc(airline_train$IsSatisfied ~ train_prob, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE, col="#377eb8",lwd=2)
train_roc$auc
#Area under the curve: 0.9636

#dla zbioru testowego
test_prob = predict(model1_glm, newdata = airline_test, type = "response")
par(pty="s")
test_roc = roc(airline_test$IsSatisfied ~ test_prob, plot = TRUE, legacy.axes = TRUE, print.auc = TRUE, col="#377eb8",lwd=2)
test_roc$auc
#Area under the curve: 0.9619

#minimalna różnica

#z rysunku krzywej ROC możemy wnioskować, że nasz model nie jest daleko od idealnego
par(mfrow=c(1,2))
par(pty="s")
train_roc
test_roc

