# Zadanie domowe 2

W realizacji zadania skorzystaj ze zbioru Allegro [https://www.dropbox.com/s/360xhh2d9lnaek3/allegro-api-transactions.csv?dl=1] 
Zadanie składa się z dwóch części:
- kodowanie zmiennych kategorycznych 

Wykonaj target encoding dla zmiennej it_location (+ komentarz jaka jest przewaga tej metody nad one-hot encoding). Jako target traktujemy kolumnę price (będzie to więc zadanie regresji). 
Zastosuj trzy metody encodingu (one-hot + "dwie nowe") dla kolumny main_category. "Nowe metody" proszę wybrać spośród wymienionych na stronie http://contrib.scikit-learn.org/categorical-encoding/. W przypadku gdy użyta metoda nie działa proszę o stosowną adnotację. Opisz wyniki.

W przypadku R proszę użyj funkcji https://rdrr.io/cran/mlr/man/createDummyFeatures.html z obiema wartościami parametru "method". Zwizalizuj wynik oraz wyjaśnij czym się różnią sposoby kodowania (czemu to działa).

- uzupełnianie braków

W tej części zadania traktujemy zmienną price nie jak target a zmienną objaśniającą. Zbiór danych ograniczamy do zmiennych numerycznych tj. price, it_seller_rating i it_quantity. 
Proszę losowo usunąć 10% wartości ze zmiennej it_seller_rating i je uzupełnić z użyciem jednego z automatycznych narzędzi: Nearest neighbors imputation lub Multivariate feature imputation (https://scikit-learn.org/stable/modules/impute.html). Następnie należy porównać wartości imputowane z oryginalnymi (polecam miarę RMSE). Eksperyment powtórzyć 10 razy i zobaczyć jakie będzie odchylenie standardowe wyniku. Następnie zrobić analogiczną analizę gdy oprócz losowego usuwania 10% wartości z kolumny it_seller_rating usuniemy także losowo 10% ze zmiennej it_quantity. (w przypadku problemów wydajnościowych proszę ograniczyć liczbę rekordów). Opisać wnioski z analizy jakości imputacji i umieścić podsumowujący wykres.

W przypadku korzystania z języka R należy użyć jedną z metod z pakietu mice (https://CRAN.R-project.org/package=mice). 

Rozwiązania (Jupyter Notebook/Rmd + html) proszę zgłaszać przez pull request do podfolderu z imieniem i nazwiskiem.

Termin oddania: 23 III 2020 r. (do godz. 23:59)
