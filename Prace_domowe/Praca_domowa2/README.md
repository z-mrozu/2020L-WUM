# Zadanie domowe 2
W przypadku zmiennych jakościowych w zbiorze danych mogą wystąpić między innymi dwa problemy:
  - zbyt duża liczba unikalnych poziomów (z różnych przyczyn, od faktycznie dużej liczby możliwych poziomów po literówki),
  - wystąpienie nowych poziomów zmiennej w danych testowych/ nowych danych.
  
Dla zmiennych jakościowych o dużej liczbie unikalnych wartości ze zbioru Allegro [https://www.dropbox.com/s/360xhh2d9lnaek3/allegro-api-transactions.csv?dl=1] 
zastosuj 3 metody kodowania ze strony (http://contrib.scikit-learn.org/categorical-encoding/ (jedną, która pojawiła się na zajęciach i dwie "nowe"). Opisz wyniki. W przypadku R proszę użyj funkcji https://rdrr.io/cran/mlr/man/createDummyFeatures.html z obiema wartościami parametru "method". Zwizalizuj wynik oraz wyjaśnij czym się różnią sposoby kodowania (czemu to działa).


Przetestuj dla jednej kolumny numerycznej automatyczne uzupełnianie braków z użyciem jednej z metod: Nearest neighbors imputation, Multivariate feature imputation (https://scikit-learn.org/stable/modules/impute.html) lub w przypadku R jednej z metod z pakietu mice (https://CRAN.R-project.org/package=mice). Zwizualizuj wyniki (np. histogram).

Rozwiązania (Jupyter Notebook/Rmd + html) proszę zgłaszać przez pull request do podfolderu z imieniem i nazwiskiem.

Termin oddania: 23 III 2020 r. (do godz. 23:59)
