# Zadanie domowe 4

Zadanie dotyczy algorytmu Support Vector Machine.
Metoda będzie omówiona dokładnie na wykładzie, ale dla łaknących wiedzy zachęcam do zapoznania się z http://pyml.sourceforge.net/doc/howto.pdf.

Wykorzystaj dwa zbiory danych:

  - apartments z R-owego pakietu DALEX (w razie problemów ze strony Pythona proszę o kontakt, wrzucę ramkę danych na repo),
  - dowolny, wybrany przez siebie zbiór danych (najlepiej z co najmniej 8 zmiennymi numerycznymi).

1. Dopasuj SVM do obu zbiorów danych.
2. Sprawdź, czy zalinkowany artykuł słusznie zwraca uwagę na skalowanie danych (pamiętaj, że większość implementacji domyślnie skaluje).
3. Spróbuj zoptymalizować metodą random search najważniejsze hiperparametry tj. :
* cost,
* gamma,
* degree, 
najprościej optymalizować hiperparametry w SVM z jądrem gaussowskim, ale można też poszukać najlepszego jądra (-;.


