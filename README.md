# Metoda Elementów Skończonych: mini-projekt

Treść zadania w pliku [zadanie_obliczeniowe](zadanie_obliczeniowe.pdf).
Moim problemem obliczeniowym jest problem potencjału grawitacyjnego (4.4).

Na rozwiązanie składają się obliczenia zapisane w dokumencie [obliczenia.pdf](obliczenia.pdf) (kod źródłowy w tak samo nazwanym [pliku .tex](obliczenia.tex)),
a także implementacja generowania i rozwiązywania otrzymanego układu równań wraz z wyrysowaniem wykresu przybliżonej szukanej funkcji dokonana w języku R - plik [implementacja.R](implementacja.R).

Projekt oceniony na 49/50 punktów. Jeden punkt stracony ze względu na rozważanie kwadratury na przedziale całego elementu, zamiast na dwóch jego połowach. Na przedziałach połowicznych każdego elementu funkcje bazowe są liniowe, a zastosowana kwadratura daje dokładne wyniki (bo dzieje się tak tylko dla wielomianów odpowiedniego stopnia). Rozważając całki po całych elementach (a tak zrobiłem) w ogólności wyniki nie muszą być poprawne. W tym szczególnym przypadku wyniki są poprawne, ale nie udowodniłem tego w opracowaniu.
