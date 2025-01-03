# Лабораторная работа №3

`Толстых М.А.  348091`

---

## Требования

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

- обязательно должна быть реализована линейная интерполяция (отрезками, link);
- настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:

  - какие алгоритмы использовать (в том числе два сразу);
  - частота дискретизации результирующих данных;

- входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- выходные данные должны подаваться на стандартный вывод;
- программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод

### Линейная интерполяция и Лагранжа 

## Ввод / вывод программы

```zsh
PS D:\fsharplab3> dotnet run  
Step is: 1
0 0
1.571 1
Linear
0.00    1.00    2.00
0.00    0.64    1.27

Not enough points for Lagrange interpolation.
3.142 0
Linear
1.57    2.57    3.57
1.00    0.36    -0.27

Not enough points for Lagrange interpolation.
4.712 -1
Linear
3.14    4.14    5.14
0.00    -0.64    -1.27

Lagrange
0.00    1.00    2.00    3.00    4.00    5.00
0.00    0.97    0.84    0.12    -0.67    -1.03

12.568 0
Linear
4.71    5.71    6.71    7.71    8.71    9.71    10.71    11.71    12.71
-1.00    -0.87    -0.75    -0.62    -0.49    -0.36    -0.24    -0.11    0.02

Lagrange
1.57    2.57    3.57    4.57    5.57    6.57    7.57    8.57    9.57    10.57    11.57    12.57
1.00    0.37    -0.28    -0.91    -1.49    -1.95    -2.26    -2.38    -2.25    -1.84    -1.11    0.00
```

```zsh
PS D:\fsharplab3> dotnet run data.csv
Linear
0.00    1.00    2.00    3.00    4.00    5.00    6.00    7.00    8.00    9.00    10.00    11.00    12.00    13.00
0.00    0.64    1.27    0.09    -0.55    -1.18    -0.84    -0.71    -0.58    -0.45    -0.33    -0.20    -0.07    0.05

Lagrange
0.00    1.00    2.00    3.00    4.00    5.00    6.00    7.00    8.00    9.00    10.00    11.00    12.00    13.00
0.00    1.00    0.82    0.11    -0.64    -1.08    -1.03    -0.44    0.59    1.81    2.79    3.00    1.71    -1.92
```

# Вывод
Были реализованы алгоритмы интерполяции линейная и Лагранжа. В рамках выполнения данной лабораторной работы я поработала с вводом и выводом в f#.