#include <iostream>

using namespace std;

// proposito: Imprimir el factorial de n.
// Precondición: n >= 0
int fc(int n) {    // 3
int x = 1;         // 1     3 ,       6 ,
while(n > 0) {     // 3     2 ,       1 , 
x = x * n;         // 1*3 = 3 , 3*2 = 6 , 
n--;               // 3-1 = 2 , 2-1 = 1 ,
}
return x;
}


int main() {
    int  x = 8;
    int res = fc(x);
    cout << res << endl;
    //cout << c << endl;
}


// Proposito: Imprimir los caracteres consecutivos desde c1 hasta c2 inclusive.
// ejemplos de resultado: c1 = 'a', c2 = 'c' .... a, b, c
// Precondición: c1 < c2
void printFromTo(char c1, char c2) {
for(int i = 0; c1 + i <= c2; i++) {
cout << c1 + i << ", ";
}
cout << endl;
}

// 
// Precondición: n <= m
int ft(int n, int m) {     // 2, 4
if (n == m) {              // false
return n;                   
}
return n + ft(n+1, m);     // 2 + (3 + 4)  
}


