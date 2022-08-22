#include<iostream>
using namespace std;

//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.

void primerosN(int n, string s) {
    for(int i=0; i<n; i++) {
        cout << s[i] << endl;
    }
}

int main() {
    primerosN(3,"Perla");
    return 0;
}