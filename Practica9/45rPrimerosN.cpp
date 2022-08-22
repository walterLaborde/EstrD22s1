#include <iostream>
using namespace std;


void imprimirSiNoSupera(string s, int inic, int fin) {
    if(inic < fin) {
        cout << s[inic] << endl;
    }
}

void imprimirSiEstaEnRango(string s, int valorIni, int cotaSup) {
    if(cotaSup == valorIni) {
        cout << s[valorIni];
    }
    else {
        imprimirSiNoSupera(s, valorIni, cotaSup);
        imprimirSiEstaEnRango(s, valorIni+1, cotaSup);
    }
}

//Propósito: imprime los primeros n char del string s, separados por un salto de línea.
//Precondición: el string tiene al menos n char.


void primerosN(int n, string s) {
    imprimirSiEstaEnRango(s,0,n);
}

// aca tendria que hacer una busqueda del char en el string
//char impCharIndexN(int n, string s) {
//    for(i=n; )
//}

int main() {
    primerosN(3,"Perla");
    return 0;
}