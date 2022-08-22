#include <iostream>
using namespace std;

// precond: n < m
void deCeroaM(int n, int m) {
    if(n==m) {
        cout << n << endl;  // caso base.. si son iguales (como yo pase 0, van a ser todos ceros)
    }
    else {
        cout << n << endl; // este elemento es siempre cero
        deCeroaM(n+1,m);   // en la recursion incremento ese cero hasta llegar a m.
    }
}

// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n) {
    deCeroaM(0, n);   
}

int main() {
    desdeCeroHastaN(4);
    return 0;
}