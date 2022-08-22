#include <iostream>
using namespace std;


// Propósito: imprime los números de 0 hasta n, separados por saltos de línea.
void desdeCeroHastaN(int n) {
    int ceroAN = n;
    for(int i=0; i<n; i++) {
        cout << n - ceroAN << endl;
        ceroAN--;
    }
}

int main() {
    desdeCeroHastaN(4);
    return 0;
}