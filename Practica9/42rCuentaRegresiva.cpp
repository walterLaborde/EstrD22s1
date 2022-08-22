#include <iostream>
using namespace std;


//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    if(n==0) {
        cout << n;             // caso base
    }
    else {
        cout << n << endl;     // el elemento que falta
        cuentaRegresiva(n-1);  // el caso recursivo
    } 
}

int main() {
    cuentaRegresiva(3);
    return 0;
}