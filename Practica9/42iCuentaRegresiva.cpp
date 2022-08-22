#include <iostream>
using namespace std;


//Propósito: imprime los números desde n hasta 0, separados por saltos de línea.
void cuentaRegresiva(int n) {
    int cReg = n;
    for(int i=0; i<=n; i++) {
        cout << cReg << endl;
        cReg--;
    }   
}

int main() {
    cuentaRegresiva(3);
    return 0;
}