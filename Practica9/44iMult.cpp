#include <iostream>
using namespace std;

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    int mult = 0;
    for(int i=0; i<n; i++) {
        mult = mult + m;
    }
    return mult;
}

int main() {
    cout << mult(2,3) << endl;   
    return 0;                    // siempre return 0;
}