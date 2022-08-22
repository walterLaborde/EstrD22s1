#include <iostream>
using namespace std;

//Propósito: realiza la multiplicación entre dos números (sin utilizar la operación * de C++).
int mult(int n, int m) {
    if(n==0) {
       return 0;
    }
    else {
        return m + mult (n-1, m);
    }
}

int main() {
    cout << mult(2,3) << endl;   
    return 0;                    // siempre return 0;
}