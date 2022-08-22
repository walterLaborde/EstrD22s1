#include <iostream>
using namespace std;

// proposito: devuelve 1 si se cumple la condicion, cero sino.
int unoSi(bool c){
    if(c) {
        return 1;
    }
    else {
        return 0;
    }
}

int sumarSiApareceEnPosicion(char c, string s, int largo) {
    int i = largo ;
    if(i==0) {
        return 0;
    }
    else {
        return unoSi(c==s[i]) + sumarSiApareceEnPosicion(c, s, (i-1));
    }
}

//Propósito: devuelve la cantidad de apariciones de un char c en el string s.
int apariciones(char c, string s) {
    return sumarSiApareceEnPosicion(c, s, s.length());
}

int main() {
    cout << apariciones('w', "awafwaw");
    return 0;
}