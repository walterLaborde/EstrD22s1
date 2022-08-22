#include <iostream>
using namespace std;

// proposito: indica si el char esta en la cadena
bool estaEnLaCadena(char c, string s, int largo) {
    int i = largo;
    return c == s[i] || estaEnLaCadena(c,s,(i-1));
}

//Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s){
    return estaEnLaCadena(c,s,s.length());
}

int main() {
    cout << pertenece('w', "rere");
    return 0;
}