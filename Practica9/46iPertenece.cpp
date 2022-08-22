#include <iostream>
using namespace std;

// proposito: suma uno al contador si el char se encuentra en el string
int contador(char c, string s) {
    int contador = 0;
    for(int i = 0; i<s.length(); i++) {
        if(c == s[i]) {
            contador++;
        }
    }
    return contador;
}

//Propósito: indica si un char c aparece en el string s.
bool pertenece(char c, string s) {
    //if(contador(c, s) == 0) {
    //    return false;
    //}
    //else {
    //    return true;
    //}

    //for(int i=0; i<s.length(); i++) {
    //    if(c==s[i]) {
//
    //    }
    //}
    int i = 0;
    //bool pertenece = c==s[i];
    while(c != s[i] || i+1 != s.length()) 
        i++;
    return c == s[i];
}

int main() {
    string s = "Perla";
    cout << pertenece('m', "Perla");
    return 0;
}


