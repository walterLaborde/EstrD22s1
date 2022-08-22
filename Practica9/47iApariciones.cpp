#include <iostream>
using namespace std;

//Propósito: devuelve la cantidad de apariciones de un char c en el strin s.
int apariciones(char c, string s){
    int cant = 0;
    for(int i = 0; i<s.length(); i++) {
        if(c == s[i]) {
            cant++;
        } 
    }
    return cant;
}

int main() {
    cout << apariciones('r', "fafafa");
    return 0;
}
