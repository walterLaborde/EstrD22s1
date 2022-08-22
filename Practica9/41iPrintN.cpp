#include <iostream>
using namespace std;


//Propósito: imprime n veces un string s.
void printN(int n, string s) {
    //string repetirNVeces;
    for(int i = 0; i < n; i++) {
        cout << s << ", ";
    }
      cout << endl;
}

int main() {
    printN(2,"poi");
    return 0;
}
