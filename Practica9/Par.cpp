#include <iostream>
#include "Par.h"
using namespace std;

// Propósito: construye un par
Par consPar(int x, int y) {
    Par p;
    p.x = x;
    p.y = y;
    return(p);
}

// Propósito: devuelve la primera componente
int fst(Par p) {
    return(p.x);
}

// Propósito: devuelve la segunda componente
int snd(Par p) {
    return(p.y);
}

// Propósito: devuelve la mayor componente
int maxDelPar(Par p) {
    if(p.x > p.y) {
        return p.x;
    } 
    else {
        return p.y;
    }
}

// Propósito: devuelve un par con las componentes intercambiadas
Par swap(Par p) {
    Par sPar;
    sPar.x = p.y;
    sPar.y = p.x;
    return(sPar);
}

// Propósito: devuelve un par donde la primer componente
// es la división y la segunda el resto entre ambos números
Par divisionYResto(int n, int m) {
    Par divRest;
    divRest.x = n / m;
    divRest.y = n % m;
    return(divRest);
}

