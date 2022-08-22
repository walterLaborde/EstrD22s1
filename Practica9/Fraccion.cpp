#include <iostream>
#include "Fraccion.h"
using namespace std;


// Propósito: construye una fraccion
// Precondición: el denominador no es cero
Fraccion consFraccion(int numerador, int denominador) {
    Fraccion f;
    f.numerador = numerador;
    f.denominador = denominador;
    return f;
}

// Propósito: devuelve el numerador
int numerador(Fraccion f) {
    return f.numerador;
}


// Propósito: devuelve el denominador
int denominador(Fraccion f) {
    return f.denominador;
}


// Propósito: devuelve el resultado de hacer la división
float division(Fraccion f) {
    return (f.numerador / f.denominador);
}


// Propósito: devuelve una fracción que resulta de multiplicar las fracciones
// (sin simplificar)
Fraccion multF(Fraccion f1, Fraccion f2) {
    Fraccion fmul;
    fmul.numerador = f1.numerador * f2.numerador;
    fmul.denominador = f1.denominador * f2.denominador;
    return fmul;
}

/*
Con este ciclo simplemente operamos mientras que b sea distinto de 0.

Dentro del ciclo, guardamos el valor de b en una variable temporal, 
después asignamos a b el valor de a % b que es sacar el residuo de dividir 
de manera entera a entre b.

Finalmente, a tomará el valor de temporal, que era el que inicialmente tenía b. 

En algún momento b es 0 y se termina el ciclo, que es en donde regresamos a, 
el cuál tendrá el último valor de b antes de haber obtenido el residuo.

fuente: https://parzibyte.me/blog/2019/12/18/maximo-comun-divisor-c-algoritmo-euclides/

*/
int MaxComDiv(int a, int b) {
    int provisorio; 
    while(b !=0) {
        provisorio = b;
        b = a % b;
        a = provisorio;
    }
    return a;
}

// Propósito: devuelve una fracción que resulta
// de simplificar la dada por parámetro
Fraccion simplificada(Fraccion p) {
    Fraccion fSimpl;
    fSimpl.numerador = p.numerador / MaxComDiv(p.numerador, p.denominador);
    fSimpl.denominador = p.denominador / MaxComDiv(p.numerador, p.denominador);
    return fSimpl;
}


// Propósito: devuelve la primera componente
Fraccion sumF(Fraccion f1, Fraccion f2) {
Fraccion fSum;
    if(f1.denominador == f2.denominador) {
        fSum.numerador = f1.numerador + f2.numerador;
        fSum.denominador = f1.denominador;
    }
    else {
        fSum.numerador = (f1.denominador * f2.numerador) + (f1.numerador * f2.denominador);
        fSum.denominador = f1.denominador * f2.denominador;
    }
    return fSum;
}

void ShowFraccion(Fraccion f){
    cout << f.numerador << endl;
    cout << "--" << endl;
    cout << f.denominador;
}

int main() {
    Fraccion fraccNueva = consFraccion(6,3);
    cout << division(fraccNueva);
    return 0;
}