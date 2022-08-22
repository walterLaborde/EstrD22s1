#include <iostream>
#include "Persona.h"
using namespace std;

struct PersonaSt {
    string nombre;
    int edad;
};

//Devuelve una persona
Persona consPersona(string nombre, int edad) {
    PersonaSt* p = new PersonaSt;
    p->nombre = nombre;
    p->edad = edad;
    return p;
}

//Devuelve el nombre de una persona
string nombre(Persona p) {
    return p->nombre;
}

//Devuelve la edad de una persona
int edad(Persona p){
    return p->edad;
}

//Aumenta en uno la edad de la persona.
void crecer(Persona p) {
    p->edad++;
}

//Modifica el nombre una persona.
void cambioDeNombre(string nombre, Persona p) {
    p->nombre = nombre;
}

//Dadas dos personas indica si la primera es mayor que la segunda.
bool esMayorQueLaOtra(Persona p1, Persona p2) {
    return (p1->edad > p2->edad);
}

//Dadas dos personas devuelve a la persona que sea mayor.
Persona laQueEsMayor(Persona p1, Persona p2) {
    PersonaSt* laMayor;
    if(p1->edad > p2->edad) {
        laMayor = p1;
    }
    else {
        laMayor = p2;
    }
    return laMayor;
}

void showPersona(Persona p) {
    cout << "Persona [ " << p << " ](";
    cout << "Nombre <- \"" << p->nombre << "\", ";
    cout << "Edad <- " << p->edad;
    cout << ")";  
}

int main() {
    // consPersona
    PersonaSt* p = consPersona("Waldo", 45);
    //showPersona(p);
    
    //nombre
    //cout << nombre(p);

    //edad
    //cout << edad(p);

    //crecer
    //crecer(p);
    //showPersona(p);

    //cambioDeNombre
    //cambioDeNombre("Jeffrey", p);
    //showPersona(p);

    //esMayorQueLaOtra
    PersonaSt* v = consPersona("Vilvi",46);
    //cout << esMayorQueLaOtra(v,p);

    //laQueEsMayor
    showPersona(laQueEsMayor(p,v));
    
    return 0;
}