#include <iostream>
#include "Pokemon.h"

using namespace std;

struct PokeSt {
TipoDePokemon tipo;
int vida;
};

//Dado un tipo devuelve un pokémon con 100% de energía.
Pokemon consPokemon(TipoDePokemon tipo) {
    PokeSt* pok = new PokeSt;
    pok->tipo = tipo;
    pok->vida = 100;
    return pok;
};

//Devuelve el tipo de un pokémon.
string tipoDePokemon(Pokemon p) {
    return p->tipo;
}

//Devuelve el porcentaje de energía.
int energia(Pokemon p) {
    return p->vida;
}

//Le resta energía al pokémon.
// PRECOND: la energia es >= 0
void perderEnergia(int energia, Pokemon p) {
    p->vida = max(0,  p->vida - energia);
}

//Dados dos pokémon indica si el primero, en base al tipo, es superior al segundo. Agua supera
//a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
bool superaA(Pokemon p1, Pokemon p2) {
    return ((p1->tipo=="Agua" && p2->tipo=="Fuego")  || 
           (p1->tipo=="Fuego" && p2->tipo=="Planta") ||
           (p1->tipo=="Planta" && p2->tipo=="Agua"));
};

// cambiar por || unidos 

void ShowPokemon (Pokemon p) {
    cout << "Pokemon [ " << p << " ]" << endl;
    cout << "TipoDePokemon <- " << p->tipo << endl;
    cout << "Energia <- " << p->vida;
};


