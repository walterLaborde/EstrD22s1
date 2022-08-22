#include <iostream>
#include "Entrenador.h"
#include "Pokemon.h"
using namespace std;

struct EntrenadorSt {
    string nombre;
    Pokemon* pokemon;
    int cantPokemon;
};

//Dado un nombre, una cantidad de pokémon, y un array de pokémon de ese tamaño, devuelve
//un entrenador.
Entrenador consEntrenador(string nombre, int cantidad, Pokemon* pokemon) {
    EntrenadorSt* e = new EntrenadorSt;
    e->nombre = nombre;
    e->cantPokemon = cantidad;
    e->pokemon = pokemon;
    return e;
};

//Devuelve el nombre del entrenador.
string nombreDeEntrenador(Entrenador e) {
    return e->nombre;
};

//Devuelve la cantidad de pokémon que posee el entrenador.
int cantidadDePokemon(Entrenador e) {
    return e->cantPokemon;
};


//Devuelve la cantidad de pokémon de determinado tipo que posee el entrenador.
int cantidadDePokemonDe(TipoDePokemon tipo, Entrenador e) {
    int cantDePokesTipo;
    for(int i=0; i<e->cantPokemon; i++) {
        if(tipoDePokemon(e->pokemon[i])==tipo) {
            cantDePokesTipo++;
        }
    }
    return cantDePokesTipo;
};

//Devuelve el pokémon número i de los pokémon del entrenador.
//Precondición: existen al menos i − 1 pokémon.
Pokemon pokemonNro(int i, Entrenador e) {
    return e->pokemon[i-1];
};

int cantPokeQueLeGanaATodos(Pokemon* p1, Pokemon* p2) {
    if(p1.empty()) {
        return 0;
    }
    else {

    }
}


//Dados dos entrenadores, indica si, para cada pokémon del segundo entrenador, el primero
//posee al menos un pokémon que le gane.
bool leGanaATodos(Entrenador e1, Entrenador e2) {
    return cantPokeQueLeGanaATodos(e1->pokemon,e2->pokemon)>0;
}

//Imprime un Entrenador
void ShowEntrenador(Entrenador e) {
    cout << "Entrenador [ " << e << " ]" << endl;
    cout << "Nombre <- " << e->nombre << endl;
    cout << "Cantidad de Pks <- " << e->cantPokemon << endl;
    cout << "Pokemones (";
    for(int i=0; i < e->cantPokemon; i++) {
        if(i>0) {cout << " ,";}
        ShowPokemon(e->pokemon[i]);
    }
    cout << ")";
}

void agregarPokeAl(Pokemon p, Entrenador e) {
    e->pokemon[e->cantPokemon++] = p;
}