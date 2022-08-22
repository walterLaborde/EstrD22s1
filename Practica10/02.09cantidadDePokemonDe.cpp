#include <iostream>
#include "Entrenador.h"
//#include "Pokemon.h"

using namespace std;

//Pokemon unPokemon(Entrenador e, int i) {
//    return e->pokemon[i-1];
//}    

int main() {
    Pokemon* pks = new Pokemon[3];
    pks[0] = consPokemon("Agua");
    pks[1] = consPokemon("Agua");
    pks[2] = consPokemon("Fuego");
    Entrenador e = consEntrenador("jason",3,pks);
    //ShowEntrenador(e);
    cout << cantidadDePokemonDe("Agua", e);
    //cout << tipoDePokemon(pokemonNro(1,e));
}