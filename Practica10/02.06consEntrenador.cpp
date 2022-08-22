#include <iostream>
#include "Entrenador.h"

using namespace std;

    

int main() {
    Pokemon* pks = new Pokemon[3];
    pks[0] = consPokemon("Agua");
    pks[1] = consPokemon("Agua");
    pks[2] = consPokemon("Fuego");
    Entrenador e = consEntrenador("jason",3,pks);
    ShowEntrenador(e);
}