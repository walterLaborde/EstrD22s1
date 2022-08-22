#include <iostream>
#include "Pokemon.h"

using namespace std;

int main() {
    Pokemon po = consPokemon("Agua");
    perderEnergia(130,po);
    ShowPokemon(po);
    return 0;
};