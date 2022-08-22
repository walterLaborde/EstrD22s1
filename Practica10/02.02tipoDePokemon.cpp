#include <iostream>
#include "Pokemon.h"

using namespace std;

int main() {
    Pokemon po = consPokemon("Agua");
    cout << tipoDePokemon(po);
    return 0;
};