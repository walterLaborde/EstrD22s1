#include <iostream>
#include "Pokemon.h"

using namespace std;

int main() {
    Pokemon p1 = consPokemon("Agua");
    Pokemon p2 = consPokemon("Fuego");
    cout << superaA(p1,p2);
    return 0;
};