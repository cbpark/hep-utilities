#ifndef LESTER_FLATRANDOM_H
#define LESTER_FLATRANDOM_H


/* Alter the implementation of the function FlatRandom() below so that
   you get your random numbers from your preferred source. */
/*
#include "CLHEP/Random/RandFlat.h"

namespace Lester {

    double FlatRandom() {
        return CLHEP::RandFlat::shoot();
        }

}
*/

/*
 *  This is added by Chan Beom Park <cbpark@gmail.com>
 *  A C++ compiler supporting C++11 is necessary.
 *
 *  The original files can be downloaded from
 *  http://www.hep.phy.cam.ac.uk/~lester/ResonanceDecayVariables.html
 */
#include <random>

namespace Lester {
double FlatRandom() {
    std::random_device rd;
    std::minstd_rand gen(rd());
    std::uniform_real_distribution<double> dr(0.0, 1.0);
    return dr(gen);
}
}  // namespace Lester

#endif
