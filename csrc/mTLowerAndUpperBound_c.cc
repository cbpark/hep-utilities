#include "mTLowerAndUpperBound.h"
#include "mTLowerAndUpperBound_c.h"

extern "C" {
    double mt_bound(double se, double sx, double sy, double sz,
                    double te, double tx, double ty, double tz,
                    double pmissx, double pmissy,
                    double mIntermediate) {
        return Lester::mTLowerAndUpperBound(se, sx, sy, sz, te, tx, ty, tz,
                                            pmissx, pmissy, mIntermediate,
                                            true);
    }
}
