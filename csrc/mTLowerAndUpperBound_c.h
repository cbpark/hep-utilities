#ifndef CSRC_MTLOWERANDUPPERBOUND_C_H_
#define CSRC_MTLOWERANDUPPERBOUND_C_H_

#ifdef __cplusplus
extern "C" {
#endif

    double mt_bound(double se, double sx, double sy, double sz,
                    double te, double tx, double ty, double tz,
                    double pmissx, double pmissy,
                    double mIntermediate);

#ifdef __cplusplus
}
#endif

#endif  // CSRC_MTLOWERANDUPPERBOUND_C_H_
