#ifndef LESTER_EXPORT_MTLOWERANDUPPERBOUND_H
#define LESTER_EXPORT_MTLOWERANDUPPERBOUND_H

/**

Copyright Christopher Lester.  Please contact Alan Barr or Christopher Lester if you wish to incorporate it in your code.  We will grant permission (!) but like to have an idea whether it is being used.

This code evaluates a kinematic variables, mTBound (aka mTLowerBound) and mTUpperBound, providing a maximal lower (respectively upper) bounds on the mass of a resonance R undergoing the decay decay:

R -> IJ

followed by           I -> SP

and                   J -> TQ

where I and J have the same mass ("mIntermediate"), and where
the intermediate mass constraint
is enforced, where P and Q are invisible and only constrained by
the sum of their transverse componente (ptmiss).


If you use this code, please cite the paper where we introduced the variables you are using.

The paper:

http://arxiv.org/abs/1106.2322

introduced mTBound in the context of H -> tau tau decays.

The paper

http://arxiv.org/abs/1110.2452

also discussed mTBound (then calling it mTLowerBound) together with mTUpperBound in the context of
of H -> WW decays.  Note that the old implementation of mT(Lower)Bound is available separately at http://www.hep.phy.cam.ac.uk/~lester/ResonanceDecayVariables.html.

In this implementation, mTUpperBound and mTLowerBound are implemented as one single function, with a boolean input variable to distibguish which variable you want to calculate.


The prototye is:

double mTLowerAndUpperBound(double se, double sx, double sy, double sz,
                              double te, double tx, double ty, double tz,
                              double pmissx, double pmissy,
                              double mIntermediate,
                              bool getLowerBound // true=return:mTBound (aka mTLower),
                                                 // false=return:mTUpperBound (aka mTUpper)
) {

and is defined at the foot of this file.

A negative return value of -10 means that mUpperAndLowerBound does not exist (this is
a legitimate possibility.  See the papers above!)  Any other negative return value
suggests an internal error in the implementation and should be reported to the
authors together with a simple test program reproducing the problem.


To evaluate mTUpperAndLowerBound call the function

double Lester::mTUpperAndLowerBound(...)

defined below.  Note that to use this code you must edit FlatRandom.h to make it contain a routine that returns high-quality random numbers in the range [0,1).

*/

#include "FlatRandom.h"

#include <iostream>
#include <cmath>


namespace Lester {

  // This is the function you want:
  double mTUpperAndLowerBound(double se, double sx, double sy, double sz,
                  double te, double tx, double ty, double tz,
                  double pmissx, double pmissy,
                  double mIntermediate,
                  bool getLowerBound);



  ///////////////////////////////////////////////////////////




  /// Higgs -> tau (1) tau (2)
  /// tau (1) -> s(vis) + p(invis)
  /// tau (2) -> t(vis) + q(invis)



  const bool MTLOWERANDUPPERBOUND_DEBUG = false;

  /// This is an implementation of what is written up in Lab Book 8B.  Roughly page 20.  Note that HiggsTauTauMassLesterAtFixedKxKy is a utility for calculating the mass of a progenitor particle (the "Higgs") under the assumption that it decayed toWW and thence to l nu l nu under the assumption that the transverse momentum splitting between the neutrinos is KNOWN (this is what is parametrised by kx and ky).  Given that splitting, this implementation finds the e and z components of the neutrinos that satisfy the on-shell mass constrains for the two W-bosons (if this is possible) and then finds the (smallest sensible) invariant mass of a higgs resulting therefrom.

  /* "wasSillyRef" is "true" on exit if and only if there is a problem of some kind.

  Possible returns of "HelperFunctionHiggsTauTauMassLesterAtFixedKxKy" are as follows:

  (1) If wasSillyRef=false, then returnValue guarenteed>=0 and sensible.

  (2) If wasSillyRef=true, then:

  (2a) If returnValue>=0 then returnValue is BIG (>14000) and is a measure of
  how badly the kx and ky were outside the physuical region.

  (2b) If returnValue<0 then either returnValue is -2 or -3.

  -2 should be very rare (I have never seen it happen) but it could perhaps happen due to rare rounding errors.  It would indicate that no real energies could be found for kx and ky splittings deemed to be in the valid region (by test of discriminant).

  -3 should never happen at all.  It would indicate a logic bug in the last part of the code.

  */

#ifndef OOPS_THIS_SHOULD_BE_IN_A_SINGLE_EXTERNAL_FILE
#define OOPS_THIS_SHOULD_BE_IN_A_SINGLE_EXTERNAL_FILE
  double HelperFunctionHiggsTauTauMassLesterAtFixedKxKy(
                            const double kx, const double ky,
                            const double se, const double sx, const double sy, const double sz,
                            const double te, const double tx, const double ty, const double tz,
                            const double pmissx, const double pmissy,
                            const double mtau,
                            bool & wasSillyRef, // if set, means that inputs were outside the physical region, but a "silly" value has been returned that will nontheless help a minimizer find a physical region.
                            bool & returnIsErrorCode, // if set on exit, indicates that something unexpected happend inside the algorithm, and the return value should be treated not as a mass (silly or otherwise) but as an error code.
                            bool biasSillyValuesHighSinceWeAreFindingALowerBound) {

    // amHuntingForMinimalVal indicates what we are going to do with the returned values.  We need to know this, because the unphysical values get returned as "silly" values that need to be biased in the right direction (if we are looking for a min, siklly values need to be too big.  It is vital that a @silly@ value always be lower that ANY POSSIBLE non-silly value, so that a silly value can never trump a non-silly one.  We could remove this requirement by better hand-shaking.  If we are looking for a max, silly values need to be to too low.  This function can be fed back into the mTBound.h sourcecode when it has been tested .

    returnIsErrorCode = false;

    wasSillyRef = true;

    const double mtausq = mtau*mtau;
    const double mssq = se*se-sx*sx-sy*sy-sz*sz;
    const double mtsq = te*te-tx*tx-ty*ty-tz*tz;

    const double pkx1 = pmissx + kx;
    const double pky1 = pmissy + ky;
    const double pkx2 = pmissx - kx;
    const double pky2 = pmissy - ky;

    const double star1 = mtausq - mssq + sx*pkx1+sy*pky1;
    const double star2 = mtausq - mtsq + tx*pkx2+ty*pky2;

    const double pk1sq = pkx1*pkx1 + pky1*pky1;
    const double pk2sq = pkx2*pkx2 + pky2*pky2;


    const double etssq = se*se-sz*sz;
    const double ettsq = te*te-tz*tz;

    bool good = true;

    const double disc1 = star1*star1 - etssq*pk1sq;
    const double disc2 = star2*star2 - ettsq*pk2sq;

    if (disc1<0 || disc2<0) {
      good = false;
    }

    const double pz_cat = (star1*sz + se*sqrt(disc1))/(2*etssq);
    const double pz_dog = (star1*sz - se*sqrt(disc1))/(2*etssq);
    const double qz_cat = (star2*tz + te*sqrt(disc2))/(2*ettsq);
    const double qz_dog = (star2*tz - te*sqrt(disc2))/(2*ettsq);

    const double px=0.5*(pmissx + kx);
    const double py=0.5*(pmissy + ky);
    const double qx=0.5*(pmissx - kx);
    const double qy=0.5*(pmissy - ky);


    const double pesq_cat = px*px + py*py + pz_cat*pz_cat;
    const double pesq_dog = px*px + py*py + pz_dog*pz_dog;
    const double qesq_cat = qx*qx + qy*qy + qz_cat*qz_cat;
    const double qesq_dog = qx*qx + qy*qy + qz_dog*qz_dog;

    const double pe_cat = sqrt(pesq_cat);
    const double pe_dog = sqrt(pesq_dog);
    const double qe_cat = sqrt(qesq_cat);
    const double qe_dog = sqrt(qesq_dog);

    // std::cout << std::endl;
    //std::cout << "p_cat " << pe_cat << " " << px << " " << py << " " << pz_cat << std::endl;
    //std::cout << "p_dog " << pe_dog << " " << px << " " << py << " " << pz_dog << std::endl;
    //std::cout << "q_cat " << qe_cat << " " << qx << " " << qy << " " << qz_cat << std::endl;
    //std::cout << "q_dog " << qe_dog << " " << qx << " " << qy << " " << qz_dog << std::endl;

    const double mhAAAsq = 2*mtausq + 2*(
                     (pe_cat + se)*(qe_cat + te)
                     -  (px + sx)*(qx + tx)
                     -  (py + sy)*(qy + ty)
                     -  (pz_cat + sz)*(qz_cat + tz)
                     );
    const bool AAAgood = pesq_cat >=0 && qesq_cat>=0;

    const double mhBBBsq = 2*mtausq + 2*(
                     (pe_cat + se)*(qe_dog + te)
                     -  (px + sx)*(qx + tx)
                     -  (py + sy)*(qy + ty)
                     -  (pz_cat + sz)*(qz_dog + tz)
                     );
    const bool BBBgood = pesq_cat >=0 && qesq_dog>=0;

    const double mhCCCsq = 2*mtausq + 2*(
                     (pe_dog + se)*(qe_cat + te)
                     -  (px + sx)*(qx + tx)
                     -  (py + sy)*(qy + ty)
                     -  (pz_dog + sz)*(qz_cat + tz)
                     );
    const bool CCCgood = pesq_dog >=0 && qesq_cat>=0;

    const double mhDDDsq = 2*mtausq + 2*(
                     (pe_dog + se)*(qe_dog + te)
                     -  (px + sx)*(qx + tx)
                     -  (py + sy)*(qy + ty)
                     -  (pz_dog + sz)*(qz_dog + tz)
                     );
    const bool DDDgood = pesq_dog >=0 && qesq_dog>=0;


    if (!good) {
      double power=1; // power to raise the -ve discriminant by in attemt to get out of bad region


      if (biasSillyValuesHighSinceWeAreFindingALowerBound) {

    // biasSillyValuesHighSinceWeAreFindingALowerBound

    if(disc1<0 && disc2>=0) {
      return pow(-disc1,power) + 14000;
    } else if (disc2<0 && disc1>=0) {
      return pow(-disc2,power) + 14000;
    } else {
      return pow(-disc1,power) + pow(-disc2,power) + 14000;
    }

      } else {

    // biasSillyValuesLow

    if(disc1<0 && disc2>=0) {
      return -pow(-disc1,power) - 30; // 30 is just an offset to push us below the most negative error code
    } else if (disc2<0 && disc1>=0) {
      return -pow(-disc2,power) -30;
    } else {
      return -pow(-disc1,power) - pow(-disc2,power) -30;
    }

      }
    }

    if ((!AAAgood) && (!BBBgood) && (!CCCgood) && (!DDDgood)) {
      returnIsErrorCode = true;
      return -2;
    }

    double mhsq = 0.0;
    bool set=false;
    const double magicSign = biasSillyValuesHighSinceWeAreFindingALowerBound ? +1 : -1;

    if (AAAgood && (!set || magicSign*mhAAAsq < magicSign*mhsq)) {
      set = true;
      mhsq = mhAAAsq;
    }
    if (BBBgood && (!set || magicSign*mhBBBsq < magicSign*mhsq)) {
      set = true;
      mhsq = mhBBBsq;
    }
    if (CCCgood && (!set || magicSign*mhCCCsq < magicSign*mhsq)) {
      set = true;
      mhsq = mhCCCsq;
    }
    if (DDDgood && (!set || magicSign*mhDDDsq < magicSign*mhsq)) {
      set = true;
      mhsq = mhDDDsq;
    }

    if (!set) {
      returnIsErrorCode = true;
      return -3;
    }

    if (mhsq<0) {
      returnIsErrorCode = true;
      return -4;
      //return -sqrt(-mhsq);
    }

    wasSillyRef = false;

    return sqrt(mhsq);


  } // end of function
#endif

  ///////////////////////////////////////////////////////////


  double mTLowerAndUpperBound(double se, double sx, double sy, double sz,
                  double te, double tx, double ty, double tz,
                  double pmissx, double pmissy,
                  double mIntermediate,
                  bool getLowerBound /* true=return:mTBound (aka mTLower), false=return:mTUpperBound (aka mTUpper)*/ ) {

    const double magicSign = getLowerBound ? +1 : -1;

    const double scale = sqrt(( sx*sx + sy*sy + sz*sz +
                tx*tx + ty*ty + tz*tz +
                pmissx*pmissx + pmissy*pmissy)/8.);
    // std::cout << "SCALE " << scale << std::endl;
    if (scale==0) {
      return mIntermediate*2.; // see discussion on Fri 16th Dec in mail folder "papers".
    }


    // OK .. scale was not zero, so let's condition the inputs by dividing out this scale so that we are insensitive to unit choices that users make:

    se /= scale;
    sx /= scale;
    sy /= scale;
    sz /= scale;

    te /= scale;
    tx /= scale;
    ty /= scale;
    tz /= scale;

    pmissx /= scale;
    pmissy /= scale;

    mIntermediate /= scale;

    // We will have to multiply that scale back into our answer at the end!

    const double mtau = mIntermediate;

    double kxStart = 0;
    double kyStart = 0;
    double bestHMassSoFar;

    bool haveValidStartPoint=false;
    bool bestPointWasSilly;


    /*


    How the calculation proceeds:

    First stage:

    Random (memoryless) splittings, parametrised by kx,ky, are repeatedly generated until either:

    (a) a valid kx and ky in the physical region is discovered (...AtFixedKxKy signals this with "wasSilly=false"), or

    (b) a number of attempts (eg 10,000) has been exhausted.

    During this memoryless search, all the while no "good" point has been found, the "best of the silly" points is nonetheless retained (i.e. the silly point whose negative discriminants have been least negative thus far) so that this may serve as a "best guess, even though un[physical" suitable start poing for the second stage of the calculation, which is memory full.

    At the end of this second state, a valid start point (even if it is "silly", i.e. "unphysical" should have been found.  The only circumstance in which it might not have been found is if every splitting registered a negative error code return from the underlying utility, which ought basically to be impossible.  If such a thing were to happen haveValidStartPoint would remain false, and we would return -15.  This should almost NEVER happen!  The universe would more likely grow cold before that will happen.  A return of -15, if seen, is more likley to indicate a bug in this code.

    Second stage:

    Now a markov chain minimiser attempts to find the splitting that leads to the smallest higgs mass (or smallest "silly" higgs mass if we are still outside the physical region and are therefore adding 14000 to discriminats etc).  Even if we were intially in an unphysical region, we will hopefully drop into the physical one here, if it exists.

    If ultimately we never found how to get into the physical region (it may not exist!) we return -10.

    Otherwise, we return the smallest (physical) progenitor mass that was found among all splittings searched.  This will be >=0.

    */

    // Attempt to get valid start point

    const double aTypicalScaleAgainstWhichAlgWasDeveloped = 26.;

    const double distFromWall /* AKA scan size */ = 2./aTypicalScaleAgainstWhichAlgWasDeveloped; // order of magnitude spread over which cauchy vals will be distributed.
    // NB

    for (int i=0; i<10000; ++i) {

      // Warning! This doggedly tries to get a start point .. and may take forever if there is no possible start point ... or if the start point is


      const double theta = (FlatRandom()-0.5)*3.14159;
      const double distToStep = distFromWall*tan(theta);
      const double angToStep = FlatRandom()*3.14159*2.0;
      const double kx = distToStep * cos(angToStep);
      const double ky = distToStep * sin(angToStep);
      bool wasSilly;
      bool isErrorCode;
      const bool biasSillyValuesHighSinceWeAreFindingALowerBound = getLowerBound;

      const double possHMass =
    Lester::HelperFunctionHiggsTauTauMassLesterAtFixedKxKy(kx,ky,
                                   se,sx,sy,sz,
                                   te,tx,ty,tz,
                                   pmissx,pmissy,
                                   mtau,
                                   wasSilly,
                                   isErrorCode,
                                   biasSillyValuesHighSinceWeAreFindingALowerBound);
      bool thisPointIsBetter;
      if (isErrorCode) {
    thisPointIsBetter = false; // can't do anything with error points
      } else {
    // it might be ok
    if  (!haveValidStartPoint) {
      // great, it is better than nothing!
      thisPointIsBetter = true;
    } else {
      // we already have a start point, so we should only supersede what we have if we improve things:
      if (!bestPointWasSilly) {
        // The best point so far was good ... so we should not be able to get here ... but we can safely say the point is better if it is smaller and not silly
        thisPointIsBetter = !wasSilly && (possHMass*magicSign < bestHMassSoFar*magicSign);
      } else {
        // The best point so far was silly, so improve if the new point is not silly, or if it is less silly than before
        thisPointIsBetter = !wasSilly || (possHMass*magicSign < bestHMassSoFar*magicSign);
      }
    }
      }

      if (thisPointIsBetter)  {
    bestHMassSoFar = possHMass;
    haveValidStartPoint = true;
    bestPointWasSilly = wasSilly;
    kxStart = kx;
    kyStart = ky;
    if (Lester::MTLOWERANDUPPERBOUND_DEBUG) {
      std::cout << "Found better start point with mass " << possHMass << std::endl;
        }
    if (!wasSilly) {
      // Don't need to work any harder ...
      break;
    }
      }


    }
    if (Lester::MTLOWERANDUPPERBOUND_DEBUG) {
      std::cout << "LESTER ANS " << bestHMassSoFar << "\tsilliness " << bestPointWasSilly << " valid " << haveValidStartPoint << std::endl;
    }


    if (haveValidStartPoint) {
      // Now we can attempt to minimise (or maximise) this function.

      double kxOld = kxStart;
      double kyOld = kyStart;
      // double oldH = bestHMassSoFar;

      double typicalStepSize = distFromWall;

      // for xavi and paper were 0.99 and 1.1
      //const double shrinkageFactor = 0.99;
      //const double growthFactor = 1.1;

      const double shrinkageFactor = 0.99; // for lower bound, 0.9, 1.1
      const double growthFactor = 1.1;


      while(typicalStepSize > 1.e-8/*was 6*//aTypicalScaleAgainstWhichAlgWasDeveloped) {
    const double theta = (FlatRandom()-0.5)*3.14159;
    const double distToStep = typicalStepSize*tan(theta);
    const double angToStep = FlatRandom()*3.14159*2.0001;
    const double newkx = kxOld + distToStep * cos(angToStep);
    const double newky = kyOld + distToStep * sin(angToStep);
    bool wasSilly;
    bool isErrorCode;
    const bool biasSillyValuesHighSinceWeAreFindingALowerBound = getLowerBound;

    const double possHMass =
      Lester::HelperFunctionHiggsTauTauMassLesterAtFixedKxKy(newkx,newky,
                                 se,sx,sy,sz,
                                 te,tx,ty,tz,
                                 pmissx,pmissy,
                                 mtau,
                                 wasSilly,
                                 isErrorCode,
                                 biasSillyValuesHighSinceWeAreFindingALowerBound);


    bool thisPointIsBetter;
    if (isErrorCode) {
      thisPointIsBetter = false; // can't do anything with error points
    } else {
      // It might be ok.
      // We already have a start point, so we should only supersede what we have if we improve things:
      if (!bestPointWasSilly) {
        // The best point so far was good ... so we can safely say the point is better if it is smaller and not silly
        thisPointIsBetter = !wasSilly && (possHMass*magicSign < bestHMassSoFar*magicSign);
      } else {
        // The best point so far was silly, so improve if the new point is not silly, or if it is less silly than before
        thisPointIsBetter = !wasSilly || (possHMass*magicSign < bestHMassSoFar*magicSign);
      }
    }


    if (thisPointIsBetter) {

      bestHMassSoFar = possHMass;
      bestPointWasSilly = wasSilly;
      kxOld = newkx;
      kyOld = newky;
      typicalStepSize *= growthFactor;
      if (Lester::MTLOWERANDUPPERBOUND_DEBUG) {
        std::cout << "Found better point with mass " << possHMass << " while typical step " << typicalStepSize << std::endl;
          }

    } // if point is an improvement
    else {
      typicalStepSize *= shrinkageFactor;
    } // point was not an improvement

      } // while we wantto keep going
      if (bestPointWasSilly) {
    return -10;
      } else {
    return bestHMassSoFar*scale;
      }
    } // have valid start
    else {
      return -15;
    } // have invalid start ... should never happen.  Tell the authors if you get any of these!


  }


} // end of namespace Lester

#endif
