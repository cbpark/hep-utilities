module HEP.Particle.ID where

type PdgID = Int

electron :: PdgID
electron = 11

antiElectron :: PdgID
antiElectron = - electron

electrons :: [PdgID]
electrons = [electron, antiElectron]

muon :: PdgID
muon = 13

antiMuon :: PdgID
antiMuon = - muon

muons :: [PdgID]
muons = [muon, antiMuon]

tau :: PdgID
tau = 15

antiTau :: PdgID
antiTau = - tau

taus :: [PdgID]
taus = [tau, antiTau]

isolatedLeptons :: [PdgID]
isolatedLeptons = electrons ++ muons

chargedLeptons :: [PdgID]
chargedLeptons = isolatedLeptons ++ taus

electronNeutrino :: PdgID
electronNeutrino = 12

antiElectronNeutrino :: PdgID
antiElectronNeutrino = - electronNeutrino

electronNeutrinos :: [PdgID]
electronNeutrinos = [electronNeutrino, antiElectronNeutrino]

muonNeutrino :: PdgID
muonNeutrino = 14

antiMuonNeutrino :: PdgID
antiMuonNeutrino = - muonNeutrino

muonNeutrinos :: [PdgID]
muonNeutrinos = [muonNeutrino, antiMuonNeutrino]

tauNeutrino :: PdgID
tauNeutrino = 16

antiTauNeutrino :: PdgID
antiTauNeutrino = - tauNeutrino

tauNeutrinos :: [PdgID]
tauNeutrinos = [tauNeutrino, antiTauNeutrino]

neutrinos :: [PdgID]
neutrinos = electronNeutrinos ++ muonNeutrinos ++ tauNeutrinos
