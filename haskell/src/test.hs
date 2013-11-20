import Peptide
import Data.ByteString.Char8
main = print $ results
  where sequence = pack "V(3D)NK(3F)NKEXCNZRAIEUALDPNLNDQQFHUKIWZIIXDC"
        weight   = 2194.9
        results  = calculationResults weight sequence

