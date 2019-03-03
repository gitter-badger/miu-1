module Syntax where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty (..))

newtype Path = Path { getPath :: Text }

newtype Var = Var { getTypeVar :: Text }

--------------------------------------------------------------------------------
-- * Type syntax

-- A, B ::= D R̅ | {C} | X
data ValueType v
  = VTData
    { _vtDataName :: Path
    , _vtDataTypeArgs :: [TypeArg v]
    }
  | VTComputation (ComputationType v)
  | VTValueVar v

-- C ::= (̅T →)̅ G
data ComputationType v = ComputationType
  { _ctPorts :: NonEmpty (Port v)
  , _ctPeg :: Peg v
  }

-- T ::= <Δ> A
data Port v = Port
  { _portAdjustments :: !(Adjustment v)
  , _portType :: !(ValueType v)
  }

-- G ::= [Σ] A
data Peg v = Peg
  { _pegAbilities :: !(Ability v)
  , _pegType :: !(ValueType v)
  }

-- Z ::= X | [E]
data TypeVar v
  = TVValueVar v
  | TVEffectVar v

-- R ::= A | [Σ]
data TypeArg v
  = TAValueType !(ValueType v)
  | TAAbility !(Ability v)

-- P ::= ∀Z̅.A
data PolyType v = PolyType
  { _boundVars :: [TypeVar v]
  , _polyType :: ValueType v
  }

-- Σ ::= ∅ ∣ Σ, I R̅ ∣ E
data Ability v
  = AbNil
  | AbSnoc (Ability v) (Interface v)
  | AbEffectVar v

-- U+2205
(∅) :: Ability v
(∅) = AbNil

data Interface v = Interface
  { _interfaceName :: Path
  , _interfaceArgs :: NonEmpty (TypeArg v)
  }

-- Δ ::= ι ∣ ∆ + I R̅
data Adjustment v
  = AdjNil
  | AdjSnoc (Adjustment v) (Interface v)

-- U+03b9
ι :: Adjustment v
ι = AdjNil

--------------------------------------------------------------------------------
-- * Term syntax

data Literal
  = LInt Int
  | LBool Bool
  | LText Text

-- m ::= x | f | c | m s
data Use v
  = ULit Literal
  | UMonoVar v
  | UPolyVar v
  | UCommand v
  | UApp (Use v) (Spine v)

-- n ::= m | k n̅ | {e} | let f : P = n in n' | letrec (̅f : P = e)̅
data Construction v
  = CUse (Use v)
  | CDataCon v [v]
  | CSuspend (Computation v)
  | CLet { _ident :: v, _type :: PolyType v, _rhs :: Construction v, _body :: Construction v }
  | CLetRec { _defs :: NonEmpty (v, PolyType v, Computation v), _body :: Construction v }

newtype Spine v = Spine { getSpine :: NonEmpty (Construction v) }

newtype Computation v = Computation
  { getComputation :: NonEmpty (NonEmpty (ComputationPattern v), Construction v) }

data ComputationPattern v
  = CPValuePattern (ValuePattern v)
  | CPRequest {_cmd :: v, _cmdPats :: [ValuePattern v], _cont :: ()}
  | CPCatchAll v

data ValuePattern v
  = VPMonoVar v
  | VPDataCon v [v]

--------------------------------------------------------------------------------
-- * Examples

mkBoundVars :: [v] -> [v] -> [TypeVar v]
mkBoundVars a b = map TVEffectVar a ++ map TVValueVar b

mapType :: PolyType Text
mapType = PolyType
  { _boundVars = mkBoundVars ["ε0"] ["X", "Y"]
  , _polyType = VTComputation $ ComputationType
    { _ctPorts = Port
      { _portAdjustments = ι
      , _portType = VTComputation $ ComputationType
        { _ctPorts = Port ι (VTValueVar "X") :| []
        , _ctPeg = Peg (AbEffectVar "ε0") (VTValueVar "Y")
        }
      } :|
      [ Port ι (VTData (Path "List") [TAValueType (VTValueVar "X")]) ]
    , _ctPeg = Peg (AbEffectVar "ε0")
               (VTData (Path "List") [TAValueType (VTValueVar "Y")])
    }
  }

stateType :: PolyType Text
stateType = PolyType
  { _boundVars = mkBoundVars ["ε1"] ["X", "S"]
  , _polyType = VTComputation $ ComputationType
    { _ctPorts = Port
      { _portAdjustments = ι
      , _portType = VTValueVar "X"
      } :| Port
      { _portAdjustments = AdjSnoc ι $ Interface
        { _interfaceName = Path "State"
        , _interfaceArgs = undefined
        }
  -- { _interfaceName :: Path
  -- , _interfaceArgs :: NonEmpty (TypeArg v)
      , _portType = undefined
      } : []
    }
  }

example :: Construction Text
example = CLetRec
  { _defs = ("map", mapType, undefined) :| []
  , _body = CLetRec
    { _defs = ("state", stateType, undefined) :| []
    , _body = CLetRec
      { _defs = ("index", indexType, undefined) :| []
      , _body = CUse (UApp (UCommand "index") . Spine $ CUse (ULit (LText "abc")) :| [])
      }
    }
  }
  where
    indexType = undefined
