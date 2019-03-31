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

-- TODO: How do monomorphic and polymorphic variables work in the syntax?
-- Shouldn't we be able to construct the syntax tree without knowing the types?
--
-- I should look at Frank's RefineSyntax.refineUse to see how this is working.
--
-- m ::= x | f | c | m s
data Use v
  = ULit Literal -- Added for convenience.
  | UMonoVar v
  | UPolyVar v
  | UCommand v
  | UApp (Use v) (Spine v)

-- n ::= m | k n̅ | {e} | let f : P = n in n' | letrec (̅f : P = e)̅
data Construction v
  = CUse (Use v)
  | CDataCon { _cDataConName :: v, _cDataConArgs :: [Construction v] }
  | CSuspend (Computation v)
  | CLet { _ident :: v, _type :: PolyType v, _rhs :: Construction v, _body :: Construction v }
  | CLetRec { _defs :: NonEmpty (v, PolyType v, Computation v), _body :: Construction v }

-- s ::= n̅
newtype Spine v = Spine { getSpine :: NonEmpty (Construction v) }

-- e ::=  (̅ r̅ -> n )̅
newtype Computation v = Computation
  { getComputation :: NonEmpty (NonEmpty (ComputationPattern v), Construction v) }

-- r ::= p | < c p̅ -> z > | < x >
data ComputationPattern v
  = CPValuePattern (ValuePattern v)
  | CPRequest { _cmd :: v, _cmdPats :: [ValuePattern v], _cont :: () }
  | CPCatchAll v

-- p ::= k p̅ | x
data ValuePattern v
  = VPDataCon { _vpDataConName :: v, _vpDataConArgs :: [ValuePattern v] }
  | VPMonoVar v

-- TODO: It isn't entirely clear to me what the difference is between
--           CPValuePattern (VPMonoVar "a") and CPCatchall "a"

--------------------------------------------------------------------------------
-- * Examples

mkBoundVars :: [v] -> [v] -> [TypeVar v]
mkBoundVars a b = map TVEffectVar a ++ map TVValueVar b

mkDataType :: Text -> [ValueType v] -> ValueType v
mkDataType n xs = VTData (Path n) (TAValueType <$> xs)

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
      [ Port ι (mkDataType "List" [VTValueVar "X"]) ]
    , _ctPeg = Peg (AbEffectVar "ε0") (mkDataType "List" [VTValueVar "Y"])
    }
  }

mapBody :: Computation Text
mapBody = Computation $
  ( CPValuePattern <$> (VPMonoVar "f" :| [VPDataCon "nil" []])
  , CDataCon "nil" []
  ) :|
  [ ( CPValuePattern <$> (VPMonoVar "f" :| [VPDataCon "cons" $ VPMonoVar <$> ["x", "xs"]])
    , CDataCon "cons" $ CUse <$>
          [ UApp (UMonoVar "f") -- TODO: Should this be UCommand?
                 (Spine (CUse (UMonoVar "x") :| []))
          , UApp (UCommand "map")
                 (Spine (CUse (UMonoVar "x") :| [CUse (UMonoVar "xs")]))
          ]
    )
  ]

stateType :: PolyType Text
stateType = PolyType
  { _boundVars = mkBoundVars ["ε1"] ["X", "S"]
   -- The code in the paper doesn't have 'S' under the quantifier for some
   -- reason. I think that's a typo.
  , _polyType = VTComputation $ ComputationType
    { _ctPorts = Port
      { _portAdjustments = ι
      , _portType = VTValueVar "X"
      } :|
      [ Port
          { _portAdjustments = AdjSnoc ι $ Interface
            { _interfaceName = Path "State"
            , _interfaceArgs = TAValueType (VTValueVar "S") :| []
            }
          , _portType = VTValueVar "X"
          }
      ]
    , _ctPeg = Peg
      { _pegAbilities = AbEffectVar "ε1"
      , _pegType = VTValueVar "S"
      }
    }
  }

stateBody :: Computation Text
stateBody = undefined

indexType :: PolyType Text
indexType = PolyType
  { _boundVars = mkBoundVars ["ε3"] ["X"]
  , _polyType = VTComputation $ ComputationType
    { _ctPorts = Port
      { _portAdjustments = ι
      , _portType = mkDataType "List" [VTValueVar "X"]
      } :| []
    , _ctPeg = Peg
      { _pegAbilities = AbEffectVar "ε3"
      , _pegType = mkDataType "List" [mkDataType "Pair" [mkDataType "Nat" [], VTValueVar "X"]]
      }
    }
  }

example :: Construction Text
example = CLetRec
  { _defs = ("map", mapType, mapBody) :| []
  , _body = CLetRec
    { _defs = ("state", stateType, stateBody) :| []
    , _body = CLetRec
      { _defs = ("index", indexType, indexBody) :| []
      , _body = CUse (UApp (UCommand "index") . Spine $ CUse (ULit (LText "abc")) :| [])
      }
    }
  }

indexBody :: Computation Text
indexBody = undefined
