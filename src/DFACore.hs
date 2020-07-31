{-# LANGUAGE DeriveGeneric #-}

module DFACore where

import GHC.Generics (Generic)
import Constants
import Grammar
import Data.Hashable
import qualified Data.HashMap.Strict as M 
import qualified Data.HashSet as S

-- The representation of a state : a RE which is assumed to be in canonical form.
type States = S.HashSet RE

newtype TransitionKey = TransitionKey { k :: (Int, RE) } deriving (Eq, Generic)

newtype Transition = Transition { internalMap :: M.HashMap TransitionKey RE } deriving (Eq, Generic)
-- The representation of a transition. The keys are all of the form (TransitionConcat i s), where i is some input RE and s is some state.



-- The representation of a DFA: (all states, initial state, accepting states, transition mapping)
type DFA = (States, RE, States, Transition)


type DFAWithCClassesEncoding = (DFA, Encoding)

instance Hashable TransitionKey

instance Hashable CRE

instance Show TransitionKey where
    show (TransitionKey (n, re)) = (intToChar n) ++ " + " ++ show re

instance Show Transition where
    show (Transition internalMap) = M.foldlWithKey' (\acc k v -> acc ++ "\n " ++ (show k) ++ " => " ++ (show v)) "" internalMap 

-- The representation of a transition. The keys are all of the form (TransitionConcat i s), where i is some input RE and s is some state.



-- The representation of a DFA: (all states, initial state, accepting states, transition mapping)
--type DFA = (States, RE, States, Transition)


--type DFAWithCClassesEncoding = (DFA, Encoding)
