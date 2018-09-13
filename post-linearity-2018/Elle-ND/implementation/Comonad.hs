{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------
-- Derive properties of the comonad             --
--------------------------------------------------

module Comonad where

import Text.Parsec
import Control.Monad.Except
import Data.List

import Syntax
import TypeCheck
import Parser
import Pretty

--------------------------------------------------
-- Derive exchange comonadicly                  --
--------------------------------------------------
