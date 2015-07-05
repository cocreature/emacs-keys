{-# LANGUAGE TemplateHaskell #-}
-- | provides the parser functions
module EmacsKeys.Parser 
  (Modifier(..)
  ,parseEmacsKeys)
  where

import Data.Char
import Data.Either
import Data.List.Split
import Language.Haskell.TH.Lift
import Text.XkbCommon

-- | Represents the accepted modifiers 'C','M','S'
data Modifier = Ctrl | Meta | Shift deriving (Show,Eq,Ord)

deriveLift ''Modifier

parseModifier :: String -> Either Modifier String
parseModifier s = case lookup s modifiers of
                    Just m -> Left m
                    Nothing -> Right s

parseKeys :: [Either Modifier String] -> Either String ([Modifier],[Keysym])
parseKeys keys =
  fmap (\keysyms -> (mods,keysyms)) $
  sequence $
  fmap (parseKeysym .
        if shift
           then fmap toUpper
           else id) $
  unparsedSyms
  where (mods,unparsedSyms) = partitionEithers keys
        shift = Shift `elem` mods

parseKeysym :: String -> Either String Keysym
parseKeysym s =  case keysymFromName s of
                   Just ks -> Right ks
                   Nothing -> Left $ "Invalid key: \"" ++ s ++ "\""

-- | Parse a string into a list of modifiers and keysyms
-- If Shift is part of the modifiers, the keysyms are converted to upper case
--
-- >>> parseEmacsKeys "M-a"
-- Right ([Meta],[Keysym 97])
--
-- >>> parseEmacsKeys "Return"
-- Right ([],[Keysym 65293])
--
-- >>> parseEmacsKeys "S-a"
-- Right ([Shift],[Keysym 65])
parseEmacsKeys :: String -> Either String ([Modifier],[Keysym])
parseEmacsKeys = parseKeys . fmap parseModifier . splitOn "-"

modifiers :: [(String,Modifier)]
modifiers = [("C",Ctrl),("M",Meta),("S",Shift)]
