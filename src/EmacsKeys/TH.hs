{-# LANGUAGE TemplateHaskell #-}
-- | provides TH helpers
module EmacsKeys.TH
  (mkEmacsKeys)
  where
import EmacsKeys.Parser
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import Text.XkbCommon

-- | TH helper, to turn parse errors into compile time errors
mkEmacsKeys :: String -> Q Exp
mkEmacsKeys s =
  case parseEmacsKeys s of
    Left err -> error (show err)
    Right (mods,keysyms) ->
      [|($(lift mods)
        ,$(ListE <$>
           traverse (\(Keysym x) ->
                       AppE (VarE (mkName "Keysym")) <$>
                       lift x)
                    keysyms))|]
