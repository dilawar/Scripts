#!/usr/bin/env runghc

import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Text.Pandoc.JSON

main :: IO ()
main = toJSONFilter replaceEnvs

-- | See stripPrefix.
stripPostfix :: Eq a => [a] -> [a] -> Maybe [a]
stripPostfix post l = reverse <$> stripPrefix (reverse post) (reverse l)

fromStr :: Inline -> Maybe String
fromStr (Str s) = Just s
fromStr _ = Nothing

stripStrPostfix :: String -> Inline -> Maybe String
stripStrPostfix post inline = fromStr inline >>= stripPostfix post

-- | If the list of Inlines starts with a Str that starts with the prefix
-- and the list contains a Str that ends with the postfix, then
-- return everything between prefix and postfix, as well as everything after.
--
-- Example:
--     inlineBetween "(" ")" [Str "(What", Space, Str "else?)", Str "Text"] =
--     Just ([Str "What", Space, Str "else?"], [Str "Text"])
inlineBetween :: String -> String -> [Inline] -> Maybe ([Inline], [Inline])
inlineBetween pre post (Str s : xs) | Just s' <- stripPrefix pre s =
  case break (isJust . stripStrPostfix post) (Str s' : xs) of
    (_, []) -> Nothing
    (x, y:ys) -> Just (x ++ [Str $ fromJust $ stripStrPostfix post y], ys)
inlineBetween _ _ _ = Nothing

envs :: [(String, String)]
envs =
  [ ("Definition", "definition")
  , ("Lemma", "lemma")
  , ("Theorem", "theorem")
  , ("Proof", "proof")
  ]

-- | Try all different environment types.
replaceEnvs :: Maybe Format -> Block -> Block
replaceEnvs fmt blk = foldr ($) blk (map (replaceEnv fmt) envs)

makeEnv :: String -> Maybe [Inline] -> Maybe String -> [Inline] -> [Inline]
makeEnv env name label text =
  maybe ([tex begin]) (\ n -> [tex $ begin ++ "["] ++ n ++ [tex "]"]) name ++
  maybe [] (\ l -> [tex $ "\\label{" ++ l ++ "}"]) label ++
  text ++ [tex end]
  where
    tex = RawInline (Format "latex")
    begin = "\\begin{" ++ env ++ "}"
    end   = "\\end{"   ++ env ++ "}"

-- | Try to read name and create environment.
nameEnv :: String -> Maybe String -> [Inline] -> Maybe Block
nameEnv env label ys = paraEnv <$> inlineBetween "(" ")." ys
  where paraEnv (name, rest) = Para $ makeEnv env (Just name) label rest

-- | Try to read label and name, and create environment.
labelNameEnv :: String -> [Inline] -> Maybe Block
labelNameEnv env ys = case ys of
  -- Example: Theorem pyth.
  Str (l:bld) : zs | l /= '(', Just bl <- stripPostfix "." bld ->
    Just $ Para $ makeEnv env Nothing (Just (l:bl)) zs
  -- Example: Theorem pyth (Pythagoras).
  Str (l:bl) : Space : zs | l /= '(' -> nameEnv env (Just (l:bl)) zs
  -- Example: Theorem (Pythagoras).
  _ -> nameEnv env Nothing ys
  

replaceEnv :: Maybe Format -> (String, String) -> Block -> Block
replaceEnv (Just (Format "latex")) (txt, latex) p@(Para (Str x : xs)) =
  if x == txt ++ "." then Para $ makeEnv latex Nothing Nothing xs
  else if x == txt then case xs of
    (Space : ys) -> fromMaybe p (labelNameEnv latex ys)
    _ -> p
  else p
replaceEnv _ _ x = x
