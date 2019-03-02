-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-

-- KOMBATE PALMANG KEVIN p1080477 , Mlle 20011289
-- Implementer par DIASSO ABDRAMANE 20057513





{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur (la partie que vous devez compléter)
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric               -- Pour la fonction showInt
import System.IO                -- Pour stdout, hPutStr
--import Data.Maybe            -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                       Left _ -> []
                       Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Pretty Printer                                                        --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint
           | Llist Ltype
           | Larw Ltype Ltype
           deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lfun Var Lexp    -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp      -- Appel de fonction, avec un argument.
          | Lnil                -- Constructeur de liste vide.
          | Lcons Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Var Var Lexp Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lletrec [(Var, Maybe Ltype, Lexp)] Lexp
          deriving (Show, Eq)

-- foldl sur des s-listes
sfoldl :: (t -> Sexp -> t) -> t -> Sexp -> t
sfoldl fdl base Snil            = base
sfoldl fdl base (Ssym _)        = base 
sfoldl fdl base (Snum _)        = base 
sfoldl fdl base (Scons car cdr) = sfoldl fdl (fdl base car) cdr
smap :: (Sexp -> t) -> Sexp -> [t]
smap fdl Snil = []
smap fdl (Scons car cdr) = (fdl car):(smap fdl cdr)

-- Fonctions auxiliaire qui permet de desucrer les formes "let"
bindParser :: Sexp -> (String, Maybe Ltype, Lexp)
bindParser (Scons (Ssym var) (Scons myType (Scons e1 Snil))) = (var, readMaybeType myType, s2l e1) 
parseFinalBinds :: Sexp -> [(String, Maybe Ltype, Lexp)]  
parseFinalBinds slist = smap bindParser slist  

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "nil") = Lnil
s2l (Ssym s) = Lvar s

s2l (Scons (Ssym "cons") (Scons e1 (Scons e2 Snil))) =
    Lcons (s2l e1) (s2l e2)
s2l (Scons (Ssym "fn") (Scons (Scons (Ssym arg) Snil) (Scons body Snil))) =
    Lfun arg (s2l body)
s2l (Scons e1 (Scons e2 Snil)) = Lcall (s2l e1) (s2l e2)

s2l (Scons (Ssym "case") (Scons e1 (Scons (Scons (Ssym arg1) Snil) (Scons (Scons 
    (Ssym arg2) Snil) (Scons e2 (Scons e3 Snil)))))) = 
    Lcase (s2l e1) arg1 arg2 (s2l e2) (s2l e3)

s2l (Scons e1 a) = Lhastype (s2l e1) (readType a)
s2l (Scons (Ssym "letrec")
               (Scons bindings (Scons body Snil))) = 
    Lletrec (parseFinalBinds bindings) (s2l body)      
         
-- ¡¡¡ COMPLETER ICI !!! --
s2l se = error ("Malformed Slip: " ++ (showSexp se))

-- Passe qui analyse un Sexp et construit un Ltype.
readType :: Sexp -> Ltype
readType (Snum _) = Lint
readType (Snil) = Llist (readType Snil)
readType (Ssym _) = error ("Not an identifier: ")
readType (Scons e1 e2) = Larw (readType e1)(readType e2)

readMaybeType :: Sexp -> Maybe Ltype 
readMaybeType mytt = case mytt of 
                      Ssym ("") -> Nothing
                      mytt -> Just (readType mytt)
                                      

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = Var -> Ltype

tenv0 :: TEnv
tenv0 "+" = Larw Lint (Larw Lint Lint)
tenv0 "-" = Larw Lint (Larw Lint Lint)
tenv0 "*" = Larw Lint (Larw Lint Lint)
tenv0 "/" = Larw Lint (Larw Lint Lint)
tenv0 x   = error ("Uknown variable: " ++ show x)

-- ¡¡¡ COMPLETER ICI !!! --
infer :: TEnv -> Lexp -> Ltype 
infer tenv (Lnum _) = Lint
infer tenv (Lvar v) = tenv v                    
infer tenv (Lhastype e _) = infer tenv e 

infer tenv (Lfun _ e) = infer tenv e

infer tenv (Lcall e _) = infer tenv e

infer tenv (Lcons e _) = infer tenv e

infer tenv (Lnil) = infer tenv Lnil

infer tenv (Lcase e _ _ _ _ ) = infer tenv e

infer tenv (Lletrec _ e ) = infer tenv e

infer tenv e = error ("Can't infer type of " ++ (show e))

check :: TEnv -> Lexp -> Ltype -> Bool
-- ¡¡¡ COMPLETER ICI !!! --
check _ (Lnum _) entier = let t' = Lint
                            in if entier == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show entier)) 
check tenv (Lvar v) carac = let t' = tenv v
                            in if carac == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show carac)) 
check tenv (Lhastype e _) dic = let t' = infer tenv e
                            in if dic == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show dic)) 
check tenv (Lfun _ e) fcann = let t' = infer tenv e
                            in if fcann == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show fcann)) 
check tenv (Lcall e _) app = let t' = infer tenv e
                            in if app == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show app)) 
check tenv (Lcons e _) conslis = let t' = infer tenv e
                               in if conslis == t' then True
                               else error ("Type mismatch "
                                   ++ (show t') ++ " != " ++ (show conslis)) 
check tenv (Lnil) lv = let t' = infer tenv Lnil
                            in if lv == t' then True
                            else error ("Type mismatch "
                                ++ (show t') ++ " != " ++ (show lv)) 
check tenv (Lcase e _ _ _ _ ) lcas = let t' = infer tenv e
                                    in if lcas == t' then True
                                    else error ("Type mismatch "
                                        ++ (show t') ++ " != " ++ (show lcas)) 
check tenv (Lletrec _ e )  letrec = let t' = infer tenv e
                                  in if letrec == t' then True
                                  else error ("Type mismatch "
                                      ++ (show t') ++ " != " ++ (show letrec)) 
check tenv e t = let t' = infer tenv e
                 in if t == t' then True
                    else error ("Type mismatch "
                                ++ (show t) ++ " != " ++ (show t'))
---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _p Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _p _ = showString "<function>"

type Env = Var -> Value

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 "+"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))
env0 "*"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))
env0 "/"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))
env0 "-"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y)))
env0 x      = error ("Uknown variable: " ++ show x)

elookup :: Env -> Var -> Value
elookup env = env

einsert :: Env -> Var -> Value -> Env
einsert env var val = \x -> if (x == var) then val else elookup env x

-- La fonction d'évaluation principale.
eval :: Env -> Lexp -> Value
eval _env (Lnum n) = Vnum n
eval env (Lvar x) = elookup env x
eval env (Lhastype e _) = eval env e
--Lfun et Lcall reglé
eval env (Lfun x e) = 
    Vfun (\v -> eval (einsert env x v) e)
eval env (Lcall e1 e2) = case eval env e1 of
                           Vfun f -> f (eval env e2)
                           _ -> error "Not a function"
eval env (Lcons e1 e2) = Vcons (eval env e1) (eval env e2)
eval env (Lcase e1 _ _ _ e2) = Vcons (eval env e1) (eval env e1) 
eval env (Lletrec xs e) =
    let lookup [] y = env y
        lookup ((x,_,e):xs) y = if x == y then eval newen e else lookup xs y
        newen = lookup xs
    in eval newen e

-- ¡¡¡ COMPLETER ICI !!! --

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                            Left _ -> [Ssym "#<parse-error>"]
                            Right es -> es
            in map (\ sexp -> let { lexp = s2l sexp
                                 ; ltyp = infer tenv0 lexp
                                 ; val = eval env0 lexp }
                             in (val, ltyp))
                   (sexps s))

-- Don't use type classes for `read'.
test :: String -> Ltype
test = infer tenv0 . s2l . read