module Main where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import Numeric
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

data LispVal
  = Atom String
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func
      { params :: [String]
      , vararg :: (Maybe String)
      , body :: [LispVal]
      , closure :: Env
      }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  | Nil ()

instance Show LispVal where
  show = showLispVal

unwordsList = unwords . map showLispVal

showLispVal :: LispVal -> String
showLispVal (String contents) = "\"" ++ contents ++ "\""
showLispVal (PrimitiveFunc _) = "<primitive>"
showLispVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++
  unwords (map show args) ++
  (case varargs of
     Nothing -> ""
     Just arg -> " . " ++ arg) ++
  ") ...)"
showLispVal (IOFunc _) = "<IO primitive>"
showLispVal (Port _) = "<IO port>"
showLispVal (Atom name) = name
showLispVal (Number val) = show val
showLispVal (Bool True) = "#t"
showLispVal (Bool False) = "#f"
showLispVal (List contents) = "(" ++ unwordsList contents ++ ")"
showLispVal (DottedList head tail) =
  "(" ++ unwordsList head ++ "." ++ showLispVal tail ++ ")"

data LispErr
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

instance Show LispErr where
  show = showLispErr

type ThrowsError = Either LispErr

type IOThrowsError = ExceptT LispErr IO

type Env = IORef [(String, IORef LispVal)]

trapError action = catchError action (return . show)

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

showLispErr :: LispErr -> String
showLispErr (NumArgs expected found) =
  "Expected " ++
  show expected ++ " args; found values (" ++ unwordsList found ++ ")"
showLispErr (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showLispErr (Parser parseErr) = "Parse error at " ++ show parseErr
showLispErr (BadSpecialForm message form) = message ++ ": " ++ show form
showLispErr (NotFunction message func) = message ++ ": " ++ func
showLispErr (UnboundVar message varname) = message ++ ": " ++ varname

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $
    case atom of
      "#t" -> Bool True
      "#d" -> Bool False
      _ -> Atom atom

parseAnyList :: Parser LispVal
parseAnyList = do
  char '('
  spaces
  head <- sepEndBy parseExpr spaces1
  tail <- (char '.' >> spaces1 >> parseExpr) <|> return (Nil ())
  spaces
  char ')'
  return $
    case tail of
      (Nil ()) -> List head
      otherwise -> DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quoted", x]

parseDec1 :: Parser LispVal
parseDec1 = many1 digit >>= (return . Number . read)

parseDec2 :: Parser LispVal
parseDec2 = do
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

hexRead x = fst (readHex x !! 0)

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  (return . Number . hexRead) x

octRead x = fst (readOct x !! 0)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#x"
  x <- many1 octDigit
  (return . Number . octRead) x

parseNumber :: Parser LispVal
parseNumber = parseDec1 <|> parseDec2 <|> parseHex <|> parseOct

parseFloat :: Parser LispVal
parseFloat = do
  i <- many1 digit
  char '.'
  d <- many1 digit
  return $ Float (fst $ readFloat (i ++ "." ++ d) !! 0)

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\"\\nrt"
  return $
    case x of
      '\\' -> x
      '"' -> x
      'n' -> '\n'
      'r' -> '\r'
      't' -> '\t'

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\"\\")
  char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

skipComment :: Parser ()
skipComment =
  optional $ char ';' >> manyTill anyChar newline >> spaces >> return ()

spaces1 :: Parser ()
spaces1 = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseExpr :: Parser LispVal
parseExpr = do
  try skipComment
  parseAtom <|> parseString <|> try parseBool <|> try parseNumber <|>
    try parseFloat <|>
    parseQuoted <|>
    parseAnyList

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input =
  case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr

readExprList = readOrThrow (endBy parseExpr spaces1)

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var =
  readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe
    (throwError $ UnboundVar "Setting an unbound variable" var)
    (liftIO . (flip writeIORef value))
    (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
           valueRef <- newIORef value
           env <- readIORef envRef
           writeIORef envRef ((var, valueRef) : env)
           return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quoted", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool True -> eval env conseq
    Bool False -> eval env alt
    otherwise -> throwError $ TypeMismatch "bool" pred
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define":List (Atom var:params):body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define":DottedList (Atom var:params) varargs:body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda":List params:body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda":DottedList params varargs:body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda":varargs@(Atom _):body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function:args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env badForm =
  throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>=
         evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = liftM last $ mapM (eval env) body
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
        Nothing -> return env
apply (IOFunc func) args = func args

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func:args) = apply func args

makeFunc varargs env params body =
  return $ Func (map showLispVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarArgs = makeFunc . Just . showLispVal

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv >>=
  (flip bindVars $
   map (makeFunc IOFunc) ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
  where
    makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+))
  , ("-", numericBinop (-))
  , ("*", numericBinop (*))
  , ("/", numericBinop div)
  , ("mod", numericBinop mod)
  , ("quotient", numericBinop quot)
  , ("remainder", numericBinop rem)
  , ("=", numBoolBinop (==))
  , ("<", numBoolBinop (<))
  , (">", numBoolBinop (>))
  , ("/=", numBoolBinop (/=))
  , (">=", numBoolBinop (>=))
  , ("<=", numBoolBinop (<=))
  , ("&&", boolBoolBinop (&&))
  , ("||", boolBoolBinop (||))
  , ("string=?", strBoolBinop (==))
  , ("string<?", strBoolBinop (<))
  , ("string>?", strBoolBinop (>))
  , ("string<=?", strBoolBinop (<=))
  , ("string>=?", strBoolBinop (>=))
  , ("car", car)
  , ("cdr", cdr)
  , ("cons", cons)
  , ("eq?", eqv)
  , ("eqv?", eqv)
  ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc)
  , ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArg = throwError $ NumArgs 1 badArg

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArg = throwError $ NumArgs 1 badArg

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs xl] = return $ DottedList (x : xs) xl
cons [x1, x2] = return $ DottedList [x1] x2
cons badArg = throwError $ NumArgs 2 badArg

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool (arg1 == arg2)
eqv [(Number arg1), (Number arg2)] = return $ Bool (arg1 == arg2)
eqv [(String arg1), (String arg2)] = return $ Bool (arg1 == arg2)
eqv [(Atom arg1), (Atom arg2)] = return $ Bool (arg1 == arg2)
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair (zip arg1 arg2))
  where
    eqvPair (x1, x2) =
      case eqv [x1, x2] of
        Left err -> False
        Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgs = throwError $ NumArgs 2 badArgs

numericBinop ::
     (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop ::
     (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool (left `op` right)

boolBoolBinop = boolBinop unpackBool

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

strBoolBinop = boolBinop unpackStr

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

numBoolBinop = boolBinop unpackNum

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runFile :: [String] -> IO ()
runFile args = do
  env <-
    primitiveBindings >>=
    flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>=
    hPutStrLn stderr

runRepl :: IO ()
runRepl =
  primitiveBindings >>=
  until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runFile $ args
