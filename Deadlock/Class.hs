module Deadlock.Class (deadCheck, deadCheckFile) where

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.ByteString as B

import Text.Parsec.Error

import AST.Class
import AST.Typ
import AST.Position

import TypeCheck.Class
import TypeCheck.TypedExpr

import Parser.Parser

import Deadlock.Expr
import Deadlock.Error
import Deadlock.Feature
import Deadlock.Context
import Deadlock.Monad
import Deadlock.Instantiate

import Generate.DepGen

import Text.Parsec.Pos

updClassVars :: TClass -> ProcDeadCtx -> ProcDeadCtx
updClassVars = updVars . const . attrMap

updClassProcs :: TClass -> ProcDeadCtx -> ProcDeadCtx
updClassProcs = updRel . addProcs . (Dot:) . procGeneric

setClassCtx :: TClass -> ProcDeadCtx -> DeadFeature ProcDeadCtx
setClassCtx c ctx =
  let f = updClassVars c . updClassProcs c
  in local f (checkScope c) >> return (f ctx)

updClassConst :: TClass -> ProcDeadCtx -> DeadFeature ProcDeadCtx
updClassConst = updRelM . (\x y -> liftError (addDecls x y)) . procExpr

checkClass :: TClass -> DeadFeature ()
checkClass c = localM (setClassCtx c)
               (localM (updClassConst c)
                (mapM_ checkFeature (features c))
               )

checkClassM :: [ClasInterface] -> TClass -> Either [PosDeadError] ()
checkClassM cis c = 
    let ctx = mkDeadCtx cis startingRel
    in case runFeature (checkClass c) ctx of
         (Right _, []) -> Right ()
         (_, e:es)   -> Left (e:es)
         (Left e, _  ) -> Left [e]

stringToError :: Clas -> String -> [PosDeadError]
stringToError clas  = (:[]) . attachPos (initialPos file) . DeadErrorString
    where file = className clas ++ ".e"

deadCheck' :: Clas -> [ClasInterface] -> Maybe [PosDeadError]
deadCheck' c cis = 
    let tc = either (Left . stringToError c) Right (typeCheckClassM cis c)
    in either Just (const Nothing) (tc >>= checkClassM cis)

deadCheckFile :: FilePath -> IO (Maybe [PosDeadError])
deadCheckFile = deadCheck <=< B.readFile

deadCheck :: B.ByteString -> IO (Maybe [PosDeadError])
deadCheck bs =
  case parseClass bs of
    Left pe -> return (Just [fromParseError pe])
    Right c -> 
        let c' = addImplLocks c 
        in do
          interfaces <- depGenInt (className c')
          case interfaces of
            Left e -> return $ Just [parseErrorToDead e]
            Right cs -> return $ deadCheck' c' (map addImplLocks cs)

fromParseError :: ParseError -> PosDeadError
fromParseError pe = attachPos (errorPos pe) (convErr pe)
    where
      convErr = DeadErrorString . showErrorMessages "or" "unknown parse error" 
                "expecting" "unexpected" "end of input" . errorMessages
