module Deadlock.Class
       (deadCheck,
        deadCheckFile,
        ReconstructOpt (..)
       ) where

import Control.Monad.Error
import Control.Monad.Reader

import qualified Data.ByteString as B
import qualified Data.Set as Set

import Text.Parsec.Error

import AST.Class
import AST.Decl
import AST.Typ
import AST.Feature
import AST.Position

import TypeCheck.Class
import TypeCheck.TypedExpr

import Parser.Parser

import Deadlock.Expr
import Deadlock.Error
import Deadlock.Feature
import Deadlock.Context
import Deadlock.Monad
import Deadlock.OrderRel
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

overClass :: ([TFeature] -> DeadFeature a) -> TClass -> DeadFeature a
overClass f c =
  localM (setClassCtx c) (localM (updClassConst c) (f (features c)))

reconstructClass :: TClass -> DeadFeature TClass
reconstructClass c =
  do fs <- reconFeatures c
     return (c {features = fs})
  where
    reconFeatures = overClass (mapM reconFeature)
    reconFeature f =
      do (ord, lks) <- reconstructFeature f
         return (f {featureReqLk = featureReqLk f ++ toProcExprs ord,
                    featureEnsLk = featureEnsLk f ++ lks})

    toProcExprs :: OrderRel Proc -> [ProcExpr]
    toProcExprs ord =
      let d = Set.toList (dom ord)
      in [LessThan x y | x <- d, y <- d, less ord x y]

checkClass :: ReconstructOpt -> TClass -> DeadFeature ()
checkClass opt = optRecon >=> overClass (mapM_ checkFeature)
  where
    optRecon =
      case opt of
        DoReconstruct -> reconstructClass
        _ -> return

checkClassM :: ReconstructOpt -> [ClasInterface] -> TClass -> Either [PosDeadError] ()
checkClassM opt cis c = 
    let ctx = mkDeadCtx cis startingRel
    in case runFeature (checkClass opt c) ctx of
         (Right _, []) -> Right ()
         (_, e:es)   -> Left (e:es)
         (Left e, _  ) -> Left [e]

stringToError :: Clas -> String -> [PosDeadError]
stringToError clas  = (:[]) . attachPos' (initialPos file) . DeadErrorString
    where file = className clas ++ ".e"

data ReconstructOpt = DoReconstruct | SkipReconstruct deriving Show

deadCheck' :: ReconstructOpt -> Clas -> [ClasInterface] -> Maybe [PosDeadError]
deadCheck' opt c cis = 
    let tc = either (Left . stringToError c) Right (typeCheckClassM cis c)
    in either Just (const Nothing) (tc >>= checkClassM opt cis)

deadCheckFile :: ReconstructOpt -> FilePath -> IO (Maybe [PosDeadError])
deadCheckFile opt = deadCheck opt <=< B.readFile

deadCheck :: ReconstructOpt -> B.ByteString -> IO (Maybe [PosDeadError])
deadCheck opt bs =
  case parseClass bs of
    Left pe -> return (Just [fromParseError pe])
    Right c -> 
        let c' = addImplLocks c 
        in do
          interfaces <- depGenInt (className c')
          case interfaces of
            Left e -> return $ Just [parseErrorToDead e]
            Right cs -> return $ deadCheck' opt c' (map addImplLocks cs)

fromParseError :: ParseError -> PosDeadError
fromParseError pe = attachPos' (errorPos pe) (convErr pe)
    where
      convErr = DeadErrorString . showErrorMessages "or" "unknown parse error" 
                "expecting" "unexpected" "end of input" . errorMessages
