module Eval (evalLine, main) where

-- import qualified Prelude() --; import MHSPrelude
import Control.Exception
import MicroHs.Compile
import MicroHs.CompileCache
import MicroHs.Desugar(LDef)
import MicroHs.Exp
import MicroHs.Expr
import MicroHs.Flags
import MicroHs.Ident(mkIdent, qualIdent)
import qualified MicroHs.IdentMap as M
import MicroHs.Parse
import MicroHs.StateIO
import MicroHs.SymTab(stEmpty)
import MicroHs.Translate
import MicroHs.TCMonad(TCState(..))
import MicroHs.TypeCheck(TModule(..), Symbols)
import MhsEval (withMhsContext, eval, run)
import MicroHs.ExpPrint (toStringCMdl)
import Unsafe.Coerce
import System.IO

-- This function evaluates a line of Haskell code and 
-- returns the result.
evalLine :: forall a. String -> IO a
evalLine line = do
  dir <- getMhsDir  
  -- Set up initial flags and cache
  let flags = defaultFlags dir
      --flags = defs { interactive = True, verbose = 0 }
  cache <- getCached flags
  
  -- Create module with preamble and the expression to evaluate
  let preamble = "module Interactive where\n"
                --   ++ "import Prelude\n"
                --   ++ "default Num (Integer, Double)\n"
                --   ++ "default IsString (String)\n"
                --   ++ "default Show (())\n"

      itName = "_it"
      itIOName = "_itIO"
      
      -- Wrap the expression to make it evaluable
      mkIt l = itName ++ " = " ++ l ++ "\n"
      --mkItIO l = mkIt l ++ itIOName ++ " = show $ " ++ itName ++ "\n"
      
      -- Combine preamble with the wrapped expression
      moduleStr = preamble ++ "\n" ++ mkIt line
  
  -- Parse the module
  let mdl = parseDie pTopModule "" moduleStr
  
  -- Compile the module
  ((dmdl, _, _), _) <- runStateIO (compileInteractive flags mdl) cache
  tmod <- evaluate $ compileToCombinators dmdl
  let allDefs = tBindingsOf tmod

  let rmn = mkIdent "Interactive"
      mainName = qualIdent rmn (mkIdent itName)
      cmdl = (allDefs, if noLink flags then Lit (LInt 0) else Var mainName)
      (numOutDefs, forExps, prg) = toStringCMdl cmdl


  putStrLn $ "Compiled program:\n" ++ prg
    -- Execute the compiled expression
  result <- withMhsContext $ \ctx ->
    eval ctx prg
  putStrLn $ "Result: " ++ result
  return (unsafeCoerce result :: a)

    


main :: IO ()
main = do
    let input = "1 + 2 * 3"  -- Example input
    result <- evalLine input :: IO Int
    print result