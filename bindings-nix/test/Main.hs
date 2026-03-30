module Main (main) where

import Foreign (nullPtr)
import Foreign.C (CChar, withCString)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr, unConstPtr))
import Foreign.C.String (peekCString)
import Foreign.Ptr (castPtr)
import Generated.Lowlevel.Nix.Api.Expr.Unsafe (nix_expr_eval_from_string, nix_gc_decref, nix_libexpr_init, nix_state_create, nix_state_free, nix_value_force)
import Generated.Lowlevel.Nix.Api.Store.Unsafe (nix_store_free, nix_store_open)
import Generated.Lowlevel.Nix.Api.Util (Nix_get_string_callback (..), Nix_get_string_callback_Aux (..))
import Generated.Lowlevel.Nix.Api.Value.Safe (nix_get_string)
import Generated.Lowlevel.Nix.Api.Value.Unsafe (nix_alloc_value)
import HsBindgen.Runtime.Prelude (withFunPtr)
import Relude

main :: IO ()
main = exampleFromNixCApiManual

-- | Like `withCString`
withConstCString :: String -> (ConstPtr CChar -> IO a) -> IO a
withConstCString string f = withCString string $ \stringPtr -> f (ConstPtr stringPtr)

-- | (almost) 1:1 port of the C example in "Embedding the Nix Evaluator" of Nix C API manual
exampleFromNixCApiManual :: IO ()
exampleFromNixCApiManual = do
  void $ nix_libexpr_init nullPtr

  store <- withConstCString "dummy://" \store ->
    nix_store_open nullPtr store nullPtr
  state' <- nix_state_create nullPtr nullPtr store
  value <- nix_alloc_value nullPtr state'
  void $ withConstCString "builtins.nixVersion" \expr ->
    withConstCString "." \path ->
      nix_expr_eval_from_string nullPtr state' expr path value
  void $ nix_value_force nullPtr state' value

  void $ withFunPtr myGetStringCb \cb ->
    nix_get_string nullPtr (ConstPtr value) (Nix_get_string_callback cb) nullPtr

  void $ nix_gc_decref nullPtr (ConstPtr (castPtr value))
  nix_state_free state'
  nix_store_free store
  where
    myGetStringCb :: Nix_get_string_callback_Aux
    myGetStringCb = Nix_get_string_callback_Aux \start _n _userData -> do
      string <- peekCString (unConstPtr start)
      -- for simplicity, we print here (inside the call back)
      -- a strict 1:1 port has to store a copied string into userData and then print outside
      print string
