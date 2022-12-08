{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Thin compatibility layer around GHC
--
-- This should be the only module with GHC-specific CPP directives, and the
-- rest of the plugin should not import from any GHC modules directly.
module Data.Record.Plugin.Shim (
    -- * Miscellaneous
    importDecl
  , conPat
  , mkFunBind
  , HsModule
  , LHsModule
  , LRdrName
  , pattern GHC.HsModule
  , putLogMsg
  , patLoc
  , viewConPat

    -- * Extensions
  , HasDefaultExt(..)

    -- * Generalized @forall@
  , HsTyVarBndr
  , LHsTyVarBndr
  , hsFunTy
  , userTyVar
  , kindedTyVar
  , hsTyVarLName
  , setDefaultSpecificity

    -- * Re-exports

    -- The whole-sale module exports are not ideal for preserving compatibility
    -- across ghc versions, but we'll deal with this on a case by case basis.
  , module GHC.Data.Bag
  , module GHC.Driver.Main
  , module GHC.Hs
  , module GHC.Plugins
  , module GHC.Tc.Types.Evidence
  , module GHC.Types.Basic
  , module GHC.Types.Name.Cache
  , module GHC.Utils.Error
  ) where

import Data.List.NonEmpty (NonEmpty(..))

import qualified Data.List.NonEmpty as NE

import GHC.Core.Class (Class)
import GHC.Core.ConLike (ConLike)
import GHC.Core.PatSyn (PatSyn)
import GHC.Data.Bag (listToBag, emptyBag)
import GHC.Driver.Main (getHscEnv)
import GHC.Hs hiding (LHsTyVarBndr, HsTyVarBndr, HsModule, mkFunBind)
import GHC.Parser.Annotation (IsUnicodeSyntax(NormalSyntax))
import GHC.Plugins hiding ((<>), getHscEnv, putLogMsg)
import GHC.Tc.Types.Evidence (HsWrapper(WpHole))
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Types.Name.Cache (NameCache(nsUniqs))
import GHC.Utils.Error (Severity(SevError, SevWarning), mkMsgEnvelope, mkWarnMsg)
import qualified GHC.Utils.Logger (putLogMsg)

import qualified GHC.Hs      as GHC
import qualified GHC.Plugins as GHC

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Optionally @qualified@ import declaration
importDecl :: ModuleName -> Bool -> LImportDecl GhcPs
importDecl name qualified = noLoc $ ImportDecl {
      ideclExt       = defExt
    , ideclSourceSrc = NoSourceText
    , ideclName      = noLoc name
    , ideclPkgQual   = Nothing
    , ideclSafe      = False
    , ideclImplicit  = False
    , ideclAs        = Nothing
    , ideclHiding    = Nothing
#if __GLASGOW_HASKELL__ < 810
    , ideclQualified = qualified
#else
    , ideclQualified = if qualified then QualifiedPre else NotQualified
#endif
#if __GLASGOW_HASKELL__ < 900
    , ideclSource    = False
#else
    , ideclSource    = NotBoot
#endif
    }

conPat :: Located RdrName -> HsConPatDetails GhcPs -> Pat GhcPs
#if __GLASGOW_HASKELL__ < 900
conPat x y = ConPatIn x y
#else
conPat x y = ConPat noExtField x y
#endif

mkFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)] -> HsBind GhcPs
#if __GLASGOW_HASKELL__ < 810
mkFunBind = GHC.mkFunBind
#else
mkFunBind = GHC.mkFunBind Generated
#endif

#if __GLASGOW_HASKELL__ < 900
type HsModule = GHC.HsModule GhcPs
#else
type HsModule = GHC.HsModule
#endif

type LHsModule = Located HsModule
type LRdrName  = Located RdrName

putLogMsg :: DynFlags -> WarnReason -> Severity -> SrcSpan -> SDoc -> IO ()
#if __GLASGOW_HASKELL__ < 900
putLogMsg flags reason sev srcspan =
    GHC.putLogMsg flags reason sev srcspan (defaultErrStyle flags)
#else
putLogMsg = GHC.Utils.Logger.putLogMsg
#endif

{-------------------------------------------------------------------------------
  Extensions
-------------------------------------------------------------------------------}

class HasDefaultExt a where
  defExt :: a

#if __GLASGOW_HASKELL__ < 810
instance HasDefaultExt NoExt where
  defExt = noExt
#else
instance HasDefaultExt NoExtField where
  defExt = noExtField
#endif

#if __GLASGOW_HASKELL__ >= 900
instance HasDefaultExt LayoutInfo where
  defExt = NoLayoutInfo
#endif

{-------------------------------------------------------------------------------
  Generalized @forall@ in 9.0
-------------------------------------------------------------------------------}

#if __GLASGOW_HASKELL__ >= 900
type  HsTyVarBndr pass =  GHC.HsTyVarBndr () pass
type LHsTyVarBndr pass = GHC.LHsTyVarBndr () pass
#endif

hsFunTy :: XFunTy pass -> LHsType pass -> LHsType pass -> HsType pass
#if __GLASGOW_HASKELL__ < 900
hsFunTy = HsFunTy
#else
hsFunTy ext = HsFunTy ext (HsUnrestrictedArrow NormalSyntax)
#endif

userTyVar ::
     XUserTyVar pass
  -> Located (IdP pass)
  -> HsTyVarBndr pass
#if __GLASGOW_HASKELL__ < 900
userTyVar = UserTyVar
#else
userTyVar ext = UserTyVar ext ()
#endif

kindedTyVar ::
     XKindedTyVar pass
  -> Located (IdP pass)
  -> LHsKind pass
  -> HsTyVarBndr pass
#if __GLASGOW_HASKELL__ < 900
kindedTyVar = KindedTyVar
#else
kindedTyVar ext = KindedTyVar ext ()
#endif

-- | Like 'hsTyVarName', but don't throw away the location information
hsTyVarLName :: HsTyVarBndr GhcPs -> LRdrName
#if __GLASGOW_HASKELL__ < 900
hsTyVarLName (UserTyVar   _ n  ) = n
hsTyVarLName (KindedTyVar _ n _) = n
hsTyVarLName _ = panic "hsTyVarLName"
#else
hsTyVarLName (UserTyVar   _ _ n  ) = n
hsTyVarLName (KindedTyVar _ _ n _) = n
#endif

#if __GLASGOW_HASKELL__ < 900
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr pass
setDefaultSpecificity = id
#else
setDefaultSpecificity :: LHsTyVarBndr pass -> GHC.LHsTyVarBndr Specificity pass
setDefaultSpecificity (L l v) = L l $ case v of
    UserTyVar   ext () name      -> UserTyVar   ext SpecifiedSpec name
    KindedTyVar ext () name kind -> KindedTyVar ext SpecifiedSpec name kind
    XTyVarBndr  ext              -> XTyVarBndr  ext
#endif

patLoc :: SrcSpan -> Pat (GhcPass id) -> LPat (GhcPass id)
#if __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ <= 920
patLoc l p = L l p
#else
patLoc _ p = p
#endif

#if __GLASGOW_HASKELL__ < 810
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (ConPatIn a b) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 810 && __GLASGOW_HASKELL__ < 900
viewConPat :: LPat (GhcPass id) -> Maybe (Located (IdP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (L _ (ConPatIn a b)) = Just (a, b)
#elif __GLASGOW_HASKELL__ >= 900
viewConPat :: LPat (GhcPass id) -> Maybe (Located (ConLikeP (GhcPass id)), HsConPatDetails (GhcPass id))
viewConPat (L _ (ConPat _ext a b)) = Just (a, b)
#endif
viewConPat _ = Nothing

