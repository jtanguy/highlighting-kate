{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Highlighting.Kate.Format.HTML
   Copyright   : Copyright (C) 2008-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to HTML.
-}

module Text.Highlighting.Kate.Format.HTML (
      formatHtmlInline, formatHtmlBlock, styleToCss
   ) where
#ifdef _LUCID
import Text.Highlighting.Kate.Format.HTML.Lucid
#else
import Text.Highlighting.Kate.Format.HTML.Blaze
#endif
