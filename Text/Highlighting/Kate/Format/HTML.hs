{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Highlighting.Kate.Format.HTML
   Copyright   : Copyright (C) 2008-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatters that convert a list of annotated source lines to HTML.
This module only reexports the 'Text.Highlighting.Kate.Format.HTML.Blaze' module
-}

module Text.Highlighting.Kate.Format.HTML (
    module Text.Highlighting.Kate.Format.HTML.Blaze
   ) where
import Text.Highlighting.Kate.Format.HTML.Blaze
