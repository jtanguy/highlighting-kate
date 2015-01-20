{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Highlighting.Kate.Format.HTML.Lucid
   Copyright   : Copyright (C) 2008-2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Formatter that convert a list of annotated source lines to HTML, using the
'lucid' library.
-}

module Text.Highlighting.Kate.Format.HTML.Lucid (
      formatHtmlInline, formatHtmlBlock, styleToCss
   ) where
import Text.Highlighting.Kate.Types
import Lucid
import Data.Monoid
import Data.List (intersperse)
import qualified Data.Text as T

-- | Format tokens using HTML spans inside @code@ tags. For example,
-- A @KeywordTok@ is rendered as a span with class @kw@.
-- Short class names correspond to 'TokenType's as follows:
-- 'KeywordTok' = @kw@, 'DataTypeTok' = @dt@,
-- 'DecValTok' = @dv@, 'BaseNTok' = @bn@, 'FloatTok' = @fl@,
-- 'CharTok' = @ch@, 'StringTok' = @st@, 'CommontTok' = @co@,
-- 'OtherTok' = @ot@, 'AlertTok' = @al@, 'FunctionTok' = @fu@,
-- 'RegionMarkerTok' = @re@, 'ErrorTok' = @er@. A 'NormalTok'
-- is not marked up at all.
formatHtmlInline :: FormatOptions -> [SourceLine] -> Html ()
formatHtmlInline opts = code_ [ class_ (T.pack $ unwords $ "sourceCode" : codeClasses opts)]
                                . mconcat . intersperse (toHtml ("\n" :: T.Text))
                                . map (sourceLineToHtml opts)

tokenToHtml :: FormatOptions -> Token -> Html ()
tokenToHtml _ (NormalTok, txt)  = toHtml txt
tokenToHtml opts (toktype, txt) =
  if titleAttributes opts
     then with sp [title_ (T.pack $ show toktype)]
     else sp
   where sp = span_ [ class_ (short toktype)] $ toHtml txt

short :: TokenType -> T.Text
short KeywordTok        = "kw"
short DataTypeTok       = "dt"
short DecValTok         = "dv"
short BaseNTok          = "bn"
short FloatTok          = "fl"
short CharTok           = "ch"
short StringTok         = "st"
short CommentTok        = "co"
short OtherTok          = "ot"
short AlertTok          = "al"
short FunctionTok       = "fu"
short RegionMarkerTok   = "re"
short ErrorTok          = "er"
short NormalTok         = ""

sourceLineToHtml :: FormatOptions -> SourceLine -> Html ()
sourceLineToHtml opts contents = mapM_ (tokenToHtml opts) contents

formatHtmlBlockPre :: FormatOptions -> [SourceLine] -> Html ()
formatHtmlBlockPre opts = pre_ . formatHtmlInline opts

-- | Format tokens as an HTML @pre@ block. If line numbering is
-- selected, this is put into a table row with line numbers in the
-- left cell.
formatHtmlBlock :: FormatOptions -> [SourceLine] -> Html ()
formatHtmlBlock opts ls = with container [ class_ (T.pack $ unwords classes) ]
  where  container = if numberLines opts
                        then table_ $ tr_ [ class_ "sourceCode" ]
                                     $ nums >> source
                        else pre
         classes = "sourceCode" :
                   [x | x <- containerClasses opts, x /= "sourceCode"]
         pre = formatHtmlBlockPre opts ls
         source = td_ [ class_ "sourceCode" ] $ pre
         startNum = startNumber opts
         nums = td_  [ class_ "lineNumbers" ]
                     $ pre_
                     $ mapM_ lineNum [startNum..(startNum + length ls - 1)]
         lineNum n = if lineAnchors opts
                        then (a_ [ id_ nStr , href_ ("#" `mappend` nStr) ] $ toHtml $ show n)
                              >> toHtml ("\n" :: T.Text)
                        else toHtml $ show n `mappend` "\n"
           where nStr = T.pack $ show n
-- | Returns CSS for styling highlighted code according to the given style.
styleToCss :: Style -> String
styleToCss f = unlines $ tablespec ++ colorspec ++ map toCss (tokenStyles f)
   where colorspec = case (defaultColor f, backgroundColor f) of
                          (Nothing, Nothing) -> []
                          (Just c, Nothing)  -> ["pre, code { color: " ++ fromColor c ++ "; }"]
                          (Nothing, Just c)  -> ["pre, code { background-color: " ++ fromColor c ++ "; }"]
                          (Just c1, Just c2) -> ["pre, code { color: " ++ fromColor c1 ++ "; background-color: " ++
                                                  fromColor c2 ++ "; }"]
         tablespec = [
           "table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {"
          ,"  margin: 0; padding: 0; vertical-align: baseline; border: none; }"
          ,"table.sourceCode { width: 100%; line-height: 100%; " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (backgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (defaultColor f) ++
             "}"
          ,"td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; " ++
             maybe "" (\c -> "background-color: " ++ fromColor c ++ "; ") (lineNumberBackgroundColor f) ++
             maybe "" (\c -> "color: " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             maybe "" (\c -> "border-right: 1px solid " ++ fromColor c ++ "; ") (lineNumberColor f) ++
             "}"
          ,"td.sourceCode { padding-left: 5px; }"
          ]

toCss :: (TokenType, TokenStyle) -> String
toCss (t,tf) = "code > span." ++ T.unpack (short t) ++ " { "
                ++ colorspec ++ backgroundspec ++ weightspec ++ stylespec
                ++ decorationspec ++ "}"
  where colorspec = maybe "" (\col -> "color: " ++ fromColor col ++ "; ") $ tokenColor tf
        backgroundspec = maybe "" (\col -> "background-color: " ++ fromColor col ++ "; ") $ tokenBackground tf
        weightspec = if tokenBold tf then "font-weight: bold; " else ""
        stylespec  = if tokenItalic tf then "font-style: italic; " else ""
        decorationspec = if tokenUnderline tf then "text-decoration: underline; " else ""

