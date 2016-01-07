{-
Copyright (C) 2008-2015 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.PmWiki
   Copyright   : Copyright (C) 2008-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to PmWiki markup.

PmWiki:  <http://www.pmwiki.org>
-}
module Text.Pandoc.Writers.PmWiki ( writePmWiki ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Pretty (render)
import Text.Pandoc.ImageSize
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.XML ( escapeStringForXML )
import Data.List ( intersect, intercalate )
import Network.URI ( isURI )
import Control.Monad.Reader
import Control.Monad.State

data WriterState = WriterState {
    stNotes     :: Bool            -- True if there are notes
  , stOptions   :: WriterOptions   -- writer options
  }

data WriterReader = WriterReader {
    options     :: WriterOptions -- Writer options
  , listLevel   :: String        -- String at beginning of list items, e.g. "**"
  , useTags     :: Bool          -- True if we should use HTML tags because we're in a complex list
  }

type PmWikiWriter = ReaderT WriterReader (State WriterState)

-- | Convert Pandoc to PmWiki.
writePmWiki :: WriterOptions -> Pandoc -> String
writePmWiki opts document =
  let initialState = WriterState { stNotes = False, stOptions = opts }
      env = WriterReader { options = opts, listLevel = [], useTags = False }
  in  evalState (runReaderT (pandocToPmWiki document) env) initialState

-- | Return PmWiki representation of document.
pandocToPmWiki :: Pandoc -> PmWikiWriter String
pandocToPmWiki (Pandoc meta blocks) = do
  opts <- asks options
  metadata <- metaToJSON opts
              (fmap trimr . blockListToPmWiki)
              inlineListToPmWiki
              meta
  body <- blockListToPmWiki blocks
  notesExist <- gets stNotes
  let notes = if notesExist
                 then "\n<references />"
                 else ""
  let main = body ++ notes
  let context = defField "body" main
                $ defField "toc" (writerTableOfContents opts) metadata
  return $ if writerStandalone opts
     then renderTemplate' (writerTemplate opts) context
     else main

-- | Escape special characters for PmWiki.
escapeString :: String -> String
escapeString =  escapeStringForXML

-- | Convert Pandoc block element to PmWiki.
blockToPmWiki :: Block         -- ^ Block element
              -> PmWikiWriter String

blockToPmWiki Null = return ""

-- XXX
blockToPmWiki (Div attrs bs) = do
  contents <- blockListToPmWiki bs
  return $ render Nothing (tagWithAttrs "div" attrs) ++ "\n\n" ++
                     contents ++ "\n\n" ++ "</div>"

blockToPmWiki (Plain inlines) =
  inlineListToPmWiki inlines

-- title beginning with fig: indicates that the image is a figure
-- XXX
blockToPmWiki (Para [Image attr txt (src,'f':'i':'g':':':tit)]) = do
  capt <- if null txt
             then return ""
             else ("|caption " ++) `fmap` inlineListToPmWiki txt
  img  <- imageToPmWiki attr
  let opt = if null txt
               then ""
               else "|alt=" ++ if null tit then capt else tit ++ capt
  return $ "[[File:" ++ src ++ "|frame|none" ++ img ++ opt ++ "]]\n"

-- XXX
blockToPmWiki (Para inlines) = do
  tags <- asks useTags
  lev <- asks listLevel
  contents <- inlineListToPmWiki inlines
  return $ if tags
              then  "<p>" ++ contents ++ "</p>"
              else contents ++ if null lev then "\n" else ""

-- XXX
blockToPmWiki (RawBlock f str)
  | f == Format "pmwiki" = return str
  | f == Format "html"   = return str
  | otherwise            = return ""

blockToPmWiki HorizontalRule = return "\n-----\n"

blockToPmWiki (Header level _ inlines) = do
  contents <- inlineListToPmWiki inlines
  let bangs = replicate level '!'
  return $ bangs ++ " " ++ contents ++ "\n"

{-
blockToMediaWiki (CodeBlock (_,classes,_) str) = do
  let at  = classes `intersect` ["actionscript", "ada", "apache", "applescript", "asm", "asp",
                       "autoit", "bash", "blitzbasic", "bnf", "c", "c_mac", "caddcl", "cadlisp", "cfdg", "cfm",
                       "cpp", "cpp-qt", "csharp", "css", "d", "delphi", "diff", "div", "dos", "eiffel", "fortran",
                       "freebasic", "gml", "groovy", "html4strict", "idl", "ini", "inno", "io", "java", "java5",
                       "javascript", "latex", "lisp", "lua", "matlab", "mirc", "mpasm", "mysql", "nsis", "objc",
                       "ocaml", "ocaml-brief", "oobas", "oracle8", "pascal", "perl", "php", "php-brief", "plsql",
                       "python", "qbasic", "rails", "reg", "robots", "ruby", "sas", "scheme", "sdlbasic",
                       "smalltalk", "smarty", "sql", "tcl", "", "thinbasic", "tsql", "vb", "vbnet", "vhdl",
                       "visualfoxpro", "winbatch", "xml", "xpp", "z80"]
  return $
    if null at
       then "<pre" ++ (if null classes
                          then ">"
                          else " class=\"" ++ unwords classes ++ "\">") ++
            escapeString str ++ "</pre>"
       else "<source lang=\"" ++ head at ++ "\">" ++ str ++ "</source>"
            -- note:  no escape!
-}
-- pmwiki does not support syntax highlighting (w/out plugins)
blockToPmWiki (CodeBlock (_,classes,_) str) = do
  return $ "[@\n" ++ str ++ "\n@]"

blockToPmWiki (BlockQuote blocks) = do
  contents <- blockListToPmWiki blocks
  return $ "->" ++ contents

{-
blockToMediaWiki (Table capt aligns widths headers rows') = do
  caption <- if null capt
                then return ""
                else do
                   c <- inlineListToMediaWiki capt
                   return $ "|+ " ++ trimr c ++ "\n"
  let headless = all null headers
  let allrows = if headless then rows' else headers:rows'
  tableBody <- intercalate "|-\n" `fmap`
                mapM (tableRowToMediaWiki headless aligns widths)
                     (zip [1..] allrows)
  return $ "{|\n" ++ caption ++ tableBody ++ "|}\n"
-}
blockToPmWiki (Table capt aligns widths headers rows') = do
  return $ "FIXME table"

blockToPmWiki x@(BulletList items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        {- This code path bailed out to html lists in mediawiki, we can't
        contents <- local (\ s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ul>\n" ++ vcat contents ++ "</ul>\n"
        -}
        return $ "FIXME <ul> bailout"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ "*" }) $ mapM listItemToPmWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

blockToPmWiki x@(OrderedList attribs items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        {- This code path bailed out to html lists in mediawiki, we can't
        contents <- local (\s -> s { useTags = True }) $ mapM listItemToMediaWiki items
        return $ "<ol" ++ listAttribsToString attribs ++ ">\n" ++ vcat contents ++ "</ol>\n"
        -}
        return $ "FIXME <ol> bailout"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ "#" }) $ mapM listItemToPmWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

blockToPmWiki x@(DefinitionList items) = do
  tags <- fmap (|| not (isSimpleList x)) $ asks useTags
  if tags
     then do
        {- This code path bailed out to html lists in mediawiki, we can't
        contents <- local (\s -> s { useTags = True }) $ mapM definitionListItemToMediaWiki items
        return $ "<dl>\n" ++ vcat contents ++ "</dl>\n"
        -}
        return $ "FIXME <dl> bailout"
     else do
        lev <- asks listLevel
        contents <- local (\s -> s { listLevel = listLevel s ++ ":" }) $ mapM definitionListItemToPmWiki items
        return $ vcat contents ++ if null lev then "\n" else ""

-- Auxiliary functions for lists:

-- | Convert ordered list attributes to HTML attribute string
{-
listAttribsToString :: ListAttributes -> String
listAttribsToString (startnum, numstyle, _) =
  let numstyle' = camelCaseToHyphenated $ show numstyle
  in  (if startnum /= 1
          then " start=\"" ++ show startnum ++ "\""
          else "") ++
      (if numstyle /= DefaultStyle
          then " style=\"list-style-type: " ++ numstyle' ++ ";\""
          else "")
-}

-- | Convert bullet or ordered list item (list of blocks) to PmWiki.
listItemToPmWiki :: [Block] -> PmWikiWriter String
listItemToPmWiki items = do
  contents <- blockListToPmWiki items
  tags <- asks useTags
  if tags
     then return $ "FIXME <li>" -- was: "<li>" ++ contents ++ "</li>"
     else do
       marker <- asks listLevel
       return $ marker ++ " " ++ contents

-- | Convert definition list item (label, list of blocks) to PmWiki.
definitionListItemToPmWiki :: ([Inline],[[Block]])
                           -> PmWikiWriter String
definitionListItemToPmWiki (label, items) = do
  labelText <- inlineListToPmWiki label
  contents <- mapM blockListToPmWiki items
  tags <- asks useTags
  if tags
     {- Can't bail out to html in pmwiki
     then return $ "<dt>" ++ labelText ++ "</dt>\n" ++
           intercalate "\n" (map (\d -> "<dd>" ++ d ++ "</dd>") contents)
     -}
     then return $ "FIXME <dt>"
     else do
       marker <- asks listLevel
       return $ marker ++ " " ++ labelText ++ "\n" ++
           intercalate "\n" (map (\d -> init marker ++ ": :" ++ d) contents)

-- | True if the list can be handled by simple wiki markup, False if HTML tags will be needed.
-- XXX
isSimpleList :: Block -> Bool
isSimpleList x =
  case x of
       BulletList items                 -> all isSimpleListItem items
       OrderedList (num, sty, _) items  -> all isSimpleListItem items &&
                                            num == 1 && sty `elem` [DefaultStyle, Decimal]
       DefinitionList items             -> all isSimpleListItem $ concatMap snd items
       _                                -> False

-- | True if list item can be handled with the simple wiki syntax.  False if
--   HTML tags will be needed.
-- XXX
isSimpleListItem :: [Block] -> Bool
isSimpleListItem []  = True
isSimpleListItem [x] =
  case x of
       Plain _           -> True
       Para  _           -> True
       BulletList _      -> isSimpleList x
       OrderedList _ _   -> isSimpleList x
       DefinitionList _  -> isSimpleList x
       _                 -> False
isSimpleListItem [x, y] | isPlainOrPara x =
  case y of
       BulletList _      -> isSimpleList y
       OrderedList _ _   -> isSimpleList y
       DefinitionList _  -> isSimpleList y
       _                 -> False
isSimpleListItem _ = False

isPlainOrPara :: Block -> Bool
isPlainOrPara (Plain _) = True
isPlainOrPara (Para  _) = True
isPlainOrPara _         = False

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

-- Auxiliary functions for tables:

{-
tableRowToMediaWiki :: Bool
                    -> [Alignment]
                    -> [Double]
                    -> (Int, [[Block]])
                    -> MediaWikiWriter String
tableRowToMediaWiki headless alignments widths (rownum, cells) = do
  cells' <- mapM (tableCellToMediaWiki headless rownum)
          $ zip3 alignments widths cells
  return $ unlines cells'

tableCellToMediaWiki :: Bool
                     -> Int
                     -> (Alignment, Double, [Block])
                     -> MediaWikiWriter String
tableCellToMediaWiki headless rownum (alignment, width, bs) = do
  contents <- blockListToMediaWiki bs
  let marker = if rownum == 1 && not headless then "!" else "|"
  let percent w = show (truncate (100*w) :: Integer) ++ "%"
  let attrs = ["align=" ++ show (alignmentToString alignment) |
                 alignment /= AlignDefault && alignment /= AlignLeft] ++
              ["width=\"" ++ percent width ++ "\"" |
                 width /= 0.0 && rownum == 1]
  let attr = if null attrs
                then ""
                else unwords attrs ++ "|"
  let sep = case bs of
                 [Plain _] -> " "
                 [Para  _] -> " "
                 _         -> "\n"
  return $ marker ++ attr ++ sep ++ trimr contents

alignmentToString :: Alignment -> String
alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"
-}

-- XXX This is probably broken somehow
imageToPmWiki :: Attr -> PmWikiWriter String
imageToPmWiki attr = do
  opts <- gets stOptions
  let (_, cls, _) = attr
      toPx = fmap (showInPixel opts) . checkPct
      checkPct (Just (Percent _)) = Nothing
      checkPct maybeDim = maybeDim
      go (Just w) Nothing  = '|':w ++ "px"
      go (Just w) (Just h) = '|':w ++ "x" ++ h ++ "px"
      go Nothing  (Just h) = "|x" ++ h ++ "px"
      go Nothing  Nothing  = ""
      dims = go (toPx $ dimension Width attr) (toPx $ dimension Height attr)
      classes = if null cls
                   then ""
                   else "FIXME image class"
  return $ dims ++ classes

-- | Convert list of Pandoc block elements to PmWiki.
blockListToPmWiki :: [Block]       -- ^ List of block elements
                  -> PmWikiWriter String
blockListToPmWiki blocks =
  fmap vcat $ mapM blockToPmWiki blocks

-- | Convert list of Pandoc inline elements to PmWiki.
inlineListToPmWiki :: [Inline] -> PmWikiWriter String
inlineListToPmWiki lst =
  fmap concat $ mapM inlineToPmWiki lst

-- | Convert Pandoc inline element to PmWiki.
inlineToPmWiki :: Inline -> PmWikiWriter String

{-
inlineToMediaWiki (Span attrs ils) = do
  contents <- inlineListToMediaWiki ils
  return $ render Nothing (tagWithAttrs "span" attrs) ++ contents ++ "</span>"
-}
inlineToPmWiki (Span attrs ils) = do
  return $ "FIXME span"

inlineToPmWiki (Emph lst) = do
  contents <- inlineListToPmWiki lst
  return $ "''" ++ contents ++ "''"

inlineToPmWiki (Strong lst) = do
  contents <- inlineListToPmWiki lst
  return $ "'''" ++ contents ++ "'''"

inlineToPmWiki (Strikeout lst) = do
  contents <- inlineListToPmWiki lst
  return $ "{-" ++ contents ++ "-}"

inlineToPmWiki (Superscript lst) = do
  contents <- inlineListToPmWiki lst
  return $ "^" ++ contents ++ "^"

inlineToPmWiki (Subscript lst) = do
  contents <- inlineListToPmWiki lst
  return $ "_" ++ contents ++ "_"

-- XXX pmwiki has no native support for small caps (neither did mediawiki,
-- which just ran it through as plain text, so we'll do the same
inlineToPmWiki (SmallCaps lst) = inlineListToPmWiki lst

-- XXX not sure what these are, but unicode seems like a safe bet to leave
inlineToPmWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToPmWiki lst
  return $ "\8216" ++ contents ++ "\8217"

-- XXX not sure what these are, but unicode seems like a safe bet to leave
inlineToPmWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToPmWiki lst
  return $ "\8220" ++ contents ++ "\8221"

-- XXX yeah, citations aren't a thing either
inlineToPmWiki (Cite _  lst) = inlineListToPmWiki lst

inlineToPmWiki (Code _ str) =
  return $ "[@" ++ escapeString str ++ "@]"

inlineToPmWiki (Str str) = return $ escapeString str

{-
inlineToMediaWiki (Math _ str) = return $ "<math>" ++ str ++ "</math>"
                               -- note:  str should NOT be escaped
-}
inlineToPmWiki (Math _ str) = return $ "FIXME math"

inlineToPmWiki (RawInline f str)
  | f == Format "pmwiki" = return str
  | f == Format "html"   = return str
  | otherwise            = return ""

inlineToPmWiki (LineBreak) = return "\\\\\n"

inlineToPmWiki SoftBreak = do
  wrapText <- gets (writerWrapText . stOptions)
  case wrapText of
       WrapAuto     -> return " "
       WrapNone     -> return " "
       WrapPreserve -> return "\n"

inlineToPmWiki Space = return " "

{-
inlineToMediaWiki (Link _ txt (src, _)) = do
  label <- inlineListToMediaWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return src
     _  -> return $ if isURI src
              then "[" ++ src ++ " " ++ label ++ "]"
              else "[[" ++ src' ++ "|" ++ label ++ "]]"
                     where src' = case src of
                                     '/':xs -> xs  -- with leading / it's a
                                     _      -> src -- link to a help page
-}
-- XXX doesn't handle pmwiki nits, like ()'s
-- XXX doesn't handle verifying http[s] present (though maybe isURI is equiv?)
inlineToPmWiki (Link _ txt (src, _)) = do
  label <- inlineListToPmWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return src
     _  -> return $ "[[" ++ src ++ "|" ++ label ++ "]]"

{-
inlineToMediaWiki (Image attr alt (source, tit)) = do
  img  <- imageToMediaWiki attr
  alt' <- inlineListToMediaWiki alt
  let txt = if null tit
               then if null alt
                       then ""
                       else '|' : alt'
               else '|' : tit
  return $ "[[File:" ++ source ++ img ++ txt ++ "]]"
-}
inlineToPmWiki (Image attr alt (source, tit)) = do
  return $ "FIXME inline image"

{-
inlineToMediaWiki (Note contents) = do
  contents' <- blockListToMediaWiki contents
  modify (\s -> s { stNotes = True })
  return $ "<ref>" ++ contents' ++ "</ref>"
  -- note - may not work for notes with multiple blocks
-}
inlineToPmWiki (Note contents) = do
  return $ "FIXME Note issue"
