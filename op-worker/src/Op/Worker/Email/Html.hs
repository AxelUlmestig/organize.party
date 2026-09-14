-- | Helpers for turning the HTML fragments we store in @email.emails.body@
-- into something a mail client (and a spam filter) is happy with.
module Op.Worker.Email.Html (
  htmlDocument,
  htmlToPlainText,
) where

import           Data.Char (isSpace)
import qualified Data.Text as Text
import           RIO

-- | Wrap a stored body fragment in a complete HTML document. Bare fragments
-- with no doctype and no declared charset are a well known spam signal, and
-- they leave the encoding for the client to guess at.
htmlDocument :: Text -> Text -> Text
htmlDocument subject fragment =
  Text.concat
    [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n"
    , "<meta charset=\"utf-8\">\n"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    , "<title>", escapeHtml subject, "</title>\n"
    , "</head>\n<body>\n"
    , fragment
    , "\n</body>\n</html>\n"
    ]

escapeHtml :: Text -> Text
escapeHtml = Text.concatMap escapeChar
  where
    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar c   = Text.singleton c

-- | Render a body fragment as plain text, for the @text/plain@ alternative.
--
-- This is deliberately a small hand rolled stripper rather than a real parser:
-- we generate all of the markup it has to deal with ourselves, and a sending
-- path is not worth a new dependency. Anchors keep their target so the plain
-- text version is still usable, e.g. @\<a href="url"\>text\</a\>@ becomes
-- @text (url)@.
htmlToPlainText :: Text -> Text
htmlToPlainText = normalizeWhitespace . stripTags

-- | Where an anchor's text started in the output, so its href can be compared
-- against it once the closing tag shows up.
data Anchor = Anchor
  { anchorHref  :: Text
  , anchorStart :: Int
  }

stripTags :: Text -> Text
stripTags = go "" Nothing
  where
    go acc anchor input =
      let (text, rest) = Text.break (== '<') input
          acc'         = acc <> decodeEntities text
      in if Text.null rest
           then acc'
           else
             let (rawTag, afterTag) = Text.break (== '>') (Text.drop 1 rest)
                 remaining          = Text.drop 1 afterTag
             in if Text.null afterTag
                  -- Unterminated tag, there is nothing sensible left to emit.
                  then acc'
                  else case tagName rawTag of
                    "br" -> go (acc' <> "\n") anchor remaining
                    "a"  -> go acc' (flip Anchor (Text.length acc') <$> hrefOf rawTag) remaining
                    "/a" -> go (closeAnchor acc' anchor) Nothing remaining
                    name
                      | name `elem` breakingTags -> go (acc' <> "\n") anchor remaining
                      | otherwise                -> go acc' anchor remaining

    closeAnchor acc anchor = case anchor of
      Nothing -> acc
      Just Anchor{anchorHref, anchorStart} ->
        let linkText = Text.strip (Text.drop anchorStart acc)
            href     = Text.strip anchorHref
        -- No point repeating the URL when it is already the link text.
        in if Text.null href || linkText == href
             then acc
             else acc <> " (" <> href <> ")"

-- | Tags that end a line. Only the closing halves of the block elements, so
-- that e.g. @\<div\>a\</div\>\<div\>b\</div\>@ does not pick up a blank line
-- between the two.
breakingTags :: [Text]
breakingTags =
  [ "/p", "/div", "/pre", "/li", "/tr", "/ul", "/ol", "/table", "/blockquote"
  , "/h1", "/h2", "/h3", "/h4", "/h5", "/h6", "hr"
  ]

-- | The name of a tag, given the text between its angle brackets.
tagName :: Text -> Text
tagName rawTag =
  let trimmed = Text.strip rawTag
      -- Drop the trailing slash of a self closing tag, e.g. "br/" or "br /".
      withoutSlash = fromMaybe trimmed (Text.stripSuffix "/" trimmed)
  in Text.toLower (Text.takeWhile (not . isSpace) withoutSlash)

-- | The href of an anchor tag, given the text between its angle brackets.
hrefOf :: Text -> Maybe Text
hrefOf rawTag =
  let (before, match) = Text.breakOn "href=" (Text.toLower rawTag)
  in if Text.null match
       then Nothing
       else
         -- Index back into rawTag rather than the lowercased copy, so the
         -- URL keeps its original casing.
         let value = Text.drop (Text.length before + Text.length "href=") rawTag
         in Just $ decodeEntities case Text.uncons value of
              Just ('"', rest)  -> Text.takeWhile (/= '"') rest
              Just ('\'', rest) -> Text.takeWhile (/= '\'') rest
              _                 -> Text.takeWhile (not . isSpace) value

decodeEntities :: Text -> Text
decodeEntities text = foldl' (\acc (entity, replacement) -> Text.replace entity replacement acc) text entities
  where
    entities =
      [ ("&nbsp;", " ")
      , ("&lt;", "<")
      , ("&gt;", ">")
      , ("&quot;", "\"")
      , ("&#39;", "'")
      , ("&apos;", "'")
      -- Ampersand last, so that text decoded above is not decoded a second
      -- time (e.g. "&amp;lt;" has to survive as the literal "&lt;").
      , ("&amp;", "&")
      ]

-- | Collapse the incidental whitespace that HTML ignores but plain text does
-- not: indentation, runs of spaces, and stacks of blank lines.
normalizeWhitespace :: Text -> Text
normalizeWhitespace =
  Text.strip . Text.intercalate "\n" . dropRepeatedBlanks . fmap collapseSpaces . Text.lines
  where
    collapseSpaces = Text.unwords . Text.words

    dropRepeatedBlanks = go False
      where
        go _ [] = []
        go previousWasBlank (line : rest)
          | Text.null line && previousWasBlank = go True rest
          | otherwise                          = line : go (Text.null line) rest
