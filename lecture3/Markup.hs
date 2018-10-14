module Markup where

type TextNode = String
type Attribute = (String, String)
data Tag = Tag String [Attribute] TextNode XML
  deriving Show
type XML = [Tag]

attributeToString :: Attribute -> String
attributeToString (name, value) = " " ++ name ++ "=\"" ++ value ++ "\""

tagToString :: Tag -> String
tagToString (Tag name attributes textNode xml) =
  "<" ++ name ++ (foldr (++) "" (map attributeToString attributes)) ++ ">" ++
  textNode ++ (xmlToString xml) ++
  "</" ++ name ++ ">"

xmlToString :: XML -> String
xmlToString xml = foldr (++) "" (map tagToString xml)

h1 :: Tag
h1 = Tag "h1" [] "A heading" []

p :: Tag
p = Tag "p" [] "A paragraph" []

body :: Tag
body = Tag "body" [("class", "content"), ("id", "main")] "" [h1, p]

title :: Tag
title = Tag "title" [] "Page Title" []

header :: Tag
header = Tag "head" [] "" [title]

html :: Tag
html = Tag "html" [] "" [header, body]

document :: XML
document = [html]