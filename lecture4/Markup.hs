module Markup where

data Tree a = Tree a [Tree a]
  deriving Show
type Name = String
type Attribute = (String, String)
data Tag = Pair Name [Attribute] [Xml] | Empty Name [Attribute]
  deriving Show
data XmlNode = Element Tag | TextNode String
  deriving Show
type Xml = Tree XmlNode

attributeToString :: Attribute -> String
attributeToString (name, value) = " " ++ name ++ "=\"" ++ value ++ "\""

tagToString :: Tag -> String
tagToString (Empty name attributes) =
  "<" ++ name ++ (concat (map attributeToString attributes)) ++ "/>"
tagToString (Pair name attributes xml) =
  "<" ++ name ++ (concat (map attributeToString attributes)) ++ ">" ++
  (concat (map xmlToString xml)) ++
  "</" ++ name ++ ">"

xmlToString :: Xml -> String
xmlToString (Tree (Element tag) trees) = tagToString tag ++ (concat (map xmlToString trees))
xmlToString (Tree (TextNode text) trees) = text ++ (concat (map xmlToString trees))

newPairTag :: Name -> [Attribute] -> [Xml] -> Tag
newPairTag name attributes children = Pair name attributes children

newEmptyTag :: Name -> [Attribute] -> Tag
newEmptyTag name attributes = Empty name attributes

newH1 :: [Attribute] -> [Xml] -> Tag
newH1 attributes children = newPairTag "h1" attributes children

newP :: [Attribute] -> [Xml] -> Tag
newP attributes children = newPairTag "p" attributes children

newBody :: [Attribute] -> [Xml] -> Tag
newBody attributes children = newPairTag "body" attributes children

newTitle :: [Attribute] -> [Xml] -> Tag
newTitle attributes children = newPairTag "title" attributes children

newHead :: [Attribute] -> [Xml] -> Tag
newHead attributes children = newPairTag "head" attributes children

newHtml :: [Attribute] -> [Xml] -> Tag
newHtml attributes children = newPairTag "html" attributes children

title = newTitle [] [Tree (TextNode "Page Title") []]
h1 = newH1 [] [Tree (TextNode "A Heading") []]
p = newP [] [Tree (TextNode "A paragraph") []]
header = newHead [] [Tree (Element title) []]
body = newBody [("class", "content"), ("id", "main")] [Tree (Element h1) [], Tree (Element p) []]
html = newHtml [] [Tree (Element header) [], Tree (Element body) []]

example :: Xml
example = Tree (Element html) []