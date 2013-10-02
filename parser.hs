import Data.List
import qualified Data.Text as T

spaces :: String
spaces = "          " -- ten spaces

lineLength :: Int
lineLength = 79

delimiter :: String
delimiter = replicate lineLength '=' -- ten spaces

data HtmlTag = Span
             | Div
             | Anchor

instance Show HtmlTag where
    show Span = "span"
    show Div = "div"
    show Anchor = "a"

data Alignment = Right
               | Left

data Line = DelimiterLine
          | MultiPartLine String String
          | SinglePartLine Alignment String
          | EmptyLine
          | ParagraphLine String
          | HeaderLine String
          | HeadingOneDelimiter Int
          | HeadingTwoDelimiter Int

instance Show Line where
    show EmptyLine                        = "EmptyLine"
    show DelimiterLine                    = "DelimiterLine"
    show (MultiPartLine left right)       = "MultiPart " 
                                            ++ parens ("left: " ++ left) 
                                            ++ parens ("right: " ++ right)
    show (SinglePartLine Main.Left text)  = "SinglePartLeft " ++ parens text
    show (SinglePartLine Main.Right text) = "SinglePartRight " ++ parens text
    show (HeaderLine text)                = "HeaderLine " ++ parens text
    show (ParagraphLine text)             = "ParagraphLine " ++ parens text
    show (HeadingOneDelimiter len)        = "HeadingDelimiter " ++ show len
    show (HeadingTwoDelimiter len)        = "SubheadingDelimiter " ++ show len

parens :: String -> String
parens str = "(" ++ str ++ ")"


parse :: String -> Line
parse "" = EmptyLine
parse text 
    | length text > 79         = error "String too long"
    | delimiter == text        = DelimiterLine
    | isHeading text '='       = HeadingOneDelimiter (length . strip $ text)
    | isHeading text '-'       = HeadingTwoDelimiter (length . strip $ text)
    | centeredText text        = HeaderLine (strip text)
    | isPrefixOf spaces text   = SinglePartLine Main.Right (strip text)
    | isInfixOf  spaces text   = MultiPartLine (firstMultiPartLine text) (secondMultiPartLine text)
    | isLeftText sText         = SinglePartLine Main.Left  (strip text)
    | otherwise                = ParagraphLine (strip text)
    where sText = strip text

firstMultiPartLine :: String ->  String
firstMultiPartLine = takeWhile (/= ' ')

secondMultiPartLine :: String ->  String
secondMultiPartLine = strip . dropWhile (/= ' ')

centeredText :: String -> Bool
centeredText x = (isPrefixOf spaces x) && (length x) < lineLength

isLeftText :: String -> Bool
isLeftText x = (not . wrappedInSpaces) x && (length x) < (lineLength `div` 2)
--isLeftText x = not (isSuffixOf spaces x)

wrappedInSpaces :: String -> Bool
wrappedInSpaces = \x -> (isPrefixOf spaces x) && (isSuffixOf spaces x)

isHeading :: String -> Char -> Bool
isHeading t c = centeredText t && (head s == c) && (last s == c)
            where s = strip t

strip :: String -> String
strip x = (T.unpack . T.strip $ T.pack x)

printHtml :: Line -> String
printHtml EmptyLine                         = ""
printHtml DelimiterLine                     = divWrap "" delimiter
printHtml (MultiPartLine left right)        = multiLineWrap left right
printHtml (HeaderLine text)                 = divWrap "center" text
printHtml (SinglePartLine Main.Left text)   = divWrap "" text
printHtml (SinglePartLine Main.Right text)  = divWrap "right" text
printHtml (ParagraphLine text)              = text
printHtml (HeadingOneDelimiter length)      = ""
printHtml (HeadingTwoDelimiter length)      = ""

divWrap :: String -> String -> String
divWrap = tagWrap Div

tagWrap :: HtmlTag -> String -> String -> String
tagWrap tag id value
    | id == "" && value == "" = wrapper ""
    | id == ""                = wrapper value
    | otherwise               = wrapperID value id
    where wrapper v ="<" ++ show tag ++ ">" ++ v ++ "</" ++ show tag ++">" 
          wrapperID v id ="<" ++ show tag ++ " id='" ++ id ++ "'>" ++ v ++ "</" ++ show tag ++">" 

spanWrap :: String -> String -> String
spanWrap = tagWrap Span

multiLineWrap :: String -> String -> String
multiLineWrap x y = divWrap "" ( x ++ spanWrap y "right"  )

printLine :: Line -> String
printLine EmptyLine                         = ""
printLine DelimiterLine                     = delimiter
printLine (MultiPartLine left right)        = left ++ remainingSpace left right ++ right
printLine (SinglePartLine Main.Left text)   = (strip text)
printLine (SinglePartLine Main.Right text)  = (whiteSpaceLeft text)
printLine (HeaderLine text)                 = centerText text
printLine (ParagraphLine text)              = text
printLine (HeadingOneDelimiter length)      = centerText $ replicate length '=' 
printLine (HeadingTwoDelimiter length)      = centerText $ replicate length '-' 

remainingSpace :: String -> String -> String
remainingSpace left right= replicate len ' '
    where len = (lineLength - (length left) - (length right))

whiteSpaceLeft :: String -> String
whiteSpaceLeft t = repSpaces remaining t
    where remaining = 79 - length t

whiteSpaceRight :: String -> String
whiteSpaceRight t = strip t

centerText :: String -> String
centerText t = repSpaces hspaces t
    where hspaces = ((79 - length t) `div` 2)

repSpaces :: Int -> String -> String
repSpaces x s  = replicate x ' ' ++ s

-- main = interact (unlines . map (printLine . parse) . lines)
main = interact (unlines . map (show . parse) . lines)
