{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.IO
import Data.Maybe
import Data.Char
import qualified Data.Text as T
import Data.List

-- ########################### Парсер для документации в HTML ###################################

------------ переменные

mainStartHTML = "<!DOCTYPE html><html lang='en'><head><meta charset='UTF-8'><meta http-equiv='X-UA-Compatible' content='IE=edge'><meta name='viewport' content='width=device-width, initial-scale=1.0'><title>Document</title><style>h1, h2, h3, h4, h5, p, a {margin: 0;padding: 0;} span {color:darkgrey;} a {color:darkgreen;} .list {display: -webkit-flex;display: -ms-flexbox;display: flex;-webkit-flex-wrap: wrap;-ms-flex-wrap: wrap;flex-wrap: wrap;justify-content:space-between; margin-top: 10px;margin-bottom: 10px;} .block {display: table-cell; margin-left: 2%; width: 21%;padding: 10px;-webkit-box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.2);-moz-box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.2);box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.2);text-align: center;margin-bottom: 10px; transition: 0.3s;} .block:hover {display: table-cell; margin-left: 2%; width: 21%;padding: 10px;-webkit-box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.4);-moz-box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.4);box-shadow: 0px 0px 15px 0px rgba(34, 60, 80, 0.4);} .block_header {font-weight: bold;margin-bottom: 10px;} .btnLink {color: #fff;padding: 4px 15px;background-color: darkgreen;font-weight: bold;text-decoration: none;cursor: pointer;border: 2px solid darkgreen;transition: 0.3s; margin-top: 1px;margin-bottom: 1px; display: inline-block} .btnLink:hover {background-color: transparent;color: darkgreen;} li {margin-top: 7px;margin-bottom: 7px;}</style></head><body>"
mainFinishHTML = "</body></html>"

------------ функуци

-- ######## поиск всех индексов подстрок
isPref "" _ = True
isPref _ "" = False
isPref f@(s:ss) (q:qs) = if (s==q) then isPref ss qs else False 

search _ "" = False
search f q@(s:ss) = (isPref f q) || (search f ss)


-- ######## печать заголовков (<H></H>)
printHeadingTag text newText count isEndH | length text == 0 || head text == '\n' = parseText text (newText ++ ("</h" ++ show(count) ++ ">")) 1
                                          | head text == ' ' && isEndH == 1 = printHeadingTag (drop 1 text) (newText ++ ("<h" ++ show(count) ++ ">")) 1 0
                                          | head text == '#' = printHeadingTag (drop 1 text) newText (count + 1) isEndH
                                          | otherwise = printHeadingTag (drop 1 text) (reverse((head text) : (reverse newText))) count isEndH

-- ######## простой выделенный текст (<a></a>)
-- root = 0 = 1 <p><li>; = 2 <div class='block'>
printLinkTag text newText startLine root | (length text == 0 || head text == '\n') && (root == 0 || root == 1) = printSimpleTag text (newText ++ ("</a>")) 1 root
                                         | (length text == 0 || head text == '\n') && root == 2 = printBlobksTag text (newText ++ ("</a>")) 1 0
                                           | startLine == 1 && head (drop 2 text) == '+' = printLinkTag (drop 2 text) (newText ++ ("<a class='btnLink' target='_blank' href='")) 0 root 
                                           | startLine == 1 = printLinkTag (drop 2 text) (newText ++ ("<a target='_blank' href='")) 0 root 
                                           | head text == ']' && (root == 0 || root == 1) = printSimpleTag (drop 1 text) (newText ++ ("</a>")) 1 root
                                           | head text == ']' && root == 2 = printBlobksTag (drop 1 text) (newText ++ ("</a>")) 0 0
                                           | head text == ')' = printLinkTag (drop 2 text) (newText ++ ("'>")) 0 root 
                                           | otherwise = printLinkTag (drop 1 text) (reverse((head text) : (reverse newText))) 0 root 

-- ######## простой выделенный серый текст (<span></span>)
printSpanTag text newText startLine root | (length text == 0 || head text == '\n') && (root == 0 || root == 1) = printSimpleTag text (newText ++ ("</span>")) 1 root
                                         | (length text == 0 || head text == '\n') && root == 2 = printBlobksTag text (newText ++ ("</span>")) 1 0
                                           | (head text) == ']' && (root == 0 || root == 1) = printSimpleTag (drop 1 text) (newText ++ ("</span>")) 0 root
                                           | (head text) == ']' && root == 2  = printBlobksTag (drop 1 text) (newText ++ ("</span>")) 0 0
                                           | startLine == 1 = printSpanTag (drop 1 text) (newText ++ ("<span>")) 0 root  
                                           | otherwise = printSpanTag (drop 1 text) (reverse((head text) : (reverse newText))) 0 root 

-- ######## простой выделенный текст (<b></b>)
printBoldTag text newText startLine root | (length text == 0 || head text == '\n') && (root == 0 || root == 1) = printSimpleTag text (newText ++ ("</b>")) 1 root
                                         | (length text == 0 || head text == '\n') && root == 2 = printBlobksTag text (newText ++ ("</b>")) 1 0
                                           | (head text) == '*' && startLine == 0 && (root == 0 || root == 1) = printSimpleTag (drop 1 text) (newText ++ ("</b>")) 0 root
                                           | (head text) == '*' && startLine == 0 && root == 2 = printBlobksTag (drop 1 text) (newText ++ ("</b>")) 0 0
                                           | (head text) == '*' && startLine == 1 = printBoldTag (drop 1 text) (newText ++ ("<b>")) 0 root 
                                           | otherwise = printBoldTag (drop 1 text) (reverse((head text) : (reverse newText))) 0 root 

-- ######## простой выделенный текст (<i></i>)
printItalicTag text newText startLine root | (length text == 0 || head text == '\n') && (root == 0 || root == 1) = printSimpleTag text (newText ++ ("</i>")) 1 root
                                           | (length text == 0 || head text == '\n') && root == 2 = printBlobksTag text (newText ++ ("</i>")) 1 0
                                           | (head text) == '_' && startLine == 0 && (root == 0 || root == 1) = printSimpleTag (drop 1 text) (newText ++ ("</i>")) 0 root
                                           | (head text) == '_' && startLine == 0 && root == 2 = printBlobksTag (drop 1 text) (newText ++ ("</i>")) 0 0
                                           | (head text) == '_' && startLine == 1 = printItalicTag (drop 1 text) (newText ++ ("<i>")) 0 root 
                                           | otherwise = printItalicTag (drop 1 text) (reverse((head text) : (reverse newText))) 0 root 

-- ######## простой текст (<p></p>)
printSimpleTag text newText startLine root | (length text == 0 || head text == '\n') && root == 0 = parseText text (newText ++ ("</p>")) 1
                                             | (length text == 0 || head text == '\n') && root == 1 = printItemTag text (newText ++ ("</p>")) 1 0
                                             | startLine == 1 = printSimpleTag text (newText ++ ("<p>")) 0 root
                                             | head text == '[' && (head (drop 1 text)) == '(' = printLinkTag text newText 1 root
                                             | head text == '[' = printSpanTag text newText 1 root
                                             | head text == '*' = printBoldTag text newText 1 root
                                             | head text == '_' = printItalicTag text newText 1 root
                                             | otherwise = printSimpleTag (drop 1 text) (reverse((head text) : (reverse newText))) 0 root

-- ######## простой текст (<ul><li></li></ul>)
printItemTag text newText startLine startTag | length text == 0 || (head text == '\n' && head (drop 1 text) /= '-') = parseText text (newText ++ ("</ul>")) 1
                                             | startTag == 1 = printItemTag text (newText ++ ("<ul>")) 1 0
                                             | head text == '-' && startLine == 1 = printItemTag (drop 1 text) (newText ++ ("<li>")) 0 startTag
                                             | head text == '\n' = printItemTag (drop 1 text) (newText ++ ("</li>")) 1 startTag
                                             | otherwise = printSimpleTag text newText 1 1

-- ######## простой текст (<div class="list"><div class="block">header+description</div></div>)
printBlobksTag text newText startLine startTag | length text == 0 || head text == '}' = parseText (drop 1 text) (newText ++ ("</div>")) 1
                                               | startTag == 1 = printBlobksTag (drop 1 text) (newText ++ ("<div class='list'>")) 1 0
                                               | head text == '>' && startLine == 1 = printBlobksTag (drop 1 text) (newText ++ ("<div class='block'><p class= 'block_header'>")) 0 startTag
                                               | head text == '|' = printBlobksTag (drop 1 text) (newText ++ ("</p><p class='block_description'>")) startLine startTag
                                               | head text == '[' && (head (drop 1 text)) == '(' = printLinkTag text newText 1 2
                                               | head text == '[' = printSpanTag text newText 1 2
                                               | head text == '*' = printBoldTag text newText 1 2
                                               | head text == '_' = printItalicTag text newText 1 2
                                               | head text == '\n' && startLine == 0 = printBlobksTag (drop 1 text) (newText ++ ("</p></div>")) 1 startTag
                                               | otherwise = printBlobksTag (drop 1 text) (reverse((head text) : (reverse newText))) startLine startTag

-- ######## обработка текста
parseText text newText startLine | length text == 0 = newText
                                 | head text == '\n' = parseText (drop 1 text) (newText ++ "<br>" ) 1
                                 | head text == '#' = printHeadingTag text newText 0 1
                                 | startLine == 1 && head text == '-' = printItemTag text newText 1 1
                                 | startLine == 1 && head text == '{' = printBlobksTag text newText 1 1
                                 | startLine == 1 = printSimpleTag text newText 1 0
                                 | otherwise = parseText (drop 1 text) (reverse((head text) : (reverse newText))) 0

------------ main

main = do
    fileRead <- openFile "index.txt" ReadMode
    fileWrite <- openFile "index.html" WriteMode

    fileText <- hGetContents fileRead
    let newText = parseText fileText "" 1
    let result = mainStartHTML ++ newText ++ mainFinishHTML
    hPutStr fileWrite result

    hClose fileRead
    hClose fileWrite

------------ test

-- rr = T.replace "<title>Document</title>" "<title>city</title>" "# Dillinger\n## _The Last Markdown Editor, Ever_"
-- rr = T.replace "## " "<h2>" "# Dillinger\n## _The Last Markdown Editor, Ever_"
                    