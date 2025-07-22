import Text.ParserCombinators.Parsec

-- Step 1: Parse the Mermaid Markdown file
parseMermaidCode :: String -> Maybe String
parseMermaidCode markdown = case parse mermaidCodeParser "" markdown of
  Left _ -> Nothing
  Right code -> Just code

-- Parser for Mermaid code
mermaidCodeParser :: Parser String
mermaidCodeParser = do
  _ <- string "```mermaid" <|> string "``` mermaid"
  _ <- manyTill anyChar (try (string "```"))
  manyTill anyChar eof

-- Example usage
main :: IO ()
main = do
  let markdown = "Some Markdown text\n```mermaid\ngraph LR\n    A --> B\n    B --> C\n```"
      maybeMermaidCode = parseMermaidCode markdown

  case maybeMermaidCode of
    Just code -> putStrLn code
    Nothing -> putStrLn "Invalid Mermaid Markdown file."
