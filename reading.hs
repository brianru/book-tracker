import Data.Time
import System.Locale
import Data.List.Split
import System.IO

data Book = Book { isbn :: Int
                 , title :: String
                 , author :: String
                 , whenRead :: [String]  -- date type
                 , whenAdded :: String -- date type
                 , pageCount :: Integer -- Maybe
                 , wordCount :: Integer -- Maybe
                 --, pred :: Book
                 --, succ :: Book
                 } deriving (Show, Eq)

bookFromShelfariEntry :: [String] -> Book
-- fill in data from api
bookFromShelfariEntry x = Book { isbn = x!!2
                               , title = x!!3
                               , author = x!!4
                               , whenRead = [x!!14]
                               , whenAdded = x!!20
                               , pageCount = 0
                               , wordCount = 0
                               }

readShelfariData :: String -> [Book]
readShelfariData f = let (x:xs) = map (splitOn "\t") $ lines f
                     in map bookFromShelfariEntry xs

importShelfari :: FilePath -> FilePath -> IO ()
importShelfari fin fout = do tsv <- readFile fin
                             let books = readShelfariData tsv
                             writeFile fout $ show books


-- TODO
-- persistence
-- connect with api
-- give me a way to update a book
-- statuses
-- load/save
-- stats
