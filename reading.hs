import Data.Time
import System.Locale
import Data.List.Split
import System.IO

data Book = Book { title :: String
                 , author :: String
                 , whenRead :: [Day]
                 , pages :: Integer
                 , words :: Integer
                 , pred :: Book
                 , succ :: Book
                 } deriving (Show, Eq)

parseDay :: String -> Day
parseDay s = readTime defaultTimeLocale "%-m%-d%Y" s

bookFromShelfariEntry :: [String] -> Book
bookFromShelfariEntry x = Book {title = x!!3,
                                author = x!!4,
                                whenRead = [(parseDay $ x!!14)] }


importShelfari :: FilePath -> [Book]
importShelfari file = do tsv <- readFile file
                         let (x:xs) = map (splitOn "\t") lines tsv
                             books = map bookFromShelfariEntry xs
                         putStrLn(books)
