import System.IO
import Data.Time
import Data.Aeson
import Data.Maybe
import Data.List.Split
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B

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

type ISBN = String
parseShelfariData :: String -> [ISBN]
parseShelfariData f = let (x:xs) = map (splitOn "\t") $ lines f
                          withQuotes = map (\a -> a!!2) xs
                          withoutQuotes = map (filter (/= '"')) withQuotes
                      in take 3 withoutQuotes -- NOTE 3 for testing

parseAPIResponses xs = mapMaybe (\x -> (decode x)::Maybe Object) xs

importShelfari :: FilePath -> FilePath -> IO ()
importShelfari fileIn fileOut = do dotTSV <- readFile fileIn
                                   let isbns = parseShelfariData dotTSV
                                       api = "https://openlibrary.org/api/books?\
                                             \jscmd=data&format=json&bibkeys=ISBN:"
                                   responses <- mapM (\s -> simpleHttp $ api ++ s) isbns 
                                   let books = parseAPIResponses responses
                                   B.putStr $ head responses
                                   --writeFile fileOut $ show books

--save :: [Book] -> Boolean

--load :: FilePath -> [Book]

--update :: [Book] -> [Book] -> [Book]

--supplementWithAmazon :: ISBN -> [String] -> [(String, String)]


-- TODO
-- persistence
-- connect with api
-- give me a way to update a book
-- statuses
-- load/save
-- stats
