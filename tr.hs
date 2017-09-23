{- | Usage:
    $ ghc tr.hs
    $ echo "hiab" | tr a b  #replaces a with b
    >> hibb
    $ echo "hiab" | tr -d a #deletes a
    >> hib
-}
import qualified Data.Map.Lazy as Map
import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    str <- getLine
    let firstArg = (!!) args 0
    let secondArg = (!!) args 1
    let m = Map.fromList (zip firstArg secondArg)
    let f = createMappingFunction m
    let output = if firstArg == "-d"
                 then Main.delete str secondArg
                 else tr f str
    putStrLn output

createMappingFunction :: Map.Map Char Char -> MappingFunction
createMappingFunction m ch =
    Map.findWithDefault ch ch m

type MappingFunction = (Char -> Char)

tr :: MappingFunction -> (String -> String)
tr f str = map f str

delete :: String -> String -> String
delete str replaceStr = filter (`notElem` replaceStr) str
