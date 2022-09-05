module Main (main) where

import System.Directory
import Data.List
import Data.Array
import Data.Text (Text, pack, unpack, append)
import System.Environment (getArgs)
import qualified Data.Set as S

-- utility
flatten :: [[a]] -> [a]
flatten = foldr (++) []

-- filters
isSpecialDir :: String -> Bool
isSpecialDir = flip elem $ [".", ".."]

-- convenience functions
isDir  :: String -> IO Bool
isFile :: String -> IO Bool
isDir = doesDirectoryExist
isFile = doesFileExist

nonSpecial :: [String] -> [String]
nonSpecial = filter ( not . isSpecialDir )

-- filter application
filterPaths :: (String -> IO Bool) -> [String] -> IO [String]
filterPaths predicate xs = f <$> dirMask xs
	where
		dirMask :: [String] -> IO [Bool] 
		dirMask = mapM predicate
		f :: [Bool] -> [String]
		f mask = [dir | (dir, condition) <- zip xs mask , condition]  -- reuse conditionallyInclude

conditionallyInclude :: [a] -> [Bool] -> [a]
conditionallyInclude xs mask = [x | (x, condition) <- zip xs mask , condition]

splitByMask :: [a] -> [Bool] -> ([a], [a])
splitByMask xs mask = let stripMask (left, right) = (map fst left, map fst right)
                      in  stripMask . partition snd $ zip xs mask

-- Text is generally used instead of String as it is more efficient storage
-- names and paths are never streamed
getContentsRecursively :: Text -> IO [Text]
getContentsRecursively path = getContentsRecursively' [path]
	where
		getContentsRecursively' :: [Text] -> IO [Text]
		getContentsRecursively' dirs = let getContents path = nonSpecial <$> getDirectoryContents path
			in do
				-- each dir has some number of contents
				extractedContents <- mapM getContents (map unpack dirs) :: IO [[String]]
				-- unfortunately, the result is Strings
				-- unfortunately-er, getContents only gives the names of the contents in each dir
				-- so the path is paired with each list of results from said path..
				-- .. and map (path ++) to each result for said path
				let packedSeparator = pack "/" :: Text
				let prefixPath = flip append $ packedSeparator :: Text -> Text
				let prependPath x = append (prefixPath x) :: Text -> Text
				let packedExtractedContents = map (map pack) extractedContents
				-- here a large list of path-prefixed contents are generated, but we're not done
				let contents = flatten [map (prependPath path) dirContents | (path, dirContents) <- zip dirs packedExtractedContents]
				-- each must be checked for it's type
				-- hopefully ghc is smart enough to fuse this stream with the next line
				-- [Bool] where true means it's a directory
				dirMask <- mapM isDir (map unpack contents)
				-- contents are here used for the last time, split by the result
				let (dirsToProcess, filesToProcess) = splitByMask contents dirMask
				-- should there be no more dirs/levels to check, we're done
				-- else, the stream is continued
				if null dirsToProcess
				then return filesToProcess                                          -- base case
				else (filesToProcess ++) <$> getContentsRecursively' dirsToProcess  -- tail recursion

-- poorly named, but can't think of anything
stripPaths :: [Text] -> [[Text]] -> [[Text]]
stripPaths toStrip paths = map stripAll pairs
	where
		stripAll :: (Text, [Text]) -> [Text]
		stripAll (a, b) = map pack [ strip (unpack a) (unpack path) | path <- b ]
		strip :: Eq a => [a] -> [a] -> [a]
		strip x [] = x
		strip [] y = y
		strip (x:xs) (y:ys) = if x == y then strip xs ys else ys
		pairs = zip toStrip paths

main :: IO ()
main = do
	args <- map pack <$> getArgs
	if (length args) < 2
	then putStrLn "at least 2 paths expected"
	else do
		-- at least 2 paths were supplied
		-- each path results in a list in `streams`
		streams <- mapM getContentsRecursively args

		let contextlessStreams = stripPaths args streams

		let sets = map S.fromList contextlessStreams
		-- at least 2 paths, 1:1 paths:sets, this is fine
		let duplicates = foldr S.intersection (head sets) (tail sets)
		let listOfDuplicates = S.elems duplicates

		let resultingTerminalOutput = foldr (\a b -> unpack a ++ "\n" ++ b) "" listOfDuplicates
		putStr resultingTerminalOutput

