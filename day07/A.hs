import Control.Monad.State
import Data.Char (isSpace)
import Data.List (isPrefixOf, sort)

data FS a = Dir String [FS a] | File String a
  deriving (Show, Eq)

allLines :: IO [String]
allLines = lines <$> getContents

buildFS :: [String] -> FS Int
buildFS = evalState (go (Dir "/" []))
  where
    go fs = do
      ls <- get
      if null ls
        then return fs
        else do
          let (l:ls') = ls
          if l == "$ cd /"
            then do
              put ls'
              go fs
            else do
              fs' <- buildFS' fs
              go fs'

    buildFS' cwd = do
      ls <- get
      if null ls || isFile cwd
      then return cwd
      else do
        let (l:ls')    = ls
        let (Dir n dl) = cwd
        if l == "$ ls"
          then do
             put ls'
             es <- takeNonCmds
             let dl' = buildList es
             buildFS' (Dir n dl')
          else if l == "$ cd .."
            then do
             put ls'
             return cwd
            else if l == "$ cd /"
              then return cwd
              else do
                put ls'
                let m = drop (length "$ cd ") l
                if containsDir m dl
                  then do
                    dl' <- modifyDirInList dl m buildFS'
                    buildFS' (Dir n dl')
                  else do
                    sd <- buildFS' (Dir m [])
                    return (Dir n (sd:dl))

    isCmd []      = False
    isCmd ('$':_) = True
    isCmd _       = False

    isFile (File _ _) = True
    isFile _          = False

    takeNonCmds = do
      ls <- get
      let (es, ls') = span (not . isCmd) ls
      put ls'
      return es

    containsDir n []             = False
    containsDir n ((Dir m _):es) = n == m || containsDir n es
    containsDir n (_:es)         = containsDir n es

    hasDirName n (File _ _) = False
    hasDirName n (Dir m _)  = n == m

    modifyDirInList []             _ _ = return []
    modifyDirInList (e:es) n f
      | hasDirName n e = do
        e' <- f e
        return (e':es)
      | otherwise = do
        es' <- modifyDirInList es n f
        return (e:es')

    buildList :: [String] -> [FS Int]
    buildList []              = []
    buildList (e:es)
      | "dir " `isPrefixOf` e = let n = drop (length "dir ") e
                                in Dir n [] : buildList es
      | otherwise             = let (l, _:n) = span (not . isSpace) e
                                in File n (read l) : buildList es

solve :: FS Int -> Int
solve fs = snd $ solve' fs
  where
    solve' (File _ s) = (s, 0)
    solve' (Dir _ es) = let ss = map solve' es
                            s  = sum $ map fst ss
                            c  = sum $ map snd ss
                        in if s <= 100000
                             then (s, c + s)
                             else (s, c)

main :: IO ()
main = do
  ls <- allLines
  let fs = buildFS ls
  print $ solve fs


