import System.FilePath.Posix
import System.Directory
import System.IO

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.IO as TLL
import qualified Data.Text as TL

type Filepath = FilePath

data VLabel = VLDirectory
            | VLSymlink
            | VLFile
type V = (Filepath, VLabel)

data ELabel = ELHardlink
            | ELSymlink
type E = (Filepath, Filepath, ELabel)

type FileGraph = ([V], [E])

readDirectoryGraph :: FilePath -> IO FileGraph
readDirectoryGraph root = do
  isSymlink <- pathIsSymbolicLink root
  if isSymlink
    then onSymlink
    else do
      isFile <- doesFileExist root
      if isFile
        then onFile
        else onDirectory
  where
    onSymlink :: IO FileGraph
    onSymlink = do
      target <- normalise <$> getSymbolicLinkTarget root
      return ([(root, VLSymlink)], [(root, target, ELSymlink)])
      
    onFile :: IO FileGraph
    onFile = do
      return ([(normalise root, VLFile)], [])
    
    onDirectory :: IO FileGraph
    onDirectory = do
      children <- map (\x -> root ++ "/" ++ x) <$> listDirectory root :: IO [FilePath]
      subgraphs <- mapM readDirectoryGraph children :: IO [FileGraph]
      let v = (normalise root, VLDirectory) :: V
          es = map (\x -> (normalise root, x, ELHardlink)) children :: [E]
          childVertices = concatMap fst subgraphs :: [V]
          childEdges = concatMap snd subgraphs :: [E]
      return (v:childVertices, es ++ childEdges)




-- GraphVisParams vertexType vertexLabeltype edgeLabelType clusterType clusterLabelType
fileGraphParams :: G.GraphvizParams FilePath VLabel ELabel () VLabel
fileGraphParams = G.defaultParams {
  G.fmtNode = \(v, vl) -> case vl of
      VLDirectory -> colorAttribute $ G.RGB 0 0 0
      VLSymlink   -> colorAttribute $ G.RGB 40 255 40
      VLFile      -> colorAttribute $ G.RGB 255 40 40,
  G.fmtEdge = \(from, to, el) -> case el of
      ELHardlink -> colorAttribute $ G.RGB 0 0 0
      ELSymlink  -> colorAttribute $ G.RGB 40 255 40
      }  
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]

main :: IO ()
main = do
  -- 1. Create our application-specific graph
  (vs, es) <- readDirectoryGraph rootDir
  -- 2. Convert it into a DotGraph
  let dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph FilePath
  -- 3. Render it into .dot text
      dotText = G.printDotGraph dotGraph :: TL.Text
  -- 4. Write the contents to a file
  TLL.writeFile "files.dot" dotText