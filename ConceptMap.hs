-- | A plugin for gitit (http://gitit.johnmacfarlane.net/) that renders a
-- concept map for each wiki page. The concept map consists of links coming
-- in and links going out.

module ConceptMap (plugin) where

import Control.Monad (liftM, when)
import Control.Monad.Trans
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Gitit.Interface
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Text.Regex
import Text.Regex.Posix

type URL = String

type Name = String

type Link = (URL, Name)

type Index = (M.Map String (S.Set Link), M.Map String (S.Set Link))

plugin :: Plugin
plugin = PageTransform pageTransform

pageTransform :: Pandoc -> PluginM Pandoc 
pageTransform x = do
    cfg <- askConfig
    layout <- liftM ctxLayout getContext
    let indexFile = staticDir cfg </> "conceptmap.idx"
        pageName = pgPageName layout
        fileName = "conceptmap-" ++ pageName ++ ".png"
        filePath = staticDir cfg </> "img" </> fileName
        url = "img/" ++ fileName

    idx <- liftIO (loadIndex indexFile)
    let idx' = updateIndex pageName x idx

    if not (pgPrintable layout)
        then do
            idx' `seq` liftIO (saveIndex indexFile idx')
            cmap <- liftIO (dot filePath (toDot pageName idx))

            let imgHtml = "<h2>Concept Map</h2><img border=\"0\" usemap='#CM-"
                        ++ pageName ++ "' src='" ++ url ++ "'/>"
            return . appendHtml imgHtml . appendHtml cmap $ x
        else return x

toDot :: String -> Index -> String
toDot x idx = "digraph " ++ graph ++ " {" ++ nodes ++ inEdges ++ outEdges ++ "}"
  where
    nodes    = unlines $ map drawNode (incoming ++ outgoing)
    inEdges  = unlines $ map (\y -> drawLink (snd y) x) incoming
    outEdges = unlines $ map (drawLink x . snd) outgoing

    graph = quote $ "CM-" ++ x

    incoming = linksIn x idx
    outgoing = linksOut x idx

    drawLink a b = quote a ++ " -> " ++ quote b ++ ";"
    drawNode y = quote (snd y) ++ " [URL=\"" ++ fst y ++ "\"];"
    quote y = "\"" ++ y ++ "\""

emptyIndex :: Index
emptyIndex = (M.empty, M.empty)

updateIndex :: String -> Pandoc -> Index -> Index
updateIndex x p idx = foldl' (flip (insertLink (x,x))) idx' links
  where
    links = queryWith extractLinks p
    idx' = removePage x idx

    extractLinks :: Inline -> [Link]
    extractLinks (Link i (l, _)) = [(l, concatInline i)]
    extractLinks _               = []

loadIndex :: FilePath -> IO Index
loadIndex = fmap (foldl' readLink emptyIndex . lines) . safeReadFile
  where
    readLink a x =
        case x =~ "^(.*[^\\\\]):(.*[^\\\\]):(.*)" :: (String, String, String, [String]) of
            (_, _, _, [f, u, t]) ->
                insertLink (unescape f,unescape f) (unescape u, unescape t) a
            _                    ->
                a
    safeReadFile = flip catch (\_ -> return "") . readFile
    unescape x = subRegex (mkRegex "\\\\:") x ":"

saveIndex :: FilePath -> Index -> IO ()
saveIndex fp = writeFile fp . foldIndex writeLink ""
  where
    writeLink k vs a = (++ a) . unlines . S.toList . S.map (toStr k) $ vs 
    toStr f (u, t)   = intercalate ":" . map escape $ [f, u, t]
    escape x         = subRegex (mkRegex ":") x "\\\\:"

foldIndex :: (k -> v -> a -> a) -> a -> (M.Map k v, c) -> a
foldIndex f d = M.foldWithKey f d . fst

insertLink :: Link -> Link -> Index -> Index
insertLink x y (os, is) = (ins (snd x) y os, ins (snd y) x is)
  where
    ins k v = snd . M.insertLookupWithKey (\_ -> S.union) k (S.singleton v)

removePage :: String -> Index -> Index
removePage x (os, is) = (M.delete x os, is)

linksIn :: String -> Index -> [Link]
linksIn x (_, is) = S.toList (M.findWithDefault S.empty x is)

linksOut :: String -> Index -> [Link]
linksOut x (os, _) = S.toList (M.findWithDefault S.empty x os)

dot :: FilePath -> String -> IO String
dot fp x = do
    (ec, out, err) <- readProcessWithExitCode "dot" args x
    when (ec /= ExitSuccess) (error $ "dot failed: " ++ err)
    return out
  where
    args = ["-Tpng", "-o", fp, "-Grankdir=LR", "-Tcmapx"]

appendHtml :: String -> Pandoc -> Pandoc
appendHtml x (Pandoc m bs) = Pandoc m (bs ++ [RawHtml x])

concatInline :: [Inline] -> String
concatInline []         = ""
concatInline (Str x:xs) = x ++ concatInline xs
concatInline (Space:xs) = ' ' : concatInline xs
concatInline (_:xs)     = concatInline xs
