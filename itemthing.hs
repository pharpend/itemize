{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative
import           Data.Monoid
import           Data.Traversable
import           Paths_itemize
import           System.IO
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.String

topStr :: IO String
topStr = getDataFileName "top.html" >>= readFile

bottomStr :: IO String
bottomStr = getDataFileName "bottom.html" >>= readFile

doSeason :: Int -> IO H.Html
doSeason season = do
  hPutStr stderr $ "How many episodes in season " ++ show season ++ "? "
  hFlush stderr
  (numEpisodes :: Int) <- read <$> getLine
  _ <- if numEpisodes <= 0
          then fail "not enough episodes"
          else return ()
  return $ mkSeason season numEpisodes

mkSeason :: Int -> Int -> H.Html
mkSeason season numEpisodes = do
  H.li ! A.class_ "item-has-children" $ do
    H.a ! A.href "#0" $ do
      H.toHtml $ "Season " ++ show season
    H.ul ! A.class_ "sub-menu" $ do
      mconcat $ map (mkEpisode season) [1 .. numEpisodes]

mkEpisode :: Int -> Int -> H.Html
mkEpisode season epinum = do
  H.li $ do
    H.a ! A.href "#" ! A.id (H.toValue $  "S" ++ show season ++ "E" ++ show epinum) $ do
      H.toHtml $ "Episode " ++ show epinum

main :: IO ()
main = do
  hPutStr stderr "How many seasons? "
  hFlush stderr
  (numSeasons :: Int) <- read <$> getLine
  _ <- if numSeasons <= 0
          then fail "not enough seasons"
          else return ()
  seasons <- mconcat <$> forM [1 .. numSeasons] doSeason
  putStr =<< topStr
  putStr $ renderHtml seasons
  putStr =<< bottomStr
