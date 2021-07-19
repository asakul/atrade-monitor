module Main where

import           Control.Concurrent            hiding (newChan, readChan,
                                                writeChan)
import           Control.Concurrent.Chan.Unagi
import           Control.Exception.Safe
import           Control.Monad
import           Data.Aeson
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time.Clock
import           Data.Time.Format
import           Safe
import           System.Environment
import           System.IO
import           System.Process

newtype ArgsMap = ArgsMap { unArgsMap :: HM.HashMap T.Text T.Text }
  deriving (Show, Eq)

data Config = Config
  {
    cGlobalLog   :: FilePath,
    cDelay       :: Int,
    cGlobalArgs  :: ArgsMap,
    cExecutables :: [ExecConfig]
  } deriving (Show, Eq)

data ExecConfig = ExecConfig
  {
    cExecutablePath :: FilePath,
    cLogPath        :: FilePath,
    cArgs           :: ArgsMap
  } deriving (Show, Eq)

instance FromJSON ArgsMap where
  parseJSON = withObject "ArgsMap" $ \o ->
    case HM.traverseWithKey tryConvert o of
      Just converted -> return $ ArgsMap converted
      Nothing        -> fail "Unable to parse argsmap"
    where
      tryConvert k (Object _) = Nothing
      tryConvert k (Array _)  = Nothing
      tryConvert k (String t) = Just t
      tryConvert k (Number n) = Just (T.pack $ show n)
      tryConvert k (Bool b)   = Just (T.pack $ show b)
      tryConvert k Null       = Just T.empty


instance FromJSON ExecConfig where
  parseJSON = withObject "ExecConfig" $ \o ->
    ExecConfig <$>
    o .: "path" <*>
    o .: "log" <*>
    o .: "config"

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o ->
    Config <$>
    o .: "global_log" <*>
    o .: "delay" <*>
    o .: "global_args" <*>
    o .: "executables"

main :: IO ()
main = do
  args <- getArgs
  case headMay args of
    Just configPath -> do
      eResult <- eitherDecodeFileStrict' configPath
      case eResult of
        Right config -> runWithConfig config
        Left err     -> print $ "Can't read config:" ++ err
    Nothing -> print $ "Usage: atrade-monitor <config-path>"

runWithConfig :: Config -> IO ()
runWithConfig config = do
  (inChan, outChan) <- newChan
  threadIds <- forM (zip [0..] (cExecutables config)) $ \(i, execcfg) ->
    forkIO $ do
      let delay = 1000000 * i * cDelay config
      putStrLn $ "Waiting " ++ show delay ++  " to spawn process: " ++ cExecutablePath execcfg
      threadDelay delay
      putStrLn $ "Creating process: " ++ cExecutablePath execcfg
      (_, Just stdOut, Just stdErr, ph) <- createProcess $ (proc (cExecutablePath execcfg) (makeArgs (cGlobalArgs config) (cArgs execcfg))) { std_out = CreatePipe, std_err = CreatePipe }
      withFile (cLogPath execcfg) AppendMode $ \log -> handleIO (exceptionHandler inChan) $ do
        stdErrLogThread <- forkIO $ forever $ do
          line <- T.pack <$> hGetLine stdErr
          now <- getCurrentTime
          let line' = lineWithTime now line
          TIO.hPutStrLn log line'
          hFlush log
          writeChan inChan line'
        forever $ do
          now <- getCurrentTime
          line <- lineWithTime now . T.pack <$> hGetLine stdOut
          TIO.hPutStrLn log line
          hFlush log
          writeChan inChan line

  globalLogger outChan

  where
    makeArgs globalArgs localArgs = fmap makeArg (HM.toList . unArgsMap $ globalArgs) ++ fmap makeArg (HM.toList . unArgsMap $ localArgs)
    makeArg (k, v) = T.unpack $ if v /= "" then "--" <> k <> "=" <> v else "--" <> k
    globalLogger chan = withFile (cGlobalLog config) AppendMode $ \log -> forever $ do
      line <- readChan chan
      TIO.hPutStrLn log line
      putStrLn $ T.unpack line
      hFlush log
      hFlush stdout
      hFlush stderr
    exceptionHandler inChan exception = do
      writeChan inChan $ "Exception: " `T.append` (T.pack . show $ exception)

    lineWithTime time line = ("[" `T.append` (T.pack $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%T.%q")) time) `T.append` "] > " `T.append` line)
