module Main (main) where

import           Path
import           Path.IO
import           RIO
import qualified RIO.ByteString         as BS
import qualified RIO.ByteString.Lazy    as LBS
import           Statechart.CodeGen.SQL
import           Statechart.SCXML       as SCXML
import           System.Environment     (getArgs)


main :: IO ()
main = do
    args <- getArgs

    chartDir <- resolveDir' "db/deploy/statechart"
    deployDir <- resolveDir' "db/deploy/statechart"
    verifyDir <- resolveDir' "db/verify/statechart"
    revertDir <- resolveDir' "db/revert/statechart"

    scxmls <- case args of
        [] -> map (first (chartDir </>)) <$> readSCXMLfiles chartDir
        _ -> for args $ \fp -> do
            af <- resolveFile' fp
            a <- BS.readFile (fromAbsFile af)
            case parse $ LBS.fromStrict a of
                Left e  -> error . show $ e
                Right p -> return (af, p)

    let charts = map snd scxmls

    -- Generation
    let sqls = generateSQL "events:statechart" charts
        verifySqls = generateSQLVerify "events:statechart" charts
        revertSqls = generateSQLRevert "events:statechart" charts

    -- Writing to disk
    writeSQLs deployDir sqls
    writeSQLs verifyDir verifySqls
    writeSQLs revertDir revertSqls

-- for_ scxmls $ \(path, chart) -> do
--     umlPath <- addExtension ".pml" path
--     let umlCode = generatePlantuml chart
--     ensureDir $ parent umlPath
--     writeFileUtf8 (fromAbsFile umlPath) umlCode
--     runProcess_ $ proc "plantuml" ["-tsvg", fromAbsFile umlPath]
