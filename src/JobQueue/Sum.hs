{-# language TemplateHaskell #-}

-- | The "obvious" solution here is to create a sum type. This has several
-- nice properties:
--
-- 1. We can exhaustively match on the sum type.
-- 2. We can easily write a job dispatch with a `case` expression
-- 3. We can easily serialize and deserialize.
module JobQueue.Sum where

import Data.Aeson
import Data.Aeson.TH
import Data.Time

-- | The type of jobs that we want to run asynchronously.
data JobPayload
    = GetStuff Int
    | DoReport String Char
    | SweepOldRecords UTCTime

deriveJSON defaultOptions ''JobPayload

-- | The actual serialized 'Job'.
data Job = Job
    { jobPayload :: JobPayload
    , jobCreatedAt :: UTCTime
    }

deriveJSON defaultOptions ''Job

-- | Sending a job is pretty easy - you just accept the payload, and
-- serialize it.
sendJob :: JobPayload -> IO ()
sendJob payload = do
    now <- getCurrentTime
    let job = Job { jobPayload = payload, jobCreatedAt = now }
    print (encode job)

-- | Handling jobs is *also* pretty easy. You define a top level function
-- that matches on the job and does whatever you want.
handleJob :: Job -> IO ()
handleJob job =
    case jobPayload job of
        GetStuff i -> print i
        DoReport name c -> putStrLn name >> print c
        SweepOldRecords begin -> print begin

-- | However, all is not well here.
--
-- A sum type gives a "false sense of security" - we are serializing a type
-- and then parsing it out. If we're storing this in a database, then
-- there's no guarantee that two different versions of the same code will
-- necessarily parse the same way, or be handled the same way. Developers
-- will need to take care to avoid breaking the assumptions here.
--
-- Furthermore, a sum type requires that the module that *defines* the sum
-- type import from *every* module that defines any type that the job uses.
-- This means that modules that *send* jobs cannot also define types that
-- are *used* in the job!
--
-- Lastly, the compilation impact of a large job type is going to become
-- big. One option is to "split" the job sum type using a "transparent"
-- wrapper sum type - allowing you to define `data JobPayload1` and `data
-- JobPayload2`, each of which are roughly the same size. But this is
-- gross.
difficulty :: ()
difficulty = ()
