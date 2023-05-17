{-# language AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, GADTs, TemplateHaskell #-}

-- | This is a different approach on the "JobQueue.Sum" problem. Instead of
-- defining a single sum type, we define a *type class* that is used to
-- represent jobs. The main complexity comes in adding handlers to our job
-- dispatch.
--
-- In "JobQueue.Sum", dispatch was handled by writing a case expression
-- over a sum type. In this module, we instead create a "list of handlers".
-- This loses exhaustivity.
module JobQueue.Class where

import Control.Applicative
import qualified Data.List as List
import Data.Proxy
import Data.Aeson
import Data.Aeson.TH
import Data.Time

-- | Datatypes will define an instance of this, and will then enroll in
-- a specific handler function.
--
-- It's a class alias for now, but you might want to extend this with
-- additional methods.
class (FromJSON a, ToJSON a) => RemoteJob a where
    handleJob :: a -> IO ()

-- | Instead of being a constructor of a sum type, we represent each job as
-- it's own type.
data GetStuff = GetStuff { getStuffCount :: Int }

deriveJSON (defaultOptions { tagSingleConstructors = True, sumEncoding = ObjectWithSingleField }) ''GetStuff

instance RemoteJob GetStuff where
    handleJob (GetStuff i) = do
        print i

data DoReport = DoReport { doReportName :: String, doReportChar :: Char }

deriveJSON (defaultOptions { tagSingleConstructors = True, sumEncoding = ObjectWithSingleField }) ''DoReport

instance RemoteJob DoReport where
    handleJob (DoReport name c) = do
        putStrLn name
        print c

-- | Instead of containing a specific value, we have a 'Value' - an
-- unparsed JSON object.
data Job = Job
    { jobPayload :: Value
    , jobCreatedAt :: UTCTime
    }

deriveJSON defaultOptions ''Job

-- | Sending a job means converting it to JSON and printing it somewhere
-- that it'll get picked up.
sendJob :: RemoteJob a => a -> IO ()
sendJob payload = do
    now <- getCurrentTime
    let job = Job { jobPayload = toJSON payload, jobCreatedAt = now }
    print (encode job)

-- | Now, parsing a job is more challenging. We have a 'Value' present in
-- in the 'Job'. We attempt to parse the 'Value' into one of our known job
-- types, without any real abstraction:
runJobGross  :: Job -> IO ()
runJobGross job = do
    case fromJSON @GetStuff (jobPayload job) of
        Success getStuff ->
            handleJob getStuff
        _ ->
            case fromJSON @DoReport (jobPayload job) of
                Success doReport ->
                    handleJob doReport
                _ ->
                    putStrLn "unknown job"
                    -- etc - repeat trying to parse for all known jobs

-- | 'runJobGross' is a bit, well, gross - we don't want to manually do all
-- that. Let's instead factor out the logic of "try all these things and
-- take the first thing that parses."
--
-- This type carries the implementation of the handler, as well as the
-- *type* that we're trying to parse. As long as the @a@ type in there has
-- an instance of 'RemoteJob', then we'll be able to construct this.
data JobHandler where
    JobHandler :: RemoteJob a => Proxy a -> JobHandler

-- | We unpack the 'JobHandler', and attempt to parse the value out. If the
-- value parses successfully as the job, then we return @'Just' (k a)@.
-- Otherwise, we return Nothing.
tryJobHandler :: JobHandler -> Value -> Maybe (IO ())
tryJobHandler (JobHandler (_ :: Proxy a)) value =
    case fromJSON @a value of
        Success a ->
            Just (handleJob a)
        _ ->
            Nothing

-- | Look through the 'JobHandler' list, and run the first job that
-- matches. Otherwise, print an error about no matching handler.
runJobWith :: [JobHandler] -> Job -> IO ()
runJobWith handlers job = do
    let mhandler = asum $ List.map (\handler -> tryJobHandler handler (jobPayload job)) handlers
    case mhandler of
        Nothing ->
            putStrLn "No matching handler"
        Just handler ->
            handler

registerHandler :: forall a. RemoteJob a => JobHandler
registerHandler = JobHandler (Proxy :: Proxy a)

-- | Finally, we assemble our runners into the list.
runJob :: Job -> IO ()
runJob = runJobWith
    [ registerHandler @GetStuff
    , registerHandler @DoReport
    ]

-- For one last convenience, you can use @discover-instances@ to discover
-- all these job handlers automagically. Wiring up a new job is then just
-- a matter of defining the datatype and ensuring the *type* is imported
-- into the job queue module.
