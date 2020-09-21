{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}


-------------
-- Exports --
-------------
module Trace
( toMs
, toUs
, setRecordingTime
, setBufferSize
, recordTrace
, readTrace
, bsToTrace
, MessageType (..)
, ResourceType (..)
, ObservationPoint (..)
, IptType (..)
, StatusType (..)
, OperationType (..)
, Message (..)
, Content (..)
, Trace
, GeneralSettings (..)
, CpuObservationSettings (..)
, TraceConfiguration (..)
, TraceType (..)
, TriggerPosition (..)
, TimestampMode (..)
, BandwidthHandling (..)
, BufferType (..)
, DTU (..)
, PTU (..)
, DataAccessType (..)
, ProgramTraceMode (..)
, QualifierType (..)
, Range (..)
, Trigger (..)
, TriggerAction (..)
, Address (..)
, Milliseconds (..)
, KiloBytes (..)
, (|=)
, (.>)
, (.#)
, (.!)
, inf
) where

-------------
-- Imports --
-------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import GHC.Generics
import Data.Aeson
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Bits
import Text.Read
import System.Process
import Control.DeepSeq
import Numeric

----------------------
-- Type Definitions --
----------------------
data MessageType = Mem
                 | Dis
                 | Sts
                 | Tim
                 | SoT
                 | EoT
                 | MessageTypeUnknown
                 deriving (Generic, Read, Show, Eq)

type ResourceType = ObservationPoint

data IptType = Call
             | Return
             | Sequence
             | IptTypeUnknown
             deriving (Generic, Read, Show, Eq)          

data StatusType = StartISR
                | EndISR
                | TraceGap
                | StatusChangeUnknown
                deriving (Generic, Read, Show, Eq)

data OperationType = Write
                   | Read
                   | OperationTypeUnknown
                   deriving (Generic, Read, Show, Eq)

type Trace = [Message]

------------------------
-- Message Definition --
------------------------
data Message = TimeEvent { index :: Integer
                         , time :: Integer
                         , origin :: ResourceType
                         , mType :: MessageType
                         , content :: Content }
             | Event { index :: Integer
                     , origin :: ResourceType
                     , mType :: MessageType
                     , content :: Content }
             deriving (Generic, Read, Show, Eq)

instance NFData Message where
      rnf (TimeEvent i t o m c) = i `deepseq` ()
      rnf (Event i o m c) = i `deepseq` ()

------------------------
-- Content Definition --
------------------------
data Content = Memory { dAddress :: Integer
                      , dValue :: Integer
                      , size :: Integer
                      , operation :: OperationType }
             | Ipt { addr1 :: Integer
                   , addr2 :: Integer
                   , fType :: IptType }
             | Status { sType :: StatusType }
             | None { }
             | ContentUnknown { }
             deriving (Generic, Read, Show, Eq)

--------------------
-- Trace Decoding --
--------------------
bsToTrace :: BS.ByteString -> Trace
bsToTrace bs    | BS.length bs >= 25    = (bsToMsg bs) : bsToTrace (BS.drop 25 bs)
                | otherwise             = []

bsToMsg :: BS.ByteString -> Message
bsToMsg bs  | (BS.head bs) == 0 = (Event (bsToW32 $ BS.tail bs) (bsToOrigin $ BS.drop 5 bs) (bsToType $ BS.drop 6 bs) (bsToContent (bsToType $ BS.drop 6 bs) $ BS.drop 7 bs))
            | (BS.head bs) == 1 = (TimeEvent (bsToW32 $ BS.tail bs) (bsToW64 $ BS.drop 5 bs) (bsToOrigin $ BS.drop 13 bs) (bsToType $ BS.drop 14 bs) (bsToContent (bsToType $ BS.drop 14 bs) $ BS.drop 15 bs))
            | otherwise = (TimeEvent 0 0 CPU0 SoT None)

bsToContent :: MessageType -> BS.ByteString -> Content
bsToContent Tim bs = None
bsToContent SoT bs = None
bsToContent EoT bs = None
bsToContent MessageTypeUnknown bs = ContentUnknown
bsToContent Mem bs  | (BS.head $ BS.drop 9 bs) == 0 = (Memory (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) (fromIntegral $ BS.head $ BS.drop 5 bs) Write)
                    | (BS.head $ BS.drop 9 bs) == 1 = (Memory (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) (fromIntegral $ BS.head $ BS.drop 5 bs) Read)
                    | otherwise                     = (Memory (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) (fromIntegral $ BS.head $ BS.drop 5 bs) OperationTypeUnknown) 
bsToContent Dis bs  | (BS.head $ BS.drop 8 bs) == 0 = (Ipt (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) Call)
                    | (BS.head $ BS.drop 8 bs) == 1 = (Ipt (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) Return)
                    | (BS.head $ BS.drop 8 bs) == 2 = (Ipt (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) Sequence)
                    | otherwise                     = (Ipt (bsToW32 bs) (bsToW32 $ BS.drop 4 bs) IptTypeUnknown)
bsToContent Sts bs  | (BS.head bs) == 0 = (Status StartISR)
                    | (BS.head bs) == 1 = (Status EndISR)
                    | (BS.head bs) == 2 = (Status TraceGap)
                    | otherwise         = (Status StatusChangeUnknown)

bsToOrigin :: BS.ByteString -> ResourceType
bsToOrigin bs   | (BS.head bs) == 0xC0    = CPU0
                | (BS.head bs) == 0xC1    = CPU1
                | (BS.head bs) == 0xC2    = CPU2
                | (BS.head bs) == 0xC3    = CPU3
                | (BS.head bs) == 0xC4    = CPU4
                | (BS.head bs) == 0xC5    = CPU5
                | (BS.head bs) == 0xE0    = MCDS
                | (BS.head bs) == 0xE1    = SPB
                | otherwise               = SRI

bsToType :: BS.ByteString -> MessageType
bsToType bs | (BS.head bs) == 20 = Mem
            | (BS.head bs) == 30 = Dis
            | (BS.head bs) == 24 = Sts
            | (BS.head bs) == 23 = Tim
            | (BS.head bs) == 0 = SoT
            | (BS.head bs) == 255 = EoT
            | otherwise         = MessageTypeUnknown

bsToW32 :: BS.ByteString -> Integer
bsToW32 bs = foldl (\acc x -> (fromIntegral x) + (acc `shiftL` 8)) 0 w
    where w = take 4 $ BS.unpack bs

bsToW64 :: BS.ByteString -> Integer
bsToW64 bs = foldl (\acc x -> (fromIntegral x) + (acc `shiftL` 8)) 0 w
    where w = take 8 $ BS.unpack bs

-------------------------
-- Trace Configuration --
-------------------------
type KiloBytes = Integer
type Milliseconds = Integer
type Gaps = Integer
type Address = Integer

data TraceConfiguration = TraceConfiguration {
      tcGeneral     :: GeneralSettings,
      tcCpuA        :: CpuObservationSettings,
      tcCpuB        :: CpuObservationSettings
} deriving (Generic, Read, Show, Eq)

instance ToJSON TraceConfiguration where
      toEncoding = genericToEncoding defaultOptions

data GeneralSettings = GeneralSettings {
      gsElfPath           :: String,
      gsTraceType         :: TraceType,
      gsTriggerPosition   :: TriggerPosition,
      gsDeviceReset       :: Bool,
      gsTimestampMode     :: TimestampMode
} deriving (Generic, Read, Show, Eq)

instance ToJSON GeneralSettings where
      toEncoding = genericToEncoding defaultOptions

data TraceType = BufferedTrace {
      btBufferType      :: BufferType,
      btBufferSize      :: KiloBytes,
      btTimeout         :: Milliseconds
} | ContinuousTrace {
      ctMaxTraceSize          :: KiloBytes,           -- not supported yet
      ctMaxTraceTime          :: Milliseconds,
      ctBandwidthHandling     :: BandwidthHandling,
      ctMaxTraceGaps          :: Gaps                 -- not supported yet
} deriving (Generic, Read, Show, Eq)

instance ToJSON TraceType where
      toEncoding = genericToEncoding defaultOptions

data BandwidthHandling  = SuspendDevice
                        | TraceGaps
                        deriving (Generic, Read, Show, Eq)
                  
instance ToJSON BandwidthHandling where
      toEncoding = genericToEncoding defaultOptions


data BufferType   = Circular
                  | StopFull
                  deriving (Generic, Read, Show, Eq)
                  
instance ToJSON BufferType where
      toEncoding = genericToEncoding defaultOptions

data TriggerPosition    = TriggerAtStart
                        | TriggerAt30
                        | TriggerAt60
                        | TriggerAt90
                        | TriggerAtEnd
                        deriving (Generic, Read, Show, Eq)

instance ToJSON TriggerPosition where
      toEncoding = genericToEncoding defaultOptions

data TimestampMode      = TimestampsDisabled
                        | OnlyTicks
                        | TicksAndTimestamps
                        | TriggeredTimestamps
                        deriving (Generic, Read, Show, Eq)

instance ToJSON TimestampMode where
      toEncoding = genericToEncoding defaultOptions

data CpuObservationSettings = CpuObservationSettings {
      cosStatusTrace       :: Bool,
      cosObservationPoint  :: ObservationPoint,
      cosPtuSettings       :: PTU,
      cosDtuSettings       :: DTU
} | CpuObservationDisabled deriving (Generic, Read, Show, Eq)

instance ToJSON CpuObservationSettings where
      toEncoding = genericToEncoding defaultOptions

data PTU    = PtuDisabled
            | PTU {
                  ptuTraceMode         :: ProgramTraceMode,
                  ptuQualifierType     :: QualifierType,
                  ptuRange1            :: Range,
                  ptuRange2            :: Range,
                  ptuTrigger           :: Trigger
            } deriving (Generic, Read, Show, Eq)

instance ToJSON PTU where
      toEncoding = genericToEncoding defaultOptions

data ProgramTraceMode   = FunctionTrace
                        | FlowTrace
                        | InstructionTrace
                        deriving (Generic, Read, Show, Eq)

instance ToJSON ProgramTraceMode where
      toEncoding = genericToEncoding defaultOptions

data QualifierType      = NoRangeQualifier
                        | InRangeQualifier
                        | OutOfRangeQualifier
                        | UsePtuQualifier -- Only available for DTU
                        deriving (Generic, Read, Show, Eq)

instance ToJSON QualifierType where
      toEncoding = genericToEncoding defaultOptions

data Trigger      = TriggerDisabled
                  | Trigger {
                        tAction      :: TriggerAction,
                        tStart       :: Address,
                        tEnd         :: Address,
                        tType        :: QualifierType
                  } deriving (Generic, Read, Show, Eq)

instance ToJSON Trigger where
      toEncoding = genericToEncoding defaultOptions

data TriggerAction      = TraceRecordControl
                        | GenerateTimestamp
                        deriving (Generic, Read, Show, Eq)

instance ToJSON TriggerAction where
      toEncoding = genericToEncoding defaultOptions

data DTU    = DtuDisabled
            | DTU {
                  dtuDataAccessType    :: DataAccessType,
                  dtuQualifierType     :: QualifierType,
                  dtuRange1            :: Range,
                  dtuRange2            :: Range,
                  dtuTrigger           :: Trigger
            } deriving (Generic, Read, Show, Eq)

instance ToJSON DTU where
      toEncoding = genericToEncoding defaultOptions

data DataAccessType     = AR
                        | AW
                        | ARW
                        | ADR
                        | ADW
                        | ADRW
                        deriving (Generic, Read, Show, Eq)

instance ToJSON DataAccessType where
      toEncoding = genericToEncoding defaultOptions

data Range  = Range {
      rStart      :: Address,
      rEnd        :: Address
} | RangeDisabled deriving (Generic, Read, Show, Eq)

instance ToJSON Range where
      toEncoding = genericToEncoding defaultOptions

data ObservationPoint   = CPU0
                        | CPU1
                        | CPU2
                        | CPU3
                        | CPU4
                        | CPU5
                        | MCDS
                        | SPB
                        | SRI
                        deriving (Generic, Read, Show, Eq)

instance NFData ObservationPoint where
      rnf m = m `deepseq` ()
                  
instance ToJSON ObservationPoint where
      toEncoding = genericToEncoding defaultOptions

setRecordingTime :: TraceConfiguration -> Integer -> TraceConfiguration
setRecordingTime tc@(TraceConfiguration (GeneralSettings _ (ContinuousTrace _ _ _ _) _ _ _) _ _) x = tc { tcGeneral = (tcGeneral tc) { gsTraceType = (gsTraceType (tcGeneral tc)) { ctMaxTraceTime = x } } }
setRecordingTime tc _ = tc

setBufferSize :: TraceConfiguration -> Integer -> TraceConfiguration
setBufferSize tc@(TraceConfiguration (GeneralSettings _ (BufferedTrace _ _ _) _ _ _) _ _) x = tc { tcGeneral = (tcGeneral tc) { gsTraceType = (gsTraceType (tcGeneral tc)) { btBufferSize = x } } }
setBufferSize tc _ = tc

---------------------
-- Trace Recording --
---------------------
recordTrace :: TraceConfiguration -> IO Trace   -- IMPURE !
recordTrace cfg = do
      writeFile "cfg.json" (BSC.unpack $ encode cfg)
      readProcess ".\\run.bat" [] ""
      tc <- BS.readFile "out.ht"
      let !tr = force $ bsToTrace tc
      return tr

readTrace :: String -> IO Trace                 -- IMPURE !
readTrace p = do
      tc <- BS.readFile p
      let !tr = force $ bsToTrace tc
      return tr

-------------------------------------------
-- Basic Trace Data Processing Functions --
-------------------------------------------
inf :: Int
inf = (maxBound::Int) - 1

infix 4 |=
(|=) :: Message -> MessageType -> Bool
m |= y = (mType m) == y

infixl 9 .>
(.>) :: a -> (a -> b) -> b
a .> b = b a

infixl 9 .#
(.#) :: [a] -> (Int, Int) -> [a]
a .# (i, j) = drop i (take (j + 1) a)

infixl 9 .!
(.!) :: [a] -> Int -> a
a .! i = a !! i

toMs :: Integer -> Integer -> Double
toMs mcdsFreq ticks = (fromIntegral $ 10^3 * ticks)
                    / (fromIntegral $ mcdsFreq * 10^6)

toUs :: Integer -> Integer -> Double
toUs mcdsFreq ticks = (fromIntegral $ 10^3 * ticks)
                    / (fromIntegral $ mcdsFreq * 10^3)

---------------
-- Test Data --
---------------
lst :: [Int]
lst = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]