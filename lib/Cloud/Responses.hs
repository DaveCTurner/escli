{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Cloud.Responses where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL

parseJsonResponseBody :: FromJSON a => BL.ByteString -> a
parseJsonResponseBody = either error id . eitherDecode

newtype DeploymentDetails = DeploymentDetails DeploymentResourceDetails deriving (Show, Eq)

instance FromJSON DeploymentDetails where
    parseJSON = withObject "DeploymentDetails" $ \v -> DeploymentDetails <$> v .: "resources"

newtype DeploymentResourceDetails = DeploymentResourceDetails [DeploymentClusterDetails] deriving (Show, Eq)

instance FromJSON DeploymentResourceDetails where
    parseJSON = withObject "DeploymentResourceDetails" $ \v -> DeploymentResourceDetails <$> v .: "elasticsearch"

data DeploymentClusterDetails = DeploymentClusterDetails
    { deploymentClusterRefId  :: String
    , deploymentClusterId     :: String
    , deploymentClusterRegion :: String
    , deploymentClusterInfo   :: DeploymentClusterInfo
    } deriving (Show, Eq)

instance FromJSON DeploymentClusterDetails where
    parseJSON = withObject "DeploymentClusterDetails" $ \v -> DeploymentClusterDetails
        <$> v .: "ref_id"
        <*> v .: "id"
        <*> v .: "region"
        <*> v .: "info"

newtype DeploymentClusterInfo = DeploymentClusterInfo { deploymentClusterInfoName :: String } deriving (Show, Eq)

instance FromJSON DeploymentClusterInfo where
    parseJSON = withObject "DeploymentClusterInfo" $ \v -> DeploymentClusterInfo <$> v .: "cluster_name"

data HeapDumpDetails = HeapDumpDetails
    { heapDumpInstanceId  :: String
    , heapDumpSize        :: Integer
    , heapDumpType        :: String
    , heapDumpStatus      :: String
    , heapDumpCaptureTime :: String
    } deriving (Show, Eq)

instance FromJSON HeapDumpDetails where
    parseJSON = withObject "HeapDumpDetails" $ \v -> HeapDumpDetails
        <$> v .:  "instance_id"
        <*> v .:? "size" .!= 0
        <*> v .:  "type"
        <*> v .:  "status"
        <*> v .:  "captured"

data RefHeapDumps = RefHeapDumps String [HeapDumpDetails] deriving (Show, Eq)

instance FromJSON RefHeapDumps where
    parseJSON = withObject "RefHeapDumps" $ \v -> RefHeapDumps
        <$> v .: "ref_id"
        <*> v .: "heap_dumps"

newtype HeapDumpsResponse = HeapDumpsResponse [RefHeapDumps] deriving (Show, Eq)

instance FromJSON HeapDumpsResponse where
    parseJSON = withObject "HeapDumpsResponse" $ \v -> HeapDumpsResponse
        <$> v .: "elasticsearch"
