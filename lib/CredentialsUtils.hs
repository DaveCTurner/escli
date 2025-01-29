{-# LANGUAGE RecordWildCards #-}

module CredentialsUtils
    ( macOsKeyringLookupArgs
    , curlCredentialsOption
    ) where

import Config

macOsKeyringLookupArgs :: String -> Maybe String -> [String]
macOsKeyringLookupArgs serviceName maybeAccountName = ["find-generic-password", "-s", serviceName]
    ++ (maybe [] (\accountName -> ["-a", accountName]) maybeAccountName)
    ++ ["-w"]

curlCredentialsOption :: Bool -> CredentialsConfig -> String
curlCredentialsOption _                   NoCredentials = ""
curlCredentialsOption esShowCurlPassword (BasicCredentials{..})        = " --user '" ++ esCredentialsBasicUser ++ ":" ++ (if esShowCurlPassword then (esCredentialsBasicPassword ++ "'") else "'$(cat escli_config.json | jq -r .credentials.pass)")
curlCredentialsOption _                  (ApiKeyCredentials{..})       = " --header \"Authorization: ApiKey ${" ++ esCredentialsApiKeyEnvVar ++ "}\" --header \"X-Management-Request: true\""
curlCredentialsOption _                  (MacOsKeyringCredentials{..}) = " --header \"Authorization: ApiKey $(security find-generic-password"
    ++ " -s " ++ show esCredentialsKeyringService
    ++ maybe "" (\a -> " -a " ++ show a) esCredentialsKeyringAccount
    ++ " -w | jq -r .key)\" --header \"X-Management-Request: true\""

