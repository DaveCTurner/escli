module CredentialsUtilsSpec where

import CredentialsUtils
import Config
import Test.Hspec

spec :: Spec
spec = do
  describe "macOsKeyringLookupArgs" $ do
    it "generates the right command with just a service name" $
        macOsKeyringLookupArgs "SERVICENAME" Nothing `shouldBe` ["find-generic-password","-s","SERVICENAME","-w"]
    it "generates the right command with both service and account names" $
        macOsKeyringLookupArgs "SERVICENAME" (Just "ACCOUNTNAME") `shouldBe` ["find-generic-password","-s","SERVICENAME","-a","ACCOUNTNAME","-w"]

  describe "curlCredentialsOption" $ do
    it "works with NoCredentials" $
        curlCredentialsOption False NoCredentials
            `shouldBe` ""
    it "works with BasicCredentials hiding the password" $
        curlCredentialsOption False (BasicCredentials "USER" "SECRET")
            `shouldBe` " --user 'USER:'$(cat escli_config.json | jq -r .credentials.pass)"
    it "works with BasicCredentials showing the password" $
        curlCredentialsOption True (BasicCredentials "USER" "SECRET")
            `shouldBe` " --user 'USER:SECRET'"
    it "works with ApiKeyCredentials" $
        curlCredentialsOption False (ApiKeyCredentials "VAR")
            `shouldBe` " --header \"Authorization: ApiKey ${VAR}\" --header \"X-Management-Request: true\""
    it "works with MacOsKeyringCredentials with just a service name" $
        curlCredentialsOption False (MacOsKeyringCredentials "SERVICENAME" Nothing)
            `shouldBe` " --header \"Authorization: ApiKey $(security find-generic-password -s \"SERVICENAME\" -w)\" --header \"X-Management-Request: true\""
    it "works with MacOsKeyringCredentials with service and account names" $
        curlCredentialsOption False (MacOsKeyringCredentials "SERVICENAME" $ Just "ACCOUNTNAME")
            `shouldBe` " --header \"Authorization: ApiKey $(security find-generic-password -s \"SERVICENAME\" -a \"ACCOUNTNAME\" -w)\" --header \"X-Management-Request: true\""
