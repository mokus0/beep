module Network.BEEP.Profile.ChannelManagement.Output where

import Network.BEEP.Profile.ChannelManagement.Types

import qualified Data.ByteString.Lazy.Char8 as BL
import Network.URI
import Text.XML.Light

mkGreeting :: [Feature] -> [Language] -> [URI] -> BL.ByteString
mkGreeting features langs = BL.pack . showContent . greetingXml features langs

greetingXml [] [] profiles = Elem (greetingElement profiles)
greetingXml _ _ _ = error "greeting: features and languages not yet implemented"

greetingElementName = QName "greeting" Nothing Nothing
greetingElement profiles = blank_element
    { elName = greetingElementName
    , elContent = map (Elem . profileElement) profiles
    }

profileElementName = QName "profile" Nothing Nothing
profileElement uri = blank_element
    { elName = profileElementName
    , elAttribs = [uriAttr uri]
    }

uriAttrName = QName "uri" Nothing Nothing
uriAttr uri = Attr uriAttrName (uriToString id uri "")