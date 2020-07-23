module HStore.Events where

import Data.Aeson

parseEvent ::
  (FromJSON e) => Value -> Either String e
parseEvent val =
  case fromJSON val of
    Success e -> Right e
    Error e -> Left e
