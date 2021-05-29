{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Lucius
import Network.HTTP.Types.Status
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")

getPage1R :: Handler Html
getPage1R = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Pagina1.lucius")
        $(whamletFile "templates/Pagina1.hamlet")

getPage2R :: Handler Html
getPage2R = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Pagina2.lucius")
        $(whamletFile "templates/Pagina2.hamlet")

getPage3R :: Handler Html
getPage3R = do
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Pagina3.lucius")
        $(whamletFile "templates/Pagina3.hamlet")
