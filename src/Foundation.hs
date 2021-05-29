{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static 
    , appConnPool    :: ConnectionPool 
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance Yesod App where
    makeLogger = return . appLogger

    authRoute _ = Just AutR

    isAuthorized HomeR _ = return Authorized
    isAuthorized Page1R _ = return Authorized
    isAuthorized Page2R _ = return Authorized
    isAuthorized Page3R _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AutR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized ListaUsR _ = isAdmin

    isAuthorized _ _ = isUsuario

isUsuario :: Handler AuthResult
isUsuario = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just _ -> return Authorized

isAdmin :: Handler AuthResult
isAdmin = do
    sess <- lookupSession "_ID"
    case sess of
        Nothing -> return AuthenticationRequired
        Just "admin" -> return Authorized
        Just _ -> return (Unauthorized "Não autorizado")

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager
