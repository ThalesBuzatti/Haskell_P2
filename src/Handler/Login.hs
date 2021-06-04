{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Text.Lucius

data Login = Login {loginEmail :: Text,
                    loginSenha :: Text}

formLogin :: Form Login
formLogin = renderDivs $ Login
    <$> areq textField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

getAutR :: Handler Html
getAutR = do
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Login.lucius")
        $(whamletFile "templates/Login.hamlet")

postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess (Login "root@root.com" "root") -> do
            setSession "_ID" "admin"
            redirect AdminR
        FormSuccess (Login email senha) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Nothing -> do
                    setMessage [shamlet|
                        <div class="msg" id="erro">
                            Usuário não encontrado. 
                    |]
                    redirect AutR
                Just (Entity userid usuario) -> do
                    if senha == usuarioSenha usuario then do
                        setSession "_ID" (toPathPiece userid)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <div class="msg" id="erro">
                                Verifique se o e-mail e a senha estão corretos.
                        |]
                        redirect AutR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR