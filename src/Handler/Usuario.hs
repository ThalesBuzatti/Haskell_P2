{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Text.Lucius

formUsuario :: Form Usuario 
formUsuario = renderDivs $ Usuario
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Email: " Nothing
    <*> areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/Cadastro.lucius")
        $(whamletFile "templates/Cadastro.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsuario
    case result of 
         FormSuccess usuario -> do
             runDB $ insert usuario
             setMessage [shamlet|
                 <div>
                      USUARIO INCLUIDO
             |]
             redirect UsuarioR
         _ -> redirect HomeR
         
getPerfilR :: UsuarioId -> Handler Html
getPerfilR userid = do
    usuario <- runDB $ get404 userid
    defaultLayout $ do
        toWidgetHead $(luciusFile "templates/Perfil.lucius")
        $(whamletFile "templates/Perfil.hamlet")
    
getListaUsR :: Handler Html
getListaUsR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do
        $(whamletFile "templates/usuarios.hamlet")
        
postApagarUsR :: UsuarioId -> Handler Html
postApagarUsR userid = do
    runDB $ delete userid
    redirect ListaUsR
