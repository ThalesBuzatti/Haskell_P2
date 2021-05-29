{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Text.Lucius

formUsuario :: Maybe Usuario -> Form (Usuario, Text) 
formUsuario mu = renderDivs $ (,)
    <$> (Usuario
        <$> areq textField "Nome: " (fmap usuarioNome mu)
        <*> areq textField "E-mail: " (fmap usuarioEmail mu)
        <*> areq passwordField "Senha: " (fmap usuarioSenha mu)
        )
    <*> areq passwordField "Confirme a senha: " (fmap usuarioSenha mu)

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost (formUsuario Nothing)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Cadastro.lucius")
        $(whamletFile "templates/Cadastro.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost (formUsuario Nothing)
    case result of 
        FormSuccess (usuario@(Usuario nome email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div class="msg">
                            E-mail já cadastrado.
                    |]
                    redirect UsuarioR
                Nothing -> do
                    if senha == conf then do
                        runDB $ insert usuario
{-                        setMessage [shamlet|
                            <div class="msg" id="sucesso">
                                Cadastro realizado com sucesso!
                        |]  -}
                        redirect UsuarioR
                    else do
                        redirect HomeR -- linha temporária
{-                        setMessage [shamlet|
                            <div class="msg">
                                As duas senhas são diferentes.
                        |]  -}
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
        toWidgetHead $(luciusFile "templates/usuarios.lucius")
        $(whamletFile "templates/usuarios.hamlet")
       
postApagarUsR :: UsuarioId -> Handler Html
postApagarUsR userid = do
    runDB $ delete userid
    redirect HomeR

getEditarUsR :: UsuarioId -> Handler Html
getEditarUsR userid = do
    usuario <- runDB $ get404 userid
    (widget,_) <- generateFormPost (formUsuario (Just usuario))
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Editar.lucius")
        $(whamletFile "templates/Editar.hamlet")

postEditarUsR :: UsuarioId -> Handler Html
postEditarUsR userid = do
    usuarioAntigo <- runDB $ get404 userid
    ((result,_),_) <- runFormPost (formUsuario Nothing)
    case result of 
        FormSuccess (novoUsuario@(Usuario nome email senha), conf) -> do
            usuarioExiste <- runDB $ getBy (UniqueEmail email)
            case usuarioExiste of
                Just _ -> do
                    setMessage [shamlet|
                        <div class="msg">
                            E-mail já cadastrado.
                    |]
                    redirect (EditarUsR userid)
                Nothing -> do
                    if senha == conf then do 
                        runDB $ replace userid novoUsuario
{-                        setMessage [shamlet|
                            <div class="msg" id="sucesso">
                                Dados alterados com sucesso!
                        |]  -}
                        redirect (PerfilR userid)
                    else do
                        redirect (PerfilR userid) -- linha temporária
{-                        setMessage [shamlet|
                            <div class="msg">
                                As duas senhas são diferentes.
                        |] -}
        _ -> redirect HomeR

getAdminR :: Handler Html
getAdminR = do
    defaultLayout $ do 
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/admin.hamlet")

