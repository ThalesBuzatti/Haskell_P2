{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import

formUsuario :: Form Usuario 
formUsuario = renderDivs $ Usuario
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Email: " Nothing
    <*> areq textField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
    (widget,_) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $
        [whamlet|
            $maybe mensa <- msg 
                <div>
                    ^{mensa}
            <h1>
                CADASTRO DE USUARIO
                
            <form method=post action=@{UsuarioR}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]

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
getPerfilR user = do
    usuario <- runDB $ get404 cid
    defaultLayout [whamlet|
          <h1>
              PAGINA DE #{usuarioNome usuario}
              
          <h2>
              Email: #{usuarioEmail usuario}
              
         <h2> 
              Senha: #{usuarioSenha usuario}
    |]
    
getListaUserR :: Handler Html
getListaUserR = do
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do
        $(whamletFile "templates/Usuarios.hamlet")
        
postApagarUserR :: UsuarioId -> Handler Html
postApagarUsR cid = do
    runDB $ delete cid
    redirect ListaUsR
