{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Receita where

import Import
import Text.Lucius
import Database.Persist.Postgresql

formReceita :: UsuarioId -> Form Receita 
formReceita userid = renderDivs $ Receita 
    <$> pure userid
    <*> areq textField "Nome: " Nothing
    <*> areq textField "Ingredientes: " Nothing
    <*> areq textField "Modo de Preparo: " Nothing  

getEnviarReceitaR :: UsuarioId -> Handler Html
getEnviarReceitaR userid = do
    (widget,_) <- generateFormPost (formReceita userid)
    msg <- getMessage
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/EnviarReceita.lucius")
        $(whamletFile "templates/EnviarReceita.hamlet")

postEnviarReceitaR :: UsuarioId -> Handler Html
postEnviarReceitaR userid = do
    ((result,_),_) <- runFormPost (formReceita userid)
    case result of 
        FormSuccess receita -> do
            runDB $ insert receita
            setMessage [shamlet|
                <div class="msg">
                    Sua receita foi enviada!
            |]
            redirect (EnviarReceitaR userid)
        _ -> redirect HomeR

getReceitaR :: ReceitaId -> Handler Html
getReceitaR receitaid = do
    let sql = "SELECT ??,?? FROM RECEITA \
          \ INNER JOIN USUARIO ON USUARIO.ID = RECEITA.USUARIOID \
          \ WHERE RECEITA.ID = ?"
    receita <- runDB $ get404 receitaid
    mostrarReceita <- runDB $ rawSql sql [toPersistValue receitaid] :: Handler [(Entity Receita, Entity Usuario)]
    defaultLayout $ do
        usuario <- lookupSession "_ID"
        toWidgetHead $(luciusFile "templates/Receita.lucius")
        $(whamletFile "templates/Receita.hamlet")


