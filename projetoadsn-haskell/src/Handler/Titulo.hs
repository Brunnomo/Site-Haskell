{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Titulo where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formTitulo :: Maybe Titulo -> Form Titulo
formTitulo mTitulo = renderBootstrap $ Titulo 
    <$> areq textField "Nome: " (fmap tituloNome mTitulo)
    <*> areq intField  "Ano: " (fmap tituloAno mTitulo)

-- ^ coloca outro html, no caso, os inputs
getTituloR :: Handler Html
getTituloR = do 
    (widget,enctype) <- generateFormPost (formTitulo Nothing)
    defaultLayout $ do
        [whamlet|
            <form action=@{TituloR} method=post>
                ^{widget}
                <input type="submit" value="cadastrar">
        |]

postTituloR :: Handler Html
postTituloR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formTitulo Nothing)
    case res of
        FormSuccess titulo -> do
            runDB $ insert titulo
            redirect TituloR
        _ -> redirect HomeR
    
-- SELECT * FROM Titulo
getTodosTitulosR :: Handler Html
getTodosTitulosR = do 
    titulos <- runDB $ selectList [] [Asc TituloNome]
    defaultLayout $(whamletFile "templates/titulo.hamlet")
    
getTodosTitulosByAnoR :: Handler Html
getTodosTitulosByAnoR = do 
    titulos <- runDB $ selectList [] [Asc TituloAno]
    defaultLayout $(whamletFile "templates/titulo.hamlet")

getTituloPerfilR :: TituloId -> Handler Html
getTituloPerfilR aluid = do 
    titulo <- runDB $ get404 aluid
    defaultLayout $ do 
        [whamlet|
            <h1>
                Titulo #{tituloNome titulo}
            <div>
                Ano: #{tituloAno titulo}
        |]

postTituloApagarR :: TituloId -> Handler Html
postTituloApagarR aluid = do
    runDB $ get404 aluid
    runDB $ delete aluid
    redirect TodosTitulosR

getTituloAlteraR :: TituloId -> Handler Html
getTituloAlteraR aluid = do
    titulo <- runDB $ get404 aluid
    (widget,enctype) <- generateFormPost (formTitulo $ Just titulo)
    defaultLayout $ do
        [whamlet|
            <form action=@{TituloAlteraR aluid} method=post>
                ^{widget}
                <input type="submit" value="atualizar">
        |]

postTituloAlteraR :: TituloId -> Handler Html
postTituloAlteraR aluid = do
    titulo <- runDB $ get404 aluid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formTitulo $ Just titulo) 
    case res of
        FormSuccess tituloNovo -> do
            runDB $ replace aluid tituloNovo
            redirect TodosTitulosR
        _ -> redirect HomeR