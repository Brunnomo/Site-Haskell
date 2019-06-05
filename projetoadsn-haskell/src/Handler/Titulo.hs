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
        addStylesheet $ StaticR css_bootstrap_css
        toWidget [lucius|
     
            .btn-cadas{
                color: white;
                background-color: brown;
                border: 2px solid aliceblue;
                padding: 7px;
                border-radius: 10px;
            }
        |]
        [whamlet|
            <div class="row" style="font: message-box; background-color: black;padding: 2px;">
                <h1 style="color : white; text-align: center;">
                    <img style="max-width: 200px;" src="https://2.bp.blogspot.com/-doLNF-VcX48/Vzq-MwhaJ8I/AAAAAAAADfs/QQ3NcKSrO6E3FEvd6KTIazYMKfWkmVjPgCLcB/s1600/CORINTHIANSlogo.png"> CADASTRO CORINTHIANS 
            
            <nav style="font: message-box; background-color: black; text-align: center; box-shadow: 0px 15px 10px -15px #111;">
                <ul style="min-width: 696px; list-style: none; padding-top: 20px; padding-bottom: 10px;">
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{JogadorR}>
                            CADASTRAR JOGADOR ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TodosJogadoresR}>
                            LISTAR JOGADOR ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TituloR}>
                            CADASTRAR TITULOS ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TodosTitulosR}>
                            LISTAR TITULO ||        
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{UsuarioR}>
                            CADASTRO DE USUARIO 
                    <li style="display: inline;">
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                    
            <div style="text-align: center; font: message-box;">                
                <h1 style="color : black; background-color: white !important;">
                    CADASTRAR TITULO
                <br>
                <form action=@{TituloR} method=post>
                    ^{widget}
                        <input class="btn-cadas" type="submit" value="cadastrar">
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
        addStylesheet $ StaticR css_bootstrap_css
        toWidget [lucius|
     
            .btn-cadas{
                color: white;
                background-color: brown;
                border: 2px solid aliceblue;
                padding: 7px;
                border-radius: 10px;
            }
        |]
        [whamlet|
            <div class="row" style="font: message-box; background-color: black;padding: 2px;">
                <h1 style="color : white; text-align: center;">
                    <img style="max-width: 200px;" src="https://2.bp.blogspot.com/-doLNF-VcX48/Vzq-MwhaJ8I/AAAAAAAADfs/QQ3NcKSrO6E3FEvd6KTIazYMKfWkmVjPgCLcB/s1600/CORINTHIANSlogo.png"> CADASTRO CORINTHIANS 
            
            <nav style="font: message-box; background-color: black; text-align: center; box-shadow: 0px 15px 10px -15px #111;">
                <ul style="min-width: 696px; list-style: none; padding-top: 20px; padding-bottom: 10px;">
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{JogadorR}>
                            CADASTRAR JOGADOR ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TodosJogadoresR}>
                            LISTAR JOGADOR ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TituloR}>
                            CADASTRAR TITULOS ||
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{TodosTitulosR}>
                            LISTAR TITULO ||        
                    <li style="display: inline;">
                        <a style="color:white; text-decoration: none !important;" href=@{UsuarioR}>
                            CADASTRO DE USUARIO 
                    <li style="display: inline;">
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                    
            <div style="text-align: center; font: message-box;">                
                <h1 style="color : black; background-color: white !important;">
                    ALTERAR TITULO
                <br>
                <form action=@{TituloAlteraR aluid} method=post>
                    ^{widget}
                        <input class="btn-cadas" type="submit" value="atualizar">
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