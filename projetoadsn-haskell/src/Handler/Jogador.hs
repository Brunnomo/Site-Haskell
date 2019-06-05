{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Jogador where

import Import
import Database.Persist.Postgresql

-- <$>, <*> CAP 7.4 do LIVRO!
formJogador :: Maybe Jogador -> Form Jogador
formJogador mJogador = renderBootstrap $ Jogador 
    <$> areq textField "Nome: "     (fmap jogadorNome mJogador)
    <*> areq intField  "Numero: "   (fmap jogadorNumero mJogador)
    <*> areq intField  "Ano: "      (fmap jogadorAno mJogador)

-- ^ coloca outro html, no caso, os inputs
getJogadorR :: Handler Html
getJogadorR = do 
    (widget,enctype) <- generateFormPost (formJogador Nothing)
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
                    CADASTRAR JOGADOR  
                <br>    
                <form action=@{JogadorR} method=post>
                    ^{widget}
                        <br>
                        <input class="btn-cadas "type="submit" value="CADASTRAR">
        |]

postJogadorR :: Handler Html
postJogadorR = do
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formJogador Nothing)
    case res of
        FormSuccess jogador -> do
            runDB $ insert jogador
            redirect JogadorR
        _ -> redirect HomeR
    
-- SELECT * FROM Jogador
getTodosJogadoresR :: Handler Html
getTodosJogadoresR = do 
    jogadores <- runDB $ selectList [] [Asc JogadorNome]
    defaultLayout $(whamletFile "templates/jogador.hamlet")


getJogadorPerfilR :: JogadorId -> Handler Html
getJogadorPerfilR aluid = do 
    jogador <- runDB $ get404 aluid
    defaultLayout $ do 
        [whamlet|
            <h1>
                Jogador #{jogadorNome jogador}
            <div>
                Numero: #{jogadorNumero jogador}
            <div>
                Ano: #{jogadorAno jogador}
        |]

postJogadorApagarR :: JogadorId -> Handler Html
postJogadorApagarR aluid = do
    runDB $ get404 aluid
    runDB $ delete aluid
    redirect TodosJogadoresR

getJogadorAlteraR :: JogadorId -> Handler Html
getJogadorAlteraR aluid = do
    jogador <- runDB $ get404 aluid
    (widget,enctype) <- generateFormPost (formJogador $ Just jogador)
    defaultLayout $ do
        [whamlet|
            <form action=@{JogadorAlteraR aluid} method=post>
                ^{widget}
                <input type="submit" value="atualizar">
        |]

postJogadorAlteraR :: JogadorId -> Handler Html
postJogadorAlteraR aluid = do
    jogador <- runDB $ get404 aluid
    -- LE DO FORM
    ((res,_),_) <- runFormPost (formJogador $ Just jogador) 
    case res of
        FormSuccess jogadorNovo -> do
            runDB $ replace aluid jogadorNovo
            redirect TodosJogadoresR
        _ -> redirect HomeR