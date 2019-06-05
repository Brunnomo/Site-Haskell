{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Confirmacao: " Nothing 
    
getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,enctype) <- generateFormPost formUsuario
    msg <- getMessage
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
            $maybe mensagem <- msg
                ^{mensagem}
                
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
                    CADASTRAR USUARIO  
                <br>                
                <form action=@{UsuarioR} method=post>
                    ^{widget}
                        <input class="btn-cadas" type="submit" value="cadastrar">
        |]

postUsuarioR :: Handler Html
postUsuarioR = do
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usuario,confirmacao) -> do
            if (usuarioSenha usuario) == confirmacao then do 
                runDB $ insert usuario
                redirect HomeR
            else do
                setMessage [shamlet|
                    <h1>
                        Usuario e senha n batem
                |]
                redirect UsuarioR
        _ -> redirect HomeR
    