{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Database.Persist.Postgresql

formLogin :: Form Usuario
formLogin = renderBootstrap $ Usuario 
        <$> areq emailField "E-mail:" Nothing
        <*> areq passwordField "Senha: " Nothing
    
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
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
                margin-top: 5px;
            }
            .div-login-externa {
                align-items: center;
                display: flex;
                flex-direction: row;
                flex-wrap: wrap;
                justify-content: center;
            }
            .div-login-interna{
                background-color: black;
                padding: 20px;
                border: 2px solid #ccc;
                border-radius: 20px;
                color: white;
                text-align: center;
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
                    LOGIN 
            <br>
            <div class="div-login-externa">
                <div class="div-login-interna">
                    <form action=@{LoginR} method=post>
                        ^{widget}
                            <input class="btn-cadas" type="submit" value="entrar">
        |]

postLoginR :: Handler Html
postLoginR = do
    ((res,_),_) <- runFormPost formLogin
    case res of
        FormSuccess (Usuario "root@root123.com" "root") -> do 
            setSession "_ID" "root"
            redirect AdminR
        FormSuccess usuario -> do
            usuBanco <- runDB $ getBy $ UniqueRestEmail (usuarioEmail usuario)
            case usuBanco of 
                Just usuarioValido -> do 
                    if ((usuarioSenha usuario) == (usuarioSenha $ entityVal usuarioValido)) then do 
                        setSession "_ID" (usuarioEmail $ entityVal usuarioValido)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            <h1>
                                Senha invalida
                        |]
                        redirect LoginR
                        
                Nothing -> do
                    setMessage [shamlet|
                        Usuario n encontrado
                    |]
                    redirect LoginR
        _ -> redirect HomeR
    
postLogoutR :: Handler Html
postLogoutR = do 
    deleteSession "_ID"
    redirect HomeR
    
getAdminR :: Handler Html
getAdminR = do 
    defaultLayout $ do 
        [whamlet|
            <h1>
                BEM-VINDO MEU REI!!!
        |]