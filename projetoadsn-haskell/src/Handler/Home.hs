{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql

-- SHAKESPEAREAN TEMPLATES
-- whamlet => html
-- julius => javascript
-- lucius|cassius => css
-- shwo $ fromSqlKey idAluno
getHomeR :: Handler Html
getHomeR = do
    sess <- lookupSession "_ID"
    defaultLayout $ do
        -- pasta: static/css/bootstrap.css
        -- / e . sao trocados por _
        addStylesheet $ StaticR css_bootstrap_css
        toWidgetHead [julius|
        
        |]
        toWidget [lucius|
            body{
                font: message-box;
                background-image: url("https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/faf09c98-ef42-42df-b5e8-d2f55f176b87/d5v4zet-33091503-4bbe-48fc-b36e-a7f6fafb45bc.jpg/v1/fill/w_1192,h_670,q_70,strp/sport_club_corinthians_paulista___background_by_lucasamarilla_d5v4zet-pre.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9NzY4IiwicGF0aCI6IlwvZlwvZmFmMDljOTgtZWY0Mi00MmRmLWI1ZTgtZDJmNTVmMTc2Yjg3XC9kNXY0emV0LTMzMDkxNTAzLTRiYmUtNDhmYy1iMzZlLWE3ZjZmYWZiNDViYy5qcGciLCJ3aWR0aCI6Ijw9MTM2NiJ9XV0sImF1ZCI6WyJ1cm46c2VydmljZTppbWFnZS5vcGVyYXRpb25zIl19.4L5zaQOK4NEEc-R6rw1k9ccgU0NR7y-kGVCDtMlhxLk");
                background-position: top;

            }
        |]
        [whamlet|
            $maybe sessao <- sess
                <div style="background-color: black; color: white;">
                    Eai Mano #{sessao}, Salve!
            $nothing
            <div class="row" style="background-color: black;padding: 2px;">
                <h1 style="color : white; text-align: center;">
                    <img style="max-width: 200px;" src="https://2.bp.blogspot.com/-doLNF-VcX48/Vzq-MwhaJ8I/AAAAAAAADfs/QQ3NcKSrO6E3FEvd6KTIazYMKfWkmVjPgCLcB/s1600/CORINTHIANSlogo.png"> CADASTRO CORINTHIANS 
            
            
        <nav style="background-color: black; text-align: center; box-shadow: 0px 15px 10px -15px #111;">
            <ul style="min-width: 696px; list-style: none; padding-top: 20px; padding-bottom: 10px;">
                <li style="display: inline;">
                    <a style="color:white;" href=@{JogadorR}>
                        CADASTRAR JOGADOR ||
                <li style="display: inline;">
                    <a style="color:white;" href=@{TodosJogadoresR}>
                        LISTAR JOGADOR ||
                <li style="display: inline;">
                    <a style="color:white;" href=@{TituloR}>
                        CADASTRAR TITULOS ||
                <li style="display: inline;">
                    <a style="color:white;" href=@{TodosTitulosR}>
                        LISTAR TITULO ||        
                <li style="display: inline;">
                    <a style="color:white;" href=@{UsuarioR}>
                        CADASTRO DE USUARIO 
                $maybe _ <- sess 
                    <li style="display: inline;">
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Sair">
                $nothing
                    <li style="display: inline;">
                            <a class="btn btn-primary" style="color:white;margin-left: 10px; padding: 4px; background-color: brown; border-color: antiquewhite;" href=@{LoginR}>
                                ENTRAR
            
        |]

getPage1R :: Handler Html
getPage1R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 1
            
            <a href=@{HomeR}>
                Voltar
        |]

getPage2R :: Handler Html
getPage2R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 2
        |]

getPage3R :: Handler Html
getPage3R = do 
    defaultLayout $ do
        [whamlet|
            <h1>
                PAGINA 3
        |]