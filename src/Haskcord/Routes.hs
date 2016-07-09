{-# LANGUAGE OverloadedStrings #-}
module Haskcord.Routes
    ( routeGateway
    , routeUsers
    , routeMe
    , routeRegister
    , routeLogin
    , routeLogout
    , routeGuilds
    , routeChannels
    , routeApplications
    )
where

discordBaseUrl :: String
discordBaseUrl = "https://discordapp.com/api"

routeGateway = discordBaseUrl ++ "/gateway"
routeUsers = discordBaseUrl ++ "/users"
routeMe = discordBaseUrl ++ "/@me"
routeRegister = discordBaseUrl ++ "/auth/register"
routeLogin = discordBaseUrl ++ "/auth/login"
routeLogout = discordBaseUrl ++ "/auth/logout"
routeGuilds = discordBaseUrl ++ "/guilds"
routeChannels = discordBaseUrl ++ "/channels"
routeApplications = discordBaseUrl ++ "/oauth2/applications"
