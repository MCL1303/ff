{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Monad (void)
import           CRDT.LamportClock (LocalTime, getRealLocalTime)
import           Data.Foldable (for_)
import qualified Data.Text as Text
import           Graphics.UI.Gtk ()
import           Graphics.UI.Gtk.Glade ()
import           System.Environment (getArgs)

import           FF (getAgenda)
import           FF.Config (Config (Config, dataDir), loadConfig)
import           FF.Storage (runStorage)

main :: IO ()
main = do
    Config{dataDir = Just dataDir} <- loadConfig
    timeVar <- newTVarIO =<< getRealLocalTime

    initGUI
    loadGlade gladepath
    mainGUI
--         mainWindow <- mkMainWindow dataDir timeVar

-- mkMainWindow :: FilePath -> TVar LocalTime -> IO QMainWindow
-- mkMainWindow dataDir timeVar = do
--     mainWindow <- QMainWindow.new
--     setCentralWidget mainWindow =<< do
--         tabs <- QTabWidget.new
--         addTab_ tabs "Agenda" =<< mkAgendaWidget dataDir timeVar
--         pure tabs
--     pure mainWindow
--
-- addTab_ :: QWidgetPtr widget => QTabWidget -> String -> widget -> IO ()
-- addTab_ tabs name widget = void $ addTab tabs widget name
--
-- mkAgendaWidget :: FilePath -> TVar LocalTime -> IO QTreeWidget
-- mkAgendaWidget dataDir timeVar = do
--     tree <- QTreeWidget.new
--     setHeaderHidden tree True
--     notes <- runStorage dataDir timeVar getAgenda
--     for_ notes $ \note ->
--         QTreeWidgetItem.newWithParentTreeAndStrings tree [Text.unpack note]
--     pure tree
