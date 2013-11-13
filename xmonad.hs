{-# LANGUAGE OverloadedStrings #-}

import XMonad 
import XMonad.Actions.DwmPromote
import XMonad.Config.Gnome
import XMonad.Layout.NoBorders
import XMonad.ManageHook
import XMonad.Hooks.FloatNext
import XMonad.Hooks.FadeInactive
import XMonad.Actions.GridSelect
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Loggers
import XMonad.Layout.Circle
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Grid
import XMonad.Prompt
import XMonad.Prompt.Window
import Control.Monad (filterM,liftM, join)
import Control.Arrow (first)
import Data.IORef
import Data.List
import Data.Maybe (fromJust)             
import qualified Data.Set as S
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig(additionalKeys,removeKeys)
import System.Environment
import System.IO
import Data.Functor
import Network.Socket
import XMonad.Util.Run(spawnPipe)
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Time
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops


--writeLog :: String -> IO ()
--writeLog message = do
--  client <- DB.mkSessionClient
--  let xNote = DB.blankNote{DB.appName="XMonadLog"
--                            ,DB.summary="Layout Update"
--                            ,DB.body=(Just $ DB.Text message)}
--  DB.notify client xNote
--  return ()

myPrettyPrinter :: D.Client -> PP
myPrettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

myFadeHook toggleFadeSet = fadeOutLogHook $ fadeIf (testCondition toggleFadeSet) 0.5
doNotFadeOutWindows = title =? "Call with " <||> className =? "xine" <||> className =? "MPlayer"

testCondition :: IORef (S.Set Window) -> Query Bool
testCondition floats =
    liftM not doNotFadeOutWindows <&&> isUnfocused
    <&&> (join . asks $ \w -> liftX . io $ S.notMember w `fmap` readIORef floats)

toggleFadeOut :: Window -> S.Set Window -> S.Set Window
toggleFadeOut w s | w `S.member` s = S.delete w s
                  | otherwise = S.insert w s


--myLogHook = dynamicLogWithPP $ defaultPP
--    { ppOutput = writeLog
--    }




myManageHook :: [ManageHook]
myManageHook = 
  [className =? "Synapse" --> doFloat
    , className =? "Google-chrome" --> doFloat
    , className =? "ij-ImageJ" --> doFloat
    , className =? "News" --> doFloat
  ]       

main = do
  _ <- spawnPipe "xcompmgr -t-5 -l-5 -r4.2 -o.55 -f -F & disown"
  logFile <- openFile "/home/zaki/xmonad.log" AppendMode
  dbusClient<- D.connectSession
  getWellKnownName dbusClient
  toggleFadeSet <- newIORef S.empty
  xmonad $ ewmh $ withNavigation2DConfig defaultNavigation2DConfig $ gnomeConfig
    { normalBorderColor    = "#FFFFFF"
      , focusedBorderColor = "#300A24"
      , borderWidth        = 3    
      , manageHook = manageHook gnomeConfig <+> floatNextHook <+> composeAll myManageHook
      , layoutHook = avoidStruts(Full ||| Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)) ||| circleSimpleDefaultResizable ||| Grid) 
      , logHook = dynamicLogWithPP (myPrettyPrinter dbusClient)  >> myFadeHook toggleFadeSet >>  dynamicLogWithPP (defaultPP { ppOutput = hPutStrLn logFile , ppExtras = [date "%c" ] })
      , modMask = mod4Mask -- set the mod key to the windows key 
      , handleEventHook    = fullscreenEventHook
    } `additionalKeys`
    [ ((mod4Mask, xK_g), goToSelected defaultGSConfig)--Gridselect
    , ((mod4Mask, xK_b), sendMessage ToggleStruts)
    , ((mod4Mask, xK_f), withFocused $ io . modifyIORef toggleFadeSet . toggleFadeOut) --Toggle Panels
    , ((mod4Mask, xK_e), toggleFloatNext)
    , ((mod4Mask, xK_r), toggleFloatAllNew)
    , ((mod4Mask, xK_Return), dwmpromote)
    , ((mod4Mask, xK_s), spawn "python /home/zaki/sphinxkeys/sphinxkeys.py")
    , ((mod4Mask .|. shiftMask, xK_g     ), windowPromptGoto defaultXPConfig { autoComplete = Just 500000 } )
    , ((mod4Mask .|. shiftMask, xK_b     ), windowPromptBring defaultXPConfig { autoComplete = Just 500000 } )
    ] `removeKeys`
    ([(mod4Mask, xK_m)])


