module Test where

import Data.Aeson
import Ui
import Song(mainSend, songNames)
import Data.String

import qualified Data.ByteString.Lazy as LB

main = do
    let res = encode root
    LB.writeFile "../test.json" $ res
    print $ LB.length res

root = Root [win] []

win = Window "raga" Nothing tabs tabKeys

simple = ui $ Tabs [Page "1" (ui $ Dial 0.5 "olive" (0.0, 1.0)) [], Page "2" (ui $ Dial 0.5 "orange" (0.0, 1.0)) []]

tabs = setId "tabs" $ ui $ Tabs [mixPage, samPage, samVolPage, syntPage, loopPage, flowPage, tapPage, otherPage, gensPage, tracksPage, phonesPage, deleteLoopPage]

toSelf    = Msg "self"
toSampler = Msg "sampler"
toFlow    = Msg "flow"

--------------------------------------

tabKeys = 
    [ key "9" 0
    , key "0" 1
    , key "-" 3 
    , key "=" 4 
    , key "o" 5
    , key "p" 6
    , key "[" 7
    , key "]" 8
    , key "'" 9 ]
    where
        key name tabId = KeyEvent (fromString name) (sendMsg $ toSelf "/tabs" [ArgInt tabId])

--------------------------------------

mixPage = Page "mix" cont keys
    where
        mixSize = 8

        cont = multiUi (mixSize, 1) el
        el n
            | n < mixSize - 1 = ui $ Ver $ 
                [ setId (faderId n) $ setMsgs (vols n)  $ ui $ VFader 70 "olive" (1, 127)
                , setId (muteId n)  $ setMsgs (mutes n) $ ui $ Toggle False "blue" ""
                , setId (soloId n)  $ setMsgs (solos n) $ ui $ Toggle False "navy" ""
                , mixNames !! n]
            | n == (mixSize - 1) = master

        master = ui $ Ver $ 
            [ setId (faderId $ mixSize - 1) $ setMsgs masterVol  $ ui $ VFader 97 "navy" (1, 127)
            , setId (muteId  $ mixSize - 1) $ setMsgs masterMute $ ui $ Toggle False "blue" ""
            , setId (soloId  $ mixSize - 1) $ setMsgs masterSolo $ ui $ Toggle False "navy" ""
            , last mixNames ]

        faderId n = "mix" ++ show (n + 1)
        muteId  n = "mix" ++ show (n + 1) ++ "-1"
        soloId  n = "mix" ++ show (n + 1) ++ "-2"

        msg = Msg "mixer" 

        genMsg1 name n = msg ("/track/" ++ name) [ArgInt n, Arg 0]
        genMsg2 name n = msg ("/track/" ++ show (n + 1) ++  "/" ++ name) [Arg 0]

        genMsgs name n = [genMsg1 name n, genMsg2 name n]

        vols  = genMsgs "volume"
        mutes = genMsgs "mute"
        solos = genMsgs "solo"

        masterMsg name = [msg ("/master/" ++ name) [Arg 0]]
        masterVol  = masterMsg "volume"
        masterMute = masterMsg "mute"
        masterSolo = masterMsg "solo"

        keys = volKeys ++ muteKeys ++ soloKeys

        volKeys = upVolKeys ++ downVolKeys
        upVolKeys = genVolKeys 0 3.1
        
        downVolKeys = genVolKeys 1 (-3.1)
        
        muteKeys = genToggle muteId 2
        soloKeys = genToggle soloId 3

        genVolKeys rowId dv = zipWith go (take mixSize (kbRow rowId)) [0 .. ]
            where go key n = KeyEvent key (sendMsg $ toSelf ("/" ++ faderId n ++ "/add-float") [ArgFloat dv])            

        genToggle mkId rowId = zipWith go (take mixSize (kbRow rowId)) [0 .. ]
            where go key n = KeyEvent key (sendMsg $ toSelf ("/" ++ mkId n ++ "/toggle") [])

mixNames = fmap (ui . Label "blue") ["flow", "s low", "s high", "tap", "synt", "loop", "tick", "main"]        

-------------------------------------

samId = "sam"

samPage = Page "sam" (setId samId $ setSend (Send [] outKeys []) $ ui cont) kbKeys
    where
        (sizeX, sizeY) = (4, 3)
        cont = MultiToggle [] (sizeX, sizeY) "olive" []

        kbKeys = fmap hkey indices
            where hkey (m, n) = KeyEvent (kbRow n !! m) $ sendMsg $ toSelf "/sam/multi-toggle" [ArgInt m, ArgInt n]

        outKeys = indices >>= (\x -> [on x, off x])
            where
                on  = gen "true"  "play"  (\x -> [x, x])
                off = gen "false" "stop"  (\x -> [x])
                gen flag name toList (m, n) = (show m ++ " " ++ show n ++ " " ++ flag, [toSampler ('/' : name) (fmap ArgInt $ toList $ linIndex (m, n))]) 

        indices = [(x, y) | x <- [0 .. sizeX - 1], y <- [0.. sizeY - 1]]
        linIndex (x, y) = y * sizeX + x

samVolPage = Page "sam-vol" cont []
    where
        cont = multiUi (4, 3) unit
        unit n = ui $ Dial 0.5 "olive" (0, 1)

-------------------------------------

syntPage = Page "synt" cont []
    where
        cont = ui $ DoubleCheck (0, 0) (replicate 5 7) "blue" "olive" [("pad1", []), ("pad2", []), ("lead", []), ("fx", []), ("oth", [])] (Orient True False False) Nothing

------------------------------------

loopPage = Page "loop" cont keys
    where
        sizes@(sizeX, sizeY) = (5, 2)

        cont = ui $ Ver 
                [ multiUi sizes singleLoop
                , multiUi (sizeX, 1) overdubLoop 
                , multiUi (sizeX, 1) ambiLoop ]

        singleLoop  n = ui $ Hor [ startSingleLoopMsg  n $ setId (startSingleLoopId  n) $ ui $ CircleButton "orange"    , toggleLoopMsg 0 n                     $ setId (singleLoopId n)  $ ui $ Toggle True "olive" (toText n) ]
        overdubLoop n = ui $ Hor [ startOverdubLoopMsg n $ setId (startOverdubLoopId n) $ ui $ CircleToggle True  "blue", toggleLoopMsg (sizeX * sizeY) n       $ setId (overdubLoopId n) $ ui $ Toggle True "blue"  (toText n) ]
        ambiLoop    n = ui $ Hor [ startAmbiLoopMsg    n $ setId (startAmbiLoopId    n) $ ui $ CircleButton   "blue"    , toggleLoopMsg (sizeX * (sizeY + 1)) n $ setId (ambiLoopId n)    $ ui $ Toggle True "navy"  (toText n) ]
        toText n = "" --show ((n `mod` 5) `div` 2 + 1)

        singleLoopId n = "single-loop-" ++ show n
        overdubLoopId n = "overdub-loop-" ++ show n
        ambiLoopId n = "ambi-loop-" ++ show n
        startSingleLoopId n = "start-single-rec-" ++ show n
        startOverdubLoopId n = "start-overdub-rec-" ++ show n
        startAmbiLoopId n = "start-ambi-rec-" ++ show n

        startSingleLoopMsg  n = setMsg (toSampler "/start_rec" [ArgInt n])

        startOverdubLoopMsg n = onBool (msg "start_rec") (msg "stop_rec")
            where msg name = toSampler ("/" ++ name) [ArgInt (sizeX * sizeY + n)]

        startAmbiLoopMsg  n = setMsg (toSampler "/start_rec" [ArgInt $ n + sizeX * (sizeY + 1)])            

        toggleLoopMsg shift n = onBool (msg "play") (msg "stop")
            where msg name = toSampler ("/" ++ name) [ArgInt (n + shift)]

        keys = singleLoopKeys ++ overdubLoopKeys ++ ambiLoopKeys ++ startSingleLoopKeys ++ startOverdubLoopKeys ++ startAmbiLoopKeys

        singleLoopKeys          = genKeys id   togMsg 0 singleLoopId indices
        overdubLoopKeys         = genKeys id   togMsg 2 overdubLoopId indices2
        ambiLoopKeys            = genKeys id   togMsg 3 ambiLoopId indices2

        startSingleLoopKeys     = genKeys alt  butMsg 0 startSingleLoopId indices
        startOverdubLoopKeys    = genKeys alt  togMsg 2 startOverdubLoopId indices2
        startAmbiLoopKeys       = genKeys alt  butMsg 3 startAmbiLoopId indices2

        togMsg str = str ++ "/toggle"
        butMsg str = str

        genKeys addKeyModifier msgType keyRowOffset mkId ixs = fmap go ixs
            where
                go (x, y) = KeyEvent (addKeyModifier key) (sendMsg $ toSelf (msgType $ "/" ++ mkId (linIndex (x, y))) [])
                    where key = kbRow (y + keyRowOffset) !! x

        indices = [ (x, y) | x <- [0 .. sizeX-1], y <- [0 .. sizeY-1] ]
        indices2 = [ (x, y) | x <- [0 .. sizeX-1], y <- [0] ]
        linIndex (x, y) = y * sizeX + x

      
deleteLoopPage = Page "del-loop" cont keys
    where
        sizes@(sizeX, sizeY) = (5, 2)

        cont = ui $ Ver 
            [ multiUi (sizeX, sizeY) singleLoop
            , multiUi (sizeX, 1) overdubLoop 
            , multiUi (sizeX, 1) ambiLoop ]

        singleLoop  n = delSingle  n $ setId (singleId  n) $ ui $ Button "olive" ""
        overdubLoop n = delOverdub n $ setId (overdubId n) $ ui $ Button "blue"  ""
        ambiLoop    n = delAmbi    n $ setId (ambiId    n) $ ui $ Button "navy"  ""

        singleId n = "del-single-" ++ show n
        overdubId n = "del-overdub-" ++ show n
        ambiId n = "del-ambi-" ++ show n

        delSingle n = delMsg n
        delOverdub n = delMsg (n + sizeX * sizeY)
        delAmbi n = delMsg (n + sizeX * sizeY + sizeX)

        delMsg n = setMsg $ toSampler "/clear_loop" [ArgInt n]

        keys = singleKeys ++ overdubKeys ++ ambiKeys

        singleKeys = fmap (genKey 0 singleId) $ take (sizeX * sizeY) [0 .. ]
        overdubKeys = fmap (genKey (sizeX * sizeY) overdubId) $ take (sizeX) [0 .. ]
        ambiKeys = fmap (genKey (sizeX * (sizeY + 1)) ambiId) $ take (sizeX) [0 .. ]

        genKey shift mkId n = KeyEvent (alt $ kbRow y !! x) $ sendMsg $ toSelf ("/" ++ mkId n) []
            where (y, x) = divMod (shift + n) sizeX


------------------------------------

flowPage = Page "flow" cont keys
    where
        (sizeX, sizeY) = (4, 2)
        cont = multiUi (sizeX, sizeY) unit
        unit n = ui $ Ver 
                [ setId (volId n)  $ setMsg (volMsg n)             $ ui $ Dial 0.5 "blue" (0, 1)
                , setId (trigId n) $ onBool (trigOn n) (trigOff n) $ ui $ Toggle False "blue" "" ]

        -- osc messages
        volId  n = "flow-dial-"  ++ show n
        trigId n = "flow-togle-" ++ show n

        volMsg  n = toFlow "/set_volume" [ArgInt n, Arg 0]
        trigOn  n = toFlow "/resume" [ArgInt n] 
        trigOff n = toFlow "/stop"   [ArgInt n]

        -- hot keys
        keys = fmap go indices
            where go (m, n) = KeyEvent (kbRow n !! m) (sendMsg $ toSelf ("/" ++ (trigId $ linIndex (m, n)) ++ "/toggle") []) 

        indices = [(x, y) | x <- [0 .. sizeX - 1], y <- [0.. sizeY - 1]]
    
        linIndex (x, y) = y * sizeX + x

------------------------------------

tapPage = Page "tap" cont []
    where
        cont = multiUi (3, 2) unit
        unit n = ui $ Button "orange" ""

------------------------------------

otherPage = Page "other" cont []
    where
        cont = ui $ DoubleCheck (0, 0) (replicate 4 4) "olive" "blue" [("1", []), ("2", []), ("3", []), ("4", [])] (Orient True True True) Nothing

------------------------------------

gensPage = Page "gens" cont []
    where
        cont = ui $ Ver [dials, toggles]
        dials = multiUi (5, 2) (const $ ui $ Dial 0.5 "blue" (0, 1))
        toggles = multiUi (5, 2) (const $ ui $ Toggle False "olive" "")

------------------------------------

trackId = "tracks"

tracksPage = Page "tracks" (setId trackId $ setSend mainSend cont) keys
    where
        cont = ui $ DoubleCheck (0, 0) (replicate sizeX sizeY) "olive" "blue" names (Orient True False False) Nothing
        names = formNames [1 .. sizeX] songNames
        (sizeX, sizeY) = (5, 8)

        keys = firstKeys ++ secondKeys 1 ++ secondKeys 2 
        
        firstKeys = zipWith go [0 .. sizeX - 1] $ kbRow 0
            where go n key = KeyEvent key (sendMsg $ toSelf ("/" ++ trackId ++ "/double-check/1") [ArgInt n])  

        secondKeys rowId = zipWith go [0 .. 3] $ kbRow rowId
            where go n key = KeyEvent key (sendMsg $ toSelf ("/" ++ trackId ++ "/double-check/2") [ArgInt (n + 4 * (rowId - 1)) ])

        formNames sections songs = case (sections, songs) of
            ([], _) -> []
            (a:as, []) -> (sectionName a, []) : formNames as []
            (a:as, xs) -> 
                let (x1, x2) = splitAt sizeY xs
                in  (sectionName a, x1) : formNames as x2
            where
                sectionName n = "page " ++ show n

------------------------------------

phonesPage = Page "phones" cont []
    where
        cont = ui $ Ver [ui $ Label "blue" "elems", elems, ui $ Label "olive" "main", mainFader]
        elems = multiUi (4, 2) (const $ ui $ Dial 0.5 "blue" (1, 127))
        mainFader = ui $ HFader 0.5 "olive" (1, 127)

      
-------------------------
-- keyboard

kbRow n = fmap (fromString . return) $ kbStrRow n

kbStrRow 0 = "1234567890-="
kbStrRow 1 = "qwertyuiop[]"
kbStrRow 2 = "asdfghjkl;'"
kbStrRow 3 = "zxcvbnm,./"
