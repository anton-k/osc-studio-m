module Song where

import Data.Default
import Data.Aeson
import qualified  Data.ByteString.Lazy.Char8 as C

import Ui

data Song = Song 
    { songName :: String
    , songFlow :: Flow
    , songSam  :: Sam 
    , songMix  :: Mix }

type Mix = [Float]

type Flow = [FlowChan]

data FlowChan = FlowChan
    { flowVol  :: Float
    , flowFile :: String
    , flowName :: String
    , flowOn   :: Bool
    , flowType :: FlowPlayType }

data FlowPlayType = FlowPlayLoop | FlowPlayOnce 

data Sam = Sam
    { samMainGain  :: Float
    , samMainBpm   :: Int
    , samChans     :: [SamChan] }

data SamChan = SamChan 
    { samVol  :: Float
    , samGain :: Float
    , samName :: String
    , samFile :: String    
    , samOn   :: Bool    
    , samPlay :: PlayType
    , samOut  :: Int }

data PlayType = Direct | Scaled { samBpm :: Int, samSpeed :: Float, samIsDrum :: Bool }

instance Default Sam where
    def = Sam 1 120 []

-------------------------------------------
-- songs

flowSong name flow = Song name flow def defMix

songs = testSongs
testSongs = [bhupali, cosmo, hams, fidel]
allSongs = [silence, intro, bhupali, cosmo, fidel, charukeshi, yaman, pinkfloyd, sakura, hams, yucatan, outro]

defMix = replicate 8 0.5

path x = "/home/anton/flow/osc-studio/" ++ x
flowPath = path . ("flow/" ++)
samPath  = path . ("sam/"  ++ )


bhupali = Song
    { songName = "bhupali"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan 
                { flowVol  = 1
                , flowFile = flowDir "bhopali-pad-G.wav"
                , flowName = "ocean"
                , flowOn   = True
                , flowType = FlowPlayLoop } ]

        sam = Sam 1 90
            [ SamChan 
                { samName = "kick"
                , samFile = samDir "stut-beat-bd-90-06.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Direct                
                , samOut  = 0 }
            , SamChan 
                { samName = "pulse"
                , samFile = samDir "stut-beat-pulse-90-06.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Direct                
                , samOut  = 1 }
            , SamChan 
                { samName = "metro"
                , samFile = samDir "complex-tambourine-pulse-120.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Scaled { samSpeed = 2, samBpm = 120, samIsDrum = True }
                , samOut  = 3 }
            ]

        mix = replicate 8 0.5

        flowDir = flowPath . ("bhupali/" ++) 
        samDir  = samPath . ("bhupali/" ++ )

cosmo = Song
    { songName = "cosmo"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan
                { flowVol  = 1
                , flowName = "Lows"
                , flowFile = flowDir "kirwani-pad-F.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan
                { flowVol  = 1
                , flowName = "Highs"
                , flowFile = flowDir "kirwani-pad-hi-F.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }                
            , FlowChan
                { flowVol  = 0.75
                , flowName = "Noise"
                , flowFile = flowDir "spacenoise1.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan
                { flowVol  = 1
                , flowName = "Vox"
                , flowFile = flowDir "appolo-problem.wav"
                , flowOn   = False
                , flowType = FlowPlayLoop }               
            ]

        sam = def

        mix = replicate 8 0.5

        flowDir = flowPath . ("cosmo/" ++) 
        

hams = Song
    { songName = "hams"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan
                { flowVol  = 1.5
                , flowName = "Pad"
                , flowFile = flowDir "hamsadhvani-pad-E.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan
                { flowVol  = 1.3
                , flowName = "Sea"
                , flowFile = flowDir "beach-pebble.wav"
                , flowOn   = False
                , flowType = FlowPlayLoop } 
            ]               

        sam = def

        mix = replicate 8 0.5

        flowDir = flowPath . ("swan/" ++) 

fidel = Song
    { songName = "fidel"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan
                { flowVol  = 2
                , flowName = "Drone"
                , flowFile = flowDir "comp-test-drone-2-E.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan
                { flowVol  = 1.5
                , flowName = "Vox"
                , flowFile = flowDir "fidel-all.wav"
                , flowOn   = False
                , flowType = FlowPlayLoop }
            ]

        sam = Sam 1 100 
            [ SamChan 
                { samName = "kick"
                , samFile = samDir "glitch-1-bd-110-04.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Scaled { samSpeed = 1, samBpm = 110, samIsDrum = True }
                , samOut  = 0 }          
            , SamChan 
                { samName = "glitch"
                , samFile = samDir "glitch-1-pulse-110.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Scaled { samSpeed = 1, samBpm = 110, samIsDrum = True }
                , samOut  = 1 } 
            , SamChan 
                { samName = "bass-light"
                , samFile = samDir "bass-100-light-06.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 0 } 
            , SamChan 
                { samName = "bass-main"
                , samFile = samDir "bass-100-main-06.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 0 }
            , SamChan 
                { samName = "tamb"
                , samFile = samDir "tambourine-pulse-120.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Scaled { samSpeed = 2, samBpm = 120, samIsDrum = True }
                , samOut  = 3 }  
            , SamChan 
                { samName = "click"
                , samFile = samDir "metro-tick-4-4-120.wav"
                , samVol  = 1
                , samGain = 1                
                , samOn   = False                
                , samPlay = Scaled { samSpeed = 1, samBpm = 120, samIsDrum = True }
                , samOut  = 3 } 
            ]

        mix = replicate 8 0.5

        flowDir = flowPath . ("fidel/" ++)
        samDir = samPath . ("fidel/" ++)

charukeshi = Song 
    { songName = "charukeshi"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan 
                { flowVol  = 0.65
                , flowName = "Drone"
                , flowFile = flowDir "tanpura-F.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            ]

        sam = Sam 1 120 
            [ SamChan 
                { samName = "drums"
                , samFile = samDir "jungle-120.wav"
                , samVol  = 1
                , samGain = 1.6                
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 0 }
            ,  SamChan 
                { samName = "bass simple"
                , samFile = samDir "bass-simple-120-F.wav"
                , samVol  = 1
                , samGain = 0.9                
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 0 }
            ,  SamChan 
                { samName = "bass simple"
                , samFile = samDir "bass-new-120-F.wav"
                , samVol  = 1
                , samGain = 0.9                
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 0 }
            ,  SamChan 
                { samName = "bass simple"
                , samFile = samDir "complex-tambourine-pulse-120.wav"
                , samVol  = 1
                , samGain = 2
                , samOn   = False                
                , samPlay = Direct
                , samOut  = 3 }
            ]

        mix = defMix

        flowDir = flowPath . ("charukeshi/" ++)
        samDir = samPath . ("charukeshi/" ++)

yaman = Song 
    { songName = "yaman"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan 
                { flowVol  = 1.4
                , flowName = "Sky"
                , flowFile = flowDir "super-comp-early-morning-3-planets-in-the-sky-long-E.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            ]

        sam = def

        mix = replicate 8 0.5

        flowDir = flowPath . ("yaman/" ++)


intro = Song
    { songName = "intro"
    , songFlow = flow
    , songSam  = sam
    , songMix  = mix }
    where
        flow = 
            [ FlowChan 
                { flowVol  = 0.3
                , flowName = "Forest"
                , flowFile = flowDir "Forest.mp3"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 0.75
                , flowName = "Sea"
                , flowFile = flowDir "Sea.mp3"
                , flowOn   = False
                , flowType = FlowPlayLoop }
            ]

        flowDir = flowPath . ("intro/" ++)
        sam = def
        mix = defMix

outro = intro { songName = "outro" }

silence = Song "silence" def def defMix

pinkfloyd = Song "pink floyd" flow sam mix
    where
        flow = 
            [ FlowChan 
                { flowVol  = 1
                , flowName = "grainy"
                , flowFile = flowDir "grainy.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "vox"
                , flowFile = flowDir "space-voices.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            ]

        flowDir = flowPath . ("pinkfloyd/" ++)            

        sam = def

        mix = defMix

sakura = Song "sakura" flow sam mix
    where
        flow =
            [ FlowChan 
                { flowVol  = 1
                , flowName = "wind"
                , flowFile = flowDir "Wind.mp3"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "fx"
                , flowFile = flowDir "sakura-fxs.mp3"
                , flowOn   = False
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "kick"
                , flowFile = flowDir "sakura-kick.wav"
                , flowOn   = False
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "piano"
                , flowFile = flowDir "passage1.wav"
                , flowOn   = True
                , flowType = FlowPlayOnce }
            ]

        flowDir = flowPath . ("sakura/" ++)

        sam = def

        mix=  defMix

yucatan = Song "yucatan" flow sam mix
    where
        flow = 
            [ FlowChan 
                { flowVol  = 1
                , flowName = "pad"
                , flowFile = flowDir "pad-A-06.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "vox"
                , flowFile = flowDir "vox.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            , FlowChan 
                { flowVol  = 1
                , flowName = "kick"
                , flowFile = flowDir "newbirds-01.wav"
                , flowOn   = True
                , flowType = FlowPlayLoop }
            ]

        flowDir = flowPath . ("yucatan/" ++)

        sam = Sam 0.75 95 [
              SamChan 
                    { samName = "tech beat"
                    , samFile = samDir "beatnew-01.wav"
                    , samVol  = 1
                    , samGain = 2.2
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 0 }            
            ,  SamChan 
                    { samName = "beat intro"
                    , samFile = samDir "beatintro-01.wav"
                    , samVol  = 1
                    , samGain = 2.2
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 0 } 
            ,  SamChan 
                    { samName = "reverse"
                    , samFile = samDir "pause-reverse-guitar-95.wav"
                    , samVol  = 1
                    , samGain = 1
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 1 }
            , SamChan 
                    { samName = "trills"
                    , samFile = samDir "trills-95.wav"
                    , samVol  = 1
                    , samGain = 1
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 1 }
            , SamChan 
                    { samName = "guitar 1"
                    , samFile = samDir "guitar-1-95.wav"
                    , samVol  = 1
                    , samGain = 0.2
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 1 }
            , SamChan 
                    { samName = "guitar 2"
                    , samFile = samDir "guitar-2-95.wav"
                    , samVol  = 1
                    , samGain = 0.2
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 1 }

            , SamChan 
                    { samName = "click"
                    , samFile = samDir "tambourine-pulse-120.wav"
                    , samVol  = 1
                    , samGain = 2
                    , samOn   = False                
                    , samPlay = Scaled { samSpeed = 2, samBpm = 120, samIsDrum = True }
                    , samOut  = 2 }

            , SamChan 
                    { samName = "bass"
                    , samFile = samDir "bassnew-01.wav"
                    , samVol  = 1
                    , samGain = 1.5
                    , samOn   = False                
                    , samPlay = Direct
                    , samOut  = 1 }
            ]

        samDir = samPath . ("yucatan/" ++)


        mix = defMix

---------------------------------------------------

printBy extract song = mapM_ (C.putStrLn . encode . toJSON) $ extract $ songToMsg song

printOns  = printBy fst
printOffs = printBy snd

songNames = fmap songName songs
mainSend = sendForSongs songIndices songs

toSelf    = Msg "self"
toSampler = Msg "sampler"
toFlow    = Msg "flow"

flowToggleId n = "flow-togle-" ++ show n
flowVolId    n = "flow-dial-"  ++ show n

samId = "sam"
samSize = (4, 3)

songIndices = [(x, y) | x <- [0 .. 4], y <- [0 .. 7]]

flowGainFactor = 2

type SongId   = (Int, Int)
type SongMsgs = ([Msg], [Msg])

songToMsg :: Song -> SongMsgs
songToMsg x = (songOn x, songOff x)

sendForSongs :: [SongId] -> [Song] -> Send
sendForSongs idx songs = Send [] ons offs
    where 
        (ons, offs) = unzip $ zipWith phi idx songs
        phi (m, n) song = ((name, ons), (name, offs))
            where 
                (ons, offs) = songToMsg song
                name = show m ++ " " ++ show n


songOn  (Song name flow sam mix) = flowMsgs ++ samMsgs ++ mixMsgs
    where
        flowMsgs = concat $ zipWith msg [0 ..] flow
            where
                msg n x  = load n x : text n x : gain n x : trig n x

                load n x = toFlow "/load" [ArgInt n, ArgString $ flowFile x, ArgInt $ fromFlowPlayType $ flowType x]

                text n x = toSelf ("/" ++ flowToggleId n ++ "/set-text") [ArgString $ flowName x]

                gain n x = toFlow ("/set_gain") [ArgInt n, ArgFloat $ flowGainFactor *  flowVol x]

                trig n x = if (flowOn x) then [toSelf ("/" ++ flowToggleId n) [ArgBool True]] else []

                fromFlowPlayType x = case x of
                    FlowPlayLoop -> 1
                    FlowPlayOnce -> 2

        samMsgs = gainMsg : bpmMsg : (concat $ zipWith msg [0 ..] (samChans sam))
            where
                gainMsg = toSampler "/set_master_gain" [ArgFloat $ samMainGain sam]

                bpmMsg  = toSampler "/set_tempo" [ArgInt $ samMainBpm sam]

                msg n x = text n x : gain n x : (trig n x ++ load n x)

                load n x = case samPlay x of
                    Direct      -> [toSampler "/load" [ArgInt n, ArgString $ samFile x, ArgInt $ samMainBpm sam, ArgBool True]]
                    spec@(Scaled _ _ _) -> [toSampler "/load" [ArgInt n, ArgString $ samFile x, ArgInt $ samBpm spec, ArgBool $ samIsDrum spec], toSampler "/set_speed" [ArgInt n, ArgFloat $ samSpeed spec]]

                text n x = toSelf ("/" ++ samId ++ "/set-text-list") [ArgInt n, ArgString $ samName x]

                gain n x = toSampler "/set_gain" [ArgInt n, ArgFloat $ samGain x]

                trig n x = if (samOn x) then [toSelf ("/" ++ samId) [ArgInt ix, ArgInt jx, ArgBool True]] else [] -- todo // is it right to set m-toggle like this ?
                    where (ix, jx) = fromSamLinIndex n

        mixMsgs = []


songOff (Song name flow sam mix) = flowMsgs ++ samMsgs ++ mixMsgs
    where
        flowMsgs = msg =<< take size [0 ..]
            where
                msg n = [text n, turnOff n]

                text n = toSelf ("/" ++ flowToggleId n ++ "/set-text") [ArgString ""]

                turnOff n = toSelf ("/" ++ flowToggleId n) [ArgBool False]

                size = length flow

        samMsgs = fadeAll : (concat $ zipWith msg [0 .. ] (samChans sam))
            where
                fadeAll = toSampler "/fade_out_and_stop_all" [ArgFloat 4.5]

                msg n x = text n : turnOff n : unspeed n x

                text n = toSelf ("/" ++ samId ++ "/set-text-list") [ArgInt n, ArgString ""]

                turnOff n = toSelf ("/cold/" ++ samId) [ArgInt ix, ArgInt jx, ArgBool False]
                    where (ix, jx) = fromSamLinIndex n

                unspeed n x = case samPlay x of
                    Direct -> []
                    spec@(Scaled _ _ _) -> [toSampler "/set_speed" [ArgInt n, ArgFloat 1]]

                size = length $ samChans sam

        mixMsgs = []

fromSamLinIndex = fromLinIndex samSize

fromLinIndex size n = (x, y)
    where
        (y, x) = n `divMod` (snd size)        
