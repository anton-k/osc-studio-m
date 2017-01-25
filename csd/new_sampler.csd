<CsoundSynthesizer>
<CsOptions>
-iadc:nil -odac:nil  -d -+rtaudio=jack  -+jack_client=sampler 
</CsOptions>

<CsInstruments>

sr = 44100
ksmps = 128
nchnls = 8
nchnls_i = 8
0dbfs  = 1

; ----------------------------------------------
; Global init config

#ifndef PORT
    #define PORT        # 7701 #
#end

#ifndef SIZE
    #define SIZE        # 16 #
#end

#ifndef MEMORY_SIZE
    #define MEMORY_SIZE # 64 #
#end

#ifndef LOOP_MEMORY_SIZE
    #define LOOP_MEMORY_SIZE #8#
#end

#define MONO_WAV    # 1 #  
#define STEREO_WAV  # 2 #
#define MP3         # 3 #

#define MONO_INSTR #51#
#define STEREO_INSTR #52#
#define REC_INSTR #53#

#define PLAYBACK_TYPE_DIRECT #0#
#define PLAYBACK_TYPE_FLEXIBLE #1#

#define MAX_TIME    # 1000000 #

; ----------------------------------------------
; Global settings

gkTempo         init 120
gkMasterVolume  init 1

gkTick          init 0

gihandle OSCinit $PORT

giPhaseLock init 1

gaL0            init 0
gaR0            init 0
gaL1            init 0
gaR1            init 0
gaL2            init 0
gaR2            init 0
gaL3            init 0
gaR3            init 0

giVolumeScale   init 1

;----------------------------------------------
; sample properties (memory)

giTab1s[]       init $MEMORY_SIZE  ; ftables for wavs
giTab2s[]       init $MEMORY_SIZE  ; ftables for wavs
giLens[]        init $MEMORY_SIZE
giBpms[]        init $MEMORY_SIZE
giFftSizes[]    init $MEMORY_SIZE

; TypesgkLocalTicks
; 0 - empty
; 1 - wav mono
; 2 - wav stereo
; 3 - mp3 stereo
gkTypes[]       init $MEMORY_SIZE

gkLoopRatios[]  init $LOOP_MEMORY_SIZE
gkLoopPoints[]  init $LOOP_MEMORY_SIZE
gkLoopLength[]  init $LOOP_MEMORY_SIZE
gkLoopLengthSeconds[] init $LOOP_MEMORY_SIZE
gkLoopSource[]  init $LOOP_MEMORY_SIZE
gkLoopChannel[] init $LOOP_MEMORY_SIZE

gkLoopAct[]     init $LOOP_MEMORY_SIZE
gkNextLoopAct[] init $LOOP_MEMORY_SIZE
gkLoopChange[]  init $LOOP_MEMORY_SIZE
gkLoopType[]    init $LOOP_MEMORY_SIZE
gkLoopIsAutoStop[] init $LOOP_MEMORY_SIZE

#define OVERDUB_LOOP #0#
#define REWRITE_LOOP #1#

; ------------------------------------------------
; channel properties

; momentary properties

gkVolumes[]             init $SIZE      ; volume of playback
gkOutputs[]             init $SIZE      ; audio port to output

; synced properties
gkRatios[]              init $SIZE      ; rate of change is allowed (in beats)
gkIds[]                 init $SIZE      ; ids of audio samples to play on channel
gkSpeeds[]              init $SIZE      ; speed of playback
gkPitches[]             init $SIZE      ; pitch

gkPlaybackTypes[]       init $SIZE      ; type of playback (direct - no scaling), scaled with temposcale
gkPeriods[]             init $SIZE      ; time to wait when the playback is over
gkOffsets[]             init $SIZE      ; offset to play the sample
gkPlayTimes[]           init $SIZE      ; how many times to repeat (negative repeat forever)
gkPlayLens[]            init $SIZE      ; how long the sample is to play

; expected next values
gkNextRatios[]          init $SIZE      ; rate of change is allowed (in beats)
gkNextIds[]             init $SIZE      ; ids of audio samples to play on channel
gkNextSpeeds[]          init $SIZE      ; speed of playback
gkNextPitches[]         init $SIZE      ; pitch

gkNextPlaybackTypes[]   init $SIZE      ; type of playback (direct - no scaling), scaled with temposcale
gkNextPeriods[]         init $SIZE      ; time to wait when the playback is over
gkNextOffsets[]         init $SIZE      ; offset to play the sample
gkNextPlayTimes[]       init $SIZE      ; how many times to repeat (negative repeat forever)
gkNextPlayLens[]        init $SIZE      ; how long the sample is to play

; -------
; aux props

gkChanges[]             init $SIZE

gkIsPlays[]             init $SIZE      ; is playing at the moment
gkNextIsPlays[]         init $SIZE

gkFadeTime              init 0.1

gkPlayStates[]          init $SIZE

#define WAIT_FOR_START_STATE #0#
#define PLAY_LOOP_STATE      #1#
#define WAIT_FOR_END_STATE   #2#
#define STOP_STATE           #3#

gkWaitStartCounters[]   init $SIZE
gkPlayLoopCounters[]    init $SIZE
gkWaitEndCounters[]     init $SIZE
gkPlayTimesCounters[]   init $SIZE

#define NONE_LOOP_ACT #0#
#define START_REC_LOOP_ACT #1#
#define STOP_REC_LOOP_ACT #3#

; --------------------------------------------------------------------
; Init program state

instr Init
    ii = 0
    iTab1 = 0
    iTab2 = 0
    ki init 0

    while ii < $MEMORY_SIZE do
        iTab1 ftgen 0, 0, 4, 7, 0, 4, 0
        iTab2 ftgen 0, 0, 4, 7, 0, 4, 0

        giTab1s[ii] = iTab1
        giTab2s[ii] = iTab2
        print iTab1
        print iTab2
        giLens[ii] = 0
        giBpms[ii] = 0
        giFftSizes[ii] = 2048

        ii += 1
    od 

    ii = 0
    while ii < $LOOP_MEMORY_SIZE do
        event_i "i", "RecLoop", 0, $MAX_TIME, ii
        ii += 1
    od 

    ki = 0
    while ki < $MEMORY_SIZE do
        gkTypes[ii] = 0        
        ki += 1        
    od   

    ii = 0
    while ii < $SIZE do
        event_i "i", "PlayLoop", 0, $MAX_TIME, ii
        ii += 1        
    od  

    ki = 0 
    while ki < $LOOP_MEMORY_SIZE do
        gkLoopChange[ki] = 0
        gkLoopAct[ki] = $NONE_LOOP_ACT
        gkNextLoopAct[ki] = gkLoopAct[ki]
        gkLoopRatios[ki] = 8
        gkLoopIsAutoStop[ki] = 1
        gkLoopType[ki] = $OVERDUB_LOOP
        ki += 1
    od

    ki = 0
    while ki < $SIZE do
        gkVolumes[ki] = 1
        gkOutputs[ki] = 0
        gkIsPlays[ki] = 0
        gkRatios[ki] = 8
        gkIds[ki] = ki
        gkSpeeds[ki] = 1
        gkPitches[ki] = 1
        gkPlaybackTypes[ki] = $PLAYBACK_TYPE_FLEXIBLE        
        gkPeriods[ki] = 0
        gkPlayLens[ki] = 4
        gkOffsets[ki] = 0
        gkPlayTimes[ki] = -1
        gkNextRatios[ki] = gkRatios[ki]
        gkNextIds[ki] = gkIds[ki]
        gkNextSpeeds[ki] = gkSpeeds[ki]
        gkNextPitches[ki] = gkPitches[ki]
        gkNextPlaybackTypes[ki] = gkPlaybackTypes[ki]
        gkNextIsPlays[ki] = gkIsPlays[ki]
        gkChanges[ki] = -1
        gkNextPeriods[ki] = gkPeriods[ki]
        gkNextOffsets[ki] = gkOffsets[ki]
        gkNextPlayLens[ki] = gkPlayLens[ki]
        gkNextPlayTimes[ki] = gkPlayTimes[ki]  

        gkPlayStates[ki] = $STOP_STATE

        ki += 1        
    od

    turnoff

endin

schedule "Init", 0, 0.01

; --------------------------------------------------------------------
; common opcodes

opcode IsMp3, k, S
    SFile xin
    kLen strlenk SFile
    SExt strsubk SFile, kLen - 4, kLen
    kCmp strcmpk SExt, ".mp3"
    kRes = (kCmp == 0) ? 1 : 0
    xout kRes
endop

opcode IsEmptyMemory, k, k
    kId xin     
    kres = (gkTypes[kId] == 0) ? 1 : 0
    xout kres
endop

; --------------------------------------------------------------------
; OSC messages

; Audio file API. Load, delete

instr LoadMsg
    SFile   init ""
    kId     init 0
    kBpm    init 0        
    kFftSize init 2048
    kk      init 0
    Sevt    init ""

    kk  OSClisten gihandle, "/load", "isfi", kId, SFile, kBpm, kFftSize
    if (kk > 0 && kId >= 0 && kId < $MEMORY_SIZE) then
        printks "load %d\n", 0, kId
        kIsMp3 IsMp3 SFile
        Sevt sprintfk {{i "LoadInstr" 0 0.01 "%s" %d %f %d %d}} , SFile, kId, kBpm, kIsMp3, kFftSize
        scoreline Sevt, 1
    endif
endin

instr LoadInstr
    SFile, iId, iBpm, iIsMp3, iFftSize passign 4

    iTab1 = giTab1s[iId]
    iTab2 = giTab2s[iId]

    if (iIsMp3 == 1) then
        iType = $MP3
        iDummy ftgen iTab1, 0, 0, 49, SFile, 0, 3
        iDummy ftgen iTab2, 0, 0, 49, SFile, 0, 4
        iLen   mp3len SFile
    else
        iChnls filenchnls SFile
        iLen   filelen SFile
        if (iChnls == 1) then
            iType = $MONO_WAV
            iDummy  ftgen iTab1, 0, 0, 1, SFile, 0, 0, 1            
        else
            iType = $STEREO_WAV
            iDummy  ftgen iTab1, 0, 0, 1, SFile, 0, 0, 1            
            iDummy  ftgen iTab2, 0, 0, 1, SFile, 0, 0, 2            
        endif
    endif  
    kId init iId  

    giFftSizes[iId] = iFftSize
    giLens[iId] = iLen
    giBpms[iId] = iBpm
    gkTypes[kId] = iType    
    turnoff
endin

instr DeleteMsg    
    kId     init 0
    kk      init 0
    Sevt    init ""

    kk  OSClisten gihandle, "/delete", "i", kId
    if (kk > 0 && kId >= 0 && kId < $MEMORY_SIZE) then
        printks "delete %d\n", 0, kId    
        Sevt sprintfk {{i "DeleteInstr" 0 0.01 %d}}, kId
        scoreline Sevt, 1    
    endif
endin

instr DeleteInstr
    iId = p4   
    iDummy  ftgen giTab1s[iId], 0, 4, 7, 0, 4, 0    
    iDummy  ftgen giTab2s[iId], 0, 4, 7, 0, 4, 0

    kId init iId  
    gkTypes[kId] = 0
    turnoff
endin

; Channel API: play, stop

#define SET_TRIGGER(INSTR'INSTR_STRING'OSC_PATH'VAL) #

instr $INSTR
    kChannel init 0
    kk  init 0

    kk OSClisten gihandle, $OSC_PATH, "i", kChannel
    if (kk > 0 && kChannel >= 0 && kChannel < $SIZE) then
        printks $INSTR_STRING, 0
        gkNextIsPlays[kChannel] = $VAL
        gkChanges[kChannel] = 1
    endif
    
endin

alwayson $INSTR_STRING
#

$SET_TRIGGER(PlayMsg'"PlayMsg"'"/play"'1)
$SET_TRIGGER(StopMsg'"StopMsg"'"/stop"'0)

; Set channel params

#define SET_PARAM(INSTR'INSTR_STRING'OSC_PATH'ARRAY) #

instr $INSTR
    kChannel init 0
    kVal     init 0
    kk       init 0

    kk OSClisten gihandle, $OSC_PATH, "if", kChannel, kVal
    if (kk > 0 && kChannel >= 0 && kChannel < $SIZE) then
        printks $INSTR_STRING, 0
        $ARRAY[kChannel] = kVal        
        gkChanges[kChannel] = 1
    endif
endin

alwayson $INSTR_STRING
#

#define SET_GLOBAL_PARAM(INSTR'INSTR_STRING'OSC_PATH'VAR) #

instr $INSTR
    kVal    init 0
    kk      init 0

    kk OSClisten gihandle, $OSC_PATH, "f", kVal
    if (kk > 0) then
        printks $INSTR_STRING, 0        
        $VAR = kVal
    endif
endin

alwayson $INSTR_STRING
#

$SET_PARAM(SetSpeedMsg'"SetSpeedMsg"'"/set_speed"'gkNextSpeeds)
$SET_PARAM(SetPitchMsg'"SetPitchMsg"'"/set_pitch"'gkNextPitches)
$SET_PARAM(SetAudioMsg'"SetAudioMsg"'"/set_audio"'gkNextIds)
$SET_PARAM(SetRatioMsg'"SetRatioMsg"'"/set_loop_ratio"'gkNextRatios)
$SET_PARAM(SetPlaybackTypeMsg'"SetPlaybackTypeMsg"'"/set_playback_type"'gkNextPlaybackTypes)
$SET_PARAM(SetPeriodMsg'"SetPeriodMsg"'"/set_period"'gkNextPeriods)
$SET_PARAM(SetOffsetMsg'"SetOffsetMsg"'"/set_offset"'gkNextOffsets)
$SET_PARAM(SetPlayTimesMsg'"SetPlayTimesMsg"'"/set_play_times"'gkNextPlayTimes)
$SET_PARAM(SetPlayLensMsg'"SetPlayLensMsg"'"/set_play_length"'gkNextPlayLens)

$SET_PARAM(SetVolumeMsg'"SetVolumeMsg"'"/set_volume"'gkVolumes)

$SET_GLOBAL_PARAM(SetMasterVolumeMsg'"SetMasterVolumeMsg"'"/set_master_volume"'gkMasterVolume)
$SET_GLOBAL_PARAM(SetTempoMsg'"SetTempoMsg"'"/set_tempo"'gkTempo)

alwayson "LoadMsg"
alwayson "DeleteMsg"


; Batch update
instr SetChannelAndAudioMsg 
    kk      init 0
    kChannel init 0    
    kId    init 0
    SFile  init ""
    Sevt   init ""
    kBpm init 0
    kFftSize init 2048
    kVolume init 1
    kSpeed init 1
    kPitch init 1
    kRatio init 8
    kPeriod init 0
    kOffset init 0
    kPlayTimes init (-1)
    kPlayLens init 4
    kPlaybackType init 0
    
    kk OSClisten gihandle, "/set_channel_audio_params", "iisfiffffiiiiii", kChannel, kId, SFile, kBpm, kFftSize, kVolume, kSpeed, kPitch, kRatio, kPeriod, kOffset, kPlayTimes, kPlayLens, kPlaybackType
    if (kk > 0) then
        printks "load and channel batch update %d\n", 0, kId
        kIsMp3 IsMp3 SFile
        Sevt sprintfk {{i "LoadInstr" 0 0.01 "%s" %d %f %d %d}} , SFile, kId, kBpm, kIsMp3, kFftSize
        scoreline Sevt, 1

        gkVolumes[kChannel] = kVolume
        gkNextSpeeds[kChannel] = kSpeed
        gkNextPitches[kChannel] = kPitch
        gkNextRatios[kChannel] = kRatio
        gkNextPeriods[kChannel] = kPeriod
        gkNextOffsets[kChannel] = kOffset
        gkNextPlayTimes[kChannel] = kPlayTimes
        gkNextPlayLens[kChannel] = kPlayLens
        gkNextPlaybackTypes[kChannel] = kPlaybackType
        gkChanges[kChannel] = 1
    endif
endin

; -------------------------------------------------------------------------
; engine

opcode RunDirectMono, aa, ii
    iId, iChannel xin
    iTab1 = giTab1s[iId]
    kRatio = gkSpeeds[iChannel] * gkPitches[iChannel]
    aSnd lposcil3 1, kRatio, 0, ftlen(iTab1), iTab1
    xout aSnd, aSnd    
endop 

opcode RunMono, aa, ii
    iId, iChannel xin    
    iTab1 = giTab1s[iId]
    prints "tab: %d\n", iTab1
    iFftSize = giFftSizes[iId]        
    ktimescal = gkTempo * gkSpeeds[iChannel] / giBpms[iId]
    aSnd temposcal ktimescal, gkVolumes[iChannel], gkPitches[iChannel], iTab1, giPhaseLock, iFftSize  

    ;aPtr phasor (1 / giLens[iId])
    ;aSnd table aPtr, iTab1, 1 
    xout aSnd, aSnd
endop

opcode RunDirectStereo, aa, ii
    iId, iChannel xin
    iTab1 = giTab1s[iId]
    iTab2 = giTab2s[iId]
    kRatio = gkSpeeds[iChannel] * gkPitches[iChannel]

    aSnd1 lposcil3 1, kRatio, 0, ftlen(iTab1), iTab1
    aSnd2 lposcil3 1, kRatio, 0, ftlen(iTab2), iTab2
    xout aSnd1, aSnd2

endop 

opcode RunStereo, aa, ii
    iId, iChannel xin    
    iTab1 = giTab1s[iId]
    iTab2 = giTab2s[iId]
    iFftSize = giFftSizes[iId] 
    ktimescal = gkTempo * gkSpeeds[iChannel] / giBpms[iId]  

    a1 temposcal ktimescal, 1, gkPitches[iChannel], iTab1, giPhaseLock, iFftSize   
    a2 temposcal ktimescal, 1, gkPitches[iChannel], iTab2, giPhaseLock, iFftSize   
    xout a1, a2
endop


opcode WriteOut, 0, aai
    aPreL, aPreR, iChannel xin
    kVol    port gkVolumes[iChannel], 0.05    
    kOutId  = gkOutputs[iChannel]

    aL = kVol * aPreL
    aR = kVol * aPreR    
    
    if (kOutId == 0) then
        gaL0     = gaL0 + aL
        gaR0     = gaR0 + aR    
    endif

    if (kOutId == 1) then
        gaL1     = gaL1 + aL
        gaR1     = gaR1 + aR    
    endif

    if (kOutId == 2) then
        gaL2     = gaL2 + aL
        gaR2     = gaR2 + aR    
    endif

    if (kOutId == 3) then
        gaL3     = gaL3 + aL
        gaR3     = gaR3 + aR    
    endif   
endop


opcode FracInstr, k, kk
    kInstrNum, kChannel xin
    kres = kInstrNum + ((kChannel + 1) / 1000)
    xout kres
endop

opcode GetInstrId, k, kk
    kId, kChannel xin
    if gkTypes[kId] == $MONO_WAV then
        kRes = $MONO_INSTR
    else
        kRes = $STEREO_INSTR
    endif
    kFracRes FracInstr kRes, kChannel
    xout kFracRes
endop

instr $MONO_INSTR
    prints "Mono play \n"
    print p1
    iId = p4
    iChannel = p5
    iPlaybackType = p6
    kPlaybackType = iPlaybackType

    if kPlaybackType == $PLAYBACK_TYPE_FLEXIBLE then        
        aL, aR RunMono iId, iChannel
    else        
        aL, aR RunDirectMono iId, iChannel 
    endif 

    WriteOut aL, aR, iChannel
endin

instr $STEREO_INSTR
    print p1, p4, p5
    iId = p4
    iChannel = p5
    iPlaybackType = p6

    if iPlaybackType == $PLAYBACK_TYPE_FLEXIBLE then        
         aL, aR RunStereo iId, iChannel        
    else
        aL, aR RunDirectStereo iId, iChannel        
    endif
    ; if directStereo -- add option for direct playback, fine tune by fftsize
    ;aL, aR RunDirectStereo iId
    WriteOut aL, aR, iChannel
endin


; Generates local ticks syncronized with loop ratio.
opcode GetLocalTick, k, i
    iChannel xin
    kCount  init 0
    kLocalTick init 0
    kChannel init iChannel    
    kLocalTick init 0

    kLocalTick = 0

    if gkTick == 1 then   
        if (kCount == 0) then
            kLocalTick = 1            
        endif

        kCount = kCount + 1
        if kCount == gkRatios[kChannel] then
            kCount = 0
        endif
    endif  
    xout kLocalTick  
endop

opcode UpdateSyncedParams, 0, i
    iChannel xin
    printks "Change\n", 0

    if (gkPlayTimes[iChannel] != gkNextPlayTimes[iChannel]) then
        gkPlayTimesCounters[iChannel] = gkNextPlayTimes[iChannel] - 1
    else
        gkPlayTimesCounters[iChannel] min gkPlayTimesCounters[iChannel], gkNextPlayTimes[iChannel] 
    endif

    gkSpeeds[iChannel]          = gkNextSpeeds[iChannel]
    gkPitches[iChannel]         = gkNextPitches[iChannel]
    gkIds[iChannel]             = gkNextIds[iChannel]
    gkRatios[iChannel]          = gkNextRatios[iChannel]
    gkPlaybackTypes[iChannel]   = gkNextPlaybackTypes[iChannel]
    gkPeriods[iChannel]         = gkNextPeriods[iChannel]
    gkOffsets[iChannel]         = gkNextOffsets[iChannel]
    gkPlayTimes[iChannel]       = gkNextPlayTimes[iChannel]
    gkPlayLens[iChannel]        = gkNextPlayLens[iChannel]
    gkIsPlays[iChannel]         = gkNextIsPlays[iChannel]

    kState = gkPlayStates[iChannel]
    if (kState == $WAIT_FOR_START_STATE) then
        gkWaitStartCounters[iChannel] min gkWaitStartCounters[iChannel], gkNextOffsets[iChannel]
    elseif (kState == $PLAY_LOOP_STATE) then
        gkPlayLoopCounters[iChannel] min gkPlayLoopCounters[iChannel], gkNextPlayLens[iChannel]       
    elseif (kState == $WAIT_FOR_END_STATE) then
        gkWaitEndCounters[iChannel] min gkWaitEndCounters[iChannel], gkNextPeriods[iChannel]
    endif

endop

opcode SetPlayLoopState, 0, k
    kChannel xin
    gkPlayStates[kChannel] = $PLAY_LOOP_STATE
    gkPlayLoopCounters[kChannel] = gkNextPlayLens[kChannel]
    printks "Play loop     : %d\n", 0, kChannel
endop

opcode SetStopLoopState, 0, k
    kChannel xin
    gkPlayStates[kChannel] = $STOP_STATE
    gkIsPlays[kChannel] = 0
    gkNextIsPlays[kChannel] = 0
    printks "Stop state    : %d\n", 0, kChannel    
endop

opcode SetWaitForStartState, 0, k
    kChannel xin    
    if (gkNextOffsets[kChannel] > 0) then 
        gkPlayStates[kChannel] = $WAIT_FOR_START_STATE
        gkWaitStartCounters[kChannel] = gkNextOffsets[kChannel]
        printks "Wait For Start: %d\n", 0, kChannel
    else 
        SetPlayLoopState kChannel
    endif
endop

opcode UpdatePlayTimes, 0, k
    kChannel xin
    if gkPlayTimesCounters[kChannel] == 0 then        
        SetStopLoopState kChannel
    else
        gkPlayTimesCounters[kChannel] = gkPlayTimesCounters[kChannel] - 1
        SetWaitForStartState kChannel
    endif
endop

opcode SetWaitForEndState, 0, k
    kChannel xin
    if (gkNextPeriods[kChannel] > 0) then
        gkPlayStates[kChannel] = $WAIT_FOR_END_STATE
        gkWaitEndCounters[kChannel] = gkNextPeriods[kChannel]
        printks "Wait for end  : %d\n", 0, kChannel
    else
        UpdatePlayTimes kChannel
    endif
endop


opcode OnWaitStartState, 0, k
    kChannel xin
    
    if gkWaitStartCounters[kChannel] <= 0 then
        SetPlayLoopState kChannel 
    else
        gkWaitStartCounters[kChannel] = gkWaitStartCounters[kChannel] - 1
    endif    
endop

opcode OnPlayLoopState, 0, k
    kChannel xin
    if gkPlayLoopCounters[kChannel] <= 0 then
        SetWaitForEndState kChannel
    else
        gkPlayLoopCounters[kChannel] = gkPlayLoopCounters[kChannel] - 1
    endif
endop


opcode OnWaitEndState, 0, k
    kChannel xin
    if gkWaitEndCounters[kChannel] <= 0 then
        UpdatePlayTimes kChannel
    else
        gkWaitEndCounters[kChannel] = gkWaitEndCounters[kChannel] - 1        
    endif
endop



opcode UpdateState, 0, k
    kChannel xin
    kPlayState = gkPlayStates[kChannel]

    if kPlayState == $WAIT_FOR_START_STATE then
        OnWaitStartState kChannel
    elseif kPlayState == $PLAY_LOOP_STATE then
        OnPlayLoopState  kChannel
    elseif kPlayState == $WAIT_FOR_END_STATE then
        OnWaitEndState   kChannel
    endif    
endop 

opcode OnStopLoop, 0, k
    kChannel xin     
    if (gkPlayStates[kChannel] != $STOP_STATE && gkNextIsPlays[kChannel] == 0) then           
        SetStopLoopState kChannel
    endif
endop

opcode ActStopLoop, 0, k
    kChannel xin
    kInstrId GetInstrId gkIds[kChannel], kChannel
    printks "Stop %f\n", 0, kInstrId
    turnoff2 kInstrId, 4, gkFadeTime    
endop

opcode ActStartLoop, 0, k
    kChannel xin
    kInstrId GetInstrId gkIds[kChannel], kChannel
    kNextInstrId GetInstrId gkNextIds[kChannel], kChannel
    turnoff2 kInstrId, 4, gkFadeTime    
    kIsEmpty IsEmptyMemory gkNextIds[kChannel]
    if (kIsEmpty == 0) then
        event "i", kNextInstrId, 0, -1, gkNextIds[kChannel], kChannel, gkNextPlaybackTypes[kChannel]
        printks "Start %f\n", 0, kNextInstrId
    else
        printks "Error: no audio is loaded\n", 0
        SetStopLoopState kChannel
    endif
endop

opcode SetStartPlayback, 0, k
    kChannel xin
    SetWaitForStartState kChannel
    gkPlayTimesCounters[kChannel] = gkNextPlayTimes[kChannel] - 1
endop

opcode OnStartLoop, 0, k
    kChannel xin

    if (gkNextIsPlays[kChannel] == 1 && (gkNextIds[kChannel] != gkIds[kChannel] || gkIsPlays[kChannel] == 0)) then
        printks "Start\n", 0
        SetStartPlayback kChannel
    endif
endop

opcode TrigLoops, 0, kkk
    kChannel, kPrevState, kState xin

    if (kPrevState != kState) then
        if (kState == $PLAY_LOOP_STATE) then
            ActStartLoop kChannel            
        endif

        if (kState == $STOP_STATE || kState == $WAIT_FOR_END_STATE) then
            ActStopLoop kChannel            
        endif
    endif
endop

instr PlayLoop
    iChannel = p4 
    kChannel   init iChannel 
    kStatePrev init $STOP_STATE

    kLocalTick GetLocalTick iChannel

    if (kLocalTick == 1) then
        UpdateState kChannel

        if (gkChanges[iChannel] == 1) then        
            OnStopLoop  iChannel
            OnStartLoop iChannel
            UpdateSyncedParams iChannel

            gkChanges[iChannel] = 0
        endif

        TrigLoops iChannel, kStatePrev, gkPlayStates[iChannel]
        kStatePrev = gkPlayStates[iChannel]
    endif
endin


instr TheHeart
    kTempo port gkTempo, 0.05
    gkTick metro (kTempo / 60) 
    kVol   port gkMasterVolume, 0.05    
    kAmp   = kVol * giVolumeScale
   
    aL0 = kAmp * gaL0
    aR0 = kAmp * gaR0

    aL1 = kAmp * gaL1
    aR1 = kAmp * gaR1

    aL2 = kAmp * gaL3
    aR2 = kAmp * gaR3

    aL3 = kAmp * gaL3
    aR3 = kAmp * gaR3

    outc  aL0, aR0, aL1, aR1, aL2, aR2, aL3, aR3
    gaL0 = 0
    gaR0 = 0

    gaL1 = 0
    gaR1 = 0

    gaL2 = 0
    gaR2 = 0

    gaL3 = 0
    gaR3 = 0
endin

alwayson "TheHeart"


; ------------------------------------------------------
; init loop

; Generates local ticks syncronized with loop ratio.
opcode GetLoopTick, k, i
    iChannel xin
    kCount  init 0
    kLocalTick init 0
    kChannel init iChannel    
    kLocalTick init 0

    kLocalTick = 0

    if gkTick == 1 then   
        if (kCount == 0) then
            kLocalTick = 1            
        endif

        kCount = kCount + 1
        if kCount == gkLoopRatios[kChannel] then
            kCount = 0
        endif
    endif  
    xout kLocalTick  
endop

instr InitLoop 
    iChannel, iId, iSource, iLength, iLoopRatio, iFftSize  passign 4
    SetStopLoopState iChannel

    iTab1 = giTab1s[iId]    

    iLengthPoints = round( (i(gkTempo) / (60 * 16)) * iLength * iLoopRatio * sr  )
    iDummy ftgen iTab1, 0, iLengthPoints, 7, 0, iLengthPoints, 0

    giLens[iId] = iLengthPoints / sr 
    giBpms[iId] = i(gkTempo)
    gkTypes[iId] = $MONO_WAV
    giFftSizes[iId] = iFftSize

    gkLoopRatios[iId] = iLoopRatio
    gkLoopPoints[iId] = iLengthPoints
    gkLoopLength[iId] = iLength
    gkLoopLengthSeconds[iId] = giLens[iId]  
    gkLoopSource[iId] = iSource
    gkLoopChannel[iId] = iChannel
    gkNextIds[iChannel] = iId
    gkNextPlayLens[iChannel] = iLength * iLoopRatio
    gkNextRatios[iChannel] = iLoopRatio
    turnoff    
endin

instr InitLoopMsg
    kk       init 0
    kChannel init 0
    kId      init 0
    kLength  init 0
    kSource  init 0
    kFftSize init 0
    kLoopRatio init 0

    kk OSClisten gihandle, "/init_loop", "iiiiii", kChannel, kId, kSource, kLength, kLoopRatio, kFftSize
    if (kk > 0) then
        if (kId >= 0 && kId < $LOOP_MEMORY_SIZE && kChannel >= 0 && kChannel < $SIZE) then
            event "i", "InitLoop", 0, -1, kChannel, kId, kSource, kLength, kLoopRatio, kFftSize
            printks "init loop %d %d\n", 0, kChannel, kId
        endif
    endif
endin

alwayson "InitLoopMsg"

instr ClearLoopMsg
    kk      init 0
    kId     init 0

    kk OSClisten gihandle, "/clear_loop", "i", kId
    if (kk > 0) then
        if (kId >= 0 && kId < $LOOP_MEMORY_SIZE) then
            event "i", "InitLoop", 0, -1, gkLoopChannel[kId], kId, gkLoopSource[kId], gkLoopLength[kId], gkLoopRatios[kId], giFftSizes[kId]
            printks "clear loop: %d\n", 0, kId
        endif
    endif
endin

alwayson "ClearLoopMsg"

instr StartRecLoopMsg
    kk init 0
    kId init 0

    kk OSClisten gihandle, "/start_rec", "i", kId
    if (kk > 0) then
        if (kId >= 0 && kId < $LOOP_MEMORY_SIZE) then
            printks "got start_rec %d\n", 0, kId
            gkNextLoopAct[kId] = $START_REC_LOOP_ACT
            gkLoopChange[kId] = 1
        endif
    endif
endin

alwayson "StartRecLoopMsg"

instr StopRecLoopMsg
    kk init 0
    kId init 0

    kk OSClisten gihandle, "/stop_rec", "i", kId
    if (kk > 0) then
        if (kId >= 0 && kId < $LOOP_MEMORY_SIZE) then
            printks "got stop_rec %d\n", 0, kId
            gkNextLoopAct[kId] = $STOP_REC_LOOP_ACT
            gkLoopChange[kId] = 1
        endif
    endif
endin

alwayson "StopRecLoopMsg"

opcode GetRecInstrId, k, k
    kId xin
    kInstr FracInstr $REC_INSTR, kId
    xout kInstr
endop 

#define SET_LOOP_PARAM(INSTR'INSTR_NAME'OSC_PATH'ARR_NAME) #

instr $INSTR
    kk init 0
    kId init 0
    kVal init 0

    kk OSClisten gihandle, $OSC_PATH, "ii", kId, kVal
    if (kk > 0) then
        if (kId >= 0 && kId < $LOOP_MEMORY_SIZE) then
            printks $INSTR_NAME, 0
            $ARR_NAME[kId] = kVal
        endif
    endif
endin

alwayson $INSTR_NAME
#

$SET_LOOP_PARAM(SetLoopTypeMsg'"SetLoopTypeMsg"'"/set_loop_type"'gkLoopType)
$SET_LOOP_PARAM(SetIsAutoStopMsg'"SetIsAutoStopMsg"'"/set_loop_auto_stop"'gkLoopIsAutoStop)

instr $REC_INSTR
    prints "Recording ... \n"
    iId = p4
    iDur = i(gkLoopLengthSeconds[iId])
    iSource = i(gkLoopSource[iId]) + 1
    iTab = giTab1s[iId]
    iSize = ftlen(iTab)
    aPrev init 0

    aSrc inch iSource

    aPtrNorm phasor (1 / iDur)
    aPtr = aPtrNorm * iSize
    if (gkLoopType[iId] == $OVERDUB_LOOP) then
        aPrev table aPtr, iTab
        tablew aSrc + aPrev, aPtr, iTab
    else 
        tablew aSrc, aPtr, iTab
    endif
endin

instr DoneRecInstr
    iId = p4
    iIsAutoStop = i(gkLoopIsAutoStop[iId])
    if (iIsAutoStop == 1) then
        prints "Done recording.\n"
    else 
        prints "Done recording first cycle.\n"
    endif
    kChannel = gkLoopChannel[iId]
    SetWaitForStartState kChannel
    gkNextIsPlays[kChannel] = 1
    gkNextPlayLens[kChannel] = gkLoopLength[iId]
    gkChanges[kChannel] = 1
    turnoff
endin

opcode OnStartRecAct, 0, i
    iId xin
    if (gkLoopAct[iId] == $START_REC_LOOP_ACT) then
        kRecInstrId GetRecInstrId iId
        printks "Go REC!\n", 0
        printks "params: %f \n", 0, gkLoopLengthSeconds[iId]
        if (gkLoopIsAutoStop[iId] == 1) then
            event "i", kRecInstrId, 0, gkLoopLengthSeconds[iId], iId
            event "i", "DoneRecInstr", gkLoopLengthSeconds[iId] - 0.01, -1, iId
        else
            event "i", kRecInstrId, 0, -1, iId
            event "i", "DoneRecInstr", gkLoopLengthSeconds[iId] - 0.01, -1, iId
        endif

        gkLoopAct[iId] = $NONE_LOOP_ACT
    endif
endop

opcode OnStopRecAct, 0, i
    iId xin
    if (gkLoopAct[iId] == $STOP_REC_LOOP_ACT) then
        printks "Done REC!\n", 0
        kRecInstrId GetRecInstrId iId
        turnoff2 kRecInstrId, 4, 0
        gkLoopAct[iId] = $NONE_LOOP_ACT
    endif
endop

instr RecLoop 
    iId = p4
    kLocalTick GetLoopTick iId

    if (kLocalTick == 1) then
        if (gkLoopChange[iId] == 1) then
            printks "Rec change %d\n", 0, iId
            if (gkNextLoopAct[iId] != gkLoopAct[iId]) then
                gkLoopAct[iId] = gkNextLoopAct[iId]

                OnStartRecAct iId
                OnStopRecAct  iId
            endif

            gkLoopChange[iId] = 0
        endif
    endif    
endin


</CsInstruments>

<CsScore>
</CsScore>
</CsoundSynthesizer>
