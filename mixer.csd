<CsoundSynthesizer>
<CsOptions>
-iadc:nil -odac:nil  -d -+rtaudio=jack  -+jack_client=mixer 
</CsOptions>
<CsInstruments>

#ifndef PORT
    #define PORT        # 8000 #
#end

#ifndef SIZE
    #define SIZE        # 8 #
#end

#ifndef CHANNEL_SIZE
    #define CHANNEL_SIZE   # 16 #
#end

sr = 44100
ksmps = 128
nchnls_i = $CHANNEL_SIZE
nchnls = $CHANNEL_SIZE
0dbfs  = 1

gihandle OSCinit $PORT

gaMasterL init 0
gaMasterR init 0

gaPhonesL init 0
gaPhonesR init 0

gkMasterVolume   init 0.75
gkMasterMute     init 0
gkMasterSolo     init 0

gkMasterPhonesVolume init 1
gkPhonesMute     init 0
gkPhonesSolo     init 0

gkVolumes[] init $SIZE
gkMutes[]   init $SIZE
gkSolos[]   init $SIZE

gkPhonesVolumes[] init $SIZE

gkSoloMode init 0

opcode Smooth, k, k
    kx xin
    ky port kx, 0.05
    xout ky    
endop 

giDbTab ftgen 0, 0, 64, -7, -70, 16, -25, 48, 7

opcode GetAmp, k,k
    kx xin
    kdb tablei (kx / 127), giDbTab, 1
    kres = ampdbfs(kdb)
    xout kres
endop

opcode GetTotalVolume, k, kkk
    kVolume, kMute, kSolo xin
    kVolume1 Smooth kVolume
    
    kMuteVolumeRaw = (kMute == 0) ? 1 : 0
    kMuteVolume Smooth kMuteVolumeRaw

    kSoloVolumeRaw = (gkSoloMode > 0 ) ? kSolo : 1
    kSoloVolume Smooth kSoloVolumeRaw

    kres = kVolume1 * kMuteVolume * kSoloVolume
    xout kres
endop

instr TrackVolumeMsg
    kVolume init 0
    kChannel init 0
    kk init 0

nxtmsg:
    kk  OSClisten gihandle, "/track/volume", "if", kChannel, kVolume
if (kk == 0 || kChannel < 0 || kChannel >= $SIZE) goto ex 
    
    kamp GetAmp kVolume    
    gkVolumes[kChannel] = kamp
    printks "set_track_volume %d %f\n", 0, kChannel, gkVolumes[kChannel]
    kgoto nxtmsg
ex:
endin

instr TrackMuteMsg
    kMute init 0
    kChannel init 0
    kk init 0  

    kk  OSClisten gihandle, "/track/mute", "ii", kChannel, kMute
    if (kk > 0 && kChannel >= 0 && kChannel < $SIZE) then
        gkMutes[kChannel] = kMute
        printks "set_track_mute %d %f\n", 0, kChannel, gkMutes[kChannel]
    endif
endin

instr TrackSoloMsg
    kSolo init 0
    kChannel init 0
    kk init 0  

    kk  OSClisten gihandle, "/track/solo", "ii", kChannel, kSolo
    if (kk > 0 && kChannel >= 0 && kChannel < $SIZE) then
        gkSolos[kChannel] = kSolo
        
        if (kSolo == 1) then
            gkSoloMode +=1
        else
            gkSoloMode -= 1
        endif

        printks "set_track_solo %d %f, solo mode: %f\n", 0, kChannel, gkSolos[kChannel], gkSoloMode
    endif
endin

instr MasterVolumeMsg
    kVolume init 0   
    kk init 0
nxtmsg:
    kk  OSClisten gihandle, "/master/volume", "f", kVolume
if (kk == 0) goto ex 
    gkMasterVolume GetAmp kVolume
    printks "set_master_volume %f\n", 0, gkMasterVolume
    kgoto nxtmsg
ex:    
endin

instr MasterPhonesVolumeMsg
    kVolume init 0   
    kk init 0
nxtmsg:
    kk  OSClisten gihandle, "/phones/volume", "f", kVolume
if (kk == 0) goto ex 
    gkMasterPhonesVolume GetAmp kVolume
    printks "set_phones_volume %f\n", 0, gkMasterPhonesVolume
    kgoto nxtmsg
ex:    
endin

instr MasterMuteMsg
    kMute init 0    
    kk init 0  

    kk  OSClisten gihandle, "/master/mute", "i", kMute
    if (kk > 0) then
        gkMasterMute = kMute
        printks "set_master_mute %f\n", 0, gkMasterMute
    endif
endin

instr MasterMuteMsg
    kMute init 0    
    kk init 0  

    kk  OSClisten gihandle, "/phones/mute", "i", kMute
    if (kk > 0) then
        gkPhonesMute = kMute
        printks "set_phones_mute %f\n", 0, gkPhonesMute
    endif
endin


instr 99
    iChannel = p4
    aL, aR  inch (2 * iChannel + 1), (2 * iChannel + 2)

    kVol GetTotalVolume gkVolumes[iChannel], gkMutes[iChannel], gkSolos[iChannel]

    gaMasterL += aL * kVol
    gaMasterR += aR * kVol
    
    kPhonesVolume Smooth gkPhonesVolumes[iChannel]
    gaPhonesL += aL * kPhonesVolume
    gaPhonesR += aR * kPhonesVolume      
endin

instr 100 
    kMasterVol GetTotalVolume gkMasterVolume, gkMasterMute, 1

    outch 1, kMasterVol * gaMasterL 
    outch 2, kMasterVol * gaMasterR

    kMasterPhonesVolume GetTotalVolume gkMasterPhonesVolume, gkPhonesMute, 1

    outch 3, kMasterPhonesVolume * gaPhonesL 
    outch 4, kMasterPhonesVolume * gaPhonesR

    gaMasterL = 0    
    gaMasterR = 0
    gaPhonesL = 0
    gaPhonesR = 0
endin

instr Init
    iCount = 0
    until (iCount == $SIZE) do
        event_i "i", 99 + iCount / 1000, 0, -1, iCount
        iCount += 1
    od

    kCount = 0 
    until (kCount == $SIZE) do
        gkVolumes[kCount] = 0.75
        gkPhonesVolumes[kCount] = 0.75
        gkMutes[kCount] = 0
        gkSolos[kCount] = 0
        kCount += 1
    od  
    gkMasterVolume = 0.75  
endin

</CsInstruments>

<CsScore>

f0 1000000

i "Init" 0 0.01

i "TrackVolumeMsg" 0 -1
i "TrackMuteMsg" 0 -1
i "TrackSoloMsg" 0 -1

i "MasterVolumeMsg" 0 -1
i "MasterMuteMsg" 0 -1
i "MasterSoloMsg" 0 -1

i "MasterPhonesVolumeMsg" 0 -1

i 100 0 -1

</CsScore>

</CsoundSynthesizer>
