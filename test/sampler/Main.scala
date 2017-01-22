import scala.audio.osc._

case class Sam(port: Int = 7701) {

    private val client = OscClient(port)

    private val loadChn = client.channel[(Int,String,Float,Int)]("/load")
    private val deleteChn = client.channel[Int]("/delete")

    private val playChn = client.channel[Int]("/play")
    private val stopChn = client.channel[Int]("/stop")

    private def floatChn(name: String) = client.channel[(Int,Float)](name)
    private def intChn(name: String) = client.channel[(Int,Int)](name)

    private val speedChn = floatChn("/set_speed")
    private val pitchChn = floatChn("/set_pitch")
    private val audioChn = intChn("/set_audio")
    private val ratioChn = floatChn("/set_loop_ratio")
    private val playbackTypeChn = intChn("/set_playback_type")
    private val periodChn = intChn("/set_period")
    private val offsetChn = intChn("/set_offset")
    private val playTimesChn = intChn("/set_play_times")
    private val volumeChn = floatChn("/set_volume")

    private val tempoChn = client.channel[Float]("/set_tempo")
    private val masterChn = client.channel[Float]("/set_master_volume")

    def tempo(bpm: Float) = tempoChn.send(bpm) 
    def master(vol: Float) = masterChn.send(vol)

    def load(id: Int, filename: String, bpm: Float = 120, fftSize: Int = 2048) = loadChn.send((id, filename, bpm, fftSize))
    def delete(id: Int) = deleteChn.send(id)

    def setFloat(chn: Channel[(Int,Float)])(id: Int, value: Float) = chn.send((id, value))
    def setInt(chn: Channel[(Int,Int)])(id: Int, value: Int) = chn.send((id, value))

    def speed = setFloat(speedChn) _
    def pitch = setFloat(pitchChn) _
    def audio = setInt(audioChn) _
    def ratio = setFloat(ratioChn) _
    def playbackType = setInt(playbackTypeChn) _
    def period = setInt(periodChn) _
    def offset = setInt(offsetChn) _
    def times = setInt(playTimesChn) _
    def volume = setFloat(volumeChn) _

    def play(id: Int*) = id.map(x => playChn.send(x))
    def stop(id: Int*) = id.map(x => stopChn.send(x))

    // -----------------------------------------
    // loops

    private val initLoopChn = client.channel[(Int,Int,Int,Int,Int,Int)]("/init_loop")

    def initLoop(channel: Int, id: Int, source: Int, leng: Int, loopRatio: Int, fftSize: Int = 2048) = initLoopChn.send((channel, id, source, leng, loopRatio, fftSize)) 

    private val startRecChn = client.channel[Int]("/start_rec")
    private val stopRecChn = client.channel[Int]("/stop_rec")

    def startRec(id: Int) = startRecChn.send(id)
    def stopRec(id: Int) = stopRecChn.send(id)

    private val loopTypeChn = client.channel[(Int,Int)]("/set_loop_type")

    def loopRewrite(id: Int)  = loopTypeChn.send((id, 1))
    def loopDub(id: Int)      = loopTypeChn.send((id, 0))

    private val loopAutoStopChn = client.channel[(Int,Boolean)]("/set_loop_auto_stop")

    def autoStop(id: Int, flag: Boolean) = loopAutoStopChn.send((id, flag))

}

object Test {
    val s = Sam()

    def init {
        s.initLoop(0, 0, 0, 8, 8, 2048)
        Thread.sleep(1000)
        s.startRec(0)
    }

    def init2 {
        s.load(2, "/home/anton/drums2.wav")
        s.play(2)
    }

    def init3 {
        s.load(0, "/home/anton/drums2.wav")
    }
}