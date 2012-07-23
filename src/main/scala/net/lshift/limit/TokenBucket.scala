package net.lshift.limit

object BucketTools {
  def floor(x: Long): Long = math.round(math.floor(x))
}

trait Clock {
  def now: Long
}

object RealTimeClock extends Clock {
  def now = System.currentTimeMillis
}

class FakeClock extends Clock {
  var fakeTime: Long = 0L

  def now = fakeTime

  def setTime(time: Long) {
    fakeTime = time
  }
}

trait Tap {
  def drip: Long
  def adjustFrequency(freq: Long): Tap
  def adjustDropSize(tokensPerDrop: Long): Tap
}

object Tap {
  def maker: TapMaker = new TapMaker
}

class TapMaker {
  var clock: Clock = RealTimeClock
  def usingClock(clock: Clock): TapMaker = {
    this.clock = clock
    this
  }
  def make: Tap = new ClockBasedTap(clock)
}

class ClockBasedTap(clock: Clock = RealTimeClock) extends Tap {
  import BucketTools.floor

  var freq = 1000L
  var dropSize = 1L
  var lastDripTime = clock.now

  def adjustFrequency(freq: Long) = {
    this.freq = freq
    this
  }

  def adjustDropSize(tokensPerDrop: Long) = {
    this.dropSize = tokensPerDrop
    this
  }

  def drip = {
    val now = clock.now
    val deltaT = now - lastDripTime
    val volume = floor(deltaT / freq) * dropSize
    if (volume > 0) {
      lastDripTime = now
    }
    volume
  }
}

trait TokenBucket {
  def tryConsume(tokens: Long): (TokenBucket, Long)
  def deposit(tokens: Long): (TokenBucket, Long)
  def contents: Long
}

object TokenBucket {
  def maker: TokenBucketBuilder = new BasicTokenBucketBuilder
}

trait TokenBucketBuilder {
  def withTokens(contents: Long): TokenBucketBuilder
  def ofLimitedCapacity(capacity: Long): TokenBucketBuilder
  def suppliedBy(tap: Tap): TokenBucketBuilder
  def make: TokenBucket
}

class BasicTokenBucketBuilder extends TokenBucketBuilder {
  private var contents = 0L
  private var capacity = 0L
  private var tap: Option[Tap] = None

  def withTokens(contents: Long) = {
    this.contents = contents
    this
  }

  def ofLimitedCapacity(capacity: Long) = {
    this.capacity = capacity
    this
  }

  def suppliedBy(tap: Tap) = {
    this.tap = Some(tap)
    this
  }

  def make = tap match {
    case None =>
      FiniteTokenBucket(capacity = capacity, contents = contents)
    case Some(tap) =>
      RefillingFiniteTokenBucket(capacity = capacity, contents = contents, tap = tap)
  }
}

case class FiniteTokenBucket(capacity: Long = 0L, contents: Long = 0L) extends TokenBucket {
  override def deposit(tokens: Long) = if (tokens + contents <= capacity) {
    (copy(contents = contents + tokens), tokens)
  } else {
    (copy(contents = capacity), capacity - contents)
  }

  override def tryConsume(tokens: Long) = {
    if (contents >= tokens) {
      (copy(contents = contents - tokens), tokens)
    } else {
      (copy(contents = 0L), contents)
    }
  }
}

case class RefillingFiniteTokenBucket(override val capacity: Long = 0L, override val contents: Long = 0L, tap: Tap = Tap.maker.make)
  extends FiniteTokenBucket(capacity, contents) {

  override def tryConsume(tokens: Long) = {
    val (bucket, _) = deposit(tap.drip)

    if (bucket.contents >= tokens) {
      (copy(contents = bucket.contents - tokens), tokens)
    } else {
      (copy(contents = 0L), bucket.contents)
    }
  }
}
