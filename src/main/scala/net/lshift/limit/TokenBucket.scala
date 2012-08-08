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
  def contents: Long
  def copyConstructor(tokens: Long): TokenBucket
  def preConsume: TokenBucket = this
  def tryConsume(tokens: Long): (TokenBucket, Long) = {
    preConsume.doConsume(tokens)
  }
  def doConsume(tokens: Long): (TokenBucket, Long)
  def deposit(tokens: Long): TokenBucket
}

object TokenBucket {
  def maker: TokenBucketBuilder = new BasicTokenBucketBuilder
}

trait TokenBucketBuilder {
  def withTokens(contents: Long): TokenBucketBuilder
  def ofLimitedCapacity(capacity: Long): TokenBucketBuilder
  def suppliedBy(tap: Tap): TokenBucketBuilder
  def relaxed: TokenBucketBuilder
  def strict: TokenBucketBuilder
  def make: TokenBucket
}

trait Strictness
case object Strict extends Strictness
case object Relaxed extends Strictness

class BasicTokenBucketBuilder extends TokenBucketBuilder {
  private var contents = 0L
  private var capacity = 0L
  private var tap: Option[Tap] = None
  private var strictness: Strictness = Relaxed

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

  def strict = {
    strictness = Strict
    this
  }

  def relaxed = {
    strictness = Relaxed
    this
  }

  def make = tap match {
    case None => strictness match {
      case Relaxed => RelaxedFiniteTokenBucket(capacity = capacity, contents = contents)
      case Strict => StrictFiniteTokenBucket(capacity = capacity, contents = contents)
    }
    case Some(tap) => strictness match {
      case Relaxed => RefillingRelaxedFiniteTokenBucket(capacity, contents, tap)
      case Strict => RefillingStrictFiniteTokenBucket(capacity, contents, tap)
    }
  }
}

trait RelaxedBucket extends TokenBucket {
  def doConsume(tokens: Long) = {
    if (contents >= tokens) {
      (copyConstructor(contents - tokens), tokens)
    } else {
      (copyConstructor(0L), contents)
    }
  }
}

trait StrictBucket extends TokenBucket {
  def doConsume(tokens: Long) = {
    if (contents >= tokens) {
      (copyConstructor(contents - tokens), tokens)
    } else {
      (copyConstructor(contents), 0L)
    }
  }
}

trait FiniteBucket extends TokenBucket {
  def capacity: Long
  def deposit(tokens: Long) = if (tokens + contents <= capacity) {
    copyConstructor(contents + tokens)
  } else {
    copyConstructor(capacity)
  }
}

case class FiniteTokenBucket(capacity: Long, contents: Long)

case class StrictFiniteTokenBucket(capacity: Long = 0L, contents: Long = 0L)
  extends FiniteBucket with StrictBucket {

  def copyConstructor(tokens: Long) = StrictFiniteTokenBucket(capacity, tokens)
}

case class RelaxedFiniteTokenBucket(capacity: Long = 0L, contents: Long = 0L)
  extends FiniteBucket with RelaxedBucket {

  def copyConstructor(tokens: Long) = RelaxedFiniteTokenBucket(capacity, tokens)
}

trait RefillingBucket extends TokenBucket {
  def tap: Tap
  override def preConsume = {
    deposit(tap.drip)
  }
}

case class RefillingStrictFiniteTokenBucket(capacity: Long, contents: Long, tap: Tap)
  extends FiniteBucket with StrictBucket with RefillingBucket {

  def copyConstructor(tokens: Long) = RefillingStrictFiniteTokenBucket(capacity, tokens, tap)
}

case class RefillingRelaxedFiniteTokenBucket(capacity: Long, contents: Long, tap: Tap)
  extends FiniteBucket with RelaxedBucket with RefillingBucket {
  
  def copyConstructor(tokens: Long) = RefillingRelaxedFiniteTokenBucket(capacity, tokens, tap)
}
