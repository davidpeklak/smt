package smt

import scalaz.concurrent.{Future, Task}
import scalaz.\/

class WTask[+W, +A](val get: Future[(W, Throwable \/ A)]) {

}
