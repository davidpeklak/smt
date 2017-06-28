package smt.util

trait Logger {
   def info(s: => String)
   def warn(s: => String)
   def error(s: => String)
}
