package libext
import spinal.core._
package object memext {
  implicit def MemExpand[T <: Data](mem: Mem[T]) : MemExpand[T] = new MemExpand(mem)
}
