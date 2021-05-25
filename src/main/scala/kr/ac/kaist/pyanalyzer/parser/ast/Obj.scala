package kr.ac.kaist.pyanalyzer.parser.ast

// Type hierarchy.
sealed trait Value

case object VNone extends Value
case object VNotImpl extends Value
case object VEllipsis extends Value

sealed trait VNum extends Value
case class VInt(i: Int) extends VNum
case class VBool(b: Boolean) extends VNum
case class VReal(f: Double) extends VNum
case class VComp(real: Double, imag: Double) extends VNum

sealed trait VSeq extends Value
case class VStr(s: String) extends VSeq
case class VTup(l: List[Obj]) extends VSeq
case class VBytes(b: List[Byte]) extends VSeq
case class VList(l: List[Obj]) extends VSeq
case class VBArr(b: List[Byte]) extends VSeq

case class VSet(s: Set[Obj]) extends Value
// TODO VMap key should reject mutable objects
case class VDict(m: Map[Obj, Obj]) extends Value

// TODO Below needs precise modeling on map
case class VFun(name: String) extends Value
case class VModule(name: String) extends Value
case class VCLass(name: String) extends Value
case class VInstance(name: String) extends Value

// TODO should model Internal objects?

// Object
case class Obj(id: Int, value: Value, mut: Boolean, map: Map[String, Obj] = Map.empty()) 

// companion object for defining some constructors
object Obj {
  def simpleAllocator(v: Value): Int => Obj = (id: Int) => Obj(id, v, false)  
  // TODO primitive objects like None, True, False, Integers 0~256 have fixed id.
  def ONone: Int => Obj = simpleAllocator(VNone)
  def ONotImpl: Int => Obj = simpleAllocator(VNotImpl)
  def OEllipsis: Int => Obj = simpleAllocator(VEllipsis)
  def OInt(i: Int): Int => Obj = simpleAllocator(VInt(i))
  def OFalse: Int => Obj = simpleAllocator(VBool(false))
  def OTrue: Int => Obj = simpleAllocator(VBool(true))
  def OReal(f: Double): Int => Obj = simpleAllocator(VReal(f)) 
  def OComp(r: Double, i: Double): Int => Obj = simpleAllocator(VComp(r, i))
  def OStr(s: String): Int => Obj = simpleAllocator(VStr(s))
  def OTup(l: List[Obj]): Int => Obj = simpleAllocator(VTup(l))
  def OBytes(b: List[Byte]): Int => Obj = simpleAllocator(VBytes(b))
  def OFrozenSet(s: Set[Obj]): Int => Obj = simpleAllocator(VSet(s))

  def mutableAllocator(v: Value): Int => Obj = (id: Int) => Obj(id, v, true)
  def OList(l: List[Obj]): Int => Obj = mutableAllocator(VList(l))
  def OBArr(b: List[Byte]): Int => Obj = mutableAllocator(VBArr(b))
  def OSet(s: Set[Obj]): Int => Obj = mutableAllocator(VSet(s))
  def ODict(m: Map[Obj, Obj]): Int => Obj = mutableAllocator(VDict(m))

  // TODO what id should i give?
  // slice object
}
