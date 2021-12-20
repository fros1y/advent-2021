import me.shadaj.scalapy.py
import me.shadaj.scalapy.py.SeqConverters
import me.shadaj.scalapy.py.writableSeqElem

@main
def test_py_loading = {
  val listLengthPython = py.Dynamic.global.len(List(1, 2, 3).toPythonProxy)
  val listLength = listLengthPython.as[Int]
  val np = py.module("numpy")
  val t = np.random.rand(10).astype(np.float32).as[Seq[Float]]
  println(t.mkString(":"))
}
object test_py {}
