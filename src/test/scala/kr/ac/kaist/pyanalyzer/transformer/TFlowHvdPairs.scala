package kr.ac.kaist.pyanalyzer.transformer

import kr.ac.kaist.pyanalyzer._
import kr.ac.kaist.pyanalyzer.transformer._
import kr.ac.kaist.pyanalyzer.util.Useful._

object TFlowHvdPairs extends TFlowHvdPairs
class TFlowHvdPairs extends FileTransformPairs {
  val rootPath: String = s"$HOROVOD_DIR"
  val target: List[String] = List(
    "Tensorflow2/DistributedGradientTape/Simple-CNN-MNIST/org/main.py",
    "Tensorflow2/DistributedGradientTape/Simple-CNN-MNIST-2/org/tensorflow2_mnist.py",
    "Tensorflow2/DistributedGradientTape/LSTM-MNIST/org/recurrent_network.py",
  )
  val pyFiles = //target.map(relPath => s"$HOROVOD_DIR/$relPath")
    walkTree(rootPath)
      .toList.map(f => f.getPath())
      .filter(s => s.endsWith(".py"))
      .filter(s => s.contains("/org/"))

  // test name
  def makeTestName(path: String): String = path.slice(path.lastIndexOf("tensorflow-to-horovod"), path.length)

  // Converts `../org/../~~.py` path to `../hvd/../~~.py`
  // **given path is full absolute path.
  // process includes conversion to relative path wrt. rootPath
  def orgToHvdPath(path: String): String = { 
    //println(s"path $path")
    assert(path endsWith ".py")("Not a .py file")
    assert(path startsWith rootPath)("Invalid location")
    val relPath = path.drop(rootPath.length + 1)
    //assert(path contains "/org/")("Not a org")
    val hvdPath = rootPath + "/" + relPath.replaceFirst("org", "hvd") 
    hvdPath
  }

  def orgToPair(path: String): TransformPair = TransformPair(path, orgToHvdPath(path))

  def transformPairs: Iterator[(String, TransformPair)] =
    for {
      orgPath <- pyFiles.iterator
    } yield (makeTestName(orgPath), orgToPair(orgPath))
}
