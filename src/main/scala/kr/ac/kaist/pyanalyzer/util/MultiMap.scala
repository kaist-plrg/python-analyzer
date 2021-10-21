package kr.ac.kaist.pyanalyzer.util

object MultiMap {
  type MultiMap[K, V] = Map[K, Set[V]]

  implicit class I[K, V](map: MultiMap[K, V]){
    def addOne(k: K, v: V): MultiMap[K, V] = map.get(k) match {
      case None => map + (k -> Set(v)) 
      case Some(vs) => map + (k -> (vs + v))
    }
    def addKey(k: K): MultiMap[K, V] = map.get(k) match {
      case None => map + (k -> Set())
      case Some(_) => map
    }
  }
  
  //implicit def i[K, V](map: MultiMap[K, V]): I[K, V] = I[K, V](map)
}


